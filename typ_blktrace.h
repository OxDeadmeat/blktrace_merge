/*
 * Previous model build an io around (the first) event, adding parts as they were discovered.  So alloc "event" (io)
 * than add to it.  New model is create events as separate entity, and promote the collection of events as an io
 * only when sure its a standalong io.
 */

/* 
 * History:
 * 01.18.2021 + chg: add xparent io pointer to parent, add xsplit_level int value to track the xsplits off
 *                   the original Q parent.  Q parent is level 0, next 1, next 2, etc.
 * 01.18.2021 + chg: add SPCFLG_XPARENT for the main io targer of a X-split
 * 05/20/2021 + cmt: note that post the below change, 'timeinfo' is more 'lineinfo' as it has the common 
 *                   no io pieces of data, line number, time, cpu, pid, (blktrace) seqnum. Just fyi that sometime
 *                   in the future we may want to change the name to 'lineinfo' but in a way that is v4 and v5
 *                   compliant!
 * 05/20/2021 + chg: need to cleanup and expand things at the same time.  We'll move pid, cpu, seqnum to the
 *                   typ_action data structure where input line number and timestamp lives. Then create and
 *                   typ_action_ext data structure which includes that .PLUS. some extended tracking per 
 *                   event type that we need for the "csv" extension.  Right now that data is scattered
 *                   around, and since it is grounded in the specific event itself, pulling it into the 
 *                   typ_action_ext structure makes sense -- it declutters other parts of ioinfo, for example,
 *                   and places the data in one context nexus.  The action_ext is then used instead of the 
 *                   action structure... but only in ioinfo->events.  The typ_event data structure is unchanged
 *                   as it still used typ_action, although fields in typ_event have been pulled back into 
 *                   typ_action as we need these fields in the per event structure, but we need to track them
 *                   separately per action in ioinfo for csv.  Right now these fields are outside of the action
 *                   structure; pid, cpu, seqnum.  These are input file values and we want at least cpu available
 *                   for Q,C if not also for D.  But Q,C are a must; who issued, who processed completion interrupt.
 */
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

/* 
 * What is a "backfill" and why is it needed?
 *
 * A backfill happens when an event is encountered "out of sequence" and previous key times and actions that
 * would have been taken at those key times need to be synthesized -- essentially backfilling missing information.
 *
 * For example, we encounter an 'I'nsert but cannot match it to any typ_io on the staging queue (looking for
 * (A)->Q->(S)->(G) event -- that is a 'S' and 'G' event will push/move io from staging to active list, but
 * some kernel versions won't have the (G) event present and instead are just Q->I) nor is an io found
 * on the active list that matches this io type (R/W) and lba+length values.  In this case a new io has to be
 * created, but it needs at least an 'Q' event for later on computations, so we backfill the missing Q time
 * with the current I time.  The event Q has no line number assigned so we know this is a synthesized value
 * if we need or care to know that information.  Since the Q was synthesized, a QSeqNo needs to be assigned,
 * so we grab one of those(?) but of course that will make the ioschedule reordering calculations a bit wonky
 * at this point until this io moves through and out of the active list, but really can't be helped.
 *
 * Backfilling should only be required need the front/beginning of the event stream read in from blkparse.
 * However, it can happen later for two very different reasons:
 *    1) the program has a flaw, or
 *    2) the event stream has a flaw (corrupted or missing/dropped events).
 *
 * The latter can happen while blkparse is running and the binary blktrace input stream is flawed and/or
 * as the blkparse is running disk space is exhausted and space restored while the program is running.
 * And while the latter can be detected and avoided, the former cannot -- its baked into the binary data
 * available.
 */

/*
 * For purposes of this program, event event has
 *     . an "action", and   (evtact, prmact)
 *     . a  "type".         (evttyp, prmtyp)
 *
 * There are event "action" and event "type", and event "action" is Queue, Insert, et.al. where as the event "type"
 * is R or WS et.al.  "action" what its doing with the typ, and the typ describes the "io" being manipulated.
 *
 * Also these actions and types can be distilled into a primary action/type.  
 *     . action: most action strings are 1 char in length so the primary action and full action string are
 *               one in the same.  The only known exception to this is the 'UT' action, unplug due to timeout.
 *               In this one case, the primary action is set to 'T'.  The program always currently expects no
 *               single 'T' event and also only expects the only non-single character action to be 'UT'.  If either
 *               of those prove to be incorrect, the event is flagged as an exception and not processed.
 *               The primary action is an ascii character (Q|S|M|...).
 *     . type  : for example, W, WS, WMS, FWMS all have a primary type of W(rite) and are so marked.
 *               The primary type is a number, IOTYP_READ, IOTYP_WRITE, etc.
 */

/* 
 * I used to think this was so simple... but corner cases and how event sequences happen can be more complicated
 * than first thought, especially with requeues, multiple overlapping simultaneous/active io, corrupted blktrace
 * files so some events hiccup and are not present {so we have to detect and synthesize if possible}, event sequences
 * are different on different kernel versions, etc.  So this is a complete re-write of the previous version.  Hopefully
 * it ends up cleaner in data structures and logic as a result.
 */

/*
State:
   active list     (typ_io   )
   starter list    (typ_io   )
   exceptions list (typ_event)
   completed list  (typ_io   ) last N completed io, JIC, as new completed io added that exceeds N, the oldest completed is removed.

   cnt_starters;
   cnt_exceptions;
   cnt_completed;

   cnt_active;     <- mainio count on active list, number of typ_io on that list
   cnt_inprogress; <- never more than cnt_active, and only incremented at Insert time or exception backfill Insert time.
		      and decremented at C time.
   cnt_inflight    <- D's without R's or C's
   cnt_inserts;    <- first insert for a mainio happened
   cnt_dispatches; <- first dispatch for a mainio happened
   cnt_completions;<- completion of the whole mainio happened
   cnt_r_events;   <- raw I count - R events = cnt_inserts by the end
   cnt_i_events;   <- raw I event count, 
   cnt_d_events;   <- raw D event count
   cnt_c_events;   <- raw C event count, these may include one per subio!

   

The "starter" list is a means to an end, the previous model had to assume all R|Q events started a mainio,
and then along came the M|F later that proved this false.  The "starter" list allows R|Q to create and capture
info into a typ_io -- but without the auxiliary data structures needed for a mainio until either S|G|I is 
encountered (mainio) or M|F encountered (subio).  Having typ_io created in this waiting room called "starter"
allows the program not to allocate a serialnumb for the io or do other things that it than has to back out of.
So only when it knows this io is a mainio does it put it onto the active io list.  This complicates some of
the pre-work done before active list a minor bit, but greatly simplifies the active io list.  For example, 
the inprogress count is still attached to the active list but now it does not bounce up as R|Q happens and
then down as the M|F happens. Active only increase when we have determined this is a mainio type.  Although,
the scheduler doesn't count active io until Insert time whereas the program is counting it at Q time.  Maaaybbeee
change that to be insert time (I or exception backfill).
    exceptions: io starts not with Q but S|G|I, backfill queue, increase active count at I time.  But this man

*/

/* 
 * Data structures:
 *    . typ_event		parsed info for a single blkparse line
 *
 *    . typ_io			composite main or subio
 *    . typ_actions		set of "actions" for composite io
 *        . typ_action		single io, lineno+timestamp.
 *    . typ_tickets		deli ticket handed out for each event type to track ordering of io
 *    
 *
 */

/*
>> TODO revamp! Flow is Q -> G|(M|F)|I .. but swear we were getting A->Ms before.
>> sooo, starter queue has to track A+Q and then S implies G implies new request, 
>> and we can assert no sleeps for M|F or we %Fatal out (which can be turned off
>> via flag or config).  But G events are fairly recent, yeah they have always been
>> defined, but kernel versions just didn't have them, we went directly from Q->I ...and
>> I only happen on mainio (first io).  So a S|G|I should move the typ_io from starter
>> to active.  That means we may need to "assume" that any io we create is a mainio and
>> therefore need to track a couple events before we can figure out its a mainio... or do we?
>> its just the remap and Q that come before we know.  So expand the remap fields to include
>> a typ_action field, done.  So now we capture any optional A event(s) leading up to the 
>> initial Q event.  We really only want to capture the first A event as this is the earliest
>> timestamp associated with the io and that time (either A or Q or for partial io whatever
>> comes first, but should be A or Q for good blkparse event stream) goes into mainio.events.first
>> and we use that time marker for computing Q2* values.
>>
>> So to recap. A -> option, captured in remap fields of typ_io for first one so we capture first
>> time reference to the io. The we capture Q in the normal place aka the "inline" event info of
>> typ_io.  Then the next events will either be S, G, M, F or I (in some versions that don't have
>> G).  A S|G|I all imply a "mainio" and we process accordingly.  Assign the io serialnumb, allocate
>> the extra data structures needed for mainio and move if from starter to active list.  For M or F
>> this is a subio that needs to be merged into an existing mainio.  If a mainio on the active list
>> is *not* found, then this is a PARTIAL/FORCED mainio creation scenario.  Do the force/create of
>> the needed active io and add this subio to that... and hope the D event clarifies the total extent
>> of the io.
  8,17   8    85999    90.073954839  4923  A  WM 8 + 8 [xfsaild/sdb]   << this is incorrect syntax
  8,16   8    85999    90.073954839  4923  Q  WM 8 + 8 [xfsaild/sdb]
  8,16   8    85999    90.073954839  4923  S  WM 8 + 8 [xfsaild/sdb]
  8,16   8    86000    90.073955650  4923  G  WM 8 + 8 [xfsaild/sdb]

  8,16   8    86001    90.073956382  4923  Q  WM 16 + 8 [xfsaild/sdb]
  8,16   8    86002    90.073956988  4923  M  WM 16 + 8 [xfsaild/sdb]

  8,16   8    86006    90.073960845  4923  I  WM 8 + 16 [xfsaild/sdb]
  8,16   8    86010    90.073970459  4923  D  WM 8 + 16 [xfsaild/sdb]
                                           R
                                           I
                                           D
  8,16   8    86013    90.074094393     0  C  WM 8 + 16 [0]
  -or-
  8,16   8    86013    90.074094393     0  C  WM 8 + 8  [0]
  8,16   8    86013    90.074094393     0  C  WM 16 + 8 [0]

>> AND... specify what happens if the "normal" sequence isn't present, and the
>> first we see of an io within the event stream from blkparse is something past
>> the Q event.  For example S, G, I events can synthesize a mainio and only do
>> not know the actual 'Q' time -- these three events imply a Q (mainio) and not
>> a M|F events as you don't have to sleep waiting for get request nor will you
>> insert a merged io -- you're attaching/merging the io to one that already has
>> been inserted.  For M|F its the same, you have enough information in this case
>> to create a subio -- and although you don't know the Q time, it does not matter
>> in that the Q is associated and from the mainio. But what does matter is the 
>> further subcase where the mainio we're merging into does not exist.  The we
>> synthesize a PARTIAL/FORCED mainio to allow the merge to have someplace to 
>> hang from.  But its gets real tricky if missed mainio is 10+8, back merge is 18+8,
>> which creates a PARTIAL/FORCED of 18+8, and then a front merge of 2+8 is 
>> encountered and we create *another* PARTIAL/FORCED mainio of 2+8 -- we don't
>> and can't know that there is a missing original Q/I mainio that is between those
>> two.  Then we get a D of 2+24 -- and it matches the 2+8 one but not the 18+8...
>> in fact we expand the 2+8 to 2+24 on the basis of this io and then detect "overlap"
>> against the 2nd PARTIAL/FORCED io when in fact no such overlap actually exists,
>> its just an artifact of how we handled merging onto a ghost mainio of unknown size.
>> The PARTIAL/FORCED logic works great as long as only back merges happen or only
>> front merges happen, but if both back and front merges happen and there is a 
>> reasonable chance the two ghost mainio created for those to types of merges are
>> one in the same, then the program is going to do something really stupid.  That
>> second PARTIAL/FORCED is set to overlapped and will be orphaned.  No 2nd dispatch
>> for same will ever happen.  The only way we can sub-case detect this is to 
>> create yet another sub-queue, the "exception" queue.  If there are front merges,
>> and at dispatch time it "covers" two PARTIAL/FORCED mainio... then a reasonable
>> expectation is that these are really the same, one io so we merge them on the
>> spot and dispatch.  But that opens us up to a later dispatch against the 2nd
>> mainio.  So instead of merging the io, we move the 2nd PARTIAL/FORCED mainio
>> to an exception queue, where we are fully expecting it to rot as the chances
>> are 99.99% that this ghost io is just that, a ghost.  We need to keep it on
>> the exception queue until we are fairly confident that it will never be dispatched
>> so we can remove/reduce the overlap counter that was set when we detected overlap...
>> or we don't bump overlap when detecting it on the 2nd ghost and the act of moving
>> the ghost to the exception queue signifies that we are delaying declaring this an
>> overlapped io and only add the output of "overlapped io detected" if and when a
>> dispatch event occurs such that it is not matched against any other active io, and
>> only then will will look into this exception list.  Really at that point we are 
>> so deeply nested into sub-corner cases we should just log the fact that we have 
>> a larger dispatch that didn't match any active io, but did match this ghost io
>> on the exception queue and just ignore them all ... but log that we're ignoring them
>> and again, this can only happen if we build out a mainio both fore and aft using
>> back and front merges *AND* the mainio and one or more previous back/front merges
>> were missing/are not present within the event stream either because we're at the
>> start of the file and blktrace wasn't running when they occurred ... or the event
>> stream capture hiccupped or is otherwise corrupt so those events are missing from
>> within the stream.

>> so now what is left is the first we see an io is at D, R, or C time.  For example,
>> if the stream was {....R} I D C -- we don't know there was an Requeue before the I
>> and its treated just like the 'Insert' case exception above.  So we're back to just
>> D, R, and C.  If its R, then we should just ignore this completely, log that we're
>> ignoring it in logged output only if we're near enough to the top of the blkparse
>> event stream or to both the parse.out and logged output if after that -- but that is
>> all.  The expectationis R will be followed by I, D, C again and so we're back to 
>> the Insert exception case.  That reduces the exceptions to just D and C.  First,
>> if we only have C then there is literally nothing we can do -- we do not have any
>> other time markers for this io.  I suppose we still should add this to the heat
>> map, but we've no additional context -- was this a seek into this io?  That type
>> of thing depends on a D event which we don't have.  So again, treat this just
>> like the R exception - log it either in just logged output or parse.out and logged
>> depending on how far into the event stream we are but otherwise take no action.
>> ....maaayyyybe take the "orphan" typ_event and add it to a list of "orphans" that
>> we ignored and dump out all such orphans at the end of the program run so they
>> have more visibility vs just scattered across what oft is a long log.  So that
>> just leaves us the exception of seeing an io for the first time at D time. As with
>> any exception I think, we need to add this to some time of exceptions list -- so
>> the above orphans turns into an exceptions list and we flag the typ_event in some
>> way so as to discern why it was put onto the exceptions list ... even if we have
>> corner-case exceptions processing for same.  We need to be able to look at the
>> exceptions list of events and ascertain why they are considered exceptions and 
>> is that designation "reasonable".  So for D events, we backfill the event time/line
>> information for Q,I with the D time but with line = -1 to indicate its valid time,
>> but its not actually from that event but a backfill.  I mean we're going to be
>> doing the same with S|G|I, backfill Q and M|F backfill Q (although this is also 
>> a ghost/PARTIAL/FORCED typ_io whereas all the other exceptions are just FORCED and
>> not partial.

*/

/* 
 * TYP_IO data structure
 * typ_io are allocated at A time[1] but, but the typ_io are kept on a side queue/list and *not* put onto
 * the active list until 'Q' or other later event time.  So search order to events is this "waiting" 
 * room list, the N entries of the active list (where N is the whole list if lba buckets are not active).
 * This is because we want to assign a io ticket (sequence) number, but the follow-on event might be a
 * 'Q' (needs a io ticket), but could be 'M' or 'F' merge instead of a Q -- so for those a io sequence
 * number is *not* assigned.  The Q sequence number becomes the io sequence/ticket number as well as the
 * assigned Q ticket number, even if later on M or F events are added (which demote the original Q info
 * to a cloned subio on the merge list.  The merge list is time ordered, so the original Q io is first
 * on the list, and later M or F are added to the end of this list.
 *
 * [1] Normally allocated at A time, but near the start of the file an io may already be "in progress"
 *     and those will be created by whatever event is detected first.  Anything past Q|M|F means we
 *     don't have an actual Q time.  The best that can be done is to substitute the first timestamp
 *     encountered as all previous timestamps.  If a timestamp is cloned like that, then the 'line'
 *     field for the timestamp is set to -1 to indicate its cloned from a later event type (which
 *     can be figured out by skipping any timestamps with -1 line number set until you hit a "real"
 *     line number -- and that will be the source.  Of course all the timestamp values will also
 *     be the same too across that set of timestamps.  The typ_io.active pointer is overloaded and
 *     reused for when this io is on the "starter" queue.  The "starter" queue is used as we do not
 *     know if this is a mainio or subio until Q|M|F time.  So we want to postpone assigning io 
 *     serialnumber or even allocating typ_events and typ_tickets as these will not be needed for
 *     subio.  But when using .active for starter queue, the typ_io.ioflags has the 'STARTER' flag
 *     set to indicate this structure is on the starter queue and has not advanced to the actual
 *     active queue. And while promotion from starter to active queie is typically expected at Q|M|F
 *     time, the case of starting at some other point within the io life cycle can occur at the 
 *     beginning of the blkparse event file (and even in the middle of the file if there is event
 *     corruption within the blktrace binary file itself).  These types of spontaneous out of the
 *     ordinary event sequences, while simply discarded in the past, should be accomodated as much
 *     as possible.  This is done by setting the typ_io.ioflags 'FORCED' flag indicating the typ_io
 *     structure was forced by an out of sequence event stream.
 *
 * The nominal lifecycle is create typ_io @A time and put it on the "starter" io queue.  A Q|M|F
 * event is found that matches the io type (R|W|WS|...) *and* 
 *   - for Q, matches sector/length exactly on starter queue[A]
 *     1) remove the event from the "starter" queue, clear the 'STARTER' ioflags bit
 *     2) allocate events (typ_actions) and typ_tickets since this is now identified as a "mainio"
 *     3) check for overlapping io; run through the current active io list looking for any io that
 *        this io overlaps.  If an overlap is found, add the overlap flag to the typ_io.ioflags.  Set
 *        the OVERLAP if not set and this is same type, set that flag on both new and existing io.  For
 *        each that this flag is being set for the first time, increase the state.overlapping io counter.
 *        If the io is not the same basic type (i.e. read but existing io is write or vice versa), set
 *        the CONFLICT flag instead, but still increase state.overlapping counter the same way.
 *     4) then add typ_io to the active io list at the top, also add it to the lba bucket, if those 
 *        are present.
 *
 *   - for M (or F), match the starting lba at the end of an existing active io (M) or ends at the 
 *     starting lba of an exiting active io (F) on an io that is on the active queue [B]
 *     1) find & remove the event from the "starter" queue, clear the 'STARTER' ioflags bit -- if it is
 *        there.  It will only be there if this io was remapped first.  If an *exact* match on
 *        the "starter" queue is not found, then no remap previously occurred and create the typ_io
 *        and fill it in just like happens at remap time. So this step is find & remove -or- create
 *        the typ_io for this subio.
 *     2) if the mainio has *NO* subio, then clone mainio information into a typ_io of its own,
 *        mark that new typ_io as a subio and add it to the mainio->subio list.  Increment the
 *        cnt_merges count on mainio.
 *     3) add this subio to the end of the subio list, increment the cnt_merges on the mainio
 *     4) expand the sector+length of the mainio to reflect this merged subio
 *     5) if the OVERLAP and CONFLICT flags are both not set, then search the active list for 
 *        sector+lenght overlapping any existing io on the active list.  If found, then set
 *        overlap/conflict as necessary and increment state counters.
 *
 *   - for S events, find matchine existing active io that matches this sector+length.  A sleep
 *     will only happen for an io that is creating a new request -- aka a Q event.
 *
 * [A] if Q|M|F event happens and the "starter" typ_io structure doesn't exist either because no 
 *     remApping is involved or we're near the front of the blkparse input file, then we create
 *     a new typ_io, and step 1) is skipped below (or we "normalize" and force create, cook up
 *     the fields as needed (if any) and add it to the "starter" queue.. then the following
 *     steps apply (or step 1 is just is the typ_io.ioflags "STARTER" flag set or not.  That is
 *     we overload typ_io.active to also be used for the starter queue, but when on the starter
 *     queue the 'STARTER' flag is also set in typ_io.ioflags.
 * [B] there can be cases where a merge request finds no "starter" active io present.  That is,
 *     the normal case is (A)+Q results in, first, typ_io created at 'A' and then moved to 
 *     active io list upon Q (if if no remaps before hand, then create typ_io as mainio directly).
 *     Then, M|F merges come along and will find the exiting "mainio" on the active list, add this
 *     length after|before the current active "mainio", etc.  However, at the beginning of blkparse
 *     or occassionally in the middle of the blkparse event stream a M|F merge comes along and cannot
 *     find the active/"mainio" typ_io structure as expected.  This should force the creation of a
 *     "mainio" typ_io and set the FORCE flag.  Also, since this is a merge, the PARTIAL typ_io.ioflags
 *     needs to be set.  Without the initial Q, we don't know what we are really merging into in terms
 *     of length.  That means, if we encounter M|F in this type of sequence, then we get a 'D' event,
 *     and that 'D' event will include a different lba+length ... within which the PARTIAL io fits.
 *     The program can assume such 'D' events should be associated with the partial io and so add the
 *     D event to the io, and update the sector+length fields to match.  However, the subio list will
 *     not include the expanded space in this case.  So, if we do attach a D to a PARTIAL io, we need
 *     to create 1 or 2 subio to cover the missing space fore/aft of the current contiguous set of subio.
 *     Such "made up" subio will have their typ_io.ioflags include the 'MAKEUP' flag set indicating these
 *     subio only have sector+length info that are valid and were created to make up the difference between
 *     what we had as a partial io vs what Dispatch indicated the full io really looked like.
 */

/* 
 * "Tickets"
 * Monotonic numbers are handed out as "tickets", like a deli ticket number, to each type of event.  The
 * ticket numbers of other events are captured at specific times.  The tickets help define time order of
 * the events in cardinal fashion but only for that type of event.  So, for example, at Q time if we capture
 * the current next D ticket number and then capture the D ticket when this io is dispatched we can determine
 * scheduling reordering amount. The difference from D ticket at Q time and the D ticket at D time should  be
 * the same if things are well time ordered.  Ditto for D time, capture the current C ticket number and if the
 * next C event isn't that ticket number then storage reordered the io processing within storage.  And that is
 * the main purpose of the tickets -- how much reordering within the io scheduler and within storage is happening.
 *
 * A ioinfo block is allocated for each typ_io at allocation, if specified (aka "normal" flow of 'A' or 'Q' would
 * trigger a typ_io w/typ_ioinfo attached).  If after 'A' the io is tagged a M,F then the typ_ioinfo block is 
 * simply freed.  But that means the typ_io needs to track not only the A/Q event that created it but also potentiall
 * a D (not likely -- haven't seen separate dispatches for subio inside of an io... yet), but definitely need to 
 * track the C completion event for those environment that issue a C for each subio and we only "complete" the mainio
 * once all subio have been marked completed.
 */

#ifndef MOVED
#define MOVED(   _field ) _field
#endif
#ifndef DELETED
#define DELETED( _field ) _field
#endif

/*
 * Event Signature
 *
 * Track state machine transistion across the io context.
 *
 * Note, while allocated size is rounded up to nearest 8 byte
 * boundary, the 'len' only reflects the actual signature string
 * length.
 */
typedef struct _typ_event_signature typ_event_signature;
typedef struct _typ_event_signature
{
    typ_event_signature *next;			/* pointer to next signature, 0 = end of list, -1 = inactive, otherwise pointer */
    int                  count;			/* count of the number of time this signature is found				*/
    int                  state_machine_conf;	/* only used on pre-allocated state machine configuration types			*/
#   define STATEMCH_CFG_NONE 0			/* "normal" state machine, populated at run-time				*/
#   define STATEMCH_CFG_RUNTIME 0		/* "normal" state machine, pre-populated in event_cfgsigs list only		*/
#   define STATEMCH_CFG_PRELOAD 1		/* pre-populated in event_cfgsigs list only					*/
#   define STATEMCH_CFG_IMB1    2               /*                                                                              */
    int                  len;                   /* signature string lenght							*/
    char                 signature[0];		/* allocated string bytes start here						*/
} typ_event_signature;
#define MAXLEN_EVTSIG   32			/* maximum expected state machine one event  signature				*/
#define MAXLEN_EVTSIGS  256			/* maximum expected state machine io context signature				*/

typedef struct _typ_event_datablk typ_event_datablk;
typedef struct _typ_event_datablk
{
    typ_event_datablk   *next;			/* pointer to next block							*/
    int                  alloc_size;		/* data block allocated size total						*/
    int                  free_size;   		/* number of bytes left unused							*/
    void                *free;			/* pointer to next free/unused byte in data block				*/
    char                 pool[0];		/* dummy entry to locate 1st free byte						*/
} typ_event_datablk;
#define DATABLK_SIZE 8192			/* currently defined data block size						*/

typedef struct _typ_event_sighdr
{
    typ_event_signature *head;			/* pointer to first (oldest) event signature structure			        */
    typ_event_signature *last;			/* pointer to last  (newest) event signature structure			        */
    typ_event_datablk   *datablk;		/* pointer to current datablk being sliced/diced for typ_event_signatures   	*/
    int                  cnt_sigs;		/* count of unique signatures created						*/
} typ_event_sighdr;

typedef struct _typ_event_signatures
{
    typ_event_sighdr    *io_nones;              /* 'n' signatures allocated for unique io.none     contexts			*/ /* these apply to 1st event in io context */
    typ_event_sighdr    *io_reads;		/* 'r' signatures allocated for unique io.read     contexts			*/ /* classification only                    */
    typ_event_sighdr    *io_writes;		/* 'w' signatures allocated for unique io.writes   contexts			*/
    typ_event_sighdr    *io_meta;		/* 'm' signatures allocated for unique io.meta     contexts			*/
    typ_event_sighdr    *io_discards;		/* 'd' signatures allocated for unique io.discards contexts			*/
    typ_event_sighdr    *io_barriers;		/* 'b' signatures allocated for unique io.barriers contexts			*/
    typ_event_sighdr    *imbs;			/* 'i' signatures allocated for unique implied io.barrier contexts		*/
    typ_event_sighdr    *mgmt;			/* 'c' signatures allocated for unique management (control) contexts (P,U,etc.) */
    typ_event_sighdr    *others;		/* 'x' signatures allocated for other context (unknown) sequences, if any       */

} typ_event_signatures;

/* For pre-configured .h event signatures we need a iosig structure with preallocated maximums */
typedef struct _typ_event_sigconf  typ_event_sigconf;
typedef struct _typ_event_sigconf
{
    typ_event_sigconf   *next;                  /* pointer to next signature, 0 = end of list, -1 = inactive, otherwise pointer */
    int                  count;                 /* count of the number of time this signature is found                          */
    int                  state_machine_conf;    /* only used on pre-allocated state machine configuration types                 */
    int                  len;                   /* signature string lenght                                                      */
    char                 signature[MAXLEN_EVTSIGS];
} typ_event_sigconf;



/*
 * Remap tracker
 *
 * Track any/all remAp (A) events from:to devices and capture read/write io count and sectors
 * Output at SUM time only to show flow of io from partitions to main device or LV to partitions
 * (to main device).
 */
typedef struct typ_remaps
{
    int from_major;
    int from_minor;
    int   to_major;
    int   to_minor;
    unsigned long read_count;
    unsigned long write_count;
    unsigned long read_sectors;
    unsigned long write_sectors;
    unsigned long discard_count;	/* not currently reported */
    unsigned long discard_sectors;	/* " "			  */
} typ_remaps;


/* 
 * C tracker
 * Tracks the different completion events against an io request.
 * If or when an io has subio (BIO components) due to merging, it
 * may be multiple C events to satisfy the main io and each BIO, or 
 * just the main or just the set of BIO.  The C tracker is there to
 * capture all that.
 */
typedef struct typ_cevent
{
        unsigned long sector;
        unsigned long length;
        unsigned long line;		/* line number  ;  event index (line) number within blkparse output			*/
        double        time;		/* event time   ; event time listed on input for this specific event			*/
} typ_cevent;
typedef struct typ_ctracker
{
    /* Snap-short of state info @1st completion event for this request 				  */
    int inProgress;                     /* for now, Q w/o C events                                      *//* for io w/o merging         */
    int inProgress_Read;
    int inProgress_Write;
    int inFlight;                       /* for now, D w/o C events                                      *//* for io w/o merging         */
    int inFlight_Read;
    int inFlight_Write;
    long   last_completed_sector;
    long   last_completed_length;
    double last_queue_t;

    /* Now the C event capture and tmo values for same						*/
    int     tmo_line_value;
    double  tmo_time_value;
    double  last_time;
    double  tmo_time;
    int     last_line;
    int     tmo_line;
    int     used_completions;
    int     numb_completions; 		/* number of completion events within the following array */
    typ_cevent completions[1];
} typ_ctracker;
#define CTRACKER_TMO_LINE   50 		/* number of lines to wait for final C events, if any additional for this request */
#define CTRACKER_TMO_TIME  0.100	/* seconds         to wait for final C events, if any additional for this request */
#define CTRACKER_FWS_LINE   10 		/* number of lines to wait for final C events, if any additional for this request */
#define CTRACKER_FWS_TIME  0.002	/* seconds         to wait for final C events, if any additional for this request */


/* 
 * Events referenced as D' (D prime) is referencing last D event before C event, whereas D is
 * the first event in time.  The D and D' (or I/I', S/S', ...) only happen with some subset 
 * of events -- typically only associated with requeues or other resource issues that end up
 * blocking progress and cause repeat events.
 */

/* NOTE: use this to flag typ_action struct copies in typ_io->events.  As we need to verify that mySeqNo is handled             */
/*       if will be overwritten, and while in create_io() that is fine, elsewhere we need to ensure the order is correct so that*/
/*       the {event}SeqNo isn't overwritten.                                                                                    */
#define LINEINFO_COPY bcopy

/* @@@TBD: rename to typ_lineinfo */
typedef struct typ_action
{
        unsigned long line;		/* line number  ;  event index (line) number within blkparse output			*/
#       define LINENUM_NONE                    0UL     /*  if this, no assigned time value present                              */
#       define LINENUM_GHOST  0xFFFFFFFFFFFFFFFEUL     /*  if this, forced (ghost) time assignment value present                */
#       define LINENUM_SPEC1  0xFFFFFFFFFFFFFFFDUL     /*  if this, is special1 case, also time assigned value present		*/
        double        time;		/* event time   ; event time listed on input for this specific event			*//* in summary these are dTime amounts */
#       define NOVALUE( _action ) (_action.line == 0)	/* if line == 0, then this action has no value, is as of yet unassigned	*/
        unsigned int  pid;		/* pid          ; process id on event line						*/
        unsigned int  cpu;		/* cpu          ; cpu on event line (important for issuer, service interrupt)		*/
        unsigned long seqnum;		/* seqnum       : from event line, this is monotonic increasing but does reset to zero  */
        unsigned long mySeqNo;		/* SeqNo        : assigned from parse, basically monotonic count of this event type     */
} typ_action;
/* Note: the numb allows detection of previously assigned value or not, which allows a composite action bitmask to be maintained*/
/*       at the composite io (main or sub) io level.										*/
/* Note: extended fields filled in at CSeqNo++, ASeqNo++ (if this is a create), DSegNo++, and CSegNo++ only                     */
/* @@@TBD: rename to typ_lineinfo_ext */
typedef struct typ_action_ext
{
        unsigned long line;		/* line number  ;  event index (line) number within blkparse output			*/
        double        time;		/* event time   ; event time listed on input for this specific event			*/
        unsigned int  pid;		/* pid          ; process id on event line						*/
        unsigned int  cpu;		/* cpu          ; cpu on event line (important for issuer, service interrupt)		*/
        unsigned long seqnum;		/* seqnum       : from event line, this is monotonic increasing but does reset to zero  */
        unsigned long mySeqNo;		/* SeqNo        : assigned from parse, basically monotonic count of this event type     */
        /* Extended fields for Q, D, C events										        */
                 long seek;		/* seek from last completed sector+length						*/
        unsigned long SeqNo     [3];    /* Q, D, C sequence numbers at time of event, event counts assigned by parse		*/ 
        unsigned int  inProgress[3];    /* total, read, write, counts at the point event recv'd                                 */
        unsigned int  inFlight  [3];    /* total, read, write									*/
	/* Extended fields for X       event											*/
	unsigned int  split_lvl;	/* 0=no split, 1,3,5,... odd levels are "original" io that was split                    */
#       ifdef NOT_YET
        unsigned int  cnt_dispatches;	/* These really are io based not individual event based, even for extended(?)           */
        unsigned long DSeqNo_Ghost;
#       endif
} typ_action_ext;



/* Note: 1) the number of S, R, I, or D events is not tracked within 'actions', the number is tracked within the composite io   */
/*          only this simply tracks that events have happened and which events.  					        */
/*       2) "_last" event is always updated, the main 'first' event is set only once if it's 'line' is 0, aka unassigned        */
typedef struct typ_actions
{
    char            first_prmtyp;
    typ_action_ext  first;		/* {A|Q|M|F} time, first time within output this subio was detected			*//* basis for Q2C time			  */

    typ_action	    remap;		/* 'A' request event, if any -- from partition or dm to main device like 8.0		*/
    typ_action	    queue;		/* 'Q' request event, if any -- should always have one of these on 2.6.18 or later	*//* unless "partial" from early in file  */
    typ_action      merge;	        /* 'M' request event, if any -- may be instead of 'Q' (new)				*//* back merge, add *after* existing io  */
    typ_action      front;		/* 'F' request event, if any -- may be instead of 'Q' (new)				*//* front merge, add *before* existing io*/
    typ_action      split;		/* 'X' request event, if any -- may be instead of 'Q' (new)				*/
    typ_action      sleep;		/* 'S' request event, if any -- last sleep due to lack of request resources		*//* typically not present		  */
    typ_action      sleep_first;	/* 'S' request event, if any -- first sleep event, if multiples 			*//* will be same as sleep if only 1      */
    typ_action      getrq;		/* 'G' request event, if any -- get request						*/

    typ_action      insert; 		/* 'I' request event, if any -- should only be present for mainio type			*//* this is *last* I event time     	  */
    typ_action_ext  dispatch;		/* 'D' request event, if any -- should only be present for mainio type			*//* this is *last* I event time	  */
  
    typ_action_ext  complete;		/* 'C' request event, if any -- may be a C event for each subio, mainio or both	        *//* for subio the C info is in lst* field*/

    /* associated with requeues		*/
    typ_action      requeue;		/* 'R' request event, if any -- the *last* one that happens				*/
    typ_action      requeue_first;	/* 'R' request event, if any -- the *first* one that happens				*/
    typ_action      insert_first;	/* 'I' request event, if any -- the *first* insert time for this io (due to requeues)	*//* no requeues last = first		  */
    typ_action_ext  dispatch_first;	/* 'D' request event, if any -- the *first* dispatch time for this io (due to requeues)	*/

    /* the following are necessary for summary block only but can be a convience for individual io contexts			*/
    typ_action      d2c_first;		/* 'D' to 'C' event time, from first dispatch						*/
    typ_action      d2c_last;		/* 'D' to 'C' event time, from last  dispatch						*/
    typ_action      d2d;		/* 'D' to 'D' event time, total from first to last dispatches				*/
} typ_actions;


/*
 * Each event, when parsed, fills out the following structure.
 * The information inside of this is transferred to a typ_io,
 * if referencing an io event.			
 */
/*   M,m   CPU     seq#    time         Pid  Act Typ Sector      Len ProcName
 *   8,16  10        1    25.775525623  6876  Q FWFSM 757449255 + 3  [dd]
 *   8,16   8    86002    90.073956988  4923  M  WM   16        + 8  [xfsaild/sdb]
 *   8,16  25        2     0.000001846  6874  G  WS   6400096   + 64 [dd]
 *   8,16   1     1395    59.993920750  4754  I FWFSM 757449264 + 3  [kworker/1:1H]
 *   8,16   0   698615    59.994643869     0  D WSM   757449264 + 3  [swapper/0]
 *   8,16   0   698608    21.105635222     0  C  WS   19200032  + 64 [0]
 *   8,16  10        3    25.775528224  6876  P   N                  [dd]
 *   8,16   1     1396    59.993922224  4754  U   N                  [kworker/1:1H] 1
 *   8,16  10        5    25.775530595  6876 UT   N                  [dd] 1
 */
#define MAXLEN_ACTION    8
#define MAXLEN_EVTTYP    8
#define MAXLEN_PROCNAME 32
#define MAXLEN_EVTXTRAS 32
#define MAXLEN_EVTTEXT 128
#define MAXLEN_REASON  128
#define _GUARDS_ unsigned long _guardians[4];
#define _MARKER_(_this) _set_guards(_this,__LINE__)
#define _CHEKER_(_this) _chk_guards(_this,__LINE__)
typedef struct typ_event		      /* blkparse line parsed into its components					*/
{
    struct typ_event *next;		      /* only used for exceptions list and debug list in typ_io			        */
    struct typ_io    *request;		      /* io "request" context that this event was assigned to, if null then discarded   */

    unsigned long specials;		      /* see io_typ for SPCFLG_ defines							*/

    /* ------------------------------------------------------------------------------------------------------------------------ */
    /* --- event body: data from event stream --------------------------------------------------------------------------------- */
    /* ------------------------------------------------------------------------------------------------------------------------ */
    unsigned long  evtflags;
#   define EVTFLG_EXCEPTION      0x00000001	  /* This event is "exception", polite way of saying "huh?, what's this?"	*/
#   define EVTFLG_IGNORED        0x00000002	  /* First event was R or C without any prior events -- skipped/ignored         */
#   define EVTFLG_BADFIELD       0x00000004	  /* bad field value or format							*/
#   define EVTFLG_UNKFIELD       0x00000008	  /* unknown/unrecognized field							*/
#   define EVTFLG_LOCFORMAT      0x00000010	  /* Location format (should be) present 					*/
#   define EVTFLG_LOCDATA        0x00000010       /* ???									*/
#   define EVTFLG_OLDSTYLE       0x00000020	  /* new style is sector+length, old style was (<scsi command bytes>)           */
#   define EVTFLG_NONIOSTYLE     0x00000040       /* D N 0 (00..) [NULL] for example, which looks like a TUR(?)                 */ /* such events have no length         */
#   define EVTFLG_VALID_FROMSECT 0x00000080	  /* has valid info from remap in from_* fields					*/
#   define EVTFLG_VALID_PROCNAME 0x00000800	  /* has procname 								*/
#   define EVTFLG_VALID_SECTOR   0x00001000       /* need flags for "I FWS [(null)]" events.      				*/ /* such events have no length         */
#   define EVTFLG_VALID_LENGTH   0x00002000       /* i.e. has neither sector nor size specified   				*/
#   define EVTFLG_VALID_SIZE     0x00002000       /* i.e. has neither sector nor size specified   				*/
#   define EVTFLG_IMB            0x00004000       /* implicit io barrier							*/
#   define EVTMSK_VALID_LOCDATA  0x00003000	  /* valid location fields mask							*/
#   define EVTFLG_WHOLE_COMPLETE 0x00010000
#   define EVTFLG_BIO_COMPLETE   0x00020000
#   define EVTFLG_SPECIAL_FWS    0x00040000   /* corner case: FWS requests can have multiple C events for some reason           */ /* [FN.1] 2nd one has no length! 	*/
#   define EVTFLG_SCSICOMMAND    0x00080000   /* event has (12,00,00...) embedded SCSI/ATA command info in the event            */
#   define EVTFLG_GHOST          0x00100000   /* forced creation of "missing" mainio request structure due to out-of-seq event  */
#   define EVTFLG_GHOST_2NDGEN   0x00200000   /* 2nd hand ghost, force created mainio as target for M|F to merge into           */
#   define EVTFLG_INPROGDONE     0x00400000   /* allow inProgress to be updated in mainio->post(->done) or mainio->done cases   */ /* set when inProgess updated       */
#   define EVTFLG_2NDHAND_MERGE  0x00800000   /* this is a temporary flag set during 2ndhand merge detection/processing         */ /* set on events in dbg_events too  */
#   define EVTFLG_EXCEPTDONE     0x01000000   /* has been output to *.exceptions, do only once!					*/
#   define EVTFLG_BESTMATCH      0x02000000   /* this io context has already participated in a best_match() call                */
#   define EVTFLG_NOFEATURE      0x04000000   /* flagged as exception because code does not support feature to process this evt */
#   define EVTFLG_NOTARGET       0x08000000   /* a data processing fault has occurred and this event cannot be tagged to an io  */
#   define EVTFLG_DISCARDED      0x10000000   /* this event has been ignored/discarded                                          */
#   define EVTFLG_UNKFORMAT      0x20000000   /* unknown event format, typically set along with NONIOSTYLE flag                 */
#   define EVTFLG_KWIOBARRIER    0x40000000   /* 'FWS 0+0', FN, et.al. implicit write barrier io sequence likely                */
#   define EVTFLG_REMAPSKIP      0x80000000   /* this matched a remap bug/nested remap, but failed line/time delta limits       */


    int           from_major;                 /* If remap event was associated... there are two lba+size sets                   */
    int           from_minor;
    long          from_sector;
    int		  major;
    int		  minor;
    char          prmact;		      /* primary (main) action, e.g. evtact="UT", prmact = 'U'				*/
#   define EVTACT_REMAP  'A'		      /* 'A'										*/
#   define EVTTYP_REMAP   1
    unsigned char prmtyp;		      /* primary (main) io type								*/ /* was 'rw' in old model    */
#   define IOTYP_UNK     0		      /* 'x' : "unknown" (program can't figure it out) type				*/ /* parse will warn on this  */	
#   define IOTYP_OTHER   1		      /* 'o' : 										*/
#   define IOTYP_NONE    2		      /* 'N' : none, e..g "P N [procname]"						*/ /* includes 'm'essage type  */
#   define IOTYP_SCHED   3		      /* 'S' : @@@???									*/ /* rare, just 'S' alone(?!) */
#   define IOTYP_READ    4		      /* 'R' : read io								        */	
#   define IOTYP_WRITE   5	              /* 'W' : write io 								*/ /* W,M,B,D = writes         */
#   define IOTYP_META    6                    /* 'M' : meta    (write type) "io" but ones that are typ 0 length                 */ /* typ overlap io req this  */
#   define IOTYP_BARRIER 7		      /* 'B' : barrier (write type)						        */ /* typ zero length write    */
#   define IOTYP_DISCARD 8		      /* 'D' : discard (write type)							*/
#   define IOTYP_MAX     8

    unsigned char imbtyp;		      /* IMplicit Barrier sub-type							*/
#   define IMBTYP_NONE   0		      /* not an implicit barrier							*/
#   define IMBTYP_xx     1		      /* no   sector, no   length							*/
#   define IMBTYP_0x     2		      /* ==0  sector, no   length							*/
#   define IMBTYP_00     3		      /* ==0  sector, ==0  length							*/
#   define IMBTYP_Sx     4		      /* > 0  sector, no   length							*/
#   define IMBTYP_S0     5		      /* > 0  sector, zero length							*/

    unsigned short int iomask;		      /* from evttyp column								*/
#   define IOMASK_READ      0x0001
#   define IOMASK_WRITE     0x0002
#   define IOMASK_DISCARD   0x0004
#   define IOMASK_NONE      0x0008
#   define IOMASK_BARRIER   0x0010
    /*                      0x0020 */
    /*                      0x0040 */
#   define IOMASK_UNKNOWN   0x0080
#   define IOMASK_MAINTYP   0x00FF
    /* --- io qualifiers ---       */
#   define IOMASK_FLUSH     0x0100
#   define IOMASK_FUA       0x0200
#   define IOMASK_SYNC      0x0400
#   define IOMASK_META      0x0800
#   define IOMASK_READAHEAD 0x1000
    /*                      0x2000 */
    /*                      0x4000 */
    /*                      0x8000 */
#   define IOMASK_QUALTYP   0xFF00


    char	  evtact[MAXLEN_ACTION];      /* the action string is *mostly* single char, but there are a few doubles like UT */
    char          evttyp[MAXLEN_EVTTYP];
    typ_action	  timeinfo;	              /* timestamp and numb (lineno) captured for this event line      			*/
/* @@@TBD, move pid, cpu, seqnum to action as these are generic */
    unsigned int  MOVED(pid);
    unsigned int  MOVED(cpu);
    unsigned int  MOVED(seqnum);
    long          length;		
    long          sector;
    char          procname[MAXLEN_PROCNAME];  /* process name, really only of value @first event referencing io		  	*/
    char	  extras  [MAXLEN_EVTXTRAS];  /* trailing number on U/UT type events -- what is it? its after procname		*/

    char          event_text[MAXLEN_EVTTEXT]; /* event line from blkparse package utility					*/

    char          event_sig [MAXLEN_EVTSIG ]; /* io context event signatures for state machine					*/

    char          reason    [MAXLEN_REASON ]; /* reason for event to be on the exceptions list (or annotation for io dbg list)	*/
    char         *garbage;                    /* trailing unparsed garbage							*/

    _GUARDS_;
} typ_event;

/* FootNotes
 * FN.1 - SPECIAL_FWS and KWIOBARRIER are closely linked and will often appear at the same time, but not always:
 *        FWS+KWIO :: 259,0   11       19     5.027073014  1889  A FWFS 575480512 + 0 <- (253,3) 407706304           
 *        FWS+KWIO :: 259,0   11       20     5.027074128  1889  Q FWFS [jbd2/dm-4-8]
 *        FWS+KWIO :: 259,0   11       21     5.027077378  1889  G FWFS [jbd2/dm-4-8]
 *            KWIO :: 259,0   11       22     5.027084885   612  D  FN [kworker/11:1H]
 *            KWIO :: 259,0   11       23     5.027852186     0  C  FN 0 [0]
 *        FWS+KWIO :: 259,0   11       24     5.027859859     0  C WFS 575480512 [0]  << Implied 'F'lush due to W+S and missing length
 *        
 *        SPECIAL_FWS will *only* be applied to events with imbtyp values != IMBTYP_NONE such as the above.  IMplicit Barriers
 *        have either and explicit length of zero, and no length specified.  But a true implicit barrier also has only specific
 *        event footprints allowed/recognized.  The SPECIAL_FWS should only appear on events that have imbtyp != NONE, and
 *        whose event footprints are known associates of implied barriers plus the key of FWS io types being present.  This logic
 *        should be contained within the parse_event() function so its centralized and not spread out within each event type
 *        parsing function.
 */



/* In the following macro, a 0 is READ, a +value is write, and -value is "other" */
#define R_OR_W(this) (this->prmtyp - IOTYP_READ)
#define _READIO 0

/* 
 * The following is only needed on the mainio, subio don't need to track this stuff.
 * The tickets herein intent is to track 'mainio' info so can deduce reordering in the ioscheduler,
 * (Q->D), and reordering of execution within storage (D'->C reorder).
 *
 * Subio do not have an lba bucket index or heat bucket, those are only applicable to the "whole" io,
 * aka the mainio.  Ditto for merges, sleeps, requeues (implies same count for inserts and dispatches),
 * and completes (if the blktrace is from kernel version that does a complete per subio).
 */
typedef struct typ_ioinfo
{
    unsigned long serialnumb;   /* unique serial number - created by program */ /* this is per io, either mainio or subio, mainio adopts the serialnumb of the 1st subio*/

    struct typ_io    *orphan;	/* "orphan" list, next                       */ /* typ_io created, but not on A,Q,M, or F event */ /* time order bot->top (old->new)    */
    struct typ_io    *bucket;	/* "bucket" list, next			     */ /* if LBA bucket indexing available		*/ /* time order top->bot (new->old)    */
    int          numb_bucket;   /* sector lies in this      bucket number    */
    int          indx_bucket;   /* sector lies in this indx bucket number    */ /* level 2 index */
    int          heat_bucket;   /* sector lies in this heat bucket number    */ /* level 1 index */

    struct typ_io      *this;   /* current merged io ref'd by find_event()   */ /* @@@ ??? from V1/V2 of program, is this used and how?					*/

    int           cnt_merges;   /* number of 'M,F' merged io in subio list    */ /* ...and 'Q' event that is first nexus for io  */ /* number of subio on the list      *//*[1]*/
    int           cnt_inserts;  /* number of 'I'nsert events		      */ /* ...will be multiple if requeues non-zero     */
    int           cnt_dispatches;/*number of 'D'ispatch events		      */ /* ...will be multiple if requeues non-zero     */
    int           cnt_requeues; /* number of 'R'equeues                       */ /* ...includes first and last requeue           */
    int           cnt_sleeps;   /* number of 'S'leep events                   */ 
    int           cnt_completes;/* number of 'C'omplete events                */ /* typically 1, but in some subio environments..*/
    int           cnt_coverages;/* number of 'C'omplete events for subio      */ /* if non-zero, then C event only partially covered this whole merged request and expect more */
    int           cnt_dispatches_ghosts;                                         /*number of ghost.'D'ispatch events added for unpaired Rs 				       */


    /* 3.10 only ... */ /* 2.6.32 C-whole + 2nd-Nth BIO on dm multipath devices too */
    /* -------------------------------*/
    int           cnt_completed_subio;	/* Track C events against subio						      */
    struct typ_event *completed_subio;  /* needed for 3.10 in that subio complete as individuals and are removed and  */
                                        /* thereby lost by the time the last subio complete and is reported.  Stash   */
                                        /* them here so at final report time in 3.10 we know what the subio were      */
    /* -------------------------------*/


    typ_actions   events; 		/* may only be allocated for main io						*/

    /* Track deli tickets of main events so can compute relative re-ordering of io within scheduler and storage      */
    /* ------------------------------------------------------------------------------------------------------------- */
    /* Ticket stateful information with regards to inkind events; both absolute and relative			     */
/* @@@ .ALL. of the following were moved to be inside events->{action}?, yes as mySeqNo field 			     */
/* @@@ these can be "cached" values, but really events.remap.mySeqNo is same as ASeqNo and is still fixed offset, so */
    unsigned long MOVED(ASeqNo);
    unsigned long MOVED(QSeqNo);		/* These are ticket numbers assigned at time of event			     */
    unsigned long MOVED(ISeqNo);
    unsigned long MOVED(DSeqNo);
    unsigned long MOVED(CSeqNo);
    unsigned long MOVED(GSeqNo);
    unsigned long MOVED(SSeqNo);
    unsigned long MOVED(RSeqNo);
    unsigned long MOVED(XSeqNo);
/*  unsigned long QSeqno[3];		 in events->first,dispatch,completion as SeqNo[]			    */
/*  unsigned long DSeqno[3];		Q, D, C counts @A|Q 							    */
/*  unsigned long CSeqno[3];		Q, D, C counts @A|Q 							    */
/*  int           QInProgress[3];       in events->first,dispatch,completion as InProgress[3];                      */
/*  int           QInFlight  [3];										    */ 
/*  int           DInProgress[3];       InProgress(tot,read,write)						    */
/*  int           DInFlight  [3];										    */
/*  int           CInProgress[3];       InProgress(tot,read,write)						    */
/*  int           CInFlight  [3];										    */

    unsigned long QSeqNo_Ghost;         /* if there are subio, this is last ticket # assigned to a subio            *//* subio are assigned a Q ticket before we know its a subio */
    unsigned long ISeqNo_Ghost;         /* if there are subio, this is last ticket # assigned to a subio            *//* subio are assigned a Q ticket before we know its a subio */
    unsigned long DSeqNo_Ghost;         /* this is a snapshot of the current state.DSeqNo @D, including requeues    */
    unsigned long CSeqNo_Ghost;         /* this is a snapshot of the current state.DSeqNo @D, including requeues    */
    unsigned long GSeqNo_Ghost;
    unsigned long SSeqNo_Ghost;
    unsigned long RSeqNo_Ghost;

    unsigned  int plug_state;           /* Plug status at each of the Q, I, D, and C main event points of interest  */
#       define PLUGSTATE_A 0x01         /* 0 = plug state was off at this event point                               */
#       define PLUGSTATE_Q 0x01         /* 0 = plug state was off at this event point                               */
#       define PLUGSTATE_M 0x01         /* 0 = plug state was off at this event point                               */
#       define PLUGSTATE_I 0x02         /* 1 = plug state was on  at this event point                               */
#       define PLUGSTATE_D 0x04
#       define PLUGSTATE_C 0x08

/* @@@ .ALL. moved into events->first,dispatch,completion typ_action_ext struct */
    unsigned long Q_DSeqNo;
    unsigned long Q_CSeqNo;             /* absolute CSeqNo at 'Q' event time, ref as C'                                 *//* C - C' = reorder amount in device/ctrl*/
    unsigned long D_QSeqNo;             /* absolute QSeqNo at 'D' event time, ref as Q'                                 *//* first D only                          */
    unsigned long D_CSeqNo;
    unsigned long C_QSeqNo;             /* absolute DSeqNo at 'C' event time, ref as D'                                 *//* D - D' = reorder amount in sched sort */
    unsigned long C_DSeqNo;             /* absolute DSeqNo at 'C' event time, ref as D'                                 *//* D - D' = reorder amount in sched sort */

/* @@@ .ALL. moved into events->first,dispatch,completion typ_action_ext struct */
    int           Q_InProgress;         /* #io queued but not completed, counts all io in sched+device                  */
    int           Q_InFlight;           /* #io dispatched but not completed, counts all io in device                    */
    int           D_InProgress;         /* #io queued but not completed, counts all io in sched+device                  */
    int           D_InFlight;           /* #io dispatched but not completed, counts all io in device                    */
    int           C_InProgress;         /* #io queued but not completed, counts all io in sched+device                  */
    int           C_InFlight;           /* #io dispatched but not completed, counts all io in device                    */

/* @@@ not really sure if these are used, but leave for now */
    int           D_QEventCnt;          /* #Q events that have happened when 'D' event happens (i.e. Q,Q,Q,D)           *//* loading  sched queue       */
    int           D_CEventCnt;          /* #C events that have happened when 'D' event happens (i.e. C,C,C,D)           *//* draining device queue      */
    double        D_QEventTime;         /* ms between 1st Q event and this D event                                      */
    double        D_CEventTime;         /* ms between 1st C event and this D event                                      */

    int           C_QEventCnt;          /* #Q events that have happened when 'C' event happens (i.e. Q,Q,Q,C)           *//* loading  sched  queue     */
    int           C_DEventCnt;          /* #D events that have happened when 'C' event happens (i.e. D,D,D,C)           *//* loading  device queue     *//* draing sched Q */
    double        C_QEventTime;         /* ms between 1st Q event and this C event                                      */
    double        C_DEventTime;         /* ms between 1st D event and this C event                                      */

    int           Q_DEventCnt;          /* #D events that have happened when 'Q' event happens (i.e. D,D,D,Q)           */
    int           Q_CEventCnt;          /* #C events that have happened when 'Q' event happens (i.e. C,C,C,Q)           */
    double        Q_DEventTime;         /* ms between 1st Q event and this C event                                      */
    double        Q_CEventTime;         /* ms between 1st D event and this C event                                      */

    int           sched_reorder;        /* the amount of sort   reordering done between Q and D by scheduler            */
    int           device_reorder;       /* the amount of device reordering done between D and C by storage              */

#   ifdef DEBUG
    int		  dbg_num_events;	/* Number of events on the following process ordered FIFO (first on top)        */
    typ_event    *dbg_events;           /* First event that was "absorbed" into this context                            */
    typ_event    *dbg_last_event;       /* Last  event on FIFO list, new events as encountered/processed added to end   */
#   endif

} typ_ioinfo;
/*
 * NOTES:
 * [1] cnt_merges indicates the number of items on subio list off of typ_io subio list, however, in some versions of the kernel
 *     we get 'C'ompletions against the subio and not the main io (this was to allow retry of just the remaining subio if need
 *     be, but was later abandonded as inefficient).  When C against subio happen, then the subio is removed from the active
 *     list in the old model and parked here:
 *
 *     * 3.10 only ...                  *
 *     * -------------------------------*
 *     int               completed_count;
 *     struct typ_event *completed_merges;  * needed for 3.10 in that subio complete as individuals and are removed and  *
 *                                          * thereby lost by the time the last subio complete and is reported.  Stash   * 
 *                                          * them here so at final report time in 3.10 we know what the subio were      *
 *     * -------------------------------*
 *
 *     but in the new model those were eliminated and instead a IS_COMPLETED flag was added to ioflags.  That is set for
 *     each subio as completed (if C on subio happen).  This way the separate accounting isn't required.
 */

/*
 * Try and move fields not needed or used by subio into separate data structures 
 * then do not allocate those data structures for subio to help manage allocated
 * memory size
 */
#define EVENTS info->events
typedef struct typ_io
{
    struct typ_io    *next;	/* active list				     */ /* FIFO list, last io created at front          */ /* time order top->bot (new->old)	*/
    struct typ_io    *subio;	/* "subio" merged io list, next		     */ /* list of merged io, add to bottom             */ /* time order bot->top (old->new)    */
    struct typ_io    *lstsubio;	/* "subio" end of chain                      */
    struct typ_io    *parent;	/* pointer to active list/mainio for subio   */ /* null if this *is* main io			*/ /* only set on subio in merge list   */
#   define isSubIO( _io ) (_io->mainio != NULL)				        /* TRUE if is a subio, meaning its no on active list directly itself                    */
#   define isMainIO(_io ) (_io->mainio == NULL)					/* TRUE if is a mainio,meaning its    on active list directly itself			*/
#   define on_list( _io ) (_io->mainio == NULL)					/* TRUE if is a mainio,meaning its    on active list directly itself			*/

    typ_ioinfo       *info;              /* pointer to data associated with mainio       *//* data/fields that we only need for mainio typ */

    long           ioflags;		/* io information flags				*/
#   define IS_COMPLETE       0x0002     /* C event has happened for this io             */
#   define VALID_FROM_SECTOR 0x0004     /* is 'A' event with remap info and valid sector*//* the 'from_*' fields are valid	*/
#   define IS_STAGED         0x0080	/* is on the 'starter' queue, not active queue  *//* before Q|M|F happens               */
#   define IS_FORCED         0x0100	/* this typ_io creation was forced              *//* the io started out of sequeunce    *//* e.g. first event was a D or C    	*/
#   define IS_SUBIO          0x0200	/* this typ_io is not a mainio                  *//* a subio in set of merged io        *//* mainio link is non-null		*/
#   define IS_PARTIAL        0x0400     /* A M|F happens without a Q - dont know mainio *//* we're missing parts of mainio      */
#   define IS_MAKEUP         0x0800     /* this is a "make up" subio to pad a partial   *//* a D or C event revealed full size  */
#   define CHECK_ONLY        0x1000     /* in find_event() check only, don't create one */
#   define IS_OVERLAPPED_IO  0x2000     /* a later active io overlaps this one          *//* both old and new io are so marked  */
#   define IS_CONFLICTED_IO  0x4000     /* a later active io overlaps this one, diff typ*//* aka a read overlaps a write, eg.   */
#   define IS_ORPHAN_IO      0x8000     /* set if this io is greater than some abs time *//* see state note [2]                 */
#   define IS_WHOLE_COMPLETE 0x00010000 /* set if C event covers whole request processed*/
#   define IS_BIO_COMPLETE   0x00020000 /* set if C event (any) covers subportion       */
#   define IS_EXCEPTION      0x00040000 /* set if multiple C behavior                   *//* output to exceptions file          */
#   define IS_POSTIO         0x00080000 /* set if on postio_list                        */
#   define IS_DUPE_COMPS     0x00100000 /* set if duplicate completions against req/bio */
#   define IS_SAMEIOSCHED    0x00200000 /* set if typ:sector+len in two io present      */ /* determined in best_match()	*/
#   define IS_SAMEIODRIVR    0x00400000 /* set if typ:sector+len in two io present      */ /* determined in best_match()	*//* @D time */
#   define IS_IOBARRIER      0x00800000 /* set if     sector+len are 0:0                */ /* io barrier, no movement, no D evt */

#   define IS_BIO_COVERED    0x10000000 /* temp flag set during part 'C' checking to indicate that this sub-BIO is covered by  */
                                        /* the current Complete event's sector:length.  This is set during 1st path and cleared*/
				        /* during 2nd pass, we want to be able to back out setting BIO_COMPLETE flag *if* we   */
					/* for some reason don't get full coverage of a partial Complete event.                */
    unsigned long specials;
#   define SPCFLG_00         0x00000001 /* Events missing 0:0 specifier except in A|Q event and is missing D event        *//* I=> D time in this case 		*/
#   define SPCFLG_XPARENT    0x00000002 /* This io is the parent of a X-split, ie first/main io and not the splitoff io   */
#   define SPCFLG_XPARENT2   0x00000004 /* This type of X parent can be split multiple times, 				  */
#   define SPCFLG_XTARGET    0x00000008 /* This io is the target of a X-split, ie 2nd/split  io and not the first/main io *//* IO can have both flags set       */
    															    /* but 1st will only have XPARENT   */
    															    /* and last in chain XTARGET only   */


    /* IO info; is composite across all subio if this is mainio on active    */
    /* --------------------------------------------------------------------- */
    unsigned int  locflg;		      /* 0 = no sector/length info, 0x1 = sector, 0x2 = length, 0x3=both		*/ /* valid sector+len info    */
#   define LOCFLG_SECTOR 0x0001
#   define LOCFLG_LENGTH 0x0002
    int           lstcpu;                     /* CPU # @'C' event time, are completions serviced on a diff cpu than initiator?  */ /* think numa!		*/
    char          lstact;                     /* last action;   the last (short) action processed from blkparse for this io  	*/ /* See Notes[1] below        */
    char          lsttyp[MAXLEN_EVTTYP];      /* last io typ;   the last (long ) io type string from last blkparse input line   */
    typ_event     lstevt;		      /* the first event info is captured in in-line copy of event below, this is last  */
					      /* event captured for this io, typically at end of io lifetime it will be the     */
					      /* 'C' complete event.  For subio, most the time it won't have any last events of */
					      /* significance.  But in some kernel versions, each subio does get its own C event*/
					      /* and this field captures that.							*/
    /* - - bgn:typ_event - - - - - - - - - - - - - - - - - - - - - - - - - - */
    unsigned int evtflags;
    int         from_major;                   /* If remap event was associated... captures only last remap? or only first?	*/
    int         from_minor;
    long        from_sector;
    int		  major;
    int		  minor;
    char          prmact;		      /* first/primary (main) action, e.g. evtact="UT", prmact = 'U'			*/ /* event that caused create  */
    unsigned char prmtyp;		      /* first/primary (main) io type							*/ /* see IOTYP_*               */
    unsigned char imbtyp;                     /* IMplicit Barrier sub-type                                                      */ /* 1st event imbtyp kept here*/
    unsigned int  iomask;                     /* flags from evttyp string                                                       */
    char	  evtact[MAXLEN_ACTION];      /* first action for this event							*/
    char          evttyp[MAXLEN_EVTTYP];      /* first type   for this event, associated with action.timestamp below		*/ /* RWBS field, see notes [3] */
    typ_action	  timeinfo;	              /* first timestamp for this event                                                 */
    unsigned int  MOVED(pid);		      /* pid that initiated this io (if evtact is A,Q,M,or F above)			*/
    unsigned int  MOVED(cpu);		      /* cpu that initiated this io					                */
    unsigned int  MOVED(seqnum);
    long          length;		      /* total length io, including any subio merges if present			        */
    long          sector;		      /* starting lba of io, after any subio merges if so present			*/
    char          procname[MAXLEN_PROCNAME];  /* initiator process name, really only of value @first event referencing io  	*/
    char	  extras  [MAXLEN_EVTXTRAS];  /* shouldn't be any, but never know...  this is more UT only stuff                */
    /* - - end:typ_event - - - - - - - - - - - - - - - - - - - - - - - - - - */
    long          orig_length;		      /* Some splits, split from the front reducing the original A/Q data captured here */
    long          orig_sector;		
    long          over_length;		      /* Sometimes a 2nd/later io overlaps are prior one removing the overlap from the  */
    long          over_sector;		      /* prior io, reducing its size as typ the overlap is on the trailing part         */
    long          split_address;	      /* Some X split events have a 'address' value unrelated to the disk, making 1st   */
    					      /* split impossible to size until follow on 'G' events, but 2nd split use this    */
    /* --------------------------------------------------------------------- */

    /* Tracking split state						     */
    /* --------------------------------------------------------------------- */
    int               xsplit_level;	       /* 0=main/orig io, 1=1st split, etc.						*/
    struct typ_io    *xparent;		       /* Parent (orig/main) io for this xsplit off io, if any				*/
#   ifdef NOTYET
    typ_action_ext split;		       /* If this io was a target of a split, the relevant information is held here     */
					       /* it is hierarchical in nature, inherited from its parent io at split.          */    
#   endif


    /* now all the sub-events that make up this event, may be from subio     							*/
    unsigned char event_count[16];	      /* track number of events of the above types.					*/
#   define EVTNDX_A    	  0		      
#   define EVTNDX_Q	  1
#   define EVTNDX_RESV2   2
#   define EVTNDX_RESV3   3
#   define EVTNDX_S	  4
#   define EVTNDX_G	  5
#   define EVTNDX_M	  6
#   define EVTNDX_F	  7
#   define EVTNDX_X	  8
#   define EVTNDX_R	  9
#   define EVTNDX_I	 10
#   define EVTNDX_D	 11
#   define EVTNDX_C	 12
#   define EVTNDX_RESV13 13  
#   define EVTNDX_RESV14 14
#   define EVTNDX_x	 15
    					      /* The following are counts only, do not have mask counterpart			*/
    					      /* And since won't participate within an io context, are define here only for use */
    					      /* at a macro/state:file level 							*/
#   define EVTNDX_P      16		      /* 'P'lug events									*/
#   define EVTNDX_U      17		      /* 'U'nplug events								*/
#   define EVTNDX_T      18		      /* 'T'imer events									*/
#   define EVTNDX_O      19		      /* other/misc events (N,@,m,-)							*/
    int         event_mask;		      /* which events have been encountered 						*/
					      /* subio only have the first few (A,{Q|M|F}), although it may have separate C     */
#   define EVTMSK_A    ( 1<<EVTNDX_A)	      /*  0x0001									*/
#   define EVTMSK_Q    ( 1<<EVTNDX_Q)         /*  0x0002									*/
					      /*  0x0004									*/
					      /*  0x0008									*/
#   define EVTMSK_S    ( 1<<EVTNDX_S)         /*  0x0010									*/
#   define EVTMSK_G    ( 1<<EVTNDX_G)         /*  0x0020									*/
#   define EVTMSK_M    ( 1<<EVTNDX_M)         /*  0x0040									*/
#   define EVTMSK_F    ( 1<<EVTNDX_F)         /*  0x0080									*/
#   define EVTMSK_X    ( 1<<EVTNDX_X)         /*  0x0100									*/
#   define EVTMSK_R    ( 1<<EVTNDX_R)         /*  0x0200									*/
#   define EVTMSK_I    ( 1<<EVTNDX_I)         /*  0x0400									*/
#   define EVTMSK_D    ( 1<<EVTNDX_D)         /*  0x0800									*/
#   define EVTMSK_C    ( 1<<EVTNDX_C)         /*  0x1000									*/
					      /*  0x2000									*/
					      /*  0x4000									*/
#   define EVTMSK_x    ( 1<<EVTNDX_x)         /*  0x8000									*/

    typ_ctracker *ctracker;		/* only used if this is moved to post-queue to wait for additional C events     */

    int  	 ghost_gradiant;	/* which event type forced the creation of this mainio/request structure        */
#   define GHOST_NONE   0		/* this is not a ghost                                                          */
#   define GHOST_S_EVT  1		/* Sleep     , but no pre-existing request/mainio found				*/
#   define GHOST_G_EVT  2		/* GetRequest, but no pre-existing request/mainio found				*/
#   define GHOST_I_EVT  3		/* Insert    , but no pre-exisitng request/mainio found				*//* special as is this Q|I or D|R}I ? */
#   define GHOST_D_EVT  4		/* Dispatch  , but no pre-existing request/mainio found                         */
#   define GHOST_R_EVT  5		/* Requeue   , but no pre-existing request/mainio found                         */
#   define GHOST_C_EVT  6		/* Complete  , but no pre-existing request/mainio found				*//* not really used, don't create ghost */
#   define GHOST_M_EVT  7		/* BMerge    , but no pre-existing request/mainio found (on staged)             */
#   define GHOST_F_EVT  8		/* FMerge    , but no pre-existing request/mainio found (on staged)             */
#   define GHOST_x_EVT  9		/* reserved									*/
#   define GHOST_M_2ND  10		/* FMerge    , but no pre-existing target request/mainio to merge into          *//* 2nd hand ghost creation		*/
#   define GHOST_F_2ND  11		/* BMerge    , but no pre-existing target request/mainio to merge into          *//* 2nd hand ghost creation		*/

    char          event_sigs[MAXLEN_EVTSIGS]; /* io context event signatures for state machine					*/

} typ_io;

#define set_event_mask( _io, _evtndx ) \
	io->event_mask |= (1<<_evtndx); io->event_count[_evtndx]++ ; \
	dprintf( "DBG[%05d]: %s(set_event_mask:: evtndx=%d,%d:%04X)\n", __LINE__, __func__, _evtndx, io->event_count[_evtndx], io->event_mask )



/* NOTES:
 * [1] event action code, nominally this is 1 character long; Q, {A,M,F}, S, G, I, D, C ... P, U, ... occassionally UT,
 *     until recently 9/2020 the program had only seen a single character event type (then UT showed up and it 'missed'
 *     this unplug event which caused issues).  So now this field has a raw or long format capture which is the whole
 *     string and a short/main format that is just 1 character long.  For example UT, the main event is U.  The parser
 *     for this field will handle multiple character strings -- but will %Warn on any multiple action other than known
 *     ones -- which right now is just 'UT'.  But part of this parse is also to defined the main/short 1 character event
 *     type into a separate field for simplified use.  The parser will also warn for *any* event type that it is unknown
 *     to it as a means of helping identify program shortfalls.
 * [2] event type can be as short as 'R' or 'W' (read or write) or as long as  FWSMB (force write sync meta barrier io).
 *     Typ = 'R'ead, 'W'rite, 'D'iscard, 'B'arrier, or 'N'one (!write & bytes!=0:ioctl?). {this is considered the main type}
 *           'F'lush, but only if this is BEFORE the R|W|D|N letter code e.g. FWSMB
 *           'F'ua,   but only if this is AFTER  the R|W|D|N letter code e.g.  WSF
 *           'S'yncronous, 'M'eta, or Read'A'head, which occur after main io type
 *           note: the above codes are set within the binary data within the kernel before data is exported.
 *           note: 'B'arrier seems to have been dropped from latest upstream blkparse utility.
 *
 * [3] IOTYP (RWBS) field: evttyp/lsttyp (old model: iotyp)
 * 
 * from blkparse utility: 
 *     (note the action field is binary set in kernel and passed out, this is just parsing that data)
 *
 * static inline void fill_rwbs(char *rwbs, struct blk_io_trace *t)
 * {
 *         int f = t->action & BLK_TC_ACT(BLK_TC_FLUSH);    'F' 1st plus
 * 
 *         int d = t->action & BLK_TC_ACT(BLK_TC_DISCARD);  'D' or
 *         int w = t->action & BLK_TC_ACT(BLK_TC_WRITE);    'W' or
 *                 t->bytes != 0                            'R' or
 *                                                          'N' none of those for 2nd plus
 * 
 *         int u = t->action & BLK_TC_ACT(BLK_TC_FUA);      'F' 3rd plus   {yikes 'F' is positional, will always be right after D|W|R|N
 * 
 *         int a = t->action & BLK_TC_ACT(BLK_TC_AHEAD);    'A' 4th        {this io is or includes readahead, so only should be with 'R'}
 *         int s = t->action & BLK_TC_ACT(BLK_TC_SYNC);     'S' 5th        {typically direct io                                         }
 *         int m = t->action & BLK_TC_ACT(BLK_TC_META);     'M' 6th
 * 
 * 
 * Example #1:
 * 8,97  25       31     0.000244187 29074  A FWS 0 + 0 <- (253,2) 0
 * 8,96  25       32     0.000244444 29074  Q FWS [(null)]
 * 8,96  25       33     0.000247287 29074  G FWS [(null)]
 * 8,96  25       34     0.000247373 29074  P   N [(null)]
 * 8,96  25       35     0.000248195 29074  I FWS [(null)]
 * 8,96  25       36     0.000248314 29074  U   N [(null)] 1
 * :
 * 8,96  27        1     0.005730559     0  C  WS 0 [0]
 *
 * Example #2:
 * 8,96   2     1438   118.503083470  5483  I  WM 3672202488 + 32 [(null)]
 * 8,96   2     1439   118.503083709  5483  I  WM 3672202808 + 32 [(null)]
 * 8,96   2     1440   118.503083974  5483  I  WM 3672216408 + 32 [(null)]
 *
 */

/* FILE: blktrace.conf
 * #
 * # group <name>: device1, device2, ...
 * #
 * ##group mpatha : sda, sdb, sdc, sdd
 * ##group optdata: sda, sdb, sdc, sdd, sde, sdf, sdg, sdh
 *         [1]      [2]--->
 *
 * [1] typ_conf_group, includes an array of [2]
 * [2] typ_conf_grpdevice
 */

typedef struct typ_conf_grpdevice                       /* conf file group name:device info                              */
{
    char device_name[256];                              /* device name to be included in typ_conf_group:group_name       */
    void *private;                                      /* allocated at process_group() time                             */
} typ_conf_grpdevice;

typedef struct typ_conf_group
{
    struct typ_conf_group     *next;
    char                       group_name[256];         /* group name from conf file                                      */
    void                      *private;                 /* allocated at parse_config() time when reading in blktrace.conf */
    int                        device_cnt;              /* number of device names from conf file in this group            */
    struct  typ_conf_grpdevice devices[1];              /* array of device_cnt device names within this group             */
} typ_conf_group;

int             groups_cnt = 0;                         /* number of defined groups in 'groups', read in from blktrace.conf */
typ_conf_group *groups = NULL;                          /* list of configured/defined groups from conf file                 */
typ_conf_group *tmp_conf_group = NULL;			/* used to point to group w/256 device array allocated for .conf    */
							/* file parsing.  Only allocated once and reused as needed          */




static typ_io req_nocontext;		/* these should be zero'd out or otherwise initialized at setup time */
static typ_io req_nonio;		/* ditto							     */
static typ_io req_message;		/* ditto							     */
#define REQ_NOCONTEXT ((typ_io *)&req_nocontext)
#define REQ_NONIO     ((typ_io *)&req_nonio)
#define REQ_MESSAGE   ((typ_io *)&req_message  )

/* 
 * "FAULT" processing.
 *
 * The purpose of the fault macros is to allow hiding _exit() behind the macro and not to exit if at all possible.  The different
 * macros handle the different faults and the conf_terminate_onfault_* in general controls default behavior, with notarget fault
 * for example being "ignored" in non-debug versions and exiting in -DDEBUG version.  So the -DDEBUG version shoudl retain the 
 * current "exit on everything" while the normal version used by users will attempt to keep going if at all possible rather than
 * bombing out.
 *
 * 1) normal exit()   like when args not provided, no additional information other than printf messages needed
 * 2) runtime exit(), like can't alloc memory. These use _exit() macro that dumps out function:lineno of where problem happened
 *                    These cause an exit that cannot be avoided or deferred.
 * 3) program fault , like trying to process a A|Q event in a place where those events should not show up -- this is a program
 *                    logic fault.  Some of the decision code within the program is a bit wonky, so there is a fair amount of
 *                    defensive programming that tries to validate the end state as to validity. Other programming faults could
 *                    be no C events detected in file after a reasonable time, possibly indicating the program does not know how
 *                    to deal with NVMe event sequences which don't have C events...
 *                    These cause an exit that cannot be avoided or deferred.
 * 4) missing feature fault.
 *    - -DDEBUG: _exit()
 *    - normal : flag the event as exception, discarded, nofeature, and return a null typ_io pointer.
 *               exception: something about the event is "special" and output this event into exceptions file
 *               discarded: this event was not processed for some reason
 *               nofeature: is wasn't processed because the program has not yet added functionality to handle this
 *                          event or the event in the current context
 *    These causes an exit that cannot be avoided or deferred if the -DDEBUG compile time is set -or- if the config_terminate_on_nofeature
 *    policy is set. Otherwise the program attempts to just discard the event.  This may cause other secondary issues, up to and including
 *    inducing a program fault.
 * 5) data processing fault. (notarget)
 *    - -DDEBUG: _exit()
 *    - normal : flag the event as exception, discarded, notareget and return a null typ_io pointer.
 *               exception: something about the event is "special" and output this event into exceptions file
 *               discarded: this event was not processed for some reason
 *                notarget: this wasn't processed due to program being unable to determine a typ_io target that
 *                          this event belongs to.
 *    These causes an exit that cannot be avoided or deferred if the -DDEBUG compile time is set -or- if the config_terminate_on_notarget
 *    policy is set. Otherwise the program attempts to just discard the event.  This may cause other secondary issues, up to and including
 *    inducing a program fault.
 *    
 * The purpose of the different levels of faults is to allow a the program to try and complete if it can, even when faced with events that
 * it cannot properly process -- either due to missing code (feature) or event context such that the program fails to identify an existing
 * typ_io target that "owns" this event.
 *
 * Rather than simply giving up as in the first iteration of this program version, punt the event and keep moving.  A majority of the issues
 * that were stopping event processing would have minor side affects if the event(s) were simply ignored.
 *
 * When ignored faults occur, the non-zero counters are displayed at SUM time to alert the user that discards (a special type of exception)
 * has occurred and which class of discard (nofeature|notarget|<other:unknown>) has occurred.
 */
#define         RUNTIME_FAULT(_event,_typ,_rsn,_ctl) return(_typ              _runtime_fault(_event,__FUNCTION__, __LINE__, _rsn, _ctl))
#define        NOTARGET_FAULT(_event,_typ,_rsn,_ctl) return(_typ             _notarget_fault(_event,__FUNCTION__, __LINE__, _rsn, _ctl))
#define    DATA_FEATURE_FAULT(_event,_typ,_rsn,_ctl) return(_typ      _data_processing_fault(_event,__FUNCTION__, __LINE__, _rsn, _ctl)) 
#define    MISS_FEATURE_FAULT(_event,_typ,_rsn,_ctl) return(_typ      _missing_feature_fault(_event,__FUNCTION__, __LINE__, _rsn, _ctl))
#define    PROG_FEATURE_FAULT(_event,_typ,_rsn,_ctl) return(_typ   _program_processing_fault(_event,__FUNCTION__, __LINE__, _rsn, _ctl))

#define   PROG_FEATURE_FAULT1(   _io   ,_typ,_rsn,_ctl) return(_typ  _program_processing_fault1(_io   ,__FUNCTION__, __LINE__, _rsn, _ctl))
#define   PROG_FEATURE_FAULT1_NO(_io   ,_typ,_rsn,_ctl)              _program_processing_fault1(_io   ,__FUNCTION__, __LINE__, _rsn, _ctl)
#define FAULT_CTL(_arg,_typ,_ftyp) \
     if (ctl == CTL_FAULT_FORCE ) _typ = -1; \
else if (ctl == CTL_FAULT_IGNORE) _typ =  1; \
else if ((conf_terminate_onfault_##_ftyp) != 0) _typ = -1; \
else                              _typ =  1;

/*
DATA FEATURE FAULT = best_match()/2nd hand merging failed
                     multiple conflicting matches, couldn't determine match
                     best_match() failed to identify match
NOTARGET     FAULT = no typ_io matched for event
                     no match on 'C' event, should ignore mostly but the logic should have detected orphan C near front of file and ignored,
                        so rather than forcing CTL_FAULT_IGNORE, use DEFAULT.  If this triggers in -DDEBUG version, then we need to fix the
                        front of file logic, or better understand the circumstances when/where this can happen
PROG FEATURE       = leave default in non -DDEBUG as default an conf onfault off, use manual ctl FORCE on those we really shouldn't continue from,
                     there may be ones we can continue with, like an event that timesout on the post queue but still isn't finished.  Continue
                     anyway aftering noting the issue.
*/


static int _set_guards(typ_event *this, int lineno)
{
    this->_guardians[0] = 
    this->_guardians[1] = 
    this->_guardians[2] = 
    this->_guardians[3] = 0x3BCDAAAA5555DCB3UL;
    return(0);
}
static int _chk_guards(typ_event *this, int lineno)
{
    if (((this->_guardians[0] | this->_guardians[1] |
          this->_guardians[2] | this->_guardians[3] ) ^ 0x3BCDAAAA5555DCB3UL ) == 0)
        return(0);
    printf( "%%F[%05d]-Guardians on event have been overwritten! Exit.\n", lineno );
    exit(1);
}



