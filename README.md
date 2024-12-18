# blktrace_merge

<code>blktrace_merge</code> takes as input a &lt;devicename>.blktrace.txt event stream file from the utility blkparse and attempts to merge the individual events into a single io context. It performs the tedious math between the initial io showing up within the event stream (A|Q events) and other key events like 'D'ispatch to driver and 'C'ompletion of the io.  It outputs those values in a single line for the set of merged events associated with a single io.</br>

*note: this program was previously named blktrace_parse, which was a bit of a misnomer as it didn't actually parse anything, but rather merges related individual blktrace events together. It was originally written while employed by Red Hat within the  storage support group.  It was available on my People page there before I retired in May 2024.  I continue to play with the code and tweak it on an as bored basis -- hey some ppl do puzzles I do code. I wrote it originally because I was tired of "doing the math" between events.*

<code>
FILE: nvme0n1.blktrace.txt
  
#Maj,Mn CPU   SeqNo     Seconds     PID  Evt Typ Sector   +Len Description
#------ --- ------- --------------- -----|--|---|---------+--- -------------------------------------
<b>259,0    3        1     0.000000000  1867  A   W 746579600 + 8 <- (253,3) 578805392</b>
<b>259,0    3        2     0.000000149  1867  A   W 763360912 + 8 <- (259,3) 746579600</b>
<b>259,0    3        3     0.000000299  1867  Q   W 763360912 + 8 [dmcrypt_write/2]</b>
<b>259,0    3        4     0.000005301  1867  G   W 763360912 + 8 [dmcrypt_write/2]</b>
259,0    3        5     0.000005809  1867  P   N [dmcrypt_write/2]
259,0    3        6     0.000006262  1867  A  WS 746579608 + 8 <- (253,3) 578805400
259,0    3        7     0.000006303  1867  A  WS 763360920 + 8 <- (259,3) 746579608
259,0    3        8     0.000006341  1867  Q  WS 763360920 + 8 [dmcrypt_write/2]
259,0    3        9     0.000006937  1867  M  WS 763360920 + 8 [dmcrypt_write/2]
<b>259,0    3       10     0.000009435  1867  D   W 763360912 + 16 [dmcrypt_write/2]</b>
<b>259,0    3       11     0.023558761     0  C   W 763360912 + 16 [0]</b>
</code>

The above events associated with a single io are merged into a single io context and upon 'C'ompletion of the io, the Q2I, Q2D, D2C and Q2C information is output for this 1 io as a single (long) line.

<code>
$ ./blktrace_merge nvme0n1.blktrace.txt

FILE: nvme0n1.blktrace.merged.txt

#PS|Prg :Flt   Queue(s)     Q2I(ms)   Q2D(ms)       D2C(ms)            Q2C(ms)     CPU      PID Typ  Size         Sector...
#--|----:--- ------------- -------- ------------- -------------+---- ------------- --- -------- ---- ---- --------------... 
<b>  0|   1:  1   0.000000000   -.----      0.009435     23.549326 % 99     23.558761   3     1867 W      16      763360912...</b>

 ...          Seek        Description            reQ Merges:sectors,...
 ...        ------------- ---------------------- --- ---:-----------------
 ...                    <b>0 [dmcrypt_write/2]        0   2:  8,  8.</b>
         
</code>

In the above case, the total <code>iostat</code> await time is the Q2C time period which can be broken down into sub-parts:
<ul><li>Q2D, "kernel-side" time, and</li>
<li>D2C, "storage-side" time.</li></ul>

In most cases the D2C storage related time dominates the total Q2C time -- in this case 23.549ms is storage and 0.009ms is kernel based time.  The D2C time is a block of time that cannot be further subdivided and covers a fixed code path amount of time in the driver, the host (storage) bus adapter's (HBA) firmware and hardware time to pass the io request out to the transport, the transport time (for fibre channel this includes switch time), the target or storage controller at the far end of the storage bus transport, the physical storage time, and the returned status and data path back to through the HBA to the kernel's io done routine.

The *blktrace_merge* program creates a number of different output files beyond the main devicename.blktrace.merged.txt output.



