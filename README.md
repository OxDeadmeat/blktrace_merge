# blktrace_merge


<code>blktrace_merge</code> takes as input a &lt;devicename>.blktrace.txt event stream file from the utility blkparse and attempts to merge the individual events into a single io context.

<code>
FILE: nvme0n1.blktrace.txt
  
#Maj,Mn CPU   SeqNo     Seconds     PID  Evt Typ Sector   +Len Description
#------ --- ------- --------------- -----|--|---|---------+--- -------------------------------------
259,0    3        1     0.000000000  1867  A   W 746579600 + 8 <- (253,3) 578805392
259,0    3        2     0.000000149  1867  A   W 763360912 + 8 <- (259,3) 746579600
259,0    3        3     0.000000299  1867  Q   W 763360912 + 8 [dmcrypt_write/2]
259,0    3        4     0.000005301  1867  G   W 763360912 + 8 [dmcrypt_write/2]
259,0    3        5     0.000005809  1867  P   N [dmcrypt_write/2]
259,0    3        6     0.000006262  1867  A  WS 746579608 + 8 <- (253,3) 578805400
259,0    3        7     0.000006303  1867  A  WS 763360920 + 8 <- (259,3) 746579608
259,0    3        8     0.000006341  1867  Q  WS 763360920 + 8 [dmcrypt_write/2]
259,0    3        9     0.000006937  1867  M  WS 763360920 + 8 [dmcrypt_write/2]
259,0    3       10     0.000009435  1867  D   W 763360912 + 16 [dmcrypt_write/2]
259,0    3       11     0.023558761     0  C   W 763360912 + 16 [0]
</code>

The above events associated with a single io are merged into a single io context and upon 'C'ompletion of the io, the Q2I, Q2D, D2C and Q2C information is output for this 1 io.

<code>
$ ../blktrace_merge nvme0n1.blktrace.txt

FILE: nvme0n1.blktrace.merged.txt

#PS|Prg :Flt   Queue(s)     Q2I(ms)   Q2D(ms)       D2C(ms)            Q2C(ms)     CPU      PID Typ  Size         Sector         Seek  Description            reQ Merges:sectors,...
#--|----:--- ------------- -------- ------------- -------------+---- ------------- --- -------- ---- ---- -------------- ------------- ---------------------- --- ---:-----------------
  0|   1:  1   0.000000000   -.----      0.009435     23.549326 % 99     23.558761   3     1867 W      16      763360912             0 [dmcrypt_write/2]        0   2:  8,  8.
</code>
