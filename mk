#!/bin/bash
#
#
 cc                  -O2 -o blktrace_merge         blktrace_merge.c   -std=c99 -lm          |& tee _cc.log 
#cc                  -O2 -o blktrace_merge.O2      blktrace_merge.c   -std=c99 -lm          |& tee _cc.log
#cc                  -O3 -o blktrace_merge.O3      blktrace_merge.c   -std=c99 -lm          |& tee _cc.log
#cc -pg -g3          -O0 -o blktrace_merge.dbg     blktrace_merge.c   -std=c99 -lm -DDEBUG  |& tee _cc.log
#cc -pg -rdynamic -g -O0 -o blktrace_merge.sym     blktrace_merge.c   -std=c99 -lm -lg      |& tee _cc.log		# to get cores with symbols
#cc -pg -rdynamic -g -O0 -o blktrace_merge.sym     blktrace_merge.c   -std=c99 -lm -lg      |& tee _cc.log		<< to get cores with symbols
#
if [ -e _cc.log ]
then
    if [ ! -s _cc.log ] ; then rm -f _cc.log ; fi
fi
if [ "$1" != "FRAMEWORK" ] ; then exit ; fi
#
#
mkdir -p ~/.bin/.local           &> /dev/null	        # contains all local account binaries via a symlink to actual location(s)
mkdir -p ~/.bin/ioperformance    &> /dev/null           # blktrace_merge binary lives here
mkdir -p ~/.config/blktrace      &> /dev/null           # blktrace.conf         lives here
cp    -v blktrace_merge      ~/.bin/ioperformance/
if [ ! -e ~/.config/blktrace/blktrace_merge.conf ]
then
   cp -v blktrace.conf       ~/.config/blktrace/
fi
_pwd=$(pwd)
cd       ~/.bin/.local
rm       blktrace_merge          &> /dev/null
ln -sv ../ioperformance/blktrace_merge ./blktrace_merge
cd $_pwd
#
#
## .bashrc add ./.bin/.local to PATH so binaries can be found
## if [ $(printenv | grep "^PATH=" | grep "/\.bin/\.local"   | wc -l) -eq 0 ] ; then PATH=$PATH:$HOME/.bin/.local   ; fi  # This should be all we really want!
## if [ $(printenv | grep "^PATH=" | grep "/\.bin/\.private" | wc -l) -eq 0 ] ; then PATH=$PATH:$HOME/.bin/.private ; fi  # ...and this for our local acccount helpers
exit
#

