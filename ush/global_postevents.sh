#!/bin/ksh
# Run under ksh (converted to WCOSS)
# This script encodes the analysis into the PREPBUFR reports
#  (interpolated to obs. locations)
#
# It is normally executed by the script ????????????????????.sh
#  but can also be executed from a checkout parent script
# -------------------------------------------------------------
#
# Script history log:
# ????-??-??  DKeyser (???)    -- Original version for implementation
# ????-??-??  DKeyser (???)    -- Transition to WCOSS (IBM idataPlex)
# 2017-03-09  DStokes          -- Modifications to run on Cray-XC40 as
#                                 well as IBM IdataPlex.
#                              -- changed out of acceptable range "exit 555" to
#                                 "exit 55" so that the intended code would be
#                                 returned to parent (aids error diagnosis)

set -aux

qid=$$

# Positional parameters passed in:
#   1 - path to COPY OF input prepbufr file --> becomes output prepbufr
#       file upon successful completion of this script (note that input
#       prepbufr file is NOT saved by this script)
#   2 - expected center date in PREPBUFR file (YYYYMMDDHH)

# Imported variables that must be passed in:
#   DATA  - path to working directory
#   SANL  - path to COPY OF global simga analysis file 1 (valid at either
#            center date of PREPBUFR file or nearest cycle time prior to
#            center date of PREPBUFR file which is a multiple of 3)
#            (Note: Right now, this file will be an analysis only if
#                   the valid cycle time is 00, 06, 12 or 18, otherwise it will
#                    be a 3-hour forecast valid at this time)
#   SANLA - path to COPY OF global simga analysis file 2 (either null if SANL
#            is valid at center date of PREPBUFR file or valid at nearest
#            cycle time after center date of PREPBUFR file which is a multiple
#            of 3 if SANL is valid at nearest cycle time prior to center date
#            of PREPBUFR file which is a multiple of 3)
#            (Note: Right now, this file will be an analysis only if
#                   the valid cycle time is 00, 06, 12 or 18, otherwise it will
#                    be a 3-hour forecast valid at this time)
#   PSTX  - path to GLOBAL_POSTEVENTS program executable

# Imported variables that can be passed in:
#   pgmout   - string indicating path to for standard output file (skipped
#              over by this script if not passed in)

cd $DATA
PRPI=$1
if [ ! -s $PRPI ] ; then exit 1 ;fi
CDATE10=$2

rm $PRPI.postevents

pgm=`basename  $PSTX`
if [ -s $DATA/prep_step ]; then
   set +u
   . $DATA/prep_step
   set -u
else
   [ -f errfile ] && rm errfile
   unset FORT00 `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`
fi

echo "      $CDATE10" > cdate10.dat
export FORT11=$PRPI
#####export FORT12=$SANL
#####export FORT13=$SANLA

# The GLOBAL_POSTEVENTS code will soon, or may now, open GFS spectral
# coefficient guess files using sigio routines (via W3EMC routine GBLEVENTS)
# via explicit open(unit=number,file=filename) statements.  This conflicts with
# the FORTxx statements above.  One can either remove the explicit open
# statements in the code or replace the above FORTxx lines with soft links.
# The soft link approach is taken below.

ln -sf $SANL              fort.12
ln -sf $SANLA             fort.13

export FORT15=cdate10.dat
export FORT51=$PRPI.postevents

#### THE BELOW LIKELY NO LONGER APPLIES ON WCOSS
#The choice in the first  line below MAY cause a failure
#The choice in the second line below works!
set +u
####[ -n "$LOADL_PROCESSOR_LIST" ]  &&  XLSMPOPTS=parthds=2:stack=64000000
[ -n "$LOADL_PROCESSOR_LIST" ]  &&  XLSMPOPTS=parthds=2:stack=20000000
set -u

TIMEIT=${TIMEIT:-""}
[ -s $DATA/time ] && TIMEIT="$DATA/time -p"
SITE=${SITE:-""}
sys_tp=${sys_tp:-$(getsystem)}
getsystp_err=$?
if [ $getsystp_err -ne 0 ]; then
   msg="***WARNING: error using getsystem to determine system type and phase"
   set +u
   [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
   set -u
fi
echo sys_tp is set to: $sys_tp
launcher_PSTX=${launcher_PSTX:-""}
$TIMEIT $launcher_PSTX $PSTX < /dev/null > outout  2> errfile
err=$?
###cat errfile
cat errfile >> outout
cat outout >> postevents.out
set +u
[ -n "$pgmout" ]  &&  cat outout >> $pgmout
set -u
rm outout
set +x
echo
echo 'The foreground exit status for GLOBAL_POSTEVENTS is ' $err
echo
set -x
if [ -s $DATA/err_chk ]; then
   $DATA/err_chk
else
   if test "$err" -gt '0'
   then
######kill -9 ${qid} # need a WCOSS alternative to this even tho commented out
                     #  in ops
      exit 55
   fi
fi

if [ "$err" -gt '0' ]; then
   exit 9
else
   mv $PRPI.postevents $PRPI
fi

exit 0
