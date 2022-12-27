#!/bin/ksh
# Run under ksh (ready for WCOSS)


# This script performs the GLERL-adjustment on a fixed set of observations near
#  the shore of the Great Lakes
#
# It is normally executed by the script prepobs_makeprepbufr.sh
#  but can also be executed from a checkout parent script
# -----------------------------------------------------------------------

set -aux

qid=$$

# Positional parameters passed in:
#   1 - path to COPY OF input prepbufr file --> becomes output prepbufr
#       file upon successful completion of this script (note that input
#       prepbufr file is NOT saved by this script)
#   2 - expected center date in PREPBUFR file (YYYYMMDDHH)        

# Imported variables that must be passed in:
#    DATA     - path to working directory
#    GLRD     - path to GLERL dictionary file
#    GLRX     - path to PREPOBS_GLERLADJ executable
#    TANK     - Path to directory containing daily lake average temperature
#               files

# Imported variables that can be passed in:
#   jlogfile - string indicating path to joblog file (skipped over by this
#              script if not passed in)
#   pgmout   - string indicating path to for standard output file (skipped
#              over by this script if not passed in)

cd $DATA
PRPI=$1
if [ ! -s $PRPI ] ; then exit 1;fi
CDATE10=$2

jlogfile=${jlogfile:=""}

pgm=`basename  $GLRX`
if [ -s $DATA/prep_step ]; then
    set +u
    . $DATA/prep_step
    set -u
else
    [ -f errfile ] && rm errfile
    unset FORT00 `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`
fi
    #confirm existance of Lake temperature file
    #file size should be 18202 bytes
    #if file not available for current date, look back up to 3 days
         #per advice from John Kelley, job should not run if lake temps are
         # more than 3 days old.
if [ -s ${TANK}/${PDY}/wtxtbul/glsea-temps.dat ] ; then
    export FORT11=${TANK}/${PDY}/wtxtbul/glsea-temps.dat
elif [ -s ${TANK}/${PDYm1}/wtxtbul/glsea-temps.dat ] ; then
    export FORT11=${TANK}/${PDYm1}/wtxtbul/glsea-temps.dat
    msg="WARNING: Lake temperatures for GLERL adjustment ${PDY} ${cyc}Z are \
dated by 1 day.  Missing lake temperature file is: \
${TANK}/${PDY}/wtxtbul/glsea-temps.dat."
#   echo "$msg" | mail.py -c ${email_GLERL}
    [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
elif [ -s ${TANK}/${PDYm2}/wtxtbul/glsea-temps.dat ] ; then
    export FORT11=${TANK}/${PDYm2}/wtxtbul/glsea-temps.dat
    msg="WARNING: Lake temperatures for RTMA GLERL adjustment ${PDY} ${cyc}Z \
are dated by 2 days.  Missing lake temperature file is: \
${TANK}/${PDY}/wtxtbul/glsea-temps.dat."
    echo "$msg" | mail.py -c ${email_GLERL}
    [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
elif [ -s ${TANK}/${PDYm3}/wtxtbul/glsea-temps.dat ] ; then
    export FORT11=${TANK}/${PDYm3}/wtxtbul/glsea-temps.dat
    msg="WARNING: Lake temperatures for RTMA GLERL adjustment ${PDY} ${cyc}Z \
are dated by 3 days.  Missing lake temperature file is: \
${TANK}/${PDY}/wtxtbul/glsea-temps.dat."
    echo "$msg" | mail.py -c ${email_GLERL}
    [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"

#elif [ -s ${TANK}/${PDYm4}/wtxtbul/glsea-temps.dat ] ; then
#    export FORT11=${TANK}/${PDYm4}/wtxtbul/glsea-temps.dat
#    msg="WARNING: Lake temperatures for RTMA GLERL adjustment ${PDY} ${cyc}Z are dated by 4 days."
#    echo "$msg" | mail.py
#    [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
#elif [ -s ${TANK}/${PDYm5}/wtxtbul/glsea-temps.dat ] ; then
#    export FORT11=${TANK}/${PDYm5}/wtxtbul/glsea-temps.dat
#    msg="WARNING: Lake temperatures for RTMA GLERL adjustment ${PDY} ${cyc}Z are dated by 5 days."
#    echo "$msg" | mail.py
#    [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
#elif [ -s ${TANK}/${PDYm6}/wtxtbul/glsea-temps.dat ] ; then
#    export FORT11=${TANK}/${PDYm6}/wtxtbul/glsea-temps.dat
#    msg="WARNING: Lake temperatures for RTMA GLERL adjustment ${PDY} ${cyc}Z are dated by 6 days."
#    echo "$msg" | mail.py
#    [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
#elif [ -s ${TANK}/${PDYm7}/wtxtbul/glsea-temps.dat ] ; then
#    export FORT11=${TANK}/${PDYm7}/wtxtbul/glsea-temps.dat
#    msg="WARNING: Lake temperatures for RTMA GLERL adjustment ${PDY} ${cyc}Z are dated by 7 days."
#    echo "$msg" | mail.py
#    [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
else
	#procesing can't run due to missing water temps - ABORT SILENTLY
    set +x
    echo
    echo "WARNING in prepobs_glerladj.sh: Lake water temperature file is \
missing - processing can't run!  Aborting prepobs_glerladj.sh, but allowing \
prepobs_makeprepbufr.sh to run to completion.  Prepbufr file will have no \
GLERL-adjusted obs."
    echo
    set -x
    msg="prepobs_glerladj.sh ERROR: Lake temperature file glsea-temps.dat \
valid for ${PDY} is more than 3 days old, missing lake temperature file is: \
${TANK}/${PDY}/wtxtbul/glsea-temps.dat --> non-fatal"
    echo $msg | mail.py -c ${email_GLERL}
    [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
    exit 0
fi
YYYY=`echo $CDATE10 | cut -c1-4`
MM=`echo $CDATE10 | cut -c5-6`
DD=`echo $CDATE10 | cut -c7-8`

#make a namelist input file with date info - used to match day of year with YMD (w3fp12)
cat <<EOF > dateinfo_input
&dateinfo
    inyear=${YYYY},
    inmonth=${MM},
    inday=${DD}
/
EOF

export FORT12=$GLRD
export FORT13=$PRPI
export FORT51=$PRPI.glerladj
export FORT91=prepbufr.wrk
TIMEIT=${TIMEIT:-""}
[ -s $DATA/time ] && TIMEIT="$DATA/time -p"
$TIMEIT $GLRX > outout 2> errfile
err=$?
err_actual=$err
err=0
cat errfile >> outout
cat outout >> glerladj.out
set +u
[ -n "$pgmout" ]  &&  cat outout >> $pgmout
set -u
rm outout
set +x
echo
echo 'The foreground exit status for PREPOBS_GLERLADJ is ' $err_actual
echo
set -x
if [ "$err_actual" -gt '0' ]; then
    msg="WARNING: prepobs_glerladj did not run properly --> leaving input \
file intact and passing prepbufr file without GLERL adjustments back to \
prepobs_makeprepbufr.sh --> non-fatal"
   set +x
   echo
   echo $msg
   echo
   set -x
   [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
   exit 0
fi

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
   mv $PRPI.glerladj $PRPI
fi

exit 0

