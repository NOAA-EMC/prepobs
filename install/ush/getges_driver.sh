#!/bin/ksh
# Run under ksh (converted to WCOSS)

####  UNIX Script Documentation Block
#
# Script name:         getges_driver.sh
# Script description:  Sets up execution of getges for FV3 & legacy NEMSIO GFS
#
# Author:       Keyser              Org: EMC          Date: 2018-03-05
#
# Abstract: This script is a driver for the ush script getges.sh.  It is needed
#   in order to handle the FV3 GFS, but it also handles the legacy (pre-FV3)
#   NEMSIO GFS.  Since the FV3 GFS directory paths ($COMINgdas and $COMINgfs)
#   now contain a suffix cycle time directory (e.g.,
#   /gpfs/hps/nco/ops/com/gfs/prod/gfs.20180305/12 vs.
#   /gpfs/hps/nco/ops/com/gfs/prod/gfs.20180305 before), the old logic in
#   getges.sh would end up looking for the global guess only in $PDY, $PDYm1,
#   $PDYm2, etc. directories with current cycle time in the suffix.  The odds
#   are that a valid global guess would not be found in the current day/cycle
#   directory, so at best a global guess would be found in a 24-hour old
#   directory and thus have a longer forecast time than a "best guess" would
#   have.  At worst, a global guess might not be found at all or be from an
#   even older initial state if there was an operational failure the previous
#   day.  In addition, a global guess would never be found in a RAP or NAM
#   network whose cycle time was not a multiple of 6 hours.
#
#   This driver essentially does the following:
#     1) First checks to see if the imported directory paths ($COMINgdas and
#        $COMINgfs) both do NOT have a cycle time directory in their suffix.  If
#        so, assumes this is a run from the legacy NEMSIO GFS.  Executes
#        getges.sh exporting a value of 384 hours for $fhend (hour to end
#        searching backward for guess, the getges default). Essentially there is
#        no change here from before in methodology for obtaining a global guess
#        from getges.sh.  This check allows this driver to be implemented prior
#        to the FV3 GFS upgrade and allows it to work for historical (pre-FV3)
#        data.  If a global guess file for the valid time is not found, this
#        driver exits with a non-zero return code (meaning the parent job will
#        likely ABORT).
#     2) If 1) above is not satisfied, next checks to see if the imported
#        directory paths ($COMINgdas and $COMINgfs) both DO have a cycle time
#        directory in their suffix.  If so, assumes this is a run from the FV3
#        NEMSIO GFS.  This driver then does the following until an acceptable
#        global guess file for the valid time is found:
#            a) First try looking for the global guess in the prior GDAS or GFS
#               cycle directory closest to, but not at, the valid cycle time
#               {e.g., if valid cycle is 2018030518, then look in GDAS and GFS
#               directories with cycle time 12 (i.e., ./<gdas><gfs>.20180305/12)
#               or if valid cycle is 2018030610 (e.g., for the RAP), then look
#               in GDAS and GFS directories with cycle time 06
#               (i.e., ./<gdas><gfs>.20180306/06)}.
#               IMPORTANT: Since the logic in getges.sh is still designed to
#                          keep marching backward in 24-hour increments if a
#                          global guess is not found for the current day, we
#                          export $fhend (hour to end searching backward for
#                          guess) as 12 hours here because, at this point, we do
#                          not want to accept a global guess from the previous
#                          day that might be a 24+ hour forecast. Instead, if
#                          no global guess is found move on to b) below.
#            b) If a global guess is still not found, next try looking for the
#               global guess in the GDAS or GFS cycle directory 6 hours prior
#               to that in a) above (i.e., in two examples above, look now in
#               ./<gdas><gfs>.20180305/06 and ./<gdas><gfs>.20180306/00).
#               IMPORTANT: Again export $fhend (hour to end searching backward
#                          for guess) as 12 hours to prevent accepting a global
#                          guess with a long forecast hour if getges.sh goes
#                          back another day.)
#            c) If a global guess is still not found, next try looking for the
#               global guess in the GDAS or GFS cycle directory 6 hours prior
#               to that in b) above (i.e., in two examples above, look now in
#               ./<gdas><gfs>.20180305/00 and ./<gdas><gfs>.20180305/18).
#               IMPORTANT: This time export $fhend (hour to end searching
#                          backward for guess) as 18 hours to allow for a
#                          slightly longer forecast hour (since we have now gone
#                          back 12+ hours) but still preventing the acceptance
#                          of a global guess with a forecast hour that becomes
#                          too long if getges.sh goes back another day.).
#            d) If a global guess is still not found, next try looking for the
#               global guess in the GDAS or GFS cycle directory 6 hours prior to
#               that in c) above (i.e., in two examples above, look now in
#               ./<gdas><gfs>.20180304/18 and ./<gdas><gfs>.20180305/12).
#               IMPORTANT: This is our last attempt to find a global guess
#                          before aborting.  Thus, export $fhend (hour to end
#                          searching backward for guess) as 384 hours (the
#                          getges default) to give it the best shot at finding a
#                          global guess, no matter what day it initiates from
#                          and no matter how long its forecast hour is.
#            e) If a global guess is still not found, exit with a non-zero
#               return code (meaning the parent job will likely ABORT).
#     3) If neither 1) nor 2) above is satisfied, exit with a non-zero return
#        code (meaning the parent job will likely ABORT).
#
#   NOTE 1: This driver will not work for the sigio global guess!!!
#
#   NOTE 2: This driver is designed to only be executed by
#           prepobs_makeprepbufr.sh.
#
#
# Script history log:
# 2018-03-05  Dennis A. Keyser -- Original version for implementation
# 2020-07-12  S. Melchior -- Modified to handle atmos suffix in $COMINgfs
#                            and $COMINgdas, introduced in GFSv16, via
#                            $COMPONENT variable.
#
#
# Usage:  getges_driver.sh
#
#   Imported Shell Variables:
#
#     These must ALWAYS be exported to this script by the parent script unless
#     otherwise noted --
#
#     COMINgdas     String indicating full path to starting location for
#                   searching for first guess (and other) files from GDAS in
#                   child script getges.sh
#     COMINgfs      String indicating full path to starting location for
#                   searching for first guess (and other) files from GFS in
#                   child script getges.sh
#     COMPONENT     String indicting coupled modeling component (e.g., atmos,
#                   wave, ocean, etc ...)
#     GETGESprep    String indicating full path to GETGES utility script
#     envir_getges  String indicating environment under which GETGES utility
#                   ush runs (see getges.sh docblock for more information)
#                   (Normally expected to be "prod".)
#     network_getges
#                   String indicating job network under which GETGES utility
#                   ush runs (see getges.sh docblock for more information)
#                   (Normally expected to be "global".)
#     fhr           String indicating specific forecast hour wanted (see
#                   getges.sh docblock for more information)
#                   (Normally expected to be "any", "-3" or "+3".)
#     stype         String indicating the filetype wanted (see getges.sh
#                   docblock for more information)
#                   (Normally expected to be "natges" if $fhr is "any", "natgm3"
#                    if $fhr is "-3", or "natgp3" if $fhr is "+3".)
#     CDATE10       String indicating the valid date wanted <YYYYMMDDHH>
#     sges          Name of file to which first guess should be copied
#                   (This is required if $fhr is "+3" or "-3". If $fhr
#                    is "any" this is ignored.))
#     LOUD          If "YES", turns on script trace (set -x)
#                   This does not have to be imported - it will default to "NO"
#
#
#   Modules and files referenced:
#     scripts    : $GETGESprep
#     executables: $NDATE (from prod_util module)
#
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
# Attributes:
#   Language: Korn shell under linux
#   Machine:  NCEP WCOSS
#
####

LOUD=${LOUD:-NO}
set -au
[ $LOUD = YES ]  &&  set -x

PDY=`echo $CDATE10 | cut -c1-8`
cyc=`echo $CDATE10 | cut -c9-10`

errges=1

bn_COMINgdas=`basename $COMINgdas`
bn_COMINgfs=`basename $COMINgfs`

#######################################################################################################
#######################################################################################################
if [[ `echo $bn_COMINgdas | cut -c1` = g && `echo $bn_COMINgfs | cut -c1` = g ]];then

# This runs the legacy NEMSIO version with no cycle time directory in the suffix of $COMINgdas and $COMINgfs

fhend=384

if [ $fhr = "-3" -o $fhr = "+3" ]; then
   $GETGESprep -e $envir_getges -n $network_getges -v $CDATE10 -t $stype $sges
   errges=$?
else
   $GETGESprep -e $envir_getges -n $network_getges -f $fhr -v $CDATE10 -t $stype > sgesprep_pathname
   errges=$?
fi

#######################################################################################################
#######################################################################################################
elif [[ `echo $bn_COMINgdas | cut -c1` != g && `echo $bn_COMINgfs | cut -c1` != g ]];then

# This runs the GFSv15 version with cycle time directory in the suffix of $COMINgdas and $COMINgfs
# -or-
# This runs the GFSv16+ version with cycle time and atmos directory in the suffix of 
# $COMINGgdas and $COMINgfs


# First try using closest directory cycle time (00, 06, 12 or 18) preceding cycle hour
#  if that doesn't work go back another 6 hours and try again
#  rinse and repeat until you've gone back 24 hours - if still no luck give up
# ------------------------------------------------------------------------------------

COMINgdas_root=`dirname $COMINgdas`
COMINgdas_root=`dirname $COMINgdas_root`
COMINgfs_root=`dirname $COMINgfs`
COMINgfs_root=`dirname $COMINgfs_root`
if [[ `echo $bn_COMINgdas | cut -c1` = a && `echo $bn_COMINgfs | cut -c1` = a ]];then
   # if atmos suffix, run dirname additional time
   COMINgdas_root=`dirname $COMINgdas_root`
   COMINgfs_root=`dirname $COMINgfs_root`
fi

PDY_this_try=$PDY
cyc_this_try=$cyc

modhr=`expr $cyc_this_try % 6`
[ $modhr -eq 0 ]  && modhr=6

PDY_this_try=`$NDATE -$modhr ${PDY_this_try}${cyc_this_try} | cut -c1-8`
cyc_this_try=`$NDATE -$modhr ${PDY_this_try}${cyc_this_try} | cut -c9-10`

if [[ `echo $bn_COMINgdas | cut -c1` = a && `echo $bn_COMINgfs | cut -c1` = a ]];then
   # if atmos suffix, define $COMINgdas and $COMINgfs accordingly 
   export COMINgdas=$COMINgdas_root/gdas.${PDY_this_try}/${cyc_this_try}/${COMPONENT}
   export COMINgfs=$COMINgfs_root/gfs.${PDY_this_try}/${cyc_this_try}/${COMPONENT}
else
   export COMINgdas=$COMINgdas_root/gdas.${PDY_this_try}/${cyc_this_try}
   export COMINgfs=$COMINgfs_root/gfs.${PDY_this_try}/${cyc_this_try}
fi
fhend=12

if [ $fhr = "-3" -o $fhr = "+3" ]; then
   $GETGESprep -e $envir_getges -n $network_getges -v $CDATE10 -t $stype $sges
   errges=$?
else
   $GETGESprep -e $envir_getges -n $network_getges -f $fhr -v $CDATE10 -t $stype > sgesprep_pathname
   errges=$?
fi

if test $errges -ne 0
then
#  problem obtaining global nemsio (or netcdf history) first guess so go back 6 hours in cycle directory
   set +x
   echo
   if [ $fhr = "-3" -o $fhr = "+3" ]; then
      echo "problem obtaining global nemsio-based (or netcdf history) guess valid $fhr hrs relative to center PREPBUFR date/time;"
   else
      echo "problem obtaining global nemsio-based (or netcdf history) guess;"
   fi
   echo "go back 6 hours in cycle directory and try again"
   echo
   [ $LOUD = YES ]  &&  set -x
else
 exit 0
fi


PDY_this_try=`$NDATE -6 ${PDY_this_try}${cyc_this_try} | cut -c1-8`
cyc_this_try=`$NDATE -6 ${PDY_this_try}${cyc_this_try} | cut -c9-10`

if [[ `echo $bn_COMINgdas | cut -c1` = a && `echo $bn_COMINgfs | cut -c1` = a ]];then
   # if atmos suffix, define $COMINgdas and $COMINgfs accordingly 
   export COMINgdas=$COMINgdas_root/gdas.${PDY_this_try}/${cyc_this_try}/${COMPONENT}
   export COMINgfs=$COMINgfs_root/gfs.${PDY_this_try}/${cyc_this_try}/${COMPONENT}
else
   export COMINgdas=$COMINgdas_root/gdas.${PDY_this_try}/${cyc_this_try}
   export COMINgfs=$COMINgfs_root/gfs.${PDY_this_try}/${cyc_this_try}
fi
fhend=12

if [ $fhr = "-3" -o $fhr = "+3" ]; then
   $GETGESprep -e $envir_getges -n $network_getges -v $CDATE10 -t $stype $sges
   errges=$?
else
   $GETGESprep -e $envir_getges -n $network_getges -f $fhr -v $CDATE10 -t $stype > sgesprep_pathname
   errges=$?
fi

if test $errges -ne 0
then
#  problem obtaining global nemsio (or netcdf history) first guess so go back 6 hours in cycle directory
   set +x
   echo
   if [ $fhr = "-3" -o $fhr = "+3" ]; then
      echo "problem obtaining global nemsio-based (or netcdf history) guess valid $fhr hrs relative to center PREPBUFR date/time;"
   else
      echo "problem obtaining global nemsio-based (or netcdf history) guess;"
   fi
   echo "go back another 6 hours in cycle directory and try again"
   echo
   [ $LOUD = YES ]  &&  set -x
else
 exit 0
fi


PDY_this_try=`$NDATE -6 ${PDY_this_try}${cyc_this_try} | cut -c1-8`
cyc_this_try=`$NDATE -6 ${PDY_this_try}${cyc_this_try} | cut -c9-10`

if [[ `echo $bn_COMINgdas | cut -c1` = a && `echo $bn_COMINgfs | cut -c1` = a ]];then
   # if atmos suffix, define $COMINgdas and $COMINgfs accordingly 
   export COMINgdas=$COMINgdas_root/gdas.${PDY_this_try}/${cyc_this_try}/${COMPONENT}
   export COMINgfs=$COMINgfs_root/gfs.${PDY_this_try}/${cyc_this_try}/${COMPONENT}
else
   export COMINgdas=$COMINgdas_root/gdas.${PDY_this_try}/${cyc_this_try}
   export COMINgfs=$COMINgfs_root/gfs.${PDY_this_try}/${cyc_this_try}
fi
fhend=18

if [ $fhr = "-3" -o $fhr = "+3" ]; then
   $GETGESprep -e $envir_getges -n $network_getges -v $CDATE10 -t $stype $sges
   errges=$?
else
   $GETGESprep -e $envir_getges -n $network_getges -f $fhr -v $CDATE10 -t $stype > sgesprep_pathname
   errges=$?
fi

if test $errges -ne 0
then
#  problem obtaining global nemsio (or netcdf history) first guess so go back 6 hours in cycle directory
   set +x
   echo
   if [ $fhr = "-3" -o $fhr = "+3" ]; then
      echo "problem obtaining global nemsio-based (or netcdf history) guess valid $fhr hrs relative to center PREPBUFR date/time;"
   else
      echo "problem obtaining global nemsio-based (or netcdf history) guess;"
   fi
   echo "go back another 6 hours in cycle directory and try again"
   echo
   [ $LOUD = YES ]  &&  set -x
else
 exit 0
fi


PDY_this_try=`$NDATE -6 ${PDY_this_try}${cyc_this_try} | cut -c1-8`
cyc_this_try=`$NDATE -6 ${PDY_this_try}${cyc_this_try} | cut -c9-10`

if [[ `echo $bn_COMINgdas | cut -c1` = a && `echo $bn_COMINgfs | cut -c1` = a ]];then
   # if atmos suffix, define $COMINgdas and $COMINgfs accordingly 
   export COMINgdas=$COMINgdas_root/gdas.${PDY_this_try}/${cyc_this_try}/${COMPONENT}
   export COMINgfs=$COMINgfs_root/gfs.${PDY_this_try}/${cyc_this_try}/${COMPONENT}
else
   export COMINgdas=$COMINgdas_root/gdas.${PDY_this_try}/${cyc_this_try}
   export COMINgfs=$COMINgfs_root/gfs.${PDY_this_try}/${cyc_this_try}
fi
fhend=384

if [ $fhr = "-3" -o $fhr = "+3" ]; then
   $GETGESprep -e $envir_getges -n $network_getges -v $CDATE10 -t $stype $sges
   errges=$?
else
   $GETGESprep -e $envir_getges -n $network_getges -f $fhr -v $CDATE10 -t $stype > sgesprep_pathname
   errges=$?
fi
#######################################################################################################
#######################################################################################################
fi

if test $errges -ne 0
then
#  problem obtaining global nemsio (or netcdf history) first guess so exit
   exit 9
fi
exit 0

