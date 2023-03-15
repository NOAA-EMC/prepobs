#!/bin/sh
######################################################################
echo "-------------------------------------------------------------"
echo "exprep_post.sh     - Runs various post-analysis processing   "
echo "                     steps on the PREPBUFR files (removes or "
echo "                     masks restricted data)                  "
echo "                   - GDAS only: Identify TimeTwin duplicate  "
echo "                     upper-air (RAOB, PIBAL, DROP) wind      "
echo "                     report parts                            "
echo "                   - 18Z GDAS only: Reformat GDAS received,  "
echo "                     selected, and assimilated data counts   "
echo "                     (both satellite and non-satellite) for  "
echo "                     all four cycles for today and save the  "
echo "                     result in the monthly archive directory "
echo "                     (run monthly summary on the second day  "
echo "                     of the next month and post to web)      "
echo "                   - 18Z GDAS only: Update the Master Ship   "
echo "                     Station List based on any new           "
echo "                     info read from the updated VOS ship     "
echo "                     list from NDBC. The Master Ship Station "
echo "                     List is read within job JRW1 at 12z on  "
echo "                     the first day of each month in order to "
echo "                     generate marine monthly statistics.     "
#  -------------------------------------------------------------------------

echo "------------------------------------------------------------------------"
echo "History:                                                                "
echo "  Jul 16 2003 - Original script - D. Keyser                             "
echo "  OCT 15 2011 - RAP replaces RUC (Keyser)                               "
echo "  Mar 11 2013 - ported to WCOSS (Melchior)                              "
echo "  Jul 01 2014 - Removed obsolete PROCESS_SATELLITE_COUNTS section       "
echo "                (Melchior)                                              "
echo "  Feb 18 2015 - Removed SENDWEB wrapper around call to gdas_summary.sh. "
echo "                Updated directory variable for ob count scripts (Stokes)"
echo "  Feb 16 2016 - Add PROCESS_MASTER_SHIP_STNLST section (Keyser)         "
echo "  Apr 11 2017 - Updated pointer to timetwin executable which is now in  "
echo "                this package and export FORT_BUFFERED=TRUE to improve   "
echo "                its performance on Cray-XC40.  Added wrapper to skip the"
echo "                yearly invocation of gdascounts_archive_pyear.sh if     "
echo "                SATCOMIN is on Cray-XC40 where it should follow new     "
echo "                YYYYMM naming convention. Update to recognize gdas or   "
echo "                gdas1 as prefix for gdas filenames.  Added variable     "
echo "                PROCESS_UNBLKBUFR to control creation of 'unblok' copy  "
echo "                of unrestricted prepbufr file (Stokes)                  "
echo "  Aug 21 2017 - Removed obsolete gdascounts_archive_pyear.sh (Stokes)   "
echo "  Apr 10 2018 - Added $path variable to include CYC_HIS sub-directory   "
echo "                for location of upper air wind files used in timetwin   "
echo "                processing (Melchior)                                   "
echo "  Jul 15 2020 - Modified $path variable to include (or exclude)         "
echo "                $COMPONENT subdir based on GFS version.                 "
echo "  Dec 09 2021 - Updated for use on WCOSS2 (Esposito)                    " 
###############################################################################

# NOTE: NET is gfs for the gdas RUN (as for the gfs RUN)
# -------------------------------------------------------

set -aux

# Make sure we are in the $DATA directory
cd $DATA

msg="Post-analysis processing of PREPBUFR file has begun on `hostname`"
$DATA/postmsg "$jlogfile" "$msg"
 
if [ ! -s break ]; then
   echo "step ############# break ##############################" > break
fi
cat break > $pgmout

hr_fraction=""
set +u
if [ -n "$cycM" ]; then
set -u
  case "$cycM" in
    00) hr_fraction='.00' ;;
    15) hr_fraction='.25' ;;
    30) hr_fraction='.50' ;;
    45) hr_fraction='.75' ;;
     *) err_xd=9
        msg="###FATAL ERROR in model script: incorrect cycM='${cycM}' - exiting"
        echo "$msg"
        postmsg "$jlogfile" "$msg"
        exit $err_xd ;;
  esac
fi

tmhr=`echo $tmmark|cut -c3-4`
cdate10=`$NDATE -$tmhr $PDY$cyc`$hr_fraction
cycp=`echo $cdate10|cut -c9-10`

net=$NET

[[ $RUN == rap_p ]]  &&  net=rap_p
[[ $RUN == rap_e ]]  &&  net=rap_e

# Imported variable cycM, if it exists, contains cycle time minutes
#  If cycM is imported, reset cycle in this script only (cycle_here) to 4-digit
#   value t<HHMM>z
cycle_here=$cycle
[ -n "$cycM" ]  &&  cycle_here=t${cyc}${cycM}z

net_uc=$(echo $net | tr [a-z] [A-Z])
set +u
[ -n "$cycM" ]  &&  net_uc=RTMA_RU
set -u
tmmark_uc=$(echo $tmmark | tr [a-z] [A-Z])
RUN_uc=$(echo $RUN | tr [a-z] [A-Z])

msg="$net_uc ANALYSIS TIME IS $PDY$cyc"
set +u
[ -n "$cycM" ]  &&  msg="$msg:${cycM}"
set -u
$DATA/postmsg "$jlogfile" "$msg"

set +x
echo
echo "CENTER TIME FOR PREPBUFR PROCESSING FOR $tmmark_uc $net_uc IS $cdate10"
echo
set -x



if [ "$PROCESS_REMOREST" = 'YES' ]; then  # normally YES in all runs

   msg="REMOVE OR MASK RESTRICTED DATA FROM $tmmark_uc $net_uc PREPBUFR files \
CENTERED ON $cdate10"
   $DATA/postmsg "$jlogfile" "$msg"
   set +x
   echo
   echo "$msg"
   echo
   set -x

   dot_tmmark=".$tmmark"
   [ $net = gdas -o $net = gfs -o $net = cdas ]  &&  dot_tmmark=""

########################################################
#  Remove or mask restricted data from PREPUFR files
########################################################

cat <<\EOFparm > bufr_remorest.prepbufr.parm
=========================================================================

  Cards for PREPBUFR Version of BUFR_REMOREST -- Version 15 March 2013

  -->   GPSIPW can be moved from MSG_RESTR to MSG_MIXED oncw dump interface to
         PREPDATA can recognize U.S.-provider (ENI) reports which are now
         restricted

 &SWITCHES
   MSG_RESTR = 'AIRCAR  ',   ! These are the Table A Entries for
               'MSONET  ',   !  BUFR messages for which ALL reports
               'GPSIPW  ',   !  are RESTRICTED and will be REMOVED.
               '        ',   !  (up to 20)
   MSG_MIXED = 'ADPSFC  ',   ! These are the Table A Entries for
               'AIRCFT  ',   !  BUFR messages which contain a MIXTURE
               '        ',   !  of restricted and unrestricted
               '        ',   !  reports (based on mnemonic "RSRD").  All
               '        ',   !  restricted reports will be REMOVED.
               '        ',   !  (up to 20)
   MSG_MASKA = 'SFCSHP  ',   ! These are the Table A Entries for
               '        ',   !  BUFR messages for which ALL reports
               '        ',   !  are RESTRICTED if their dump report type is
               '        ',   !  one of up to 10 possible listed in switch
               '        ',   !  IMASK_T29 below (each line in IMASK_T29 applies
               '        ',   !  to the Table A entry in the same line number
               '        ',   !  here). Restricted reports will not be removed,
               '        ',   !  but their report ids will be unilaterally
               '        ',   !  changed to "MASKSTID"
               '        ',   !  (up to 20)
               '        ' 
   IMASK_T29 = 522,523,8*99999, ! Dump report types restricted in MSG_MASKA(1)
               10*99999,        ! Dump report types restricted in MSG_MASKA(2)
               10*99999         ! etc., {up to 20 for MSG_MASKA(20)}

 /

    Note 1: A particular Table A entry should NEVER appear in more than one
            of MSG_RESTR, MSG_MIXED or MSG_MASKA.
    Note 2: Any Table A entry not in either MSG_RESTR, MSG_MIXED or MSG_MASKA
            is assumed to be a Table A entry for BUFR messages for which
            ALL reports are UNRESTRICTED (these messages are copied
            intact, no reports are unpacked).
    Note 3: Always fill in the arrays MSG_RESTR, MSG_MIXED and MSG_MASKA
            beginning with word 1.  If there are less than 20 words filled in
            an array, either set the extra words to "        " (8 blank
            characters) or do not specify them here (they default to
            "        ").
    Note 4: In array IMASK_T29, a value of "99999" means not applicable whereas
            a value of "000" means reports in all dump report types in the
            corresponding Table A entry in MSG_MASKA should be restricted
            (masked) {in this case IMASK_T29(1,x) would be set to 000 and
            IMASK_T29(2:10,x) would be set to 99999 for all reports in Table A
            entry MSG_MASKA(x) since they would all be ignored - this is the
            default for all Table A entries MSG_MASKA(1:20) if this is not set
            (i.e., for data dump files)}

=========================================================================
EOFparm

REMX=${REMX:-$EXECobsproc/bufr_remorest}
REMC=${REMC:-bufr_remorest.prepbufr.parm}

if [ -f $COMIN/$RUN.$cycle_here.prepbufr${dot_tmmark} ]; then
   if [ ! -f $COMOUT/$RUN.$cycle_here.prepbufr${dot_tmmark}.nr ]; then
      cp $COMIN/$RUN.$cycle_here.prepbufr${dot_tmmark} \
       $RUN.$cycle_here.prepbufr${dot_tmmark}
      $USHobsproc/bufr_remorest.sh \
       $RUN.$cycle_here.prepbufr${dot_tmmark}
      errsc=$?
      [ "$errsc" -ne '0' ]  &&  exit $errsc
      cp $RUN.$cycle_here.prepbufr${dot_tmmark} \
       $COMOUT/$RUN.$cycle_here.prepbufr${dot_tmmark}.nr
      chmod 664 $COMOUT/$RUN.$cycle_here.prepbufr${dot_tmmark}.nr
      msg="$RUN.$cycle_here.prepbufr${dot_tmmark}.nr successfully created"
      $DATA/postmsg "$jlogfile" "$msg"
      if test "$SENDDBN" = "YES"; then
         if test "$net" = "gdas"; then
            $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_PREPda_nr $job \
             $COMOUT/$RUN.$cycle_here.prepbufr.nr
         elif test "$net" = "nam"; then
            $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_PREPda_nr $job \
             $COMOUT/$RUN.$cycle_here.prepbufr${dot_tmmark}.nr
         elif test "$net" = "gfs"; then
            $DBNROOT/bin/dbn_alert MODEL GFS_BUFR_PREPda_nr $job \
             $COMOUT/$RUN.$cycle_here.prepbufr.nr
         elif test "$net" = "rap" -o "$net" = "rap_e" -o "$net" = "rap_p"; then
            $DBNROOT/bin/dbn_alert MODEL ${net_uc}_BUFR_PREPda_nr $job \
             $COMOUT/$RUN.$cycle_here.prepbufr${dot_tmmark}.nr
         fi
      fi
   else
      msg="$RUN.$cycle_here.prepbufr${dot_tmmark}.nr NOT created because it \
already exists"
      $DATA/postmsg "$jlogfile" "$msg"
   fi
   # Remove the following logic to create unblocked nr prepbufr files once we
   #   know it is definitely no longer needed.
   if [ "${PROCESS_UNBLKBUFR:-YES}" = 'YES' ]; then
      if [   -f $COMIN/$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok -a \
           ! -f $COMOUT/$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok.nr ]; then
# make unblocked unrestricted prebufr file
#  ---> ON WCOSS prepbufr is already unblocked, so for now just copy it to the
#       unblok file location used before on CCS - hopefully this can be removed
#       someday!
         cp -p $RUN.$cycle_here.prepbufr${dot_tmmark} \
$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok.nr 
         err_cp=$?
         if [ $err_cp -eq 0 ]; then
            cp $RUN.$cycle_here.prepbufr${dot_tmmark}.unblok.nr \
             $COMOUT/$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok.nr
            chmod 664 $COMOUT/$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok.nr
            msg="$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok.nr successfully created"
            $DATA/postmsg "$jlogfile" "$msg"
            if test "$SENDDBN" = "YES"; then
               if test "$net" = "gdas"; then
                  $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_PREPda_unblok_nr \
                   $job $COMOUT/$RUN.$cycle_here.prepbufr.unblok.nr
               elif test "$net" = "gfs"; then
                  $DBNROOT/bin/dbn_alert MODEL GFS_BUFR_PREPda_unblok_nr $job \
                   $COMOUT/$RUN.$cycle_here.prepbufr.unblok.nr
               fi
            fi
         else
            msg="$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok.nr NOT created \
because cp command had return code $err_cp"
         $DATA/postmsg "$jlogfile" "$msg"
         fi
      else
         if [ ! -f $COMIN/$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok ]; then
            msg="$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok.nr NOT created \
because unblocked prepbufr file does not exist"
            $DATA/postmsg "$jlogfile" "$msg"
         else
            msg="$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok.nr NOT created \
because it already exists"
            $DATA/postmsg "$jlogfile" "$msg"
         fi
      fi
   fi
else
   msg="$RUN.$cycle_here.prepbufr${dot_tmmark}.nr NOT created because \
prepbufr file does not exist"
   $DATA/postmsg "$jlogfile" "$msg"
   if [   -f $COMIN/$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok -a \
        ! -f $COMOUT/$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok.nr ]; then
      msg="$RUN.$cycle_here.prepbufr${dot_tmmark}.unblok.nr NOT created \
because prepbufr file does not exist"
   fi
fi

if [   -f $COMIN/$RUN.$cycle_here.prepbufr_pre-qc${dot_tmmark} -a \
     ! -f $COMOUT/$RUN.$cycle_here.prepbufr_pre-qc${dot_tmmark}.nr ]; then
   cp $COMIN/$RUN.$cycle_here.prepbufr_pre-qc${dot_tmmark} \
    $RUN.$cycle_here.prepbufr_pre-qc${dot_tmmark}
   $USHobsproc/bufr_remorest.sh \
    $RUN.$cycle_here.prepbufr_pre-qc${dot_tmmark}
   errsc=$?
   [ "$errsc" -ne '0' ]  &&  exit $errsc
   cp $RUN.$cycle_here.prepbufr_pre-qc${dot_tmmark} \
    $COMOUT/$RUN.$cycle_here.prepbufr_pre-qc${dot_tmmark}.nr
   chmod 664 $COMOUT/$RUN.$cycle_here.prepbufr_pre-qc${dot_tmmark}.nr
   msg="$RUN.$cycle_here.prepbufr_pre-qc${dot_tmmark}.nr successfully created"
   $DATA/postmsg "$jlogfile" "$msg"
else
   if [ ! -f $COMIN/$RUN.$cycle_here.prepbufr_pre-qc${dot_tmmark} ]; then
      msg="$RUN.$cycle_here.prepbufr_pre-qc${dot_tmmark}.nr NOT created \
because prepbufr_pre-qc file does not exist"
      $DATA/postmsg "$jlogfile" "$msg"
   else
      msg="$RUN.$cycle_here.prepbufr_pre-qc${dot_tmmark}.nr NOT created because \
it already exists"
      $DATA/postmsg "$jlogfile" "$msg"
   fi
fi

fi # test for PROCESS_REMOREST=YES



# PROCESS_TIMETWINS can only be YES in all GDAS cycles (where default is YES)
# ---------------------------------------------------------------------------
PROCESS_TIMETWINS=${PROCESS_TIMETWINS:-YES}
[ $net != gdas ] && PROCESS_TIMETWINS=NO

if [ "$PROCESS_TIMETWINS" = 'YES' ]; then

   msg="FIND UPPER-AIR TIME-TWINS IN $tmmark_uc $net_uc PREPBUFR files \
CENTERED ON $cdate10"
   $DATA/postmsg "$jlogfile" "$msg"
   set +x
   echo
   echo "$msg"
   set -x

###########################################################################
#  Find upper-air TimeTwins (duplications in current RAOB, PIBAL, or DROP
#   wind report "parts" vs. those over the past 35 days) (executed only
#   for the GDAS run, for every cycle)
##########################################################################

   set +x
   echo "Upper-air TimeTwins are current reports that are duplicates of \
past reports"
   echo
   echo "Calculate date 35 days ago to limit old data accumulation." 
   echo
   set -x

   F35DAYL=`$NDATE -840 ${PDY}${cyc}`
   cat << EOH > cardec
${F35DAYL} ${PDY}
EOH

   if [ -f $COMIN/$RUN.t${cyc}"z".prepbufr ]; then
      BUFFILE=$COMIN/$RUN.t${cyc}"z".prepbufr
      errbufr=0
   elif [ -f $COMINGFS/gfs.t${cyc}"z".prepbufr ]; then
      BUFFILE=$COMINGFS/gfs.t${cyc}"z".prepbufr
      errbufr=0
   else
      errbufr=1
   fi

   if [ $errbufr -eq 0 ]; then
      set +x
      echo
      echo "Wind reports taken from: "
      echo    $BUFFILE
      echo
      set -x
   else
      set +x
      echo
      echo "Could not find the current" ${cyc}"z" "GFS or GDAS prepbufr file"
      echo
      set -x
      $DATA/err_exit
   fi

   pgm="timetwin"
   export pgm

   THR=6
   ICNT=1
   error=FALSE

   while [ $error = FALSE ]; do

      prevgoodcyc=`$NDATE -$THR ${PDY}${cyc}`
      DATE_HIS=`echo $prevgoodcyc | cut -c1-8`
      CYC_HIS=`echo $prevgoodcyc | cut -c9-10`
      path="${COMIN1}${DATE_HIS}/${CYC_HIS}/${COMPONENT}"

      . $DATA/prep_step
      export FORT10="$BUFFILE"
      export FORT24="${path}/upa_his_twinwind.ttaa.t${CYC_HIS}"z""
      export FORT25="${path}/upa_his_twinwind.ttbb.t${CYC_HIS}"z""
      export FORT26="${path}/upa_his_twinwind.ttcc.t${CYC_HIS}"z""
      export FORT27="${path}/upa_his_twinwind.ttdd.t${CYC_HIS}"z""
      export FORT28="${path}/upa_his_twinwind.ppbb.t${CYC_HIS}"z""
      export FORT29="${path}/upa_his_twinwind.ppdd.t${CYC_HIS}"z""
      export FORT61="upa_cur_twinwind.ttaa.t${cyc}"z""
      export FORT62="upa_cur_twinwind.ttbb.t${cyc}"z""
      export FORT63="upa_cur_twinwind.ttcc.t${cyc}"z""
      export FORT64="upa_cur_twinwind.ttdd.t${cyc}"z""
      export FORT65="upa_cur_twinwind.ppbb.t${cyc}"z""
      export FORT66="upa_cur_twinwind.ppdd.t${cyc}"z""
      export FORT54="new.ttaa.t${cyc}"z""
      export FORT55="new.ttbb.t${cyc}"z""
      export FORT56="new.ttcc.t${cyc}"z""
      export FORT57="new.ttdd.t${cyc}"z""
      export FORT58="new.ppbb.t${cyc}"z""
      export FORT59="new.ppdd.t${cyc}"z""
      export FORT74="timetwin.diags.t${cyc}"z""
      export FORT75="twin"
####  export FORT76="timetwin.debug1.diags.t${cyc}"z""
####  export FORT77="timetwin.debug2.diags.t${cyc}"z""

      startmsg
      FORT_BUFFERED=TRUE $EXECprepobs/timetwin < cardec >>$pgmout 2> errfile
      export err=$?
####  $DATA/err_chk

      if [ $err -eq 0 ]; then
         error=TRUE
      else
         cp -p errfile errfile.timetwin.$ICNT
         let ICNT=$ICNT+1
         if [ $ICNT -eq 28 ]; then
            set +x
            echo
            echo " Upperair wind reports are beyond 7 days...exit"
            echo
            set -x
            $DATA/err_chk
         fi
         let THR=$ICNT*6
         echo "THR" $THR $ICNT
      fi
   done

   if [ $SENDCOM = "YES" ]; then
      cp new.ttaa.t${cyc}"z" $COMOUT/upa_his_twinwind.ttaa.t${cyc}"z"
      cp new.ttbb.t${cyc}"z" $COMOUT/upa_his_twinwind.ttbb.t${cyc}"z"
      cp new.ttcc.t${cyc}"z" $COMOUT/upa_his_twinwind.ttcc.t${cyc}"z"
      cp new.ttdd.t${cyc}"z" $COMOUT/upa_his_twinwind.ttdd.t${cyc}"z"
      cp new.ppbb.t${cyc}"z" $COMOUT/upa_his_twinwind.ppbb.t${cyc}"z"
      cp new.ppdd.t${cyc}"z" $COMOUT/upa_his_twinwind.ppdd.t${cyc}"z"
      cp upa_cur_twinwind.ttaa.t${cyc}"z" $COMOUT/upa_cur_twinwind.ttaa.t${cyc}"z"
      cp upa_cur_twinwind.ttbb.t${cyc}"z" $COMOUT/upa_cur_twinwind.ttbb.t${cyc}"z"
      cp upa_cur_twinwind.ttcc.t${cyc}"z" $COMOUT/upa_cur_twinwind.ttcc.t${cyc}"z"
      cp upa_cur_twinwind.ttdd.t${cyc}"z" $COMOUT/upa_cur_twinwind.ttdd.t${cyc}"z"
      cp upa_cur_twinwind.ppbb.t${cyc}"z" $COMOUT/upa_cur_twinwind.ppbb.t${cyc}"z"
      cp upa_cur_twinwind.ppdd.t${cyc}"z" $COMOUT/upa_cur_twinwind.ppdd.t${cyc}"z"
      cp timetwin.diags.t${cyc}"z" $COMOUT/timetwin.diags.t${cyc}"z"

      if [ -f twin ]; then
         cp twin $COMOUT/upa_time_twin.t${cyc}"z"
      else
         echo "NO TWIN REPORTS" | cat >> upa_time_twin.t${cyc}"z"
         cp upa_time_twin.t${cyc}"z" $COMOUT/upa_time_twin.t${cyc}"z" 
      fi
   fi
fi # test for PROCESS_TIMETWINS=YES



# PROCESS_ALL_REPORT_COUNTS can only be YES in 18z GDAS (where default is YES)
# ----------------------------------------------------------------------------
PROCESS_ALL_REPORT_COUNTS=${PROCESS_ALL_REPORT_COUNTS:-YES}
[ $net != gdas -o $cyc != 18 ] && PROCESS_ALL_REPORT_COUNTS=NO

if [ "$PROCESS_ALL_REPORT_COUNTS" = 'YES' ]; then
 
   msg="ARCHIVE ALL OBS COUNTS (SAT AND NON-SAT) FOR ALL 4 $net_uc CYCLES \
FOR $PDY"
   $DATA/postmsg "$jlogfile" "$msg"
   set +x
   echo
   echo "$msg"
   echo
   set -x

#############################################################################
#  Reformat GDAS received, selected, and assimilated data counts (both
#   satellite and non-satellite) for all four cycles for today and save the
#   result in the monthly archive directory (run monthly summary on the
#   second day of the next month and post to web) (executed only for the 18z
#   GDAS run)
#############################################################################

   $USHprepobs/gdas_counts.sh

#  Run monthly summary only on the second day of the month, post to web
#  --------------------------------------------------------------------
   monsummary_dat=${monsummary_dat:-`date +"%Y%m"02`}
   if [ $monsummary_dat -eq $PDY ]; then
      msg="....it's day 2 of month - post last month's obs count summary to web"
      $DATA/postmsg "$jlogfile" "$msg"
      set +x
      echo
      echo "$msg"
      echo
      set -x
      $USHprepobs/gdas_summary.sh
   fi
fi # test for PROCESS_ALL_REPORT_COUNTS=YES



# PROCESS_MASTER_SHIP_STNLST can only be YES in 18z GDAS (where default is YES)
# -----------------------------------------------------------------------------
PROCESS_MASTER_SHIP_STNLST=${PROCESS_MASTER_SHIP_STNLST:-YES}
[ $net != gdas -o $cyc != 18 ] && PROCESS_MASTER_SHIP_STNLST=NO

if [ "$PROCESS_MASTER_SHIP_STNLST" = 'YES' ]; then
 
   msg="UPDATE MASTER SHIP STATION LIST FOR $PDY"
   $DATA/postmsg "$jlogfile" "$msg"
   set +x
   echo
   echo "$msg"
   echo
   set -x

#############################################################################
#  Update the "Master Ship Station List" based on any information read from
#   the updated VOS ship list from NDBC
#############################################################################

   $USHprepobs/mstr_shp_stn_lst_update.sh

fi # test for PROCESS_MASTER_SHIP_STNLST=YES


#######################


# GOOD RUN
set +x
echo " "
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " "
set -x


# save standard output

cat  break $pgmout break > allout
cat allout
# rm allout

sleep 10

msg='ENDED NORMALLY.'
$DATA/postmsg "$jlogfile" "$msg"
 
################## END OF SCRIPT #######################
