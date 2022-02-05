#!/bin/sh
#
#--------------------------------------------------------------------------
#
#  gdas_summary.sh     
#
#  This script invokes script gdas_countstat.sh  each
#  of four cycle times (00,06,12,18) and then computes daily average counts
#  with monthly_ave.  In addition, counts for several satellite data types
#  are made.  These counts are written into web pages and forwarded to the 
#  WOK for display on the GDAS observational counts website.
#  
#  History:
#  L. Sager   09/07  Modified the script to work properly in operations.
#  V. Krishna Kumar 03/23 Modified the script to copy the final index file to
#                         the $DATCOM_monsum_base directory.
#                         For any reason, the final index file (for the month
#                         the stats are computed, for example, February 2009) 
#                         gets corrupted (could happen if this job is repeatedly run),
#                         please delete the corrupted index.shtml and copy over 
#                         from the previous month's backup (in this example, January 2009) 
#                         cp $DATCOM_monsum_base/index_backup.shtml
#                         $DATCOM_monsum_base/index.shtml 
#  D. Stokes  04/2015     Moved to vertical structure package obsproc_prep_post 
#                         and updated variables pointing to other software
#                         included in this migration.  Added variables in place
#                         of some hardwired settings to aid non-prod runs.  
#                         Added option to send html updates to developer 
#                         website for checkout runs.
#  D. Keyser  02/2016     Use NCO-established variable $COMROOT to point to root
#                         directory for input/output directories in order to run
#                         on WCOSS Phase 1 or Phase 2. Use NCO-established
#                         variables (presumably obtained from modules) to point
#                         to prod utilities:
#                          - ush finddate.sh where prepended-path $PATH from
#                            module prod_util (default or specified version)
#                            replaces horizontal structure utility directory
#                            path defined by imported variable $utilscript. Echo
#                            full path to finddate.sh to stdout.
#                          - ush month_name.sh where prepended-path $PATH from
#                            module util_shared/v1.0.2 (currently only specified
#                            version) replaces horizontal structure utility
#                            directory path defined by imported variable
#                            $utilscript. Echo full path to month_name.sh to
#                            stdout.
#  D. Keyser  05/2016     Split imported script variables $DATCOM1, $SATCOM and
#                         $DATCOM into separate input and output variables
#                         $DATCOM1_IN and $DATCOM1_OUT, $SATCOMIN (root to
#                         $SATCOMIN_dir, replacing $SATCOM_monsum_base which
#                         had been set here) and $SATCOMOUT_dir, and $DATCOMIN
#                         (root to $DATCOMIN_dir, replacing $DATCOM_monsum_base
#                         which had been set here) and $DATCOMOUT_dir set in
#                         parent Job Script.
#                         BENEFIT: Allows checkout runs to specify a production
#                                  input location and a local output location.
#  D. Stokes  03/2017     Removed dependency on month_name.sh utility script.
#                         (Use local arrays instead).  Corrected prep_step 
#                         placement and invocation.  Added transitional logic to
#                         pick up satcounts from either old directory structure
#                         (satcounts/$MON or satcounts_archive/$YEAR/$MON) or
#                         new monthly directories (satcounts.YYYYMM)
#  D. Stokes  08/2017     Removed logic to pick up satcounts from old $MON 
#                         directory structures.
#
#--------------------------------------------------------------------------


  set -x

# define arrays to store month names in various styles that might be used
  MONS=(null JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC)
  Mons=(null Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec)
  MONTHS=(null JANUARY FEBRUARY MARCH APRIL MAY JUNE
               JULY AUGUST SEPTEMBER OCTOBER NOVEMBER DECEMBER)
  Months=(null January February March April May June
               July August September October November December)

# Parse arguments, if any

if [ $# -eq 3 ]; then

  date=$1
  YEAR=`echo $date | cut -c1-4 `
  MM=`echo $date | cut -c5-6`
  last_mon=$MM
  month=$2
  numdays=$3

  MON=`echo $month | tr [a-z] [A-Z]`
  MONTH=${MONTHS[10#$MM]}   # get uppercase month string from array MONTHS.
  Month=${Months[10#$MM]}   # get Month string from array Months.

else

#  CUR_DAY=`date +%d`
#  CUR_DATE=`date +%Y%m%d`
  CUR_DATE=${monsummary_dat:-`date +%Y%m%d`}
  CUR_DAY=`echo $CUR_DATE|cut -c 7-8`

set +x; echo -e "\n---> path to finddate.sh below is: `which finddate.sh`"; set -x
  last_date=`sh finddate.sh ${CUR_DATE} d-${CUR_DAY}`

  numdays=`echo ${last_date} | cut -c7-8`
  YEAR=`echo ${last_date} | cut -c1-4`
  last_mon=`echo $last_date | cut -c5-6`
  MM=$last_mon                      

  date=${YEAR}${last_mon}01

  MON=${MONS[10#$last_mon]}       # get uppercase month abbrev from array MONS
  MONTH=${MONTHS[10#$last_mon]}   # get uppercase month string from array MONTHS
  Month=${Months[10#$last_mon]}   # get Month string from array Months
 
fi

  export MM MON MONTH Month YEAR
  echo -e "\n MON = ${MON}   MONTH = $MONTH   Month = $Month   YEAR = $YEAR \n"
  FIRSTDAY=$date
  NDAYS=$numdays

  echo -e "\n FIRSTDAT = $FIRSTDAY   NDAYS = $NDAYS \n" 

  export FIRSTDAY NDAYS

#  Summarize last month so reset file date to last month. 
#  SATCOMIN and DATCOMIN allow for alternate input source.

  export SATCOMIN_dir=${SATCOMIN}/satcounts.${YEAR}${last_mon}
  export DATCOMIN_dir=${DATCOMIN}/data_counts.${YEAR}${last_mon}

#  Extract the non-sat data counts from the monthly archive

#    00Z counts

  $USHprepobs/gdas_countstat.sh $date gdas $numdays 00 ${MONTH} 

#    06Z counts

  $USHprepobs/gdas_countstat.sh $date gdas $numdays 06 ${MONTH}

#    12Z counts

  $USHprepobs/gdas_countstat.sh $date gdas $numdays 12 ${MONTH} 
 
#    18Z counts

  $USHprepobs/gdas_countstat.sh $date gdas $numdays 18 ${MONTH}


#   Execute monthly_avg
#  Compute average daily non-sat counts for the month ( output is in file tmp1 )

  export pgm=gdascounts_ave
  . $DATA/prep_step

  export FORT10=gdas_${MONTH}_dumpstats.t00z
  export FORT11=gdas_${MONTH}_dumpstats.t06z
  export FORT12=gdas_${MONTH}_dumpstats.t12z
  export FORT13=gdas_${MONTH}_dumpstats.t18z
  export FORT50=tmp1

  $EXECprepobs/gdascounts_ave 1>aa.out 2>aa.out
  export err=$?; err_chk


#  Build the script that creates the html table for non-sat counts

sed -e 's/= /=/g' tmp1 > tmp2
sed -e 's/= \./=0\./g' tmp2  > tmp3
sed -e 's/= */=/g' tmp3 > ${MONTH}.gdas.outfile
echo "MONTH=${MONTH}" > ${MONTH}.htmlscript
echo "YEAR=$YEAR" >> ${MONTH}.htmlscript
cat ${MONTH}.gdas.outfile >> ${MONTH}.htmlscript
echo "cat <<EOF > ${MONTH}.html" >> ${MONTH}.htmlscript
cat $FIXprepobs/gdascounts_html  >> ${MONTH}.htmlscript


#  Run the script

  sh ${MONTH}.htmlscript
  cp ${MONTH}.html FILE_NONSAT

# Generate 3 satellite count files (received,selected,assimilated)

  $USHprepobs/satellite_summary.sh                             

# Combine 4 files ( 1 non-sat, 3 sat ) into 1

  $USHprepobs/gdascounts_combine.sh                           

  cp temp2 index.shtml           
  cp temp2 ${Month}_${YEAR}.shtml
#
# Copy the newly generated index.shtml file to $DATCOM1_OUT directory
#
 cp index.shtml $DATCOM1_OUT/.
#
# Move this over to the public web server.
#
if [ "$SENDWEB" = 'YES' ]; then
  if [ "$USER" = nwprod ]; then
    scp ${Month}_${YEAR}.html nwprod@ncorzdm:/home/people/nco/nwprod/pmb/nw${envir}/gdas/
    scp index.shtml nwprod@ncorzdm:/home/people/nco/nwprod/pmb/nw${envir}/gdas/
  else # developer
    if [[ -z "$webhost" || -z "$webhostid" || -z "$webdir" ]]; then
      echo -e "\n*** WARNING:  Missing one or more variables required to send content to web! ***"
      echo -e " Check the following or set SENDWEB=NO"
      echo -e "   webhost: ${webhost:-"NOT SET!!  Should point to remote host."}"
      echo -e "   webhostid: ${webhostid:-"NOT SET!!  Should be userid for remote host."}"
      echo -e "   webdir: ${webdir:-"NOT SET!!  Should point to directory on remote host."}"
      echo -e "*** SKIPPING SCP TO REMOTE HOST ***\n"
      set -x
    else
      scp ${Month}_${YEAR}.html ${webhostid}@${webhost}:${webdir}/
      scp index.shtml ${webhostid}@${webhost}:${webdir}/
    fi
  fi
fi
