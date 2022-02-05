#!/bin/bash
#--------------------------------------------------------------------------
#
#  satellite_summary.sh
#
#  This script creats three HTML pages of satellite observational counts.  
#  These pages are summarized from the EMC counts data file and the 
#  status file from the GDAS model run.  The HTML pages summarize the 
#  received reports, selected reports and assimilated reports for many
#  different types of satellite data.
#
#  History:
#  L. Sager   09/07  Modified the script to work properly in operations.
#  D. Stokes 04/2015 Moved to vertical structure package obsproc_prep_post and
#                    updated variables pointing to other software included in
#                    this migration. Updated the parsing of info for cosmic/gps
#                    data.  Made minor changes to reduce unnecessary warnings.
#  D. Keyser 05/2015 Split imported script variables $SATCOM and $DATCOM into
#     separate input and output variables $SATCOMIN (root to $SATCOMIN_dir) and
#     $SATCOMOUT_dir, and $DATCOMIN (root to $DATCOMIN_dir) and $DATCOMOUT_dir
#     set in parent Job Script.
#     BENEFIT: Allows checkout runs to specify a production input location and
#              a local output location.
#--------------------------------------------------------------------------
#
#   Summarize the satellite counts files
#

 for hh in 00 06 12 18  
 do 
    file=`ls $SATCOMIN_dir/*t${hh}z`
    rm -f tmpsat_file
    for satfile in $file
    do
       echo copy $satfile to tmpsat_file
       echo $satfile
       cat $satfile >> tmpsat_file
    done
    sed 's/ type gps_bnd jiter   3/o-g 03 rad  gps       cosmic/' tmpsat_file > testa
    sed 's/nread//' testa > testb     
    sed 's/nkeep//' testb > testc    
    sed 's/num//' testc  > tmpsat_file    
    sort tmpsat_file > tmpsat_final${hh}     
 done
#
#   Process the gdas status file for satwnd
#     and quikscat
#
 for hh in 00 06 12 18  
 do 
    file=`ls $DATCOMIN_dir/*t${hh}z*`
    rm -f tmpdat1_file
    rm -f tmpdat2_file
    rm -f tmpdat3_file
    for datfile in $file
    do
       grep satwnd $datfile | grep -v Domain >> tmpdat1_file${hh}
       grep qkscat $datfile  >> tmpdat2_file${hh}
       grep QUIKSC $datfile  >> tmpdat3_file${hh}
    done
    days=`ls $DATCOMIN_dir/*t${hh}z* | wc -l`
###############################################################################
###############################################################################
# NOTICE: DO NOT CHANGE "iS" to "is" below as it will cause satellite_daily.pl
#         to not work properly!!
#         BE VERY CAREFUL WHEN MAKING ANY CHANGES TO THIS SCRIPT!!
###############################################################################
###############################################################################
    echo Number of days iS  $days  >> tmpdat_days${hh}
    
 done
#
#   Sum the satellites by cycle time
#
 $USHprepobs/satellite_daily.pl
  sort -u   tmpsat_list > tmpsat_types  
  sort -o   tmpsat_davg00 tmpsat_davg00  
  sort -o   tmpsat_davg06 tmpsat_davg06  
  sort -o   tmpsat_davg12 tmpsat_davg12  
  sort -o   tmpsat_davg18 tmpsat_davg18  

#
#   Create the html pages       
#
  cp $FIXprepobs/satellite_counts.tbl .
  $USHprepobs/satellite_html.pl $MONTH $YEAR

exit

          


