#!/bin/ksh
################################################################################
#
#  gdascount_combine.sh, merges 4 html files into a single file.
#  The files contain non-sat data counts and 3 sat data counts
#  (received, selected and assimilated).  The resulting html file
#  is sent to the rzdm for display in the GDAS Observational Data
#  counts website.        
#
#  History
#  Ralph Jones   10/04/2005  -  Original version.
#  L. Sager      09/30/2007  -  Modified for operations
#  V. Krishna Kumar 11/15/2008 - Modified to adjust the index html file
#                                for new years
#  D. Stokes  04/2015 Moved to vertical structure package obsproc_prep_post.
#                     Added shebang to run under ksh.  (Since WCOSS transition
#                     this script has been running under bash which does not
#                     capture output from a piped command as expected here).
#                     Removed typeset of variable last so as not to limit it to 
#                     three characters. Similarly, modified sed statement to
#                     prevent corruption of the html file should it reach more 
#                     than 999 lines.  Added checks to reduce risk main page 
#                     corruption if a month is rerun.  Add option to rebuild
#                     complete main page.
#  D. Keyser  05/2015 Split imported script variable $DATCOM1 into separate
#                     input and output variables $DATCOM1_IN and $DATCOM1_OUT
#                     set in parent Job Script.
#                     BENEFIT: Allows checkout runs to specify a production
#                              input location and a local output location.
#
################################################################################

set -x
  file0=FILE_NONSAT
  file1=received_counts.html
  file2=selected_counts.html
  file3=assimilated_counts.html

 
# Remove satellite counts from the first file

  line1=`fgrep -n "Satellite Soundings"  $file0 | cut -c1-3` 
  line2=`fgrep -n "BR WP" $file0 | cut -c1-3 | head -1`
  let line2=line2-1


    echo -e "\n line1 = ${line1}   line2 = ${line2} \n"

  sed ${line1},${line2}d $file0 > temp


# Append 3 satellite data count files

  cat ${file1} >> temp
  cat ${file2} >> temp
  cat ${file3} >> temp

  cp temp temp.sav
  mv temp ${Month}_${YEAR}.html


###############################


  HTML="          <a href=\"${Month}_${YEAR}.html\">${Month}</a>"
    echo $HTML


# Backup the index file and make a temporary working copy

  cp $DATCOM1_IN/index.shtml $DATCOM1_OUT/index_backup.shtml
  cp $DATCOM1_IN/index.shtml temp

# We have option here to rebuild the gdas counts main page starting with 
# content from $FIXprepobs/gdascounts_base.shtml.  Before doing so,
# ensure the header and footer content that will surround the new table is
# the same as (or preferred to) that in the current index.shtml file.
if [[ "$REBUILD_MAINPAGE" == YES ]]; then

  export StartYr=2002
  export EndYr=$YEAR
  export LastMon_EndYr=$MM
  $USHprepobs/build_mainpage.sh
  err=$?
  if [[ $err -eq 0 ]]; then
    cp index.shtml temp2  # this may seem odd, but parent script expects temp2
  else
    echo -e  "\n *** WARNING:  Attempt to build mainpage from scratch failed... \n "
    exit 7
  fi

else

# update content of existing index.shtml file (as has been done for years)

# If this is January, load an empty calendar in the front of the index file 

# Insert below in index.shtml after </table> and then change as counts are added 
# for a given month.  Always save old index before modifying.  This will help to 
# eliminate the shoot in foot syndrome.  Eliminate these comments in new index.
#
  if [ $MON = JAN ]; then

#  However, only load new calendar for this year if one does not already exist.
#  (One mmight exist in the event of a rerun.  Note: Must update the following
#  $YEAR check if the format for the empty calendar changes below)
#
    if [ $(grep -c "<b>$YEAR</b>" temp) -eq 0 ]; then

cat << EOF  > newyear
    <center>
    <table width="600" border="1" cellpadding="6" cellspacing="0">
      <tr align="center" bgcolor="#ffcccc">
        <td colspan="6">
          <font size="+1"><b>YEAR</b></font>
        </td>
      </tr>
      <tr>
        <td width="17%">January</td>
        <td width="17%">February</td>
        <td width="16%">March</td>
        <td width="16%">April</td>
        <td width="17%">May</td>
        <td width="17%">June</td>
      </tr>
      <tr>
        <td width="17%">July</td>
        <td width="17%">August</td>
        <td width="16%">September</td>
        <td width="16%">October</td>
        <td width="17%">November</td>
        <td width="17%">December</td>
      </tr>
    </table>

EOF

#    sed 1,3d newyear > new1
#    sed s/YEAR/$YEAR/ new1 > new2

    sed s/YEAR/$YEAR/ newyear > new2

# note, the following head statement assumes prexisting format of html file 
# with calendar table starting at or just below line 15
    head -14 temp > new1

    cat new2 >> new1

    sed -n 15,\$p temp >> new1

    cp new1 temp
    else
      echo -e "\n *** Warning: A table with header $YEAR already existed ***\n"
    fi
  fi

  
# note the following check on "$MONTH</td>" must be in sync with table format
  line=`fgrep -n "$Month</td>" temp | head -1`
  if [ -n "$line" ]; then
  echo $line | IFS=":" read line_num dum

    echo -e "\n line = $line  line_num = ${line_num}"

# note the following pattern "$MONTH</td>" must be in sync with table format
  sed ${line_num}s/"$Month<\/td>"/""/ temp > temp1

  sed -n 1,${line_num}p temp1 > temp2

  echo "$HTML" >> temp2

  echo "        </td>" >> temp2

  let next=line_num+1

  last=`wc -l < temp1`

  sed -n ${next},${last}p temp1 >> temp2

  else  # a link for this month already exists. (this is likely a rerun).
   echo -e "\n *** Warning: Link for $Month already existed ***\n"
   cp temp temp2
  fi

set +x 
  exit

fi
