#!/bin/sh
#
# build_mainpage.sh
#
# Abstract: 
#   Rebuild gdas counts main webpage such as that posted at:
#     http://www.nco.ncep.noaa.gov/pmb/nwprod/gdas/
#
# Imported Variables:
#   StartYr                 4-digit year (Optional)
#   EndYr                   4-digit year
#   LastMon_EndYr           2-digit month
#   FIXprepobs              Directory containing input base html file
#
# Fixed files:
#   $FIXprepobs/gdascounts_base.shtml
#
# Output:
#   index.shtml
#
# Condition codes
#   0 - no problem encountered
#   7 - Format of input base html file is no longer as expected.
#   >0 - some other problem encountered
#
# History:
#  D. Stokes     05/01/2015  -  Original version.
#
 
set -x

StartYr=${StartYr:-2002}
EndYr=${EndYr:?}
LastMon_EndYr=${LastMon_EndYr:?}

# Set up array containing month names
#
declare -A Month
Month[01]=January
Month[02]=February
Month[03]=March
Month[04]=April
Month[05]=May
Month[06]=June
Month[07]=July
Month[08]=August
Month[09]=September
Month[10]=October
Month[11]=November
Month[12]=December

# Bring in fresh page base.
# 
cp $FIXprepobs/gdascounts_base.shtml gdascounts_base.shtml

# Above file should have the demarcation patterns expected in following sed commands
#
nlines_header=$(grep -n "<!-- table starts below this line" gdascounts_base.shtml|cut -f1 -d":")
nlines_footer=$(grep -n "<!-- table ends above this line" gdascounts_base.shtml|cut -f1 -d":")
if [[ -z "$nlines_header" || -z "$nlines_footer" ]]; then
  echo "Warning: Unexpected content in input shtml file"
  exit 7
fi
sed -n 1,${nlines_header}p gdascounts_base.shtml | sed \$d  > page_header
sed -n ${nlines_footer},\$p gdascounts_base.shtml | sed 1d  > page_footer

# Set up empty calendar template used to create table
#
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

# Add header to new shtml file
#
cat page_header > index.shtml

# Loop through years, adding calendar to shtml file
# Add monthly links up through last requested month.
#
for YEAR in $(seq $EndYr -1 $StartYr); do
  sed s/YEAR/$YEAR/ newyear > cal$YEAR
  if [[ $YEAR -lt $EndYr ]]; then
    EndMon=12
  else
    EndMon=$LastMon_EndYr
  fi
  for MM in $(seq -w 01 $EndMon); do
    sed -i -e s/"${Month[$MM]}<\/td>"/"<a href=\"${Month[$MM]}_${YEAR}.html\">${Month[$MM]}<\/a><\/td>"/ cal$YEAR 
  done

  cat cal$YEAR >> index.shtml
done
  
# Add footer to shtml page
#
cat page_footer >> index.shtml

