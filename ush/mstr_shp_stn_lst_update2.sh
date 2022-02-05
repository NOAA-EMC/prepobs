#!/bin/ksh
#    mstr_shp_stn_lst_update2.sh
#
#    This script updates the "Master Ship Station List" file
#     and the (modified) NDBC active ship list file and copies
#     them both into the archive so they will be ready for
#     tomorrow's run
#
# History
#      Steve Lilly 02/01/2016   -  Original script
#      Dennis Keyser 02/12/2016 -  Modifications to put into production
#
#

set -xa

 cd $DATA


#  initializing
#  ------------

 cat ship_names2 > ship_names
 cat activeShiprev2 > activeShiprev
 cat actships3 > unqiships
 ick="0"


#  checking for ships which have been renamed
#
#  prints duplicate lines once, but no unique lines
#  ------------------------------------------------

cut -c 1-9 unqiships4 | uniq -d | cat > cklst


#  ckcnt provides a count of ships that are renamed
#    if ckcnt = 0, there are no ships that have been renamed
#    if ckcnt > 0, there are ships that have been renamed
#  ---------------------------------------------------------

ckcnt=`wc -l < cklst` 


#  remove the old ship names from previous (modified) NDBC active ship list
#   file and from previous (modified) "Master Ship Station List" file
#
#  this allows the new ship names to be added to the above files 
#  --------------------------------------------------------------------------

if [ $ckcnt -gt 0 ];then
  grep -v -f cklst ship_names2 | cat > ship_names
  grep -v -f cklst activeShiprev2 | cat > activeShiprev


#  repeating the process to generate a list of ship names {found in previous
#   (modified) NDBC active ship list file and in previous (modified) "Master
#   Ship Station List" file} which are renamed 
#  -------------------------------------------------------------------------

  cat actshplst > actships3
  cat activeShiprev >> actships3
  sort -d actships3 | uniq -u | cat > unqiships
  ick="1"
fi

cut -c 1-65 activeShiprev | cut -c 1-9,15-72 | cat > actshiplst
cut -c 1-65 ship_names | cut -c 1-10,11-60 | cat > shipnames


#  $ick determines how the file "unqiships" needs to be cut
#  --------------------------------------------------------

#####echo $ick

if [ $ick -eq 0 ]; then
  set +x
  echo
  echo "ick=0, there are no ships that have been renamed -> unqiships (plus \
ancillary data) copied to unqiships2"
  echo
  set -x
  cut -c 1-65 unqiships | cut -c 1-10,11-72 | cat > unqiships2
else
  set +x
  echo
  echo "ick=1, there are ships that have been renamed -> unqiships (minus \
ancillary data) copied to unqiships2"
  echo
  set -x
  cut -c 1-65 unqiships | cut -c 1-9,15-72 | cat > unqiships2
fi

cat actshiplst > actship1
cat shipnames >> actship1

#  complete ship reports with any duplicates removed in actship1 written to
#   shipnmnew
#  ------------------------------------------------------------------------

sort -d actship1 | sort -u | cat > shipnmnew

cat unqiships2 > actship2
cat shipnmnew >> actship2


#  compare the size of the previous (modified) NDBC active ship list file to
#   the size of the current NDBC active ship list file to determine if ships
#   need to be added to or deleted from the "Master Ship Station List"
#  -------------------------------------------------------------------------

ncolst=`wc -l < activeShiprev2`
ndbclst=`wc -l < actshplst`

#####echo $ncolst
#####echo $ndbclst

if [ $ncolst -gt $ndbclst ]; then

#  ships are to be removed(/renamed) from the "Master Ship Station List"
#  unique complete ship reports in actship2 written to shipnmnew
#   (replacing existing file)
#  ---------------------------------------------------------------------

  set +x
  echo
  echo "ships are to be removed(/renamed) from the \"Master Ship Station List\""
  echo
  set -x
  sort -d actship2 | uniq -u | cat > shipnmnew
fi
 
if [ $ncolst -le $ndbclst ]; then

#  ships are to be added(/renamed) to the "Master Ship Station List"
#  complete ship reports with any duplicates removed in actship2 are
#   written to shipnmnew (replacing existing file)
#  -----------------------------------------------------------------

  set +x
  echo
  echo "ships are to be added(/renamed) to the \"Master Ship Station List\""
  echo
  set -x
  sort -d actship2 | sort -u | cat > shipnmnew
fi


#  test to determine if the number of ships in the just updated "Master Ship
#   Station List" file is greater than the limit of 90,000 set in the program
#   "marine_stats_newstats" which reads this file on the first day of each
#   month (in JRW1)
#  --------------------------------------------------------------------------

inshipcnt=`wc -l < shipnmnew`

if [ $inshipcnt -gt 90000 ]; then

  msg="**WARNING: >90K ships in updated \"Master Ship Station List\" file - \
this file and (modified) NDBC active ship list file cannot be updated in \
$VOSarch_OUT"
  msg1="**WARNING (cont.): The current NDBC active ship list file \
$SHPNAMDIR_DCOM/activeShipLst has too many reports or is corrupted"
  $DATA/postmsg "$jlogfile" "$msg"
  $DATA/postmsg "$jlogfile" "$msg1"
  set +x
  echo
  echo "$msg"
  echo
  echo "$msg1"
  echo
  set -x

  exit
fi


#  copy the updated "Master Ship Station List" and (modified) NDBC active ship
#   list files into the archive
#  ---------------------------------------------------------------------------

cp shipnmnew $VOSarch_OUT/ship_names
errcp=$?
if [ $errcp -eq 0 ]; then
  msg="UPDATED \"MASTER SHIP STATION LIST\" SUCCESSFULLY COPIED TO \
$VOSarch_OUT/ship_names"
else
  msg="**WARNING: ERROR COPYING UPDATED \"MASTER SHIP STATION LIST\" TO \
$VOSarch_OUT/ship_names"
fi
$DATA/postmsg "$jlogfile" "$msg"
set +x
echo
echo "$msg"
echo
set -x

cp actshplst $VOSarch_OUT/activeShiprev
errcp=$?
if [ $errcp -eq 0 ]; then
  msg="MODIFIED NCDC ACTIVE SHIP LIST SUCCESSFULLY COPIED TO \
$VOSarch_OUT/activeShiprev"
else
  msg="**WARNING: ERROR COPYING MODIFIED NCDC ACTIVE SHIP LIST TO \
$VOSarch_OUT/activeShiprev"
fi
$DATA/postmsg "$jlogfile" "$msg"
set +x
echo
echo "$msg"
echo
set -x

exit
