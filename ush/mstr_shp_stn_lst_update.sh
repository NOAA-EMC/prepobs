#!/bin/ksh 
#    mstr_shp_stn_lst_update.sh
#
#    This script copies an updated VOS ship list from
#      NDBC and merges the file with a copy of the
#      previous ship_names list
#
# History
#      Steve Lilly 02/01/2016   -  Original script
#      Dennis Keyser 02/12/2016 -  Modifications to put into production
#      Dennis Keyser 05/05/2016 -  If input archive directory .ne. output
#            archive directory and there are no files in output archive
#            directory, then if no changes to files, copy them from input
#            archive directory to output archive directory.
#            BENEFIT:  Allows checkout runs to populate $$VOSarch_OUT even when
#                      there are no file changes.
#

set -xa

cd $DATA

date

cd $DATA


#  copy input files to temporary working directory
#  -----------------------------------------------

 # current NDBC active ship list
cp $SHPNAMDIR_DCOM/activeShipLst actshplst2

 # previous (modified) NDBC active ship list
cp $VOSarch_IN/activeShiprev activeShiprev2

 # previous updated "Master Ship Station List"
cp $VOSarch_IN/ship_names ship_names3


#  initializing
#  ------------

cat actshplst2 > actshplst
cat ship_names3 > ship_names2


#  identify ship names sharing same "call_sign" in current NDBC active ship list
#  -----------------------------------------------------------------------------

cut -c 1-9 actshplst2 | uniq -d | cat > ckids


#  if ckids = 0, indicates that each ship has a unique "call_sign"
#
#  if ckids > 0, indicates that various ships are sharing a unique "call_sign",
#                these ships are removed from current NDBC active ship list
#  ----------------------------------------------------------------------------

cckids=`wc -l < ckids`

if [ $cckids -gt 0 ]; then
  grep -v -f ckids actshplst2 | cat > actshplst
fi


#  test to determine if current NDBC active ship list contains:
#    ships which are to be added,
#    ships which are to be deleted,
#      or
#    ships which are to be renamed
#  ------------------------------------------------------------

cat actshplst > actships2
cat activeShiprev2 >> actships2

#  format required to attach current NDBC active ship list file with previous
#   (modified) NDBC active ship list file (providing a list of ships requiring
#   action)

cut -c 1-9,10-65 actships2 | sort -d | uniq -u | cat > unqiships4

#  format required to attach current NDBC active ship list file with previous
#   updated "Master Ship Station List" file (providing a list of ships requiring
#   action)

cut -c 1-9,15-65 actships2 | sort -d | uniq -u | cat > actships3

#  check to see if any of the unique "call_signs" coming out of the combined
#   current NDBC active ship list file and the previous (modified) NDBC active
#   ship list file are found in the previous updated "Master Ship Station List"
#   file
#
#  ckuniq provides a count of ships that are unique
#    if ckuq = 0, there are no similar unique "call_signs" found
#    if ckuq > 0, there are similar unique "call_signs" found

cut -c 1-10 actships3 | uniq -u | cat > ckuniq
ckuq=`wc -l < ckuniq`

#  remove ships that have the same "call_sign" from previous updated "Master
#   Ship Station List" file - this prevents duplication of "call_sign" in
#   previous updated "Master Ship Station List" file 

if [ $ckuq -gt 0 ]; then
  grep -v -f ckuniq ship_names3 | cat > ship_names2
fi

#  unshpcnt provides a count of ships requiring action
#    if unshpcnt = 0, no action is required to update previous (modified) NDBC
#                     active ship list and previous updated "Master Ship
#                     Station List" files 
#    if unshpcnt > 0, action is required to update these files

unshpcnt=`wc -l < unqiships4`

if [ $unshpcnt -eq 0 ]; then
  msg="No changes to previous (modified) NDBC active ship list file or to \
previous updated \"Master Ship Station List\" file are required" 
  $DATA/postmsg "$jlogfile" "$msg"
  set +x
  echo
  echo "$msg"
  echo
  set -x

  if [ $VOSarch_IN != $VOSarch_OUT -a ! -s $VOSarch_OUT/activeShiprev -a \
                                      ! -s $VOSarch_OUT/ship_names ]; then

# ... checkout

    msg="Input and Output VOS archive locations differ, output location is \
empty -- copy Input VOS arch files to Output location -- likely checkout" 
    $DATA/postmsg "$jlogfile" "$msg"
    set +x
    echo
    echo "$msg"
    echo
    set -x
    cp $VOSarch_IN/activeShiprev $VOSarch_OUT/activeShiprev
    cp $VOSarch_IN/ship_names $VOSarch_OUT/ship_names
  fi
  msg="VOS2_DAILY PROCESSING COMPLETED NORMALLY"
  $DATA/postmsg "$jlogfile" "$msg"
  set +x
  echo
  echo "$msg"
  echo
  set -x

  exit 
fi
     
msg="Changes to previous (modified) NDBC active ship list file and to previous \
updated \"Master Ship Station List\" file ARE REQUIRED" 
$DATA/postmsg "$jlogfile" "$msg"
set +x
echo
echo "$msg"
echo
set -x

$USHprepobs/mstr_shp_stn_lst_update2.sh

msg="VOS2_DAILY PROCESSING COMPLETED NORMALLY"
$DATA/postmsg "$jlogfile" "$msg"
set +x
echo
echo "$msg"
echo
set -x

exit
