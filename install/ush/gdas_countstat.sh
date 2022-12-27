#!/bin/sh
#------------------------------------------------------------------------------
#
#  gdas_countstat.sh                         
#
#
#  This script collects gdas observational data counts from Dennis Keysers
#  status count files.
#
#  Usage: gdas_countstat.sh<start_date> <network> <num days> <hour> <month> [run]
#
#  History
#  L. Sager  09/07  Modified script to work properly in operations.
#  C. Magee  12/08  Modified script to remove REPROCESSED references.
#  D. Stokes 04/2015  Moved to vertical structure package obsproc_prep_post 
#                     and updated variables pointing to other software
#                     included in this migration.  Made additional minor 
#                     changes to reduce unnecessary warnings.
#  D. Keyser 05/2015  Split imported script variable $DATCOM into separate
#                     input and output variables $DATCOMIN (root to
#                     $DATCOMIN_dir) and $DATCOMOUT_dir set in parent Job
#                     Script.
#                     BENEFIT: Allows checkout runs to specify a production
#                              input location and a local output location.
#  D. Stokes 03/2017  Modified report count parsing in preparation for future
#                     field width increase in dump status files.  Removed
#                     unnecessary commands and rearranged some logic to improve
#                     performance on Cray-XC40.  Replaced hardwired filename 
#                     prefix "gdas1" with new variable "$run" in preparation for
#                     gdas filename change (gdas1->gdas).  Added optional 6th
#                     argument to pass in a value for $run (to aid generation of 
#                     statistics in delayed mode).
#
#------------------------------------------------------------------------------

set -x 

start=$1
net=$2
numdays=$3
hour=$4
CYCLE=t${hour}z
month=$5
run=${6:-${RUN:-gdas}}

num=`expr $numdays - 1 `
set +x; echo -e "\n---> path to finddate.sh below is: `which finddate.sh`"; set -x
string=`finddate.sh $start s+$num`
#echo "num=$num"
#echo "string=$string"

daystring="$start ""$string"
#echo $daystring 
echo "&DATA" > ${net}_${month}_dumpstats.$CYCLE
  type0="000.001 000.007"
  type1="001.001 001.002 001.003 001.004 001.005 001.006"
  type2="002.001 002.002 002.003 002.004 002.005 002.007 002.009 002.008"
  type3="003.001 003.104"
  type4="004.001 004.002 004.003 004.004 004.005"
  type5="005.010 005.012 005.013 005.041 005.042 005.043 005.064" 
  type8="008.010"
  type12="012.001 012.002 012.013 TMI 012.103 012.137 REPROCESSED"
  type21="021.021 021.022 021.023 021.024 021.025 021.041"
  type99="COMBINED"
  types="$type0 $type1 $type2 $type3 $type4 $type5 $type8 $type12 $type21 $type99"

  for type in $types
  do
      rm -f $type
  done

  for day in $daystring
  do
      echo -e "\n     DAY = $day"
      file=$DATCOMIN_dir/$run.$CYCLE.status.tm00.bufr_d.$day
#     echo file is $file
      if test -f $file
      then
        TF=$run.$CYCLE.status.tm00.bufr_d.$day.trimmed.$$
        grep -v Domain $file > $TF
        for type in $types
        do
            if test $type = '012.137'
            then
#             num=`grep $type $TF | grep -vi Global | grep qkscat | cut -c65-71`
              num=$(grep $type $TF | grep qkscat | sed -ne 's/.* HAS\( *\)\([0-9]\+\) REPORTS\( *\)$/\2/p')
#             echo num=$num
            elif test $type = 'REPROCESSED'
            then
              num=`grep 'REPROCESSED QUIKSCAT' $TF | awk '{ print $11 }'`
            elif test $type = 'COMBINED'
            then
              num=`grep 'COMBINED TOTAL' $TF | awk '{ print $9 }'`
            elif test $type = 'TMI'
            then
              num=`grep $type $TF | awk '{ print $12 } '`
            else
#             num=`grep $type $TF | grep -vi Global | cut -c65-71`
              num=$(grep $type $TF | sed -ne 's/.* HAS\( *\)\([0-9]\+\) REPORTS\( *\)$/\2/p')
            fi
            if test -n "$num";then
              if test $num -ge 0
              then
                echo $num >> $type
              fi
            fi
        done
      echo "Processing complete for $day"
      else
        echo "$file does not exist.  No processing for day $day"
      fi
  done

  for type in $types
  do
        if test -s $type
        then 
          echo "get stats for $type"
          mnemonic=`grep $type $FIXprepobs/gdascounts_types | awk '{print $2}'`
          awk -f $FIXprepobs/gdascounts_avg.awk $type var=$mnemonic >> ${net}_${month}_dumpstats.$CYCLE
        else
          echo $type is not dumped for $net
        fi
  done

mkdir $DATA/${MON}
sed -e 's/ = /=/g' ${net}_${month}_dumpstats.$CYCLE > tmps                                             
echo "/" >>tmps

cp  tmps  $DATA/${net}_${month}_dumpstats.$CYCLE

exit

