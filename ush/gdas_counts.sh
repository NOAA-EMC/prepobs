#!/bin/sh
#    gdas_counts.sh
#
#    This scripts copies the files containing 
#      observational data counts to an archive  
#      for compiling at the end of month
#
# History
#      Larry Sager 06/2007   -  Implementation of
#                               Ralph Jones' script
#      Krishna Kumar 10/2012    Modified from the generic name of the 
#                               type gps to the specific type gps_bnd
#      D. Keyser     05/2015    Split imported script variables $SATCOM and
#                               $DATCOM into separate input and output
#                               variables $SATCOMIN (root to $SATCOMIN_dir) and
#                               $SATCOMOUT_dir, and $DATCOMIN (root to
#                               $DATCOMIN_dir) and $DATCOMOUT_dir set in parent
#                               Job Script.
#                               BENEFIT: Allows checkout runs to specify a
#                                        production input location and a local
#                                        output location.
#      D. Stokes     03/2017    Changed explicit gdas1 refs to $RUN
#      S. Melchior   04/2018    Modified INFILE to use $COMIN1 to accommodate
#                               the $cyc subdir in com introduced in FV3GFS.
#      S. Melchior   07/2020    Introduced logic to define INFILE based on
#                               version of GFS to determine directory structure.
#
  cd $DATA 

#  Loop through each cycle and create reformatted files

   for d_cyc in t00z t06z t12z t18z
     do
      cycdir=$(echo ${d_cyc} | sed 's/^.\(.*\).$/\1/')
      INFILE="${COMIN1}${PDY}/${cycdir}/$COMPONENT/$RUN.${d_cyc}.status.tm00.bufr_d"
      if [ ! -f $INFILE ]; then
            echo -e "\n\n  INFILE:  $INFILE  does not exist \n\n"
            break
      fi
      grep "in data group" ${INFILE}  > tmp
      grep "total for all" ${INFILE}  >> tmp
      grep "COMBINED TOTAL" ${INFILE} >> tmp
      grep "(SUPEROBED) TRMM" ${INFILE} | cut -c1-80 >> tmp
      grep "REPROCESSED QUIKSCAT" ${INFILE}   >> tmp
      cp tmp ${DATCOMOUT_dir}/$RUN.${d_cyc}.status.tm00.bufr_d.$PDY   
      echo COUNTS  created ${DATCOMOUT_dir}/$RUN.${d_cyc}.status.tm00.bufr_d.$PDY
    done


#
# Loop through the 4 gsistats files and collect the assimilated counts 
#
    for d_cyc in t00z t06z t12z t18z
    do
      cycdir=$(echo ${d_cyc} | sed 's/^.\(.*\).$/\1/')
      INFILE=${COMIN1}${PDY}/${cycdir}/$COMPONENT/$RUN.${d_cyc}.gsistat        
      if [ ! -f $INFILE ]; then
            echo -e "\n\n  INFILE:  $INFILE  does not exist \n\n"
            break
      fi
      grep "o-g 03 rad" $INFILE  > tmp
      grep "o-g 03 pcp" $INFILE  >> tmp
      grep "o-g 03 oz" $INFILE  >> tmp
#      grep "type   gps jiter   3" $INFILE >> tmp
      grep "type gps_bnd jiter   3" $INFILE >> tmp
      grep -v "0          0          0   " tmp > tmpa
      cp tmpa ${SATCOMOUT_dir}/gdas_gsistat.${PDY}.${d_cyc}
      echo COUNTS  created ${SATCOMOUT_dir}/gdas_gsistat.${PDY}.${d_cyc}
    done 

exit
