#!/bin/bash 
#---------------------------------------------------
# Check FLEXPART jobs completed
#---------------------------------------------------
# Guide to use:
#   1. edit the directories, list of stations
#      months and year below
#   2. run the script:
#      ./check_flexpart.sh > fileout
#---------------------------------------------------

# To run tests reference to current directory
DIR=$PWD
LEN=`expr length $DIR - 13`
ROOTDIR=${DIR:0:$LEN}

#---------------------------------------------------

# User settings
DIROPTIONS=${ROOTDIR}TEST_INPUT/FLEXPART/GHG/NEST/
DIROUT=${ROOTDIR}TEST_OUTPUT/FLEXOUT/GHG/NEST/
STNLIST=(MHD)
YEAR=2012
MONLIST=(01)

#---------------------------------------------------

NSTN=${#STNLIST[@]}
echo "number of stations: "${NSTN}
ARRAYSTN=`seq 1 ${NSTN}`

NMON=${#MONLIST[@]}
echo "number of months: "${NMON}
ARRAYMON=`seq 1 ${NMON}`

# loop over FLEXPART runs

for k in ${ARRAYSTN}
do
  j=`expr $k - 1`
  STN=${STNLIST[$j]}

  for i in ${ARRAYMON}
  do
    l=`expr $i - 1`
    MON=${MONLIST[$l]}

    # check RELEASES file exists if not go to next
    echo ${DIROPTIONS}${STN}/${YEAR}${MON}/options/RELEASES
    if [ ! `ls ${DIROPTIONS}${STN}/${YEAR}${MON}/options/RELEASES` ]; then
      continue
    fi

    # runs exist - check if completed
    if grep -q CONGRATULATIONS ${DIROPTIONS}${STN}/${YEAR}${MON}/flex.log ; then
     echo "ok"
    else
      echo "RUN NOT COMPLETED: "${STN}/${YEAR}${MON}
    fi

    # check if not screwed-up
    nline=`wc -l < ${DIROUT}${STN}/${YEAR}${MON}/dates`
    echo $nline
    if [ $nline -gt 1000 ]; then
      echo "RUN APPEARS MESSED-UP: "${STN}/${YEAR}${MON}
    fi 

  done

done


