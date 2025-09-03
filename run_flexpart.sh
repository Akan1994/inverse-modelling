#!/bin/bash 
#---------------------------------------------------
# Run FLEXPART jobs
#---------------------------------------------------
# Guide to use:
#   1. edit the directories, exename, list of stations
#      months and year below
#   2. for slurm set SLURM = 1 otherwise uses nohup  
#   3. specific options for compiling flexpart given
#      in variable COMMAND 
#   4. run the script:
#      ./run_flexpart.sh
#---------------------------------------------------

# To run tests reference to current directory
DIR=$PWD
LEN=`expr length $DIR - 13`
ROOTDIR=${DIR:0:$LEN}

#---------------------------------------------------

# User settings
SLURM=1
TIMELIM=12:00:00
PARTITION="nilu"
EXENAME=FLEXPART
COMMAND="-j serial 'ncf=yes'"
DIRFLEX=/mypath/flexpart/
DIROPTIONS=/mypath/flexpart_options/
STNLIST=(SSL)
MONLIST=(01)
YEAR=2012

#---------------------------------------------------

NSTN=${#STNLIST[@]}
echo "number of stations: "${NSTN}
ARRAYSTN=`seq 1 ${NSTN}`

NMON=${#MONLIST[@]}
echo "number of months: "${NMON}
ARRAYMON=`seq 1 ${NMON}`

# check FLEXPART executable

cd ${DIRFLEX}src/
if [ ! `ls ${DIRFLEX}src/${EXENAME}` ]; then
  make ${COMMAND}
  if [ ! `ls ${DIRFLEX}src/${EXENAME}` ]; then                               
    echo "cannot create exec: "${DIRFLEX}src/${EXENAME}
    exit -1
  fi
fi 
chmod u+x ${EXENAME}

# loop over FLEXPART runs

for k in ${ARRAYSTN}
do
  j=`expr $k - 1`
  STN=${STNLIST[$j]}
  for i in ${ARRAYMON} 
  do 
    l=`expr $i - 1`
    MON=${MONLIST[$l]}

    # check RELEASES file exists if not go to next iteration
    echo ${DIROPTIONS}${STN}/${YEAR}${MON}/options/RELEASES
    if [ ! `ls ${DIROPTIONS}${STN}/${YEAR}${MON}/options/RELEASES` ]; then 
      continue
    fi

    # set-up job
    cd ${DIROPTIONS}${STN}/${YEAR}${MON}
    cp ${DIRFLEX}src/${EXENAME} job_${STN}_${MON}
    rm -f flex.log
    OUTPUT=flex.log
    echo "submitting job for: "${STN}" "${MON}

    if [ ${SLURM} -eq 1 ]; then
    # create submit.sh for submitting to slurm
cat <<EOF > submit.sh
#!/bin/bash
./job_${STN}_${MON}
EOF
sbatch --job-name=job_${STN}_${MON} --partition=${PARTITION} --time=${TIMELIM} --mem-per-cpu=8000 --output=${OUTPUT} submit.sh
      sleep 5 
      rm -f submit.sh
    else
    # use nohup
      nohup ./job_${STN}_${MON} > ${OUTPUT} 
      sleep 5
    fi

  done
done

# the end


