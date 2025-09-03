================================================================

 FLEXPART pre-processor 

================================================================

Description:
  Creates options files needed to run FLEXPART for subsequent 
  use with FLEXINVERT. A new options folder is created for each 
  station (fixed location receptor) and month. For ships and 
  aircraft (moving receptors) a new folder is created for each 
  campaign (and month).

Note:
  For running FLEXINVERT, the releases are made for each 
  observation or (observation average) according to laverage, 
  lselect and intaverage in SETTINGS.
  The selected and/or averaged observations corresponding to the 
  releases are written to the directory path_obsout in SETTINGS. 
  These are then used as input for FLEXINVERT.
  Currently subroutines are included for reading ObsPack and
  WDCGG formatted observations. 
  Tested with FLEXPART Version 10.1beta

Pre-processing:
  1) compile with gfortran using: make
  2) edit the SETTINGS file
  3) edit the bash script: job_prep_flexpart.sh 
     (or alternatively for slurm: sbatch_prep_flexpart.sh)
  4) run the bash script: ./job_prep_flexpart.sh
     (or ./sbatch_prep_flexpart.sh)

Running FLEXPART:
  1) edit the bash script: run_flexpart.sh
  2) run the bash script: ./run_flexpart.sh

Checking FLEXPART jobs completed:
  1) edit the bash script: check_flexpart.sh
  2) run ./check_flexpart.sh

