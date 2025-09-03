#!/bin/bash 
#---------------------------------------------------
partition=main
settings_files='./SETTINGS'
#---------------------------------------------------

cat <<EOF > run_job.sh
#!/bin/bash
./prep_flexpart ${settings_files} 
EOF

sbatch --job-name=prep_flexpart --mem-per-cpu=1000 --partition=${partition} run_job.sh

rm -f run_job.sh

