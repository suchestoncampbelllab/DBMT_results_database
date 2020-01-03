#!/bin/bash
#SBATCH --time=06:00:00
#SBATCH --nodes=1
#SBATCH --mem=48000
#SBATCH --mail-user=karaesmen.1@osu.edu
#SBATCH --ntasks-per-node=1
#SBATCH --mail-type=END
#SBATCH --partition=general-compute --qos=general-compute
#SBATCH--job-name=snptbl_split
#SBATCH --output=/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/DBprep/log/snptbl_miss_rsID.out
#SBATCH --error=/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/DBprep/log/snptbl_miss_rsID.err

#Get date and time
tstart=$(date +%s)
echo "###### start time: "`date`

echo "************************"

module load R/3.5.3

R --file=/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/DBprep/code/fix_missing_rsIDs.R

echo  "###### All Done: "`date`




