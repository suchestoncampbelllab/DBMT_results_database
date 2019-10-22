#!/bin/bash
#SBATCH --time=02:00:00
#SBATCH --nodes=1
#SBATCH --mem=24000
#SBATCH --mail-user=karaesmen.1@osu.edu
#SBATCH --ntasks-per-node=4
#SBATCH --mail-type=END
#SBATCH --partition=general-compute --qos general-compute
#SBATCH--job-name= snptbl_split
#SBATCH --output= /projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/DBprep/lo/snptbl_split.out
#SBATCH --error=//projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/DBprep/log/snptbl_split.err

#Get date and time
tstart=$(date +%s)
echo "###### start time:"`date`

echo "************************"

module load R/3.5.3

R --file=/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/DBprep/code/snp_tbl_split.R

echo "All Done!"




