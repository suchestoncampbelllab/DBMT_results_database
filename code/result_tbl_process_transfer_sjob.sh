#!/bin/bash
FILEDIR=/projects/rpci/lsuchest/lsuchest/DBMT_results/DBprep/test;

FILES=$(ls $FILEDIR);
FILES=${FILES//.res/};

for patt in ${FILES[@]};
do

cat <<EOM > ${patt}.sh
#!/bin/bash
#SBATCH --time=01:00:00
#SBATCH --nodes=1
#SBATCH --mem=32000
#SBATCH --mail-user=karaesmen.1@osu.edu
#SBATCH --mail-type=END
#SBATCH --partition=general-compute --qos=general-compute
#SBATCH --job-name=mixed_SNPxBuVsTBI_manPlots
#SBATCH --output=/projects/rpci/lsuchest/lsuchest/DBMT_results/DBprep/log/%j_${patt}.out
#SBATCH --error=/projects/rpci/lsuchest/lsuchest/DBMT_results/DBprep/log/%j_${patt}.err

DBPREP=/projects/rpci/lsuchest/lsuchest/DBMT_results/DBprep

module load R/3.4.1

source $MKL/bin/mklvars.sh intel64

R --file=\$DBPREP/code/result_tbl_process_transfer.R -q --args patt ${patt} path_in \$FILEDIR path_out \$DBPREP/out

exit
EOM

echo -e "\tsubmitting file: ${patt}\n";
sbatch ${patt}.sh;

done