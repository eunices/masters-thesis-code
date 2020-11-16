cd $PBS_O_WORKDIR

source /etc/profile.d/rec_modules.sh
module load miniconda
source activate renv

Rscript ch2-test/02-model/main.r