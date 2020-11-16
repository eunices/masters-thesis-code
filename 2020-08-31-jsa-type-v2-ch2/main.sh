cd $PBS_O_WORKDIR

source /etc/profile.d/rec_modules.sh
module load miniconda
source activate renv

Rscript 2020-08-31-jsa-type-v2-ch2/02-model/main.r