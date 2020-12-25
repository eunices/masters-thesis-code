# Install packages on super computer
cd $PBS_O_WORKDIR

source /etc/profile.d/rec_modules.sh
module load miniconda
source activate renv

Rscript install/install.r