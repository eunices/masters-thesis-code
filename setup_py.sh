#!/bin/bash

cd .. 
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
bash Miniconda3-latest-Linux-x86_64.sh

conda env create -f environment-py-msc.yaml
conda activate msc
python -m ipykernel install --user --name msc --display-name "msc"

# conda env update -f environment-py.yaml
# conda activate msc
# conda info --env