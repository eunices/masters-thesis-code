#!/bin/bash
conda init bash
conda activate msc
python -m data_download
bash data_download_auto.sh