 #!/bin/bash
folder_dir=$(pwd)
base_dir=${PWD%/*}
geo_data_dir="$base_dir/data/geo"
chelsa_dir="$geo_data_dir/chelsa"


echo "Activate conda environment"
conda activate msc


echo "Making all dirs"
mkdir $geo_data_dir
mkdir $chelsa_dir


echo "Download metadata for chelsa"
mkdir $chelsa_dir
cd $chelsa_dir
url="http://chelsa-climate.org/wp-admin/download-page/index.js"
wget $url -O "index.js"


echo "Modify metadata for chelsa to have a list of links"
python -m data_chelsa part1 # add extra commands in js file
cd $chelsa_dir 
npm init --yes && npm install file-system --save && node "index.js" # dump as json
cd $folder_dir
python -m "$folder_dir/data_chelsa" part2 # dump list of links as text


echo "Download chelsea data to $chelsa_dir"
# First option
# cd $dir
# wget -r --no-parent -R "index.html*" https://www.wsl.ch/lud/chelsa/data/bioclim/integer/

# Second option
cd $folder_dir
python -m "$folder_dir/data_chelsa" part3 # to be written if necessary
cd $chelsa_dir
wget -i $"$folder_dir/CHELSA-url.txt" --directory-prefix=bioclim -nc 
