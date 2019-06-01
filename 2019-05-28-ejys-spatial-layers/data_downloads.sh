 #!/bin/bash
folder_dir=$(pwd)
base_dir=${PWD%/*}
geo_data_dir="$base_dir/data/geo"
gmted_dir="$geo_data_dir/gmted2010"
beck_dir="$geo_data_dir/beck_koppen"
teow_dir="$geo_data_dir/teow"
gwater_dir="$geo_data_dir/global_water"
gwater1_dir="$gwater_dir/1"
gwater2_dir="$gwater_dir/2"
gwater3_dir="$gwater_dir/3"

echo "Activate conda environment"
conda activate msc


echo "Making all dirs"
mkdir $geo_data_dir
mkdir $gmted_dir
mkdir $beck_dir
mkdir $teow_dir
mkdir $gwater_dir
mkdir $gwater1_dir
mkdir $gwater2_dir
mkdir $gwater3_dir


echo "Download GMTED2010 data to $gmted_dir"
cd $gmted_dir
url=http://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/mn30_grd.zip
wget $url -nc
unzip *.zip


echo "Download Beck's Koppen data to $beck_dir"
cd $beck_dir
url="https://ndownloader.figshare.com/files/12407516"
wget $url -nc
mv $file $file.zip
unzip *.zip

echo "Download WWF's TEOW data to $teow_dir"
cd $teow_dir
url="https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip"
wget $url -nc
unzip *.zip

echo "Download WWF's global water data to $teow_dir"
cd $gwater1_dir
url="https://c402277.ssl.cf1.rackcdn.com/publications/16/files/original/GLWD-level1.zip"
wget $url -nc
unzip *.zip
cd $gwater2_dir
url="https://c402277.ssl.cf1.rackcdn.com/publications/17/files/original/GLWD-level2.zip"
wget $url -nc
unzip *.zip
cd $gwater3_dir
url="https://c402277.ssl.cf1.rackcdn.com/publications/18/files/original/GLWD-level3.zip"
wget $url -nc
unzip *.zip