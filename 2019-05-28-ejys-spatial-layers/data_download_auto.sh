#!/bin/bash
# This script is automatically generated from {lookup} and data_downloads.py

folder_dir=$(pwd)
base_dir=${PWD%/*}
geo_data_dir="$base_dir/data/geo"

dir_usgs_gmted2010=$geo_data_dir/usgs_gmted2010
dir_beck_koppen=$geo_data_dir/beck_koppen
dir_teow=$geo_data_dir/teow
dir_global_water1=$geo_data_dir/global_water/1
dir_global_water2=$geo_data_dir/global_water/2
dir_global_water3=$geo_data_dir/global_water/3
dir_et0=$geo_data_dir/et0
dir_intact_forest=$geo_data_dir/intact_forest


echo "Activate conda environment"
conda activate msc

echo "Making all dirs"
mkdir $dir_usgs_gmted2010 -p
mkdir $dir_beck_koppen -p
mkdir $dir_teow -p
mkdir $dir_global_water1 -p
mkdir $dir_global_water2 -p
mkdir $dir_global_water3 -p
mkdir $dir_et0 -p
mkdir $dir_intact_forest -p


echo "Download 'Global Multi-resolution Terrain Elevation Data 2010 (GMTED2010)' to $dir_usgs_gmted2010..."
cd $dir_usgs_gmted2010
wget http://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/mn30_grd.zip -nc
unzip *.zip

echo "Download 'Present and future Kppen-Geiger climate classification maps at 1-km resolution' to $dir_beck_koppen..."
cd $dir_beck_koppen
wget https://ndownloader.figshare.com/files/12407516 -nc
mv $file $file.zip
unzip *.zip

echo "Download 'Terrestrial Ecoregions of the World (TEOW)' to $dir_teow..."
cd $dir_teow
wget https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip -nc
unzip *.zip

echo "Download 'Global Lakes and Wetlands Database: Large Lake Polygons (Level 1)' to $dir_global_water1..."
cd $dir_global_water1
wget https://c402277.ssl.cf1.rackcdn.com/publications/16/files/original/GLWD-level1.zip -nc
unzip *.zip

echo "Download 'Global Lakes and Wetlands Database: Small Lake Polygons (Level 2)' to $dir_global_water2..."
cd $dir_global_water2
wget https://c402277.ssl.cf1.rackcdn.com/publications/17/files/original/GLWD-level2.zip -nc
unzip *.zip

echo "Download 'Global Lakes and Wetlands Database: Lakes and Wetlands Grid (Level 3)' to $dir_global_water3..."
cd $dir_global_water3
wget https://c402277.ssl.cf1.rackcdn.com/publications/18/files/original/GLWD-level3.zip -nc
unzip *.zip

echo "Download 'Global Aridity Index and Potential Evapotranspiration Climate Database v2' to $dir_et0..."
cd $dir_et0
wget https://ndownloader.figshare.com/articles/7504448/versions/3 -nc
mv $file $file.zip
unzip *.zip

echo "Download 'Intact Forest Landscapes (2016)' to $dir_intact_forest..."
cd $dir_intact_forest
wget https://opendata.arcgis.com/datasets/fe75a027854245c183a9aa3f339d74f4_19.zip -nc
unzip *.zip

