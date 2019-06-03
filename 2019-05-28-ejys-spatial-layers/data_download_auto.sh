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
dir_atlas_built=$geo_data_dir/atlas_built
dir_atlas_growingdd=$geo_data_dir/atlas_growingdd
dir_usgs_gelu=$geo_data_dir/usgs_gelu
dir_usgs_landcover=$geo_data_dir/usgs_landcover
dir_usgs_veg=$geo_data_dir/usgs_veg
dir_esa_landcover=$geo_data_dir/esa_landcover
dir_terrslope1=$geo_data_dir/terr/slope/1
dir_terrslope2=$geo_data_dir/terr/slope/2
dir_terrslope3=$geo_data_dir/terr/slope/3
dir_terrslope4=$geo_data_dir/terr/slope/4
dir_terrslope5=$geo_data_dir/terr/slope/5
dir_terrslope6=$geo_data_dir/terr/slope/6
dir_terrslope7=$geo_data_dir/terr/slope/7
dir_terrslope8=$geo_data_dir/terr/slope/8
dir_terraspect1=$geo_data_dir/terr/aspect/1
dir_terraspect2=$geo_data_dir/terr/aspect/2
dir_terraspect3=$geo_data_dir/terr/aspect/3
dir_terraspect4=$geo_data_dir/terr/aspect/4
dir_terraspect5=$geo_data_dir/terr/aspect/5


echo "Activate conda environment"
conda init bash
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
mkdir $dir_atlas_built -p
mkdir $dir_atlas_growingdd -p
mkdir $dir_usgs_gelu -p
mkdir $dir_usgs_landcover -p
mkdir $dir_usgs_veg -p
mkdir $dir_esa_landcover -p
mkdir $dir_terrslope1 -p
mkdir $dir_terrslope2 -p
mkdir $dir_terrslope3 -p
mkdir $dir_terrslope4 -p
mkdir $dir_terrslope5 -p
mkdir $dir_terrslope6 -p
mkdir $dir_terrslope7 -p
mkdir $dir_terrslope8 -p
mkdir $dir_terraspect1 -p
mkdir $dir_terraspect2 -p
mkdir $dir_terraspect3 -p
mkdir $dir_terraspect4 -p
mkdir $dir_terraspect5 -p


echo "Download 'Global Multi-resolution Terrain Elevation Data 2010 (GMTED2010)' to $dir_usgs_gmted2010..."
cd $dir_usgs_gmted2010
wget http://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/mn30_grd.zip -nc
unzip -o *.zip

echo "Download 'Present and future Kppen-Geiger climate classification maps at 1-km resolution' to $dir_beck_koppen..."
cd $dir_beck_koppen
wget https://ndownloader.figshare.com/files/12407516 -nc
for file in *; do cp $file "$file.zip"; done
unzip -o *.zip

echo "Download 'Terrestrial Ecoregions of the World (TEOW)' to $dir_teow..."
cd $dir_teow
wget https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip -nc
unzip -o *.zip

echo "Download 'Global Lakes and Wetlands Database: Large Lake Polygons (Level 1)' to $dir_global_water1..."
cd $dir_global_water1
wget https://c402277.ssl.cf1.rackcdn.com/publications/16/files/original/GLWD-level1.zip -nc
unzip -o *.zip

echo "Download 'Global Lakes and Wetlands Database: Small Lake Polygons (Level 2)' to $dir_global_water2..."
cd $dir_global_water2
wget https://c402277.ssl.cf1.rackcdn.com/publications/17/files/original/GLWD-level2.zip -nc
unzip -o *.zip

echo "Download 'Global Lakes and Wetlands Database: Lakes and Wetlands Grid (Level 3)' to $dir_global_water3..."
cd $dir_global_water3
wget https://c402277.ssl.cf1.rackcdn.com/publications/18/files/original/GLWD-level3.zip -nc
unzip -o *.zip

echo "Download 'Global Aridity Index and Potential Evapotranspiration Climate Database v2' to $dir_et0..."
cd $dir_et0
wget https://ndownloader.figshare.com/articles/7504448/versions/3 -nc
for file in *; do cp $file "$file.zip"; done
unzip -o *.zip

echo "Download 'Intact Forest Landscapes (2016)' to $dir_intact_forest..."
cd $dir_intact_forest
wget https://gfw2-data.s3.amazonaws.com/forest_cover/zip/intact_forest_landscapes_change_2016.zip -nc
unzip -o *.zip

echo "Download 'Atlas of Biosphere: Built-Up Land' to $dir_atlas_built..."
cd $dir_atlas_built
wget https://nelson.wisc.edu/sage/data-and-models/atlas/data/builtupland.zip -nc
unzip -o *.zip

echo "Download 'Atlas of Biosphere: Growing Degree Days' to $dir_atlas_growingdd..."
cd $dir_atlas_growingdd
wget https://nelson.wisc.edu/sage/data-and-models/atlas/data/gdd.zip -nc
unzip -o *.zip

echo "Download 'Global Ecological Land Units (ELUs)' to $dir_usgs_gelu..."
cd $dir_usgs_gelu
wget https://rmgsc.cr.usgs.gov/outgoing/ecosystems/Global/World_Ecological_2015.zip -nc
unzip -o *.zip

echo "Download 'Global Land Cover' to $dir_usgs_landcover..."
cd $dir_usgs_landcover
wget https://archive.usgs.gov/archive/sites/landcover.usgs.gov/documents/GlobalLandCover_tif.zip -nc
unzip -o *.zip

echo "Download 'Maximum Green Vegetation Fraction' to $dir_usgs_veg..."
cd $dir_usgs_veg
wget https://archive.usgs.gov/archive/sites/landcover.usgs.gov/documents/average.tif.zip -nc
unzip -o *.zip

echo "Download 'GlobCover 2009 (Global Land Cover Map)' to $dir_esa_landcover..."
cd $dir_esa_landcover
wget http://due.esrin.esa.int/files/Globcover_V2.2_Global.zip -nc
unzip -o *.zip

echo "Download 'Global Terrain Slope and Aspect Data: slope 1' to $dir_terrslope1..."
cd $dir_terrslope1
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloSlopesCl1_30as.rar -nc
unrar e *.rar

echo "Download 'Global Terrain Slope and Aspect Data: slope 2' to $dir_terrslope2..."
cd $dir_terrslope2
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloSlopesCl2_30as.rar -nc
unrar e *.rar

echo "Download 'Global Terrain Slope and Aspect Data: slope 3' to $dir_terrslope3..."
cd $dir_terrslope3
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloSlopesCl3_30as.rar -nc
unrar e *.rar

echo "Download 'Global Terrain Slope and Aspect Data: slope 4' to $dir_terrslope4..."
cd $dir_terrslope4
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloSlopesCl4_30as.rar -nc
unrar e *.rar

echo "Download 'Global Terrain Slope and Aspect Data: slope 5' to $dir_terrslope5..."
cd $dir_terrslope5
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloSlopesCl5_30as.rar -nc
unrar e *.rar

echo "Download 'Global Terrain Slope and Aspect Data: slope 6' to $dir_terrslope6..."
cd $dir_terrslope6
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloSlopesCl6_30as.rar -nc
unrar e *.rar

echo "Download 'Global Terrain Slope and Aspect Data: slope 7' to $dir_terrslope7..."
cd $dir_terrslope7
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloSlopesCl7_30as.rar -nc
unrar e *.rar

echo "Download 'Global Terrain Slope and Aspect Data: slope 8' to $dir_terrslope8..."
cd $dir_terrslope8
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloSlopesCl8_30as.rar -nc
unrar e *.rar

echo "Download 'Global Terrain Slope and Aspect Data: aspect 1' to $dir_terraspect1..."
cd $dir_terraspect1
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloAspectClN_30as.rar -nc
unrar e *.rar

echo "Download 'Global Terrain Slope and Aspect Data: aspect 2' to $dir_terraspect2..."
cd $dir_terraspect2
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloAspectClE_30as.rar -nc
unrar e *.rar

echo "Download 'Global Terrain Slope and Aspect Data: aspect 3' to $dir_terraspect3..."
cd $dir_terraspect3
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloAspectClN_30as.rar -nc
unrar e *.rar

echo "Download 'Global Terrain Slope and Aspect Data: aspect 4' to $dir_terraspect4..."
cd $dir_terraspect4
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloAspectClE_30as.rar -nc
unrar e *.rar

echo "Download 'Global Terrain Slope and Aspect Data: aspect 5' to $dir_terraspect5..."
cd $dir_terraspect5
wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/Terrain/GloAspectClN_30as.rar -nc
unrar e *.rar

