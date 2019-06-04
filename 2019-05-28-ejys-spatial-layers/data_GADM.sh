 #!/bin/bash
base_dir=${PWD%/*}
geo_data_dir="$base_dir/data/geo"
dir="$geo_data_dir/1_separate/gadm"
folder_dir=$(pwd)

conda activate msc

echo "Scraping urls of specified file type"
python -m data_GADM


echo "Downloading files"
mkdir $dir
wget -i "$folder_dir/GADM-countries.txt" --directory-prefix=$dir -nc 


echo "Unzipping files"
cd $dir
unzip -o "*.zip"
rm *.zip

echo "Reorganizing files"

cd $dir # creating folder for all primary divisions (countries)
shp_pri_dir="shp_pri"
mkdir $shp_pri_dir
mv *_0.* "$shp_pri_dir/"
cd $shp_pri_dir && rename -f 's/_ABW_0/\_0/' *_ABW_0* # copying file to be merged
cd ".."

shp_sec_dir="shp_sec" # creating folder for all sec divisions
mkdir $shp_sec_dir
mv *_1.* "$shp_sec_dir/"
cd $shp_sec_dir && rename -f 's/_AFG_1/\_1/' *_AFG_1* # copying file to be merged
cd ".."

# echo "Running script to merge shp_pri" # can be extended for shp_sec later
cd $folder_dir
# creating IND_0 as it does not exist 
# R part cannot run in WSL as R cannot be installed/ conda R does not have all packages required
Rscript process_GADM.r 
python -m process_GADM