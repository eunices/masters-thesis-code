 #!/bin/bash
base_dir=${PWD%/*}
geo_data_dir="$base_dir/data/geo"
dir="$geo_data_dir/gadm"
folder_dir=$(pwd)


echo "Scraping urls of specified file type"
python -m data_GADM


echo "Downloading files"
mkdir $dir
wget -i countries_GADM.txt --directory-prefix=$dir -nc 


echo "Unzipping files"
cd $dir
unzip -o "*.zip"
rm *.zip

cd $dir
shp_pri_dir="shp_pri"
mkdir $shp_pri_dir
mv *_0.* "$shp_pri_dir/"
cd $shp_pri_dir && rename -f 's/_ABW_0/\_0/' *_ABW_0* && cd ".."

shp_sec_dir="shp_sec"
mkdir $shp_sec_dir
mv *_1.* "$shp_sec_dir/"
cd $shp_sec_dir && rename -f 's/_AFG_1/\_1/' *_AFG_1* && cd ".."


echo "Running script to merge shp"
cd $folder_dir
python -m process_GADM