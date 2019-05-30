base_dir=${PWD%/*}
geo_data_dir="$base_dir/data/geo"
bioclim_dir="$geo_data_dir/chelsa/bioclim"
files="$dir/*.tif"
asc_subfolder="asc"

# Convert from geotiff to asc
cd $bioclim_dir
for f in $files
do 
    echo "Processing $f file."
    filename=$(basename $f)
    echo $filename
    gdal_translate -of GTiff $filename $filename.asc; 
done

# Move files to subfolder
mkdir $asc_subfolder
mv -f *.asc "$asc_subfolder"