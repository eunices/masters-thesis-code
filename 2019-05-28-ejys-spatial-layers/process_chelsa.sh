base_dir=${PWD%/*}
geo_data_dir="$base_dir/data/geo"
dir="$geo_data_dir/chelsa/bioclim"
files="$dir/*.tif"

cd $dir
for f in $files
do 
    echo "Processing $f file."
    filename=$(basename $f)
    echo $filename
    gdal_translate -of GTiff $filename $filename.asc; 
done
