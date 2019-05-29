 #!/bin/bash
dir="../data/geo/gadm"
mkdir $dir
wget -i countries_GADM.txt --directory-prefix=$dir -nc 

echo "Unzipping files"
unzip -o "$dir/*.zip" -d "$dir"
cd $dir && rm *.zip