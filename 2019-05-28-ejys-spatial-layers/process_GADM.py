# https://automating-gis-processes.github.io/
# https://gis.stackexchange.com/questions/183606/
import fiona
import glob
import shapefile
import geopandas as gpd 

fdir = '../data/geo/01_separate/gadm/shp_pri'
files = glob.glob(f'{fdir}/*.shp')
file = '../data/geo/01_separate/gadm/shp_pri/gadm36_0.shp' # only primary

if __name__ == "__main__":
    # Read first file
    data = gpd.read_file(file, layer=fiona.listlayers(file)[0])
    cols = data.columns
    meta = fiona.open(file).meta

    with fiona.open(file, 'w', **meta) as output:
        for i, file in enumerate(files[1:]):
            for features in fiona.open(file):
                print(f'Appending from {file}')
                output.write(features)


