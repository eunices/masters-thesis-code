# Species Distribution Modelling spatial covariates

## Downloaded sources

30 arc seconds ~ 1km

[name of layer] - [type of data, resolution and data format if applicable] - date downloaded

1. GADM country boundaries - vector (.shp and .gpkg) - 27 May 2019
2. Chelsa bioclim - raster 30 arc sec (.geoTIFF) - 28 May 2019
3. GMTED2010 - raster 7.5 arc second (.adf) - 28 May 2019
4. Beck's Koppen - raster 1km resolution (.geoTIFF) - 29 May 2019

## To download

1. USGS HydroSheds https://hydrosheds.cr.usgs.gov/datadownload.php?reqdata=15rivs
2. USGS LandCover https://archive.usgs.gov/archive/sites/landcover.usgs.gov/globallandcover.html
3. NASA Anthropogenic bioms https://sedac.ciesin.columbia.edu/data/collection/anthromes/sets/browse
4. Soils https://www.isric.org/explore/soil-geographic-databases / http://www.fao.org/geonetwork/srv/en/main.home?uuid=446ed430-8383-11db-b9b2-000d939bc5d8 / http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HTML/SoilQuality.html?sb=10

Explore http://www.gisresources.com/free-gis-data-ecology/

## Potential sources

### Climatic variables

- WorldClim (Zeng et al. 2016)
  - [bio1-19](https://pubs.usgs.gov/ds/691/ds691.pdf)
- IPCC (Zeng et al. 2016)
  - tmp, dtr, frs, pre, cld, tmn, tmx, vap, wet
- [Chelsa](http://chelsa-climate.org/)
- [Koppen](http://www.gloh2o.org/koppen/)

Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., Zimmermann, N.E., Linder, H.P. & Kessler, M. (2017) Climatologies at high resolution for the earth’s land surface areas. Scientific Data 4, 170122.

Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., Zimmermann, N.E., Linder, H.P., Kessler, M. (2017) Data from: Climatologies at high resolution for the earth’s land surface areas. Dryad Digital Repository. https://doi.org/10.5061/dryad.kd1d4 

### Elevation

- Digital Elevation Model GMTED 2010 (Zeng et al. 2016)
- USGS HydroSHEDS (Zeng et al. 2016)
  - cond_dem, flow_accum, slope, aspect

### Vegetation and other fauna

- USGS landcover (Zeng et al. 2016)
  - avg-MGVF
- [Plant diversity](https://www.nees.uni-bonn.de/research-/systematics-evolution-ecology/biogeography-and-macroecology-biomaps/worldmaps/worldmaps-of-plant-diversity)
- [Other diversity](http://guides.lib.berkeley.edu/VegMaps)

## Resolution

- Zeng et al. (2016): "Environmental variable layers were at a resolution of 2.5 arc-minutes and had a global extent (before limiting via buffer-ing) of −55–60◦N and −135–175◦E."

## Interesting tutorials

- [Using QGIS with python](https://www.e-education.psu.edu/geog489/print/root1405.html)