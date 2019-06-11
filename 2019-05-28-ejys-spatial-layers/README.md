# Species Distribution Modelling spatial covariates

See `data/lookup/2019-06-01-spatial-layers-metadata.csv`


## Tools

1. pyModis http://pymodis.org/ 
2. MODIStsp https://cran.r-project.org/web/packages/MODIStsp/index.html
3. Data exploration https://lpdaac.usgs.gov/resources/e-learning/masking-visualizing-and-plotting-appeears-output-geotiff-time-series-python/


## To download

1. Soils https://www.isric.org/explore/soil-geographic-databases / http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HTML/SoilQuality.html?sb=10
2. https://www.nature.com/articles/sdata201669 Tree density
3. https://www.nature.com/articles/sdata20184 GDP
4. https://www.climond.org/ClimateData.aspx Humidity

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