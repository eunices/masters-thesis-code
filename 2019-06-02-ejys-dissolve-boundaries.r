source('src/util.R')

filepath = 'data/geo/0_manual/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp'
dissolvePolygon(filepath)

filepath = 'data/geo_processed/teow/official_2/wwf_terr_ecos.shp'
dissolvePolygon(filepath, by="REALM_EDIT")