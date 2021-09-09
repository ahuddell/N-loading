#load libraries
library(tidyverse)
library(sf)
library(nhdplusTools)
library(here)
library(mapview)
library(leaflet)

#read data
dat<-read_csv( file = here('data', 'ECHO_data_clean.csv'))
write_rds(dat, paste0(here(), "/test.rds"))
#remove NAs from lat/ long
dat_noNA<- dat %>% drop_na(c('LONGITUDE_MEASURE','LATITUDE_MEASURE'))

#convert to sf obj
dat_sf <- st_as_sf(dat_noNA, coords = c('LONGITUDE_MEASURE', 'LATITUDE_MEASURE'), 
                   crs = 4326)
dat_sf


#extract huc8 codes 
HUC8<-nhdplusTools::get_huc12(AOI=dat_sf$geometry)

#viewing data extents on map
mapview(HUC8)
mapview(dat_sf)


## intersect polygons with points, keeping the information from both
dat_join_sf = st_intersection(dat_sf, HUC8)


#leaflet map
leaflet() %>% 
  #addTiles() %>%
  addProviderTiles(providers$Esri.WorldTerrain) %>% 
  addPolygons(data = HUC8$geometry) %>%
  addCircleMarkers(data=dat_join_sf$geometry, col="red",
                   fillOpacity = .2, radius = .5)

## transform into a 'data.frame' by removing the geometry
st_geometry(dat_join) = NULL
head(dat_join)

