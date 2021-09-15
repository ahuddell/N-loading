#load libraries
library(tidyverse)
library(sf)
library(here)
library(mapview)
library(leaflet)



# download HUC8 shapefiles from USGS --------------------------------------


#list of HUC 8 ids in LIS watershed
id<-c('01080101','01080102','01080103','01080104','01080105','01080106','01080107','01080201',
      '01080202','01080203','01080204','01080205','01080206','01080207','01100001',
      '01100002','01100003','01100004','01100005','01100006','02030102','02030201')

#NOTE: I removed HUC '01090005' because the AWS link is broken

for (hucID in id){
  print(hucID)
  
  url<-paste0('https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_',
              hucID,'_HU8_Shape.zip')
  
  # Specify destination where file should be saved
  dlfile <- here('data','dl files', paste0('HUC8_',hucID,'.zip'))
  download.file(url, dlfile)
}

for (hucID in id){
  print(hucID)
  unzip(zipfile=here('data','dl files', paste0('HUC8_',hucID,'.zip')),
        exdir=here('data','huc8 shapes', paste0('HUC8_',hucID)))
}
#problem with '01090005' link broken

#read in each layer manually
huc8_01080101<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080101'),'Shape'), 
  layer='WBDHU8')
huc8_01080102<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080102'),'Shape'), 
  layer='WBDHU8')
huc8_01080103<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080103'),'Shape'), 
  layer='WBDHU8')
huc8_01080104<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080104'),'Shape'), 
  layer='WBDHU8')
huc8_01080105<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080105'),'Shape'), 
  layer='WBDHU8')
huc8_01080106<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080106'),'Shape'), 
  layer='WBDHU8')
huc8_01080107<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080107'),'Shape'), 
  layer='WBDHU8')
huc8_01080201<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080201'),'Shape'), 
  layer='WBDHU8')
huc8_01080202<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080202'),'Shape'), 
  layer='WBDHU8')
huc8_01080203<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080203'),'Shape'), 
  layer='WBDHU8')
huc8_01080204<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080204'),'Shape'), 
  layer='WBDHU8')
huc8_01080205<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080205'),'Shape'), 
  layer='WBDHU8')
huc8_01080206<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080206'),'Shape'), 
  layer='WBDHU8')
huc8_01080207<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080207'),'Shape'), 
  layer='WBDHU8')
huc8_01080207<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01080207'),'Shape'), 
  layer='WBDHU8')
huc8_01100001<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01100001'),'Shape'), 
  layer='WBDHU8')
huc8_01100002<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01100002'),'Shape'), 
  layer='WBDHU8')
huc8_01100003<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01100003'),'Shape'), 
  layer='WBDHU8')
huc8_01100004<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01100004'),'Shape'), 
  layer='WBDHU8')
huc8_01100005<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01100005'),'Shape'), 
  layer='WBDHU8')
huc8_01100006<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01100006'),'Shape'), 
  layer='WBDHU8')
huc8_02030102<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','02030102'),'Shape'), 
  layer='WBDHU8')
huc8_02030201<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','02030201'),'Shape'), 
  layer='WBDHU8')

#now transforming all CRS
huc8_01080101<-st_transform(huc8_01080101, crs=4326)
huc8_01080102<-st_transform(huc8_01080102, crs=4326)
huc8_01080103<-st_transform(huc8_01080103, crs=4326)
huc8_01080104<-st_transform(huc8_01080104, crs=4326)
huc8_01080105<-st_transform(huc8_01080105, crs=4326)
huc8_01080106<-st_transform(huc8_01080106, crs=4326)
huc8_01080107<-st_transform(huc8_01080107, crs=4326)
huc8_01080201<-st_transform(huc8_01080201, crs=4326)
huc8_01080202<-st_transform(huc8_01080202, crs=4326)
huc8_01080203<-st_transform(huc8_01080203, crs=4326)
huc8_01080204<-st_transform(huc8_01080204, crs=4326)
huc8_01080205<-st_transform(huc8_01080205, crs=4326)
huc8_01080206<-st_transform(huc8_01080206, crs=4326)
huc8_01080207<-st_transform(huc8_01080207, crs=4326)
huc8_01100001<-st_transform(huc8_01100001, crs=4326)
huc8_01100002<-st_transform(huc8_01100002, crs=4326)
huc8_01100003<-st_transform(huc8_01100003, crs=4326)
huc8_01100004<-st_transform(huc8_01100004, crs=4326)
huc8_01100005<-st_transform(huc8_01100005, crs=4326)
huc8_01100006<-st_transform(huc8_01100006, crs=4326)
huc8_02030102<-st_transform(huc8_02030102, crs=4326)
huc8_02030201<-st_transform(huc8_02030201, crs=4326)


# read in our data convert to sf object -----------------------------------

#read data
dat<-read_csv( file = here('data', 'ECHO_data_clean.csv'))

#remove NAs from lat/ long
dat_noNA<- dat %>% drop_na(c('LONGITUDE_MEASURE','LATITUDE_MEASURE'))

dat_summary<- dat %>%
  group_by(key,)

#convert to sf obj
dat_sf <- st_as_sf(dat_noNA, coords = c('LONGITUDE_MEASURE', 'LATITUDE_MEASURE'), 
                   crs = 4326)
dat_sf


# map ---------------------------------------------------------------------

#viewing data extents on map
mapview(dat_sf)



## intersect polygons with points, keeping the information from both
dat_join_sf = st_intersection(dat_sf, HUC8)

# Create a color palette for the map:
get_viridis_colors <- function(no_colors){
  appsilon_col2Hex(viridis::viridis_pal(option = 'C')(no_colors))
}

#names of polygons to plot
to_plot<-paste0('huc8_', c(id))

#leaflet map
leaflet() %>% 
  #addTiles() %>%
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data = huc8_01080101$geometry) %>%
  addPolygons(data = huc8_01080102$geometry) %>%
  addPolygons(data = huc8_01080103$geometry) %>%
  addPolygons(data = huc8_01080104$geometry) %>%
  addPolygons(data = huc8_01080105$geometry) %>%
  addPolygons(data = huc8_01080106$geometry) %>%
  addPolygons(data = huc8_01080107$geometry) %>%
  addPolygons(data = huc8_01080201$geometry) %>%
  addPolygons(data = huc8_01080202$geometry) %>%
  addPolygons(data = huc8_01080203$geometry) %>%
  addPolygons(data = huc8_01080204$geometry) %>%
  addPolygons(data = huc8_01080205$geometry) %>%
  addPolygons(data = huc8_01080206$geometry) %>%
  addPolygons(data = huc8_01080207$geometry) %>%
  addPolygons(data = huc8_01100001$geometry) %>%
  addPolygons(data = huc8_01100002$geometry) %>%
  addPolygons(data = huc8_01100003$geometry) %>%
  addPolygons(data = huc8_01100004$geometry) %>%
  addPolygons(data = huc8_01100005$geometry) %>%
  addPolygons(data = huc8_01100006$geometry) %>%
  addPolygons(data = huc8_02030102$geometry) %>%
  addPolygons(data = huc8_02030201$geometry) %>%
  addCircleMarkers(data=dat_sf$geometry, col='red',
                   fillOpacity = .2, radius = .5) %>%
  setView( lat=43, lng=-73.2 , zoom=7) 

#


addPolygons( fillColor = ~mypalette(kg_N_TN_per_month), stroke=FALSE )%>%
  addLegend(position = 'bottomright',
            colors = get_viridis_colors(2),
            labels = types)






