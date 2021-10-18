#load libraries
library(tidyverse)
library(sf)
library(here)
library(mapview)
library(leaflet)
library(lubridate)


# download HUC8 shapefiles from USGS --------------------------------------

#list of HUC 8 ids in LIS watershed
id<-c('01080101','01080102','01080103','01080104','01080105','01080106','01080107',
      '01080201','01080202','01080203','01080204','01080205','01080206','01080207',
      '01100001','01100002','01100003','01100004','01100005','01100006','01100007',
      '01090006','02030102','02030201')


#creating directory to place the zipfiles
#make this a temp folder later 
#i.e. https://hydroecology.net/downloading-extracting-and-reading-files-in-r/
dir.create(here("data", "dl files"))

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
huc8_01090006<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01090006'),'Shape'), 
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
huc8_01100007<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','01100007'),'Shape'), 
  layer='WBDHU8')
huc8_02030102<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','02030102'),'Shape'), 
  layer='WBDHU8')
huc8_02030201<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','02030201'),'Shape'), 
  layer='WBDHU8')



#join polygons
huc8_combined <- dplyr::bind_rows(list(huc8_01080101,huc8_01080102,huc8_01080103,
                                       huc8_01080104,huc8_01080105,huc8_01080106,
                                       huc8_01080107,huc8_01080201,huc8_01080202,
                                       huc8_01080203,huc8_01080204,huc8_01080205,
                                       huc8_01080206,huc8_01080207,huc8_01090006,
                                       huc8_01100001,huc8_01100002,huc8_01100003,
                                       huc8_01100004,huc8_01100005,huc8_01100006,
                                       huc8_01100007,huc8_02030102,huc8_02030201))
mapview(huc8_combined) 

#now transforming all CRS
huc8_combined<-st_transform(huc8_combined, crs=4326)

# read in our data convert to sf object -----------------------------------

#read data
dat<-read_csv( file = here('data', 'ECHO_data_clean_with_locations.csv'))


# #calculating monthly TN totals by watershed
# dat_summary<- dat %>%
#   group_by(huc8,date) %>%
#   summarise(kgN_huc8=sum(kg_N_TN_per_month,na.rm=T))

# #join huc spatial data to monthly totals
# N_load_huc_join<-left_join(dat_summary,huc8_combined)
# N_load_huc_join<-st_as_sf(N_load_huc_join)
# names(N_load_huc_join)

#we only need annual data for the spatial join, so modifying the workflow here
dat_annual_summary<- dat %>%
  group_by(huc8, year=year(date)) %>%
  summarise(kgN_huc8_yr=sum(kg_N_TN_per_month,na.rm=T)) 

#join huc spatial data to monthly totals
N_load_huc_join<-left_join(dat_annual_summary,huc8_combined)
N_load_huc_join<-st_as_sf(N_load_huc_join)
names(N_load_huc_join)

#writing out huc_join for spatial app
dir.create(here('data', 'huc_8_dat_join'))

st_write(N_load_huc_join,
         here('data','huc_8_dat_join','N_load_huc_join.shp'))

#for plotting outfall points; convert to sf obj

#remove NAs from lat/ long
dat_noNA<- dat %>% drop_na(c('LONGITUDE_MEASURE','LATITUDE_MEASURE'))

dat_sf <- st_as_sf(dat_noNA, coords = c('LONGITUDE_MEASURE', 'LATITUDE_MEASURE'), 
                   crs = 4326)

dat_sf

#viewing data extents on map
mapview(dat_sf)

# map ---------------------------------------------------------------------
centroids <- dat_sf%>%
  group_by(name) %>%
  slice(1) %>%
  st_centroid() 

#color palette
#bins <- unname(quantile(dat_sf$kgN_huc8 , c(.2,.6,.8,1)))
bins <- c(0,2,3,6,7000,8000)
pal <- colorBin("Blues", domain = dat_sf$kgN_huc8 , bins = bins)
#in future make this colorQuantiles with more categories

labels <- sprintf(
  "<strong>%s</strong><br/>%g kg total N / month<sup>-1</sup>",
  dat_sf$name, dat_sf$kgN_huc8 
) %>% lapply(htmltools::HTML)

#leaflet map
m <- leaflet(dat_sf) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons()   %>%
  setView(lat = 43,
          lng = -73.2 ,
          zoom = 6.5) %>%
  addPolygons(
    fillColor = ~ pal(dat_sf$kgN_huc8),
    weight = 2,
    color = 'black',
    opacity = 1,
    fillOpacity = 0.7,
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addCircleMarkers(
    data = dat_sf$geometry,
    col = 'darkgrey',
    fillOpacity = .8,
    radius = .5
  ) %>% addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                  position = "bottomright") %>%
  addLabelOnlyMarkers(data = centroids,
                      lng = ~unlist(map(centroids$geometry,1)), 
                      lat = ~unlist(map(centroids$geometry,2)), label = ~name,
                      labelOptions = labelOptions(noHide = TRUE, 
                                                  direction = 'top', textOnly = TRUE)) 

m