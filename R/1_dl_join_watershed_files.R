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
      '01090006','02030102','02030201','02030202','02030203')

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
# huc8_01100007<-st_read(
#   here('data','huc8 shapes',paste0('HUC8_','01100007'),'Shape'), 
#   layer='WBDHU8') #the shape file for Long Island Sound no longer exists on the USGS website
huc8_02030102<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','02030102'),'Shape'), 
  layer='WBDHU8')
huc8_02030201<-st_read(
  here('data','huc8 shapes',paste0('HUC8_','02030201'),'Shape'), 
  layer='WBDHU8')
# huc8_02030202<-st_read(
#   here('data','huc8 shapes',paste0('HUC8_','02030202'),'Shape'), 
#   layer='WBDHU8')# the shape file for Southern Long Island no longer exists on the USGS website
# huc8_02030203<-st_read(
#   here('data','huc8 shapes',paste0('HUC8_','02030203'),'Shape'), 
#   layer='WBDHU8')


#join polygons
huc8_combined <- dplyr::bind_rows(list(huc8_01080101,huc8_01080102,huc8_01080103,
                                       huc8_01080104,huc8_01080105,huc8_01080106,
                                       huc8_01080107,huc8_01080201,huc8_01080202,
                                       huc8_01080203,huc8_01080204,huc8_01080205,
                                       huc8_01080206,huc8_01080207,huc8_01090006,
                                       huc8_01100001,huc8_01100002,huc8_01100003,
                                       huc8_01100004,huc8_01100005,huc8_01100006,
                                       #huc8_01100007,
                                       huc8_02030102,huc8_02030201#,
                                       #huc8_02030202,huc8_02030203
                                       )
                                  )
mapview(huc8_combined) 

#now transforming all CRS
huc8_combined<-st_transform(huc8_combined, crs=4326)

#writing out huc_join for spatial app
dir.create(here('data', 'huc_8_dat_join'))

st_write(huc8_combined,
         here('data','huc_8_dat_join','huc8_combined.shp'))


# outfalls join -----------------------------------------------------------

outfall_locations <-
  read_csv(file = here("data", "npdes_outfalls_layer.csv"))
names(outfall_locations)

outfall_locations<-outfall_locations %>%
  select(EXTERNAL_PERMIT_NMBR,
         FACILITY_NAME,
         SIC_CODES,
         SIC_DESCRIPTIONS,
         PERM_FEATURE_NMBR,
         STATE_WATER_BODY_NAME,
         LATITUDE83,
         LONGITUDE83)

outfall_locations$permit_outfall <-
  paste0(outfall_locations$EXTERNAL_PERMIT_NMBR,
         "_",
         outfall_locations$PERM_FEATURE_NMBR)



#convert outfalls to an sf object
no_na<-drop_na(outfall_locations, LONGITUDE83)
outfall_locations_sf<-st_as_sf(no_na, coords =c('LONGITUDE83','LATITUDE83'))

#set CRS of outfalls to match the huc8 layer
st_crs(outfall_locations_sf) <- st_crs(huc8_combined)


joined = st_join(huc8_combined,outfall_locations_sf)

st_write(joined, here('data','huc8_outfall_join.csv'), layer_options = "GEOMETRY=AS_XY")

