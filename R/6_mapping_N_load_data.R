#load libraries
library(tidyverse)
library(sf)
library(here)
library(mapview)
library(leaflet)
library(lubridate)

huc8<-st_read(here('data','huc_8_dat_join','huc8_combined.shp'))

pt1 = st_point(c(-73.919139,40.787833))
pt2 = st_point(c(-73.965583,40.731139))
pt3 = st_point(c(-73.984389,40.705722))

st_sfc(pt1, pt2,pt3)
d = data.frame(permit_outfall=c('NY0026131_1','NY0026204_1',
                                'NY0027073_1'))
d$geom = st_sfc(pt1, pt2, pt3)
d

outfalls<-st_as_sf(d,sf_column_name='geom')

#set CRS of outfalls to match the huc8 layer
st_crs(outfalls) <- st_crs(huc8)



# explore outfalls in East River ------------------------------------------
huc8_names <- huc8%>%
  group_by(name) %>%
  slice(1) %>%
  st_centroid() 

#leaflet map
m <- leaflet(huc8) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons( fillColor = ~ "lightgrey",
               weight = 1,
               color = 'black',
               opacity = 1,
               fillOpacity = 098)  %>%
  addCircleMarkers(
    data = outfalls$geom,
    col = 'red',
    fillOpacity = .8,
    radius = 2
  )%>%
  addLabelOnlyMarkers(data = huc8_names,
                      lng = ~unlist(map(huc8_names$geometry,1)), 
                      lat = ~unlist(map(huc8_names$geometry,2)), label = ~name,
                      labelOptions = labelOptions(noHide = TRUE, 
                                direction = 'top', textOnly = TRUE)) %>%
  setView(lat = 40.8,
        lng = -73.9 ,
        zoom = 10) 

m




# read in our data convert to sf object -----------------------------------

#read data
dat<-read_csv( file = here('data', 'ECHO_data_clean.csv'))


#calculating monthly TN totals by watershed
dat_summary<- dat %>%
  group_by(huc8,month_year) %>%
  summarise(kgN_huc8=sum(kg_N_TN_per_month,na.rm=T))

#join huc spatial data to monthly totals
N_load_huc_join<-left_join(dat_summary,huc8_combined)
N_load_huc_join<-st_as_sf(N_load_huc_join)
names(N_load_huc_join)

# #we only need annual data for the spatial join, so modifying the workflow here
# dat_annual_summary<- dat %>%
#   group_by(huc8, year=year(date)) %>%
#   summarise(kgN_h8=sum(kg_N_TN_per_month,na.rm=T)) 
# 
# #join huc spatial data to monthly totals
# N_load_huc_join<-left_join(dat_annual_summary,huc8_combined)
# N_load_huc_join<-st_as_sf(N_load_huc_join)
# names(N_load_huc_join)

#writing out huc_join for spatial app
dir.create(here('data', 'huc_8_dat_join'))

st_write(N_load_huc_join,
         here('data','huc_8_dat_join','N_load_huc_join.shp'))

#for plotting outfall points; convert to sf obj

#remove NAs from lat/ long
dat_noNA<- dat %>% drop_na(c('LONGITUDE83','LATITUDE83'))

dat_sf <- st_as_sf(dat_noNA, coords = c('LONGITUDE83', 'LATITUDE83'), 
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
