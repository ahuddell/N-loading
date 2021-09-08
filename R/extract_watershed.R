#load libraries
library(tidyverse)
library(sf)
library(nhdplusTools)
library(here)

#read data
dat<-read_csv( file = here('data', 'ECHO_data_clean.csv'))

#remove NAs from lat/ long
dat_noNA<- dat %>% drop_na(c('LONGITUDE_MEASURE','LATITUDE_MEASURE'))

#convert to sf obj
dat_sf <- st_as_sf(dat_noNA, coords = c('LONGITUDE_MEASURE', 'LATITUDE_MEASURE'), 
                   crs = 4326)
dat_sf


#extract huc12 codes 
nhdplusTools::get_huc12(AOI=dat_sf)
