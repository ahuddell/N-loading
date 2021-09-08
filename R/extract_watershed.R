#load libraries
library(tidyverse)
library(sf)
library(nhdplusTools)
library(here)

#read data
dat<-read_csv( file = here("data", "ECHO_data_clean.csv"))

st_multipoint(dat,
              multipoint_id = "permit_outfall",
              x = 
                y = "")


points <- sf::st_as_sf(data.frame(x =dat$LONGITUDE_MEASURE, 
                                  y = dat$LATITUDE_MEASURE), 
                       coords = c("x", "y"), crs = 4326)

