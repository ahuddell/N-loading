library(tidyverse)
library(lubridate)
library(here)
library(lme4)
library(lmerTest)

#load both datasets
PCS_all<-read_csv(file=here("data","PCS_data_clean.csv"))
ECHO_all<-read_csv(file = here("data", "ECHO_data_clean.csv"))

PCS_all$month_year<-ym(PCS_all$month_year)
ECHO_all$month_year<-ym(ECHO_all$month_year)



#organizing keys
PCS_key<-list(unique(PCS_all$key))
ECHO_key<-tibble(key=as.character(unique(ECHO_all$key)))
PCS_key
ECHO_key

#filtering for keys in both datasets
dup_keys<-ECHO_key %>% 
  filter(key %in% unlist(PCS_key))
dup_keys<-list(dup_keys)
dup_keys

# combine PCS and ECHO data-------------------------------------------------------------------------

#looking at outliers
ECHO_all%>%filter(kg_N_TN_per_month>1000000) %>% group_by(key) %>%
  summarize(key=first(key))

#cleaning up values that are obvious typos
#CT0100323_1_1999-08-31 clearly has a typo in the N concentration--impute the mean of that year maybe?
#CT0100447_1_2005-02-28 seems to have entered the flow rate 1000X higher than it should; for parameter 50050 - Flow, in conduit or thru treatment plant
#NY0026204_1_2015-08-31 seems to have entered the load 10X higher that it should


ECHO_all$kg_N_TN_per_month<-ifelse(
  ECHO_all$key == 'NY0026204_1_2015-08-31', ECHO_all$kg_N_TN_per_month/10, ECHO_all$kg_N_TN_per_month) #load looked like it was 10X too large

ECHO_all$kg_N_TN_per_month<-ifelse(
  ECHO_all$key == 'NY0026204_1_2015-08-31', ECHO_all$kg_N_TN_per_month/10, ECHO_all$kg_N_TN_per_month) #load looked like it was 10X too large


  
  
#editing outliers 
PCS_all$kg_N_TN_per_month<-ifelse(
  PCS_all$key == 'MA0110264_2_2003-11-30',
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large

PCS_all$kg_N_TN_per_month<-ifelse(
  PCS_all$key == 'NH0001180_1_1991-09-30',
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large

PCS_all$kg_N_TN_per_month<-ifelse(
  PCS_all$key == 'NH0001180_1_1992-04-30',
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large

PCS_all$kg_N_TN_per_month<-ifelse(
  PCS_all$key == 'NH0001180_1_1999-11-30',
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large

PCS_all$kg_N_TN_per_month<-ifelse(
  PCS_all$key == 'NH0001180_1_1991-07-31',
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large

PCS_all$kg_N_TN_per_month<-ifelse(
  PCS_all$key == 'NH0001180_1_1991-08-31',
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large

PCS_all$kg_N_TN_per_month<-ifelse(
  PCS_all$key == 'NH0001180_1_1991-12-31',
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large

PCS_all$kg_N_TN_per_month<-ifelse(
  PCS_all$key == 'NH0001180_1_1991-11-30',
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large

PCS_all$kg_N_TN_per_month<-ifelse(
  PCS_all$key == 'NH0001180_1_1991-10-31',
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large

PCS_all$kg_N_TN_per_month<-ifelse(
  PCS_all$key == 'NH0001180_1_1992-03-31',
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large

PCS_all$kg_N_TN_per_month<-ifelse(
  PCS_all$key == 'NH0001180_1_1991-06-30',
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large

PCS_all$kg_N_TN_per_month<-ifelse(
  PCS_all$key == 'NH0001180_1' &  PCS_all$kg_N_TN_per_month>500,
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large for multiple observations



#points to clean up for ECHO CT data
#not needed currently since we are using CTDEEP data instead

# impute_value<-as.numeric(ECHO_all %>% filter(permit_outfall=='CT0100323_1' & date>'1999-06-30'
#                                              & date <'1999-10-31') %>% #grabbing months before and after August
#                            filter(!date=='1999-08-31') %>% #removing problematic date
#                            summarize(mean(kg_N_TN_per_month)))
# ECHO_all$kg_N_TN_per_month<-ifelse(
#   ECHO_all$key == 'CT0100323_1_1999-08-31', impute_value, ECHO_all$kg_N_TN_per_month)
# ECHO_all$kg_N_TN_per_month<-ifelse(
#       ECHO_all$key == 'CT0100323_1_1999-08-31', impute_value, ECHO_all$kg_N_TN_per_month)
# ECHO_all$kg_N_TN_per_month<-ifelse(
#   ECHO_all$key == 'CT0100447_1_2005-02-28', ECHO_all$kg_N_TN_per_month/1000, ECHO_all$kg_N_TN_per_month) #flow looked like it was 1000X too large
# ECHO_all$kg_N_TN_per_month<-ifelse(
#   ECHO_all$key == 'CT0100447_1_2005-02-28', ECHO_all$kg_N_TN_per_month/10, ECHO_all$kg_N_TN_per_month)
# ECHO_all$kg_N_TN_per_month<-ifelse(
#   ECHO_all$key == 'CT0100447_1_2005-02-28', ECHO_all$kg_N_TN_per_month/1000, ECHO_all$kg_N_TN_per_month)
# ECHO_all$kg_N_TN_per_month<-ifelse(
#   ECHO_all$key == 'CT0100447_1_2005-02-28', ECHO_all$kg_N_TN_per_month/1000, ECHO_all$kg_N_TN_per_month) #flow looked like it was 1000X too large
# ECHO_all$kg_N_TN_per_month<-ifelse(
#   ECHO_all$key == 'CT0100455_1_2018-04-30', ECHO_all$kg_N_TN_per_month/1000, ECHO_all$kg_N_TN_per_month) #flow looked like it was 1000X too large


# 
# #clean up a few problematic observations
# 
# ECHO_CT_ouliers<- ECHO_all %>%
#   filter(state=='CT') %>%
#   filter(TN_kg_d >3000000)
# 
#          TN_lbs_d/2.205, TN_kg_d) #this converts the lbs/day amount for a few outliers in flow and concentration that are clearly wrong
# 


PCS_join<-PCS_all %>% filter(!key %in% dup_keys) #remove PCS data that also exists in ECHO
names(PCS_join)
names(ECHO_all)
PCS_join<-select(PCS_join,-quant_avg,-conc_avg,-permit_outfall_designator,-param,-days_per_month)


dat_joined<-rbind(PCS_join, ECHO_all)


# filtering out small sources of N ----------------------------------------

##calculating median monthly loads through all time and selecting 15 top
median_loads<-dat_joined %>%
  group_by(permit_outfall) %>%
  summarise(median_kg_N_TN_mo=median(kg_N_TN_per_month, na.rm = T)) %>%
  arrange (desc(median_kg_N_TN_mo)) %>%
  slice_head(n=15) 
median_loads

median_annual<-dat_joined %>%
  filter(!permit_outfall=='NH0001180_1'& kg_N_TN_per_month>1.2e+01) %>%# filtering out problematic permits
 # filter(!permit_outfall=='MA0110264_2'& kg_N_TN_per_month>3e+07) %>% #filtering out problematic permits
  group_by(permit_outfall, year=year(month_year)) %>%
  summarise(kg_N_TN_yr=sum(kg_N_TN_per_month, na.rm = T)) %>%
  group_by(permit_outfall)%>%
  summarise(median_annual=median(kg_N_TN_yr, na.rm = T)) %>%
  arrange (desc(median_annual)) %>%
  slice_head(n=15) 
median_annual

median_loads$permit_outfall %in% median_annual$permit_outfall
#the same 15 outfalls are listed as top polluters by both measures of median monthly and median annual loads


# join data and clean up facility names -----------------------------------

top_15_permit_outfalls<-(median_annual$permit_outfall)

dat<-filter(dat_joined, permit_outfall %in% top_15_permit_outfalls)

unique(dat$facility)

#cleaning up conflicting names
dat<-dat %>% 
  mutate(facility=
        case_when(
          facility=='RED HOOK WATER POLLUTION CONTR' ~ "NYCDEP - RED HOOK WPCP",
          facility=="HUNT'S POINT WPC" ~ "NYCDEP - HUNT'S POINT WPCP",
          facility=="TALLMANISLAND WPC" ~"NYCDEP - TALLMAN ISLAND WPCP",
          facility=="NEWTOWNCREEK WPC" ~ "NYCDEP - NEWTOWN CREEK WPCP",
          facility=="RED HOOK WATER POLLUTION CONTR" ~"NYCDEP - RED HOOK WPCP",
          facility=="WARD ISLAND WPC" ~"NYCDEP - WARD'S ISLAND WPCP",
          TRUE ~ facility)
        )


unique(dat$facility) #now there are only 15

#filtering out more points that are too far from Western Basin
facilities_remove<-c("NEW HAVEN EAST WPCF","MATTABASSET WPCF","DANBURY WPCF",
                     "BRISTOL WPCF","WATERBURY WPCF" ,"HARTFORD WPCF",
                     "WATERBURY WPCF" )


dat<-dat %>% filter(!facility %in% facilities_remove)

unique(dat$facility) #now there are only 9


write_csv(dat,
          file = here("data", 'combined_top9_WLIS_clean_dat.csv'))


# map median monthly loads all data ------------------------------------------

library(sf)
library(leaflet)
library(mapview)
library(ma)

huc8<-st_read(here('data','huc_8_dat_join','huc8_combined.shp'))



median_loads_all<-dat_joined %>%
  group_by(permit_outfall,LATITUDE83,LONGITUDE83,name) %>%
  summarise(median_kg_N_TN_mo=median(kg_N_TN_per_month, na.rm = T)) 
median_loads_all

dat_sf=st_as_sf(median_loads_all, 
              coords=c("LONGITUDE83","LATITUDE83"),
              crs=st_crs(huc8))


huc8_names <- huc8%>%
  group_by(name) %>%
  slice(1) %>%
  st_centroid() 

#leaflet map
bigm <- leaflet(huc8) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons( fillColor = ~ "lightgrey",
               weight = 1,
               color = 'black',
               opacity = 1,
               fillOpacity = .7)  %>%
  addCircleMarkers(
    data = dat_sf$geometry,
    col = 'red',
    fillOpacity = .8,
    radius = dat_sf$median_kg_N_TN_mo/10^4.5) %>%
  # addLabelOnlyMarkers(data = huc8_names,
  #                     lng = ~unlist(map(huc8_names$geometry,1)), 
  #                     lat = ~unlist(map(huc8_names$geometry,2)), label = ~name,
  #                     labelOptions = labelOptions(noHide = TRUE, 
  #                                                 direction = 'top', textOnly = TRUE)) %>%
   setView(lat = 42.5,
          lng = -72.7,
          zoom = 7) 

bigm

mapshot(bigm, file = here("maps", "median_monthly_load_big_map.png"))


# map top 15 only ---------------------------------------------------------

median_loads_top_9<-dat %>%
  group_by(permit_outfall,LATITUDE83,LONGITUDE83,name) %>%
  summarise(median_kg_N_TN_mo=median(kg_N_TN_per_month, na.rm = T)) 
median_loads_top_9

dat_sf2=st_as_sf(median_loads_top_9, 
                coords=c("LONGITUDE83","LATITUDE83"),
                crs=st_crs(huc8))

#zoomed in map near western LIS
#leaflet map
top9m <- leaflet(huc8) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons( fillColor = ~ "lightgrey",
               weight = 1,
               color = 'black',
               opacity = 1,
               fillOpacity = .7)  %>%
  addCircleMarkers(
    data = dat_sf2$geometry,
    col = 'red',
    fillOpacity = .8,
    radius = dat_sf2$median_kg_N_TN_mo/10^4.5) %>%
  # addLabelOnlyMarkers(data = huc8_names,
  #                     lng = ~unlist(map(huc8_names$geometry,1)), 
  #                     lat = ~unlist(map(huc8_names$geometry,2)), label = ~name,
  #                     labelOptions = labelOptions(noHide = TRUE, 
  #                                                 direction = 'top', textOnly = TRUE)) %>%
  setView(lat = 40.8,
          lng = -73.6 ,
          zoom = 10) 

top9m

mapshot(top9m, file = here("maps", "median_monthly_load_top9.png"))

# selecting final list ----------------------------------------------------

# map top 9 only ---------------------------------------------------------

median_loads_top_15<-dat %>%
  group_by(permit_outfall,LATITUDE83,LONGITUDE83,name) %>%
  summarise(median_kg_N_TN_mo=median(kg_N_TN_per_month, na.rm = T)) 
median_loads_top_15

dat_sf=st_as_sf(median_loads_top_15, 
                coords=c("LONGITUDE83","LATITUDE83"),
                crs=st_crs(huc8))

#zoomed in map near western LIS
#leaflet map
top15m <- leaflet(huc8) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons( fillColor = ~ "lightgrey",
               weight = 1,
               color = 'black',
               opacity = 1,
               fillOpacity = .7)  %>%
  addCircleMarkers(
    data = dat_sf$geometry,
    col = 'red',
    fillOpacity = .8,
    radius = dat_sf$median_kg_N_TN_mo/10^4.5) %>%
  # addLabelOnlyMarkers(data = huc8_names,
  #                     lng = ~unlist(map(huc8_names$geometry,1)), 
  #                     lat = ~unlist(map(huc8_names$geometry,2)), label = ~name,
  #                     labelOptions = labelOptions(noHide = TRUE, 
  #                                                 direction = 'top', textOnly = TRUE)) %>%
  setView(lat = 41.5,
          lng = -73 ,
          zoom = 8) 

top15m

mapshot(top15m, file = here("maps", "median_monthly_load_top15.png"))
