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

#checking that there are no duplicates among the keys
PCS_all %>% 
    group_by(key) %>% 
    filter(n()>1)

ECHO_all %>% 
  group_by(key) %>% 
  filter(n()>1)

#there are no more duplicates among keys

#organizing keys
PCS_key<-list(unique(PCS_all$key))
ECHO_key<-tibble(key=as.character(unique(ECHO_all$key)))
PCS_key
ECHO_key


#filtering for keys in both datasets
dup_keys<-ECHO_key %>% 
  filter(key %in% unlist(PCS_key))
dup_keys


# combine PCS and ECHO data-------------------------------------------------------------------------


PCS_join<-PCS_all %>% filter(!key %in% dup_keys$key) #remove PCS data that also exists in ECHO

names(PCS_join)
names(ECHO_all)
PCS_join<-select(PCS_join,-quant_avg,-conc_avg,-permit_outfall_designator,-param,-days_per_month)

names(PCS_join)
names(ECHO_all)

nrow(unique(as_tibble(PCS_join$key)))+nrow(unique(as_tibble(ECHO_all$key)))

dat_joined<-rbind(PCS_join, ECHO_all)
nrow(dat_joined)

#checking for duplicates
dat_joined %>%
  group_by(key) %>%
  filter(n()>1)

#filtering out data that have fewer than 4 years worth of data
tally<-dat_joined %>% 
  group_by(permit_outfall) %>%
  tally() %>%
  arrange(n) %>%
  filter(n>48) #filter out outfalls with fewer than 4 years worth of observations

tally

dat_joined<-dat_joined %>% 
  filter(permit_outfall %in% tally$permit_outfall)

outliers <- dat_joined %>%
  group_by(permit_outfall) %>%
  filter(kg_N_TN_per_month < quantile(kg_N_TN_per_month, 0.25, na.rm=T) - 3*IQR(kg_N_TN_per_month, na.rm=T) |
      kg_N_TN_per_month > quantile(kg_N_TN_per_month, 0.75, na.rm=T) + 3*IQR(kg_N_TN_per_month, na.rm=T)
  )
outliers #244 points fall 3X outside of IQR but many are NAs
outliers_keys<-outliers$key[!is.na(outliers$key)]

dat_joined$outlier<-ifelse(dat_joined$key %in% outliers_keys, TRUE, FALSE)
summary(dat_joined$outlier) #now those 244 points are marked as "outlier" TRUE


#add seasons
dat_joined<-dat_joined %>%
  mutate(season= case_when(
    month(month_year)=='12' ~ 'winter',
    month(month_year)=='1' ~ 'winter',
    month(month_year)=='2' ~ 'winter',
    month(month_year)=='3' ~ 'spring',
    month(month_year)=='4' ~ 'spring',
    month(month_year)=='5' ~ 'spring',
    month(month_year)=='6' ~ 'summer',
    month(month_year)=='7' ~ 'summer',
    month(month_year)=='8' ~ 'summer',
    month(month_year)=='9' ~ 'fall',
    month(month_year)=='10' ~ 'fall',
    month(month_year)=='11' ~ 'fall'
  ))


#compute and impute within seasonal means for outlier data within season
season_mean <- dat_joined %>% 
  filter(outlier==FALSE) %>% #exclude outliers from rolling mean calculation
  group_by(permit_outfall, year=year(month_year), season) %>% 
  mutate(season_mean = mean(kg_N_TN_per_month, na.rm=T)) %>% 
  ungroup() %>%
 # select(permit_outfall, season_mean, season, year) %>%
  group_by(permit_outfall, season, year) %>%
  summarise(season_mean=first(season_mean))


#join the within seasonal means to the original data
#removing some columns before join
dat_joined<-dat_joined %>% 
  select(-EXTERNAL_PERMIT_NMBR.y, -PERM_FEATURE_NMBR.y, -EXTERNAL_PERMIT_NMBR.x,
          -FACILITY_NAME, -SIC_CODES,- SIC_DESCRIPTIONS,-PERM_FEATURE_NMBR.x,
         -STATE_WATER_BODY_NAME, -impute_name, -impute_lat, -impute_lon) %>%
  mutate(year=year(month_year))
dat_joined_2<-left_join(dat_joined,season_mean)


#plot rolling mean vs. observed values 
ggplot(dat_joined_2, aes(x=kg_N_TN_per_month, y=season_mean))+
  geom_point()+
  geom_abline(slope=1)+
  xlim(0,750000)+
  ylim(0,750000) +
  annotate('text', x=1000,y=700000, label='R2=0.91')+
  theme_minimal()
summary(lm(season_mean~kg_N_TN_per_month, data=dat_joined_2))
#R2=0.99


#plot with outliers 
dat_joined_2 %>% filter(state=="CT") %>%
ggplot(aes(x=month_year, y=kg_N_TN_per_month, col=outlier))+
  geom_point()+
  facet_wrap(~permit_outfall, scale="free")

dat_joined_2 %>% filter(state!="CT") %>%
  ggplot(aes(x=month_year, y=kg_N_TN_per_month, col=outlier))+
  geom_point()+
  facet_wrap(~permit_outfall, scale="free")


# correcting some records manually ----------------------------------------


#remove the suspiciously low values from one permit
MA0101681<- dat_joined_2 %>% 
  filter(permit_outfall == 'MA0101681_3') %>%
  filter(kg_N_TN_per_month>10000)  #I couldn't pinpoint what was incorrect about the earlier dates, but based on the NPDES permits, they were too low

#remove that permit from dataset then rejoin the filtered 
dat_joined_2<-dat_joined_2 %>% 
  filter(!permit_outfall == 'MA0101681_3') 

dat_joined_2<-rbind(dat_joined_2,MA0101681)

#manually correcting permits with marked outliers=T that were real declines in N loads
dat_joined_2$outlier<-ifelse(
  dat_joined_2$permit_outfall == 'NY0021750_2' &  dat_joined_2$outlier==TRUE,
  dat_joined_2$outlier==FALSE, dat_joined_2$outlier) #this seems to be a real decline--probably a treatment plant upgrade

#removing one duplicated month (there were both 5/30 and 5/31 dates reported)
dat_joined_2<- filter(dat_joined_2,!key=='NY0021822_1_2003-05-30')


#impute seasonal mean for outlier values
dat_joined_2$kg_N_TN_per_month<-ifelse(
  dat_joined_2$outlier=="TRUE",
  dat_joined_2$season_mean, 
  dat_joined_2$kg_N_TN_per_month) #impute seasonal means for outliers

#plot with outliers corrected
dat_joined_2 %>% filter(state=="CT") %>%
  ggplot(aes(x=month_year, y=kg_N_TN_per_month, col=outlier))+
  geom_point()+
  facet_wrap(~permit_outfall, scale="free")

dat_joined_2 %>% filter(state!="CT") %>%
  ggplot(aes(x=month_year, y=kg_N_TN_per_month, col=outlier))+
  geom_point()+
  facet_wrap(~permit_outfall, scale="free")


# cleaning up conflicting names -------------------------------------------


dat<-dat_joined_2 %>% 
  mutate(facility=
        case_when(
          facility=='RED HOOK WATER POLLUTION CONTR' ~ "NYCDEP - RED HOOK WPCP",
          facility=="HUNT'S POINT WPC" ~ "NYCDEP - HUNT'S POINT WPCP",
          facility=="TALLMANISLAND WPC" ~"NYCDEP - TALLMAN ISLAND WPCP",
          facility=="NEWTOWNCREEK WPC" ~ "NYCDEP - NEWTOWN CREEK WPCP",
          facility=="RED HOOK WATER POLLUTION CONTR" ~"NYCDEP - RED HOOK WPCP",
          facility=="WARD ISLAND WPC" ~"NYCDEP - WARD'S ISLAND WPCP",
          facility== "MAMARONECK (V) SANITARY SD"~"MAMARONECK SANITARY SD WWTP",
          facility== "HUNTINGTON (T) STP"~ "HUNTINGTON SEWER DISTRICT STP",
          facility=="GARDNERW P C F" ~ "GARDNER WWTF", 
          facility=="GARDNER W W T F" ~"GARDNER WWTF" ,
          facility=="PITTSFIELD W W T P"  ~"PITTSFIELD WWTP",
          facility=="AMHERST W W T P" ~ "AMHERST WWTP",
          facility=="EASTHAMPTON W W T P"~"EASTHAMPTON WWTP",
          facility=="CHICOPEE W P C"~"CHICOPEE WPC",
          facility=="PORT WASHINGTON WPCD" ~"PORT WASHINGTON WPCP",
          facility=="PORT CHESTER STP" ~"PORT CHESTER SANITARY SD WWTP",
          facility=="HOLYOKE W P C F" ~ "HOLYOKE WPCF",
          facility=="NORTHAMPTON W W T F"~"NORTHAMPTON WWTF",
          facility=="NEW ROCHELLE SD"  ~"NEW ROCHELLE STP",  
          facility=="GLEN COVE (C) WTP"~"GLEN COVE (C) WWTF",
          facility=="GRAFTONW W T P" ~"GRAFTON WASTEWATER TREATMENT",
          facility=="TOWN OFREDDING" ~"REDDING, TOWN OF",
          facility=="ATHOL W W T P"~"ATHOL WWTP",
          facility=="ATHOL WW T P"~"ATHOL WWTP",
          facility=="SPENCERW W T P"~"SPENCER WWTP",
          facility=="SPENCER W W T P"~"SPENCER WWTP",
          facility=="BELCHERTOWN W W T P"~"BELCHERTOWN WWTP",
          facility=="WAUSAU PAPERS OF NH, INC."~"GROVETON ACQUISITION  L.L.C",
          facility=="NEWPORT-DORR WOOLEN W P C F"~"TOWN OF NEWPORT",
          facility=="SUFFOLKCO SD#6 - KINGS PK STP"~"SCSD#6 - KINGS PARK STP",
          facility=="SUFFOLKCO SD#21 SUNY"~"SUFFOLK CO SD#21 SUNY",
          facility=="SOUTHBRIDGE W W T P" ~ "SOUTHBRIDGE WWTP",
          facility=="MARLBOROUGH WESTERLY W W T P"~"MARLBOROUGH WESTERLY WWTP",
          facility=="WARE W W T P" ~"WARE WWTP",
          facility=="WEST FITCHBURG W W T F" ~"WEST FITCHBURG WWTF",
          facility=="MONTAGUE W P C F" ~"MONTAGUE WPCF",
          facility=="HUNTINGTON W W T P" ~"HUNTINGTON WWTP",
          facility=="HATFIELD W W T F" ~"HATFIELD WWTF",
          facility=="ERVING P O T W #1" ~"ERVING POTW #1",
          facility=="WARREN W W T F" ~"WARREN WWTF",
          facility=="LANCASTER W W T P" ~"LANCASTER WWTP",
          TRUE ~ facility)
        )

#now there are no duplicate facility names
dat %>%
  group_by(permit) %>%
  summarize(n=n_distinct(facility)) %>%
  filter(n>1)

unique(dat$facility)

ggplot(dat,aes(x=month_year,y=kg_N_TN_per_month))+
  geom_point()+
  facet_wrap(~facility, scales="free")


dat<-distinct(dat)


#write out clean results
write_csv(dat,
          file = here("data", 'clean_PCS_ECHO_dat.csv'))

# map median monthly loads all data ------------------------------------------

library(sf)
library(leaflet)
library(mapview)
library(ma)

huc8<-st_read(here('data','huc_8_dat_join','huc8_combined.shp'))



median_loads_all<-dat %>%
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

# #leaflet map
# bigm <- leaflet(huc8) %>%
#   addProviderTiles(providers$Stamen.TonerLite) %>%
#   addPolygons( fillColor = ~ "lightgrey",
#                weight = 1,
#                color = 'black',
#                opacity = 1,
#                fillOpacity = .7)  %>%
#   addCircleMarkers(
#     data = dat_sf$geometry,
#     col = 'red',
#     fillOpacity = .8,
#     radius = dat_sf$median_kg_N_TN_mo/10^4.5) %>%
#   # addLabelOnlyMarkers(data = huc8_names,
#   #                     lng = ~unlist(map(huc8_names$geometry,1)), 
#   #                     lat = ~unlist(map(huc8_names$geometry,2)), label = ~name,
#   #                     labelOptions = labelOptions(noHide = TRUE, 
#   #                                                 direction = 'top', textOnly = TRUE)) %>%
#    setView(lat = 42.5,
#           lng = -72.7,
#           zoom = 7) 
# 
# bigm
# 
# mapshot(bigm, file = here("maps", "median_monthly_load_big_map.png"))


# map top 15 only ---------------------------------------------------------
# 
# median_loads_top_9<-dat %>%
#   group_by(permit_outfall,LATITUDE83,LONGITUDE83,name) %>%
#   summarise(median_kg_N_TN_mo=median(kg_N_TN_per_month, na.rm = T))
# median_loads_top_9
# 
# dat_sf2=st_as_sf(median_loads_top_9,
#                 coords=c("LONGITUDE83","LATITUDE83"),
#                 crs=st_crs(huc8))
# 
# #zoomed in map near western LIS
# #leaflet map
# top9m <- leaflet(huc8) %>%
#   addProviderTiles(providers$Stamen.TonerLite) %>%
#   addPolygons( fillColor = ~ "lightgrey",
#                weight = 1,
#                color = 'black',
#                opacity = 1,
#                fillOpacity = .7)  %>%
#   addCircleMarkers(
#     data = dat_sf2$geometry,
#     col = 'red',
#     fillOpacity = .8,
#     radius = dat_sf2$median_kg_N_TN_mo/10^4.5) %>%
#   # addLabelOnlyMarkers(data = huc8_names,
#   #                     lng = ~unlist(map(huc8_names$geometry,1)),
#   #                     lat = ~unlist(map(huc8_names$geometry,2)), label = ~name,
#   #                     labelOptions = labelOptions(noHide = TRUE,
#   #                                                 direction = 'top', textOnly = TRUE)) %>%
#   setView(lat = 40.8,
#           lng = -73.6 ,
#           zoom = 10)
# 
# top9m
# 
# mapshot(top9m, file = here("maps", "median_monthly_load_top9.png"))

# selecting final list ----------------------------------------------------

# map top 9 only ---------------------------------------------------------

# median_loads_top_15<-dat %>%
#   group_by(permit_outfall,LATITUDE83,LONGITUDE83,name) %>%
#   summarise(median_kg_N_TN_mo=median(kg_N_TN_per_month, na.rm = T))
# median_loads_top_15
# 
# dat_sf=st_as_sf(median_loads_top_15,
#                 coords=c("LONGITUDE83","LATITUDE83"),
#                 crs=st_crs(huc8))
# 
# #zoomed in map near western LIS
# #leaflet map
# top15m <- leaflet(huc8) %>%
#   addProviderTiles(providers$OpenStreetMap) %>%
#   addPolygons( fillColor = ~ "lightgrey",
#                weight = 1,
#                color = 'black',
#                opacity = 1,
#                fillOpacity = .7)  %>%
#   addCircleMarkers(
#     data = dat_sf$geometry,
#     col = 'red',
#     fillOpacity = .8,
#     radius = dat_sf$median_kg_N_TN_mo/10^4.5) %>%
#   # addLabelOnlyMarkers(data = huc8_names,
#   #                     lng = ~unlist(map(huc8_names$geometry,1)),
#   #                     lat = ~unlist(map(huc8_names$geometry,2)), label = ~name,
#   #                     labelOptions = labelOptions(noHide = TRUE,
#   #                                                 direction = 'top', textOnly = TRUE)) %>%
#   setView(lat = 41.5,
#           lng = -73 ,
#           zoom = 8)
# 
# top15m
# 
# mapshot(top15m, file = here("maps", "median_monthly_load_top15.png"))
# 

# write out shape files ---------------------------------------------------
#read in the rest of the huc8 shapes 
huc8_combined<-st_read(here('data','huc_8_dat_join','huc8_combined.shp'))
#select columns of interest
huc8_combined<- huc8_combined %>%
  select(huc8, areasqkm,huc8,name,geometry)

er<-st_read(here('data','east-river','nyu_2451_34507.shp'))
##need to manually add a polygon for the east river
# er <- data.frame(long=c(-74.0146,-74.0094,-73.9943,
#                         -73.9806,-73.9738,-73.9618,-73.9601,
#                         -73.9459,-73.9209, -73.9113, #10
#                         -73.8961, -73.8893,-73.8855, -73.8721,
#                         -73.8604, -73.8508, -73.8440, -73.8515,
#                         -73.8598, -73.8398, -73.8361, -73.8309,
#                         -73.8292, -73.8216, -73.7952, -73.8127,
#                         -73.8316, -73.8323, -73.8385, -73.8398,
#                         -73.8419, -73.84607, -73.8471, -73.8584,
#                         -73.8652, -73.8721, -73.8855, -73.8972,
#                         -73.9116,-73.9257, -73.9418, -73.9428,
#                         -73.9748, -73.9772, -73.9964, -74.0146),
#                  lat = c(40.7002, 40.6892,40.7012,40.7054, 40.7007,
#                          40.7223,40.7369,40.7600,40.7821, 40.7915, #10
#                          40.7881, 40.7761, 40.7785, 40.7857, 
#                          40.7652,40.7592, 40.7652, 40.7824, 
#                          40.7865, 40.7974, 40.7909, 40.7891, 40.7959,
#                          40.8006, 40.7951, 40.8141, 40.8099, 40.8055,
#                          40.8065, 40.8138, 40.8187, 40.8120, 40.8060,
#                          40.8058, 40.8110, 40.8011, 40.8016, 40.8073,
#                          40.7961, 40.7824, 40.7844, 40.7772, 40.7379,
#                          40.7124, 40.7082, 40.7002))
# er <- rbind(er, er[1,])
# 
# 
# 
# er_poly <- st_sf(geometry=st_sfc(st_polygon(list(as.matrix(er)))), crs = st_crs(huc8_combined))
# er_poly
# 
# er_poly<-er_poly %>%
#   mutate(huc8=0,areasqkm=0, name="East River") %>%
#   select(huc8, areasqkm,huc8,name,geometry)
# er_poly

#check polygon on map
leaflet(er_poly) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons()  

#check polygon on map
leaflet(er) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons()  

leaflet(huc8_combined) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons() 

er_poly<-st_crop(er, xmin=-73.981, ymin=40.701, xmax=-73.911, ymax=40.791)


#check polygon on map
leaflet(er_poly) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons() 

#combine data
er_poly<-er_poly %>%
  mutate(huc8=0,areasqkm=0, name="East River") %>%
  select(huc8, areasqkm,huc8,name,geometry)
er_poly

huc8_combined<-rbind(huc8_combined,er_poly)

#calculating monthly TN totals by watershed
dat_summary<- dat %>%
  group_by(name,month_year) %>%
  summarise(kgN_huc8=sum(kg_N_TN_per_month,na.rm=T))

#join huc spatial data to monthly totals
N_load_huc_join<-left_join(dat_summary,huc8_combined)
N_load_huc_join<-st_as_sf(N_load_huc_join)

#sf::sf_use_s2(FALSE)

#check polygon on map
leaflet(N_load_huc_join) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons()  

#writing out huc_join for spatial app
dir.create(here('data', 'N_load_huc8_join'))

st_write(N_load_huc_join,
         here('data','N_load_huc8_join','N_load_huc_join.shp'))

