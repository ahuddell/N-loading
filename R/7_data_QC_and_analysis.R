library(tidyverse)
library(lubridate)
library(here)
library(lme4)
library(lmerTest)
library(astsa)

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
PCS_join<-select(PCS_join,-quant_avg,-conc_avg,-permit_outfall_designator,-param,-days_per_month, -impute_name.x, -impute_name.y)


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

# join data and clean up facility names -----------------------------------


#the same 15 outfalls are listed as top polluters by both measures of median monthly and median annual loads

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

#split data apart and impute facility names for the mismatches
corrected_facility<-dat %>% filter(!is.na(impute_facility)) %>%
  mutate(facility=impute_facility)
original_facility<-dat %>% filter(is.na(impute_facility)) 

#recombine data
dat<-rbind(corrected_facility,original_facility)

unique(dat$facility)
filter(dat, is.na(facility))

# monthly plots -----------------------------------------------------------

ggplot(dat,aes(x=month_year,y=kg_N_TN_per_month)) +
  geom_point()+
  geom_line(col='darkgrey')+
  ylab('monthly total N load by outfall (kg N/mo)')+
  ggtitle('Monthly totals top 15 facilities only')+
  facet_wrap(~facility, scales = "free")



# annual sum --------------------------------------------------------------

annual_sum<-dat %>%
  group_by(permit_outfall, facility,year=year(month_year)) %>%
  summarise(kg_N_TN_yr=sum(kg_N_TN_per_month, na.rm = T))


ggplot(annual_sum,aes(x=year,y=kg_N_TN_yr/1000)) +
  geom_point()+
  geom_smooth(method = "loess")+
  ylab('Annual total N load by outfall (Mg N/yr)')+
  ggtitle('Annual totals top 15 facilities only')+
  facet_wrap(~facility, scales = "free")

#the annual totals look wonky
# 
# watershed_annual_sum<-dat_joined %>%
#   group_by(watershed=name, year=year(month_year)) %>%
#   summarise(kg_N_TN_yr=sum(kg_N_TN_per_month, na.rm = T))
# 
# ggplot(watershed_annual_sum,aes(x=year,y=kg_N_TN_yr/1000)) +
#   geom_point()+
#   geom_smooth(method = "loess")+
#   ylab('Annual total N load by outfall (Mg/yr)')
# 
# ggplot(watershed_annual_sum,aes(x=year,y=kg_N_TN_yr/1000)) +
#   geom_point()+
#   geom_smooth(method = "loess")+
#   ylab('Annual total N load by watershed (Mg/yr)')+
#   facet_wrap(~watershed, scales = "free")
# 




# seasons -----------------------------------------------------------------

quarterly_sums<-dat_joined %>%
  mutate(season= case_when(
    month(month_year)=='12' ~ 'winter',
    month(month_year)=='1' ~ 'winter',
    month(month_year)=='2' ~ 'winter',
    month(month_year)=='3' ~ 'spring',
    month(month_year)=='3' ~ 'spring',
    month(month_year)=='5' ~ 'spring',
    month(month_year)=='6' ~ 'summer',
    month(month_year)=='7' ~ 'summer',
    month(month_year)=='8' ~ 'summer',
    month(month_year)=='9' ~ 'fall',
    month(month_year)=='10' ~ 'fall',
    month(month_year)=='11' ~ 'fall'
  )) %>%
  group_by(facility, state,permit_outfall, season) %>%
  summarise(kg_N_TN_season=sum(kg_N_TN_per_month, na.rm = T))



ggplot(quarterly_sums, aes(x=season, y=kg_N_TN_season)) +
  geom_point() +
  geom_boxplot(alpha=.4)+
  ggtitle('Seasonal differences through time')+
  ylab('Seasonal TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylim(0,10000)



quarterly_sums_nonzero<-filter(quarterly_sums,kg_N_TN_season>0)
summary(lm(log(quarterly_sums_nonzero$kg_N_TN_season)~as.factor((quarterly_sums_nonzero$season))))

lm1<-aov(lm(log(ECHO_nonzero$kg_N_TN_per_month)~as.factor(month(ECHO_nonzero$month_year))))
TukeyHSD(lm1)


lmer1<-lmer(log(kg_N_TN_per_month)~
              as.factor(month(month_year)) + 
              (1|permit_outfall), data=ECHO_nonzero)
summary(lmer1)

# test for seasonality ----------------------------------------------------

#see https://online.stat.psu.edu/stat510/lesson/4/4.2
#and https://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=2030&context=jmasm
#https://www.r-bloggers.com/2021/04/timeseries-analysis-in-r/
#http://fable.tidyverts.org/


#these commands below are designed for time series of n=1, but I need to figure out how to
#analyze for the many different time series in our data for each permit/outfall


Chicopee_monthly_totals<-dat_joined %>%
  filter(name=='Chicopee River') %>%
  group_by(month_year) %>%
  summarise(kg_N_TN_month=sum(kg_N_TN_per_month, na.rm = T))

plot(Chicopee_monthly_totals)

Chicopee_monthly_totals_ts<-ts(Chicopee_monthly_totals)
Chicopee_monthly_totals_ts

diff12=diff(Chicopee_monthly_totals_ts, 12)
plot(diff12)
acf2(diff12)
diff1and12 = diff(diff12, 1) 

diff4 = diff(Chicopee_monthly_totals_ts, 4)
diff1and4 = diff(diff4,1)
acf2(diff1and4,24)

