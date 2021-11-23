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

PCS_join<-PCS_all %>% filter(!key %in% dup_keys) #remove PCS data that also exists in ECHO
names(PCS_join)
names(ECHO_all)
PCS_join<-select(PCS_join,-quant_avg,-conc_avg,-permit_outfall_designator,-param,-days_per_month)
ECHO_all<-select(ECHO_all,-kg_N_sum_per_month)


#looking at outliers
ECHO_all%>%filter(kg_N_TN_per_month>1000000) %>% group_by(key) %>%
  summarize(key=first(key))

#cleaning up values that are obvious typos
#CT0100323_1_1999-08-31 clearly has a typo in the N concentration--impute the mean of that year maybe?
#CT0100447_1_2005-02-28 seems to have entered the flow rate 1000X higher than it should; for parameter 50050 - Flow, in conduit or thru treatment plant
#NY0026204_1_2015-08-31 seems to have entered the load 10X higher that it should

impute_value<-as.numeric(ECHO_all %>% filter(permit_outfall=='CT0100323_1' & date>'1999-06-30'
                                             & date <'1999-10-31') %>% #grabbing months before and after August
                           filter(!date=='1999-08-31') %>% #removing problematic date
                           summarize(mean(kg_N_TN_per_month)))
#editing the  outliers
# ECHO_all$kg_N_TN_per_month<-ifelse(
#       ECHO_all$key == 'CT0100323_1_1999-08-31', impute_value, ECHO_all$kg_N_TN_per_month)
# ECHO_all$kg_N_TN_per_month<-ifelse(
#   ECHO_all$key == 'CT0100447_1_2005-02-28', ECHO_all$kg_N_TN_per_month/1000, ECHO_all$kg_N_TN_per_month) #flow looked like it was 1000X too large
ECHO_all$kg_N_TN_per_month<-ifelse(
  ECHO_all$key == 'NY0026204_1_2015-08-31', ECHO_all$kg_N_TN_per_month/10, ECHO_all$kg_N_TN_per_month) #load looked like it was 10X too large
# ECHO_all$kg_N_TN_per_month<-ifelse(
#   ECHO_all$key == 'CT0100447_1_2005-02-28', ECHO_all$kg_N_TN_per_month/1000, ECHO_all$kg_N_TN_per_month) #flow looked like it was 1000X too large
# ECHO_all$kg_N_TN_per_month<-ifelse(
#   ECHO_all$key == 'CT0100455_1_2018-04-30', ECHO_all$kg_N_TN_per_month/1000, ECHO_all$kg_N_TN_per_month) #flow looked like it was 1000X too large

#editing the three outliers 
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
  PCS_all$key == 'NH0001180_1_1992-05-31',
  PCS_all$kg_N_TN_per_month/10^6, PCS_all$kg_N_TN_per_month) #flow looked like it was 10^6 too large


#points to clean up for ECHO CT data
#not needed currently since we are using CTDEEP data instead

# impute_value<-as.numeric(ECHO_all %>% filter(permit_outfall=='CT0100323_1' & date>'1999-06-30'
#                                              & date <'1999-10-31') %>% #grabbing months before and after August
#                            filter(!date=='1999-08-31') %>% #removing problematic date
#                            summarize(mean(kg_N_TN_per_month)))
# ECHO_all$kg_N_TN_per_month<-ifelse(
#   ECHO_all$key == 'CT0100323_1_1999-08-31', impute_value, ECHO_all$kg_N_TN_per_month)
# ECHO_all$kg_N_TN_per_month<-ifelse(
#   ECHO_all$key == 'CT0100447_1_2005-02-28', ECHO_all$kg_N_TN_per_month/10, ECHO_all$kg_N_TN_per_month)
# ECHO_all$kg_N_TN_per_month<-ifelse(
#   ECHO_all$key == 'CT0100447_1_2005-02-28', ECHO_all$kg_N_TN_per_month/1000, ECHO_all$kg_N_TN_per_month)



#clean up a few problematic observations

CT$TN_kg_d<-ifelse(
  CT$TN_kg_d >3000000, CT$TN_lbs_d/2.205, CT$TN_kg_d) #this converts the lbs/day amount for a few outliers in flow and concentration that are clearly wrong


dat_joined<-rbind(PCS_join, ECHO_all)


# annual sum --------------------------------------------------------------

annual_sum<-dat_joined %>%
  group_by(facility, state,permit_outfall, year=year(month_year)) %>%
  summarise(kg_N_TN_yr=sum(kg_N_TN_per_month, na.rm = T))

ggplot(annual_sum,aes(x=year,y=kg_N_TN_yr/1000)) +
  geom_point()+
  geom_smooth(method = "loess")+
  ylab('Annual total N load by outfall (Mg/yr)')

filter(annual_sum,state=='CT')%>%
  ggplot(aes(x=year,y=kg_N_TN_yr/1000)) +
  geom_point()+
  geom_smooth(method = "loess")+
  ylab('Annual total N load by outfall (Mg/yr)')+
  ggtitle('CT facilities only')+
  facet_wrap(~facility, scales = "free")

filter(annual_sum,!state=='CT')%>%
  ggplot(aes(x=year,y=kg_N_TN_yr/1000)) +
  geom_point()+
  geom_smooth(method = "loess")+
  ylab('Annual total N load by outfall (Mg/yr)')+
  ggtitle('Non-CT facilities only')+
  facet_wrap(~facility, scales = "free")


watershed_annual_sum<-dat_joined %>%
  group_by(watershed=name, year=year(month_year)) %>%
  summarise(kg_N_TN_yr=sum(kg_N_TN_per_month, na.rm = T))

ggplot(watershed_annual_sum,aes(x=year,y=kg_N_TN_yr/1000)) +
  geom_point()+
  geom_smooth(method = "loess")+
  ylab('Annual total N load by outfall (Mg/yr)')

ggplot(watershed_annual_sum,aes(x=year,y=kg_N_TN_yr/1000)) +
  geom_point()+
  geom_smooth(method = "loess")+
  ylab('Annual total N load by watershed (Mg/yr)')+
  facet_wrap(~watershed, scales = "free")

# filtering out small sources of N ----------------------------------------

median_loads<-dat_joined %>%
  group_by(state,permit_outfall) %>%
  summarise(median_kg_N_TN_mo=median(kg_N_TN_per_month, na.rm = T)) %>%
  arrange(desc(median_kg_N_TN_mo)) 



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

