library(tidyverse)
library(lubridate)
library(here)
library(lme4)
library(lmerTest)
library(imputeTS)
library(tsibble)
#library(astsa)


#load data
dat<-read_csv(file=here("data","combined_top9_WLIS_clean_dat.csv"))

# monthly plots -----------------------------------------------------------

ggplot(dat,aes(x=month_year,y=kg_N_TN_per_month/1000)) +
  geom_point()+
  geom_line(col='darkgrey')+
    ylab('monthly total N load by outfall (1000 kg N/mo)')+
  ggtitle('Monthly totals top 9 facilities Western LIS only')+
  xlab('Date')+
  facet_wrap(~facility, scales = "free")


ggplot(dat,aes(x=month_year,y=kg_N_TN_per_month/1000)) +
  geom_point()+
  geom_line(col='darkgrey')+
  ylab('monthly total N load by outfall (1000 kg N/mo)')+
  ggtitle('Monthly totals top 9 facilities Western LIS only')+
  xlab('Date')+
    facet_wrap(~permit)


# annual sum --------------------------------------------------------------

annual_sum<-dat %>%
  group_by(permit_outfall, facility,year=year(month_year)) %>%
  summarise(kg_N_TN_yr=sum(kg_N_TN_per_month, na.rm = T))


ggplot(annual_sum,aes(x=year,y=kg_N_TN_yr/1000)) +
  geom_point()+
  geom_smooth(method = "loess")+
  ylab('Annual total N load by outfall (1000 kg N/yr)')+
  ggtitle('Annual totals top 9 facilities Western LIS only')+
  facet_wrap(~facility)


# seasons -----------------------------------------------------------------

dat<-dat %>%
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

ggplot(dat, aes(x=season, y=kg_N_TN_per_month)) +
  geom_jitter(width=.05,alpha=.4) +
  geom_violin(fill='lightgrey', color='lightgrey',alpha=.4)+
  theme_minimal()+
  geom_pointrange()+
  ggtitle('Monthly loads by season')+
  ylab('monthly total N load by outfall (kg N/mo)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))

hist(dat$kg_N_TN_per_month)

dat_nonzero<-filter(dat,kg_N_TN_per_month>0)
summary(lm(log(dat_nonzero$kg_N_TN_per_month)~as.factor((dat_nonzero$season))))

lmer1<-lmer(log(kg_N_TN_per_month)~
              as.factor(season) + 
              (1|permit_outfall), data=dat_nonzero)
summary(lmer1)

# impute time series missing data -----------------------------------------

library(tsibble)
weather <- nycflights13::weather %>% 
  select(origin, time_hour, temp, humid, precip)
weather

weather_tsbl <- as_tsibble(weather, key = origin)



dat<-distinct(dat)
dups<-duplicates(dat, key = permit,index=month_year)

dups<-distinct(dups)

dat_ts<-as_tsibble(dat, key = permit,index=month_year)

full_ts <- dat %>%
  fill_gaps(kg_N_TN_per_month = 0) %>% 
  group_by_key() %>% 
  tidyr::fill(temp, humid, .direction = "down")
full_weather

# dat %>% 
#   select(facility, month_year, kg_N_TN_per_month) %>% 
#   
dat_split<-split(dat,dat$permit) 

for (i in 1:length(dat_split)) {
    assign(paste0(names(dat_split)[i]), dat_split[[i]])
           }
permit_list<-unique(dat$permit)



NY0026131<-select(NY0026131,month_year,kg_N_TN_per_month) %>% arrange(month_year)
NY0026191<-select(NY0026191,month_year,kg_N_TN_per_month) %>% arrange(month_year)
NY0026239<-select(NY0026239,month_year,kg_N_TN_per_month) %>% arrange(month_year)
NY0026204<-select(NY0026204,month_year,kg_N_TN_per_month) %>% arrange(month_year)
NY0027073<-select(NY0027073,month_year,kg_N_TN_per_month) %>% arrange(month_year)
NY0026158<-select(NY0026158,month_year,kg_N_TN_per_month) %>% arrange(month_year)
CT0100056<-select(CT0100056,month_year,kg_N_TN_per_month) %>% arrange(month_year)
CT0101087<-select(CT0101087,month_year,kg_N_TN_per_month) %>% arrange(month_year)
CT0101249<-select(CT0101249,month_year,kg_N_TN_per_month) %>% arrange(month_year)



NY0026131<-ts(NY0026131[, 2], start=c(1996,1), frequency=12)
NY0026191<-ts(NY0026191[, 2], start=c(1996,1), frequency=12)
NY0026239<-ts(NY0026239[, 2], start=c(1992,1), frequency=12)
NY0026204<-ts(NY0026204[, 2], start=c(1992,1), frequency=12)
NY0027073<-ts(NY0027073[, 2], start=c(1992,1), frequency=12)
NY0026158<-ts(NY0026158[, 2], start=c(2006,1), frequency=12)
CT0100056<-ts(CT0100056[, 2], start=c(2002,1), frequency=12)
CT0101087<-ts(CT0101087[, 2], start=c(2002,1), frequency=12)
CT0101249<-ts(CT0101249[, 2], start=c(2002,1), frequency=12)

statsNA(NY0026131)
statsNA(NY0026191)
statsNA(NY0026239)
statsNA(NY0026204)
statsNA(NY0027073)
statsNA(NY0026158)
statsNA(CT0100056)
statsNA(CT0101087)
statsNA(CT0101249)



plot(NY0026131)
plot(NY0026191)
plot(NY0026239)
plot(NY0026204)
plot(NY0027073)
plot(NY0026158)



# load vs. hypoxia --------------------------------------------------------


hyp<-read_csv(file=here("data",'CTDEEP_hypoxia.csv'),
              col_types = c('cnn'))

hyp$Start_Date_UTC<-ymd_hms(hyp$Start_Date_UTC)
hyp$Start_Date_UTC<-as_date(hyp$Start_Date_UTC)

hyp$month_year<-substr(hyp$Start_Date_UTC,1,7)
hyp$month_year<-ym(hyp$month_year)

min(hyp$month_year, na.rm=T)
max(hyp$month_year, na.rm=T)

hyp_month<-hyp %>%
  group_by(month_year) %>%
  summarise(max_Area_under_3_mgL =max(Area_under_3_mgL, na.rm=T),
            mean_Area_under_3_mgL =mean(Area_under_3_mgL, na.rm=T),
            max_Area_under_5_mgL =max(Area_under_5_mgL, na.rm=T),
            mean_Area_under_5_mgL =mean(Area_under_5_mgL, na.rm=T)) 
hyp_month


hyp_dat<-left_join(hyp_month,dat)
hyp_dat<-filter(hyp_dat,max_Area_under_5_mgL>0) #remove zeroes for plotting

ggplot(hyp_dat,aes(x=month_year,y=max_Area_under_5_mgL)) +
  geom_point()+
  #geom_smooth(method = "lm")+
  ylab('area under 5 mg/L dissolved oxygen')+
  xlab('date')+
  theme_minimal()

ggplot(hyp_dat,aes(x=kg_N_TN_per_month/1000,y=max_Area_under_5_mgL,
                   col=facility)) +
  geom_point()+
  ylab('area under 5 mg/L dissolved oxygen')+
  xlab('total N load by outfall (1000 kg N/yr)')+
  theme_minimal()+
  ggtitle('Annual totals top 9 facilities Western LIS only')

test<-filter(hyp_dat, !is.na(max_Area_under_5_mgL) & !is.na(kg_N_TN_per_month))
filter(test, !is.na(max_Area_under_5_mgL))
test1<-filter(hyp_dat, !is.na(max_Area_under_5_mgL))
test1

hyp<-read_csv(file=here("data",'JOD_hyp_area_volume.csv'))
hyp$year<-year(ym(hyp$date))


ggplot(hyp,aes(x=year,y=area_4.8mg_l_km2)) +
  geom_point()+
  #geom_smooth(method = "lm")+
  ylab('area under 5 mg/L dissolved oxygen')+
  xlab('date')+
  y_date_range
  theme_minimal()


# test for seasonality ----------------------------------------------------
# 
# #see https://online.stat.psu.edu/stat510/lesson/4/4.2
# #and https://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=2030&context=jmasm
# #https://www.r-bloggers.com/2021/04/timeseries-analysis-in-r/
# #http://fable.tidyverts.org/
# 
# 
# #these commands below are designed for time series of n=1, but I need to figure out how to
# #analyze for the many different time series in our data for each permit/outfall
# 
# 
# Chicopee_monthly_totals<-dat_joined %>%
#   filter(name=='Chicopee River') %>%
#   group_by(month_year) %>%
#   summarise(kg_N_TN_month=sum(kg_N_TN_per_month, na.rm = T))
# 
# plot(Chicopee_monthly_totals)
# 
# Chicopee_monthly_totals_ts<-ts(Chicopee_monthly_totals)
# Chicopee_monthly_totals_ts
# 
# diff12=diff(Chicopee_monthly_totals_ts, 12)
# plot(diff12)
# acf2(diff12)
# diff1and12 = diff(diff12, 1) 
# 
# diff4 = diff(Chicopee_monthly_totals_ts, 4)
# diff1and4 = diff(diff4,1)
# acf2(diff1and4,24)
