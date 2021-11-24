library(tidyverse)
library(lubridate)
library(here)
library(lme4)
library(lmerTest)
library(astsa)

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
    facet_wrap(~facility)


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
