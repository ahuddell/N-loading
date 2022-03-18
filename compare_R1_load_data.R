library(tidyverse)
library(here)
library(lubridate)

dat<-read_csv(file=unzip(zipfile=here('data','complete_time_series_with_missing_data_imputed.zip')))
names(dat)
dat$days_in_month<-days_in_month(dat$date)


R1dat<-read_csv(here('data','R1 WWTP data.csv'))

dat_summary<-dat %>%
  mutate(year=year(date),
         days_in_month=days_in_month(date)) %>%
  filter(year > 2016)  %>%
  mutate(monthly_lbs_day=kg_N_TN_per_month/days_in_month*2.20462)%>%
  group_by(permit,permit_outfall,year) %>%
  summarise(annual_TN_mean_lbs_day=mean(monthly_lbs_day, na.rm=T)) %>%
  mutate(key=paste0(permit,'_',year))
dat_summary  

R1dat_summary<-R1dat %>%
  pivot_longer(cols=ends_with('_avg_TN_lb_day'),
               names_to = 'year',
               values_to = 'avg_TN_lb_day'
              ) %>%
  separate(col=year,sep='_',c('year',NA, NA, NA,NA)) %>%
  mutate(key=paste0(Permit,'_',year))
R1dat_summary  

join=left_join(dat_summary,R1dat_summary, by='key')

summary(lm(annual_TN_mean_lbs_day~avg_TN_lb_day, data = join))
ggplot(join, aes(x=avg_TN_lb_day, y=annual_TN_mean_lbs_day))+
  geom_point()+
  geom_abline(slope=1, linetype='dashed')+
  xlab('R1 estimates annual average (TN lbs/day)') +
  ylab('Alex estimates annual average (TN lbs/day)') +
  ylim(0,2000) +
  theme_minimal()+
  annotate('text', x= 1800, y=200, label='R-squared: 0.98')


