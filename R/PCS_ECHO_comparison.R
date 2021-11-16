library(tidyverse)
library(lubridate)
library(here)
library(lme4)
library(lmerTest)
library(astsa)

#load both datasets
PCS_all<-read_csv(file=here("data","PCS_data_clean.csv"))
ECHO_all<-read_csv(file = here("data", "ECHO_data_clean.csv"))
# ECHO_all$month_year<-my(ECHO_all$date)


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

#add column identifier (PCS or ECHO) and filter and join data
PCS_dup<-PCS_all %>% 
  filter(key %in% unlist(dup_keys)) %>%
  mutate(source=rep('PCS'))%>%
  select(key, kg_N_TN_per_month,date,source)
PCS_dup

ECHO_dup<-ECHO_all %>% 
  filter(key %in% unlist(dup_keys)) %>%
  mutate(source=rep('ECHO')) %>%
  select(key, kg_N_TN_per_month,date,source)
ECHO_dup

dat<-left_join(PCS_dup, ECHO_dup, by = 'key', suffix = c(".PCS", ".ECHO"))



# ggplot(dat, aes(x=kg_N_TN_per_month.ECHO, y=kg_N_TN_per_month.PCS)) +
#   geom_point() +
#   geom_abline(slope=1)

#everything looks good
dat$ECHO_minus_PCS<-dat$kg_N_TN_per_month.ECHO-
  dat$kg_N_TN_per_month.PCS

difference<-filter(dat, ECHO_minus_PCS>1 | ECHO_minus_PCS < -1)
difference
#there are no differences

#plot observations by permit/outfall
ECHO_all %>% 
  filter(!is.na(kg_N_TN_per_month)) %>%
  ggplot(aes(x=permit_outfall, y=kg_N_TN_per_month)) +
  geom_boxplot()+
  geom_point() +
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))

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

#editing the three outliers 
ECHO_all$kg_N_TN_per_month<-ifelse(
      ECHO_all$key == 'CT0100323_1_1999-08-31', impute_value, ECHO_all$kg_N_TN_per_month)
ECHO_all$kg_N_TN_per_month<-ifelse(
  ECHO_all$key == 'CT0100447_1_2005-02-28', ECHO_all$kg_N_TN_per_month/1000, ECHO_all$kg_N_TN_per_month) #flow looked like it was 1000X too large
ECHO_all$kg_N_TN_per_month<-ifelse(
  ECHO_all$key == 'NY0026204_1_2015-08-31', ECHO_all$kg_N_TN_per_month/10, ECHO_all$kg_N_TN_per_month) #load looked like it was 10X too large
ECHO_all$kg_N_TN_per_month<-ifelse(
  ECHO_all$key == 'CT0100447_1_2005-02-28', ECHO_all$kg_N_TN_per_month/1000, ECHO_all$kg_N_TN_per_month) #flow looked like it was 1000X too large

#Broken, need to fix:
ECHO_all<- ECHO_all %>%
  mutate(case_when (permit == 'CT0100617' &
           date>='2000-05-31' &
           date<='2005-11-30' ~ kg_N_TN_per_month*10^6)) #the flow was entered as gal/day instead of million gal/day


#need to check that the amounts are similar from 2002-2021
ECHO_all %>%
  filter(permit == 'CT0100617') %>%
  ggplot(aes(x=date,y=kg_N_TN_per_month))+
  geom_point()


summary(ECHO_all$kg_N_TN_per_month)

#plot observations by permit/outfall
ECHO_all %>% 
  filter(!is.na(kg_N_TN_per_month)) %>%
  ggplot(aes(x=permit_outfall, y=kg_N_TN_per_month)) +
  geom_boxplot()+
  geom_point() +
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))

###############

#plot through time all together
ECHO_all %>% 
  filter(!is.na(kg_N_TN_per_month)) %>%
  #filter(date>'1992-01-01' & date < '2021-11-01') %>%
  #filter(kg_N_TN_per_month < 1e6) %>%
  ggplot(aes(x=date, y=kg_N_TN_per_month)) +
  #geom_boxplot()+
  geom_point(alpha=.5) +
 # ylim(0,50000)+
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')#+
 # theme(axis.text.x = element_text(angle = 60, hjust=1))+
  #scale_x_date(date_breaks =('5 years'))+
  #facet_wrap(~permit)

#plot through time with permit facet for non CT states
ECHO_all %>% 
  filter(state!="CT") %>%
  filter(!is.na(kg_N_TN_per_month)) %>%
  filter(date>'1992-01-01' & date < '2021-11-01') %>%
  filter(kg_N_TN_per_month < 1e6) %>%
  ggplot(aes(x=date, y=kg_N_TN_per_month)) +
  #geom_boxplot()+
  geom_point(alpha=.5) +
  # ylim(0,50000)+
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')+
theme(axis.text.x = element_text(angle = 60, hjust=1))+
#scale_x_date(date_breaks =('5 years'))+
facet_wrap(~facility, scales = "free")

#plot through time with permit facet for CT
ECHO_all %>% 
  filter(state=="CT") %>%
  filter(!is.na(kg_N_TN_per_month)) %>%
  filter(date>'1992-01-01' & date < '2021-11-01') %>%
  filter(kg_N_TN_per_month < 1e6) %>%
  ggplot(aes(x=date, y=kg_N_TN_per_month)) +
  #geom_boxplot()+
  geom_point(alpha=.5) +
  # ylim(0,50000)+
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  #scale_x_date(date_breaks =('5 years'))+
  facet_wrap(~facility, scales = "free")

unique(ECHO_all$permit_outfall)

NY<-filter(ECHO_all, state=="NY")
unique(NY$facility)

#
PCS_all %>% 
  filter(state!="CT") %>%
  filter(!is.na(kg_N_TN_per_month)) %>%
  filter(date>'1992-01-01' & date < '2021-11-01') %>%
  filter(kg_N_TN_per_month < 1e6) %>%
  ggplot(aes(x=date, y=kg_N_TN_per_month)) +
  #geom_boxplot()+
  geom_point(alpha=.5) +
  # ylim(0,50000)+
  ggtitle('PCS data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  #scale_x_date(date_breaks =('5 years'))+
  facet_wrap(~facility, scales = "free")

PCS_all %>% 
  filter(state=="CT") %>%
  filter(!is.na(kg_N_TN_per_month)) %>%
  filter(date>'1992-01-01' & date < '2021-11-01') %>%
  filter(kg_N_TN_per_month < 1e6) %>%
  ggplot(aes(x=date, y=kg_N_TN_per_month)) +
  #geom_boxplot()+
  geom_point(alpha=.5) +
  # ylim(0,50000)+
  ggtitle('PCS data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  #scale_x_date(date_breaks =('5 years'))+
  facet_wrap(~facility, scales = "free")


#plot observations by permit/outfall
PCS_all %>% 
  #filter(!is.na(kg_N_TN_per_month)) %>%
  ggplot(aes(x=permit_outfall, y=kg_N_TN_per_month)) +
  geom_boxplot()+
  geom_point() +
  ggtitle('PCS data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))


#plot observations by month
ECHO_all %>% 
  filter(!is.na(kg_N_TN_per_month)) %>%
  filter(month_year>'1992-01-01' & month_year < '2021-11-01') %>%
  filter(kg_N_TN_per_month < 1e6) %>%
  group_by(month(month_year),year(month_year)) %>%
  summarise(mean(kg_N_TN_per_month, na.rm=T)) %>%
  ggplot(aes(x=as.factor((month(month_year))), y=kg_N_TN_per_month)) +
  geom_point() +
  geom_violin()+
  geom_smooth(method = "loess")+
  #ylim(c(0,100000))+
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))

#plot observations by year
ECHO_all %>% 
  filter(!is.na(kg_N_TN_per_month)) %>%
  filter(month_year>'1992-01-01' & month_year < '2021-11-01') %>%
  filter(kg_N_TN_per_month < 1e6) %>%
  group_by(permit_outfall, year=year(month_year))%>%
  summarize(kg_N_TN_per_yr=sum(kg_N_TN_per_month)) %>%
  ggplot(aes(x=year, y=kg_N_TN_per_yr)) +
  geom_point() +
  #geom_violin(draw_quantiles = T)+
  geom_smooth(method = "loess")+
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')#+
  # ylim(c(0,100000))+
  # theme(axis.text.x = element_text(angle = 60, hjust=1))+
  # ylim(0,30)

hist(ECHO_all$kg_N_TN_per_month)

ECHO_nonzero<-filter(ECHO_all,kg_N_TN_per_month>0)
summary(lm(log(ECHO_nonzero$kg_N_TN_per_month)~as.factor(month(ECHO_nonzero$month_year))))

lm1<-aov(lm(log(ECHO_nonzero$kg_N_TN_per_month)~as.factor(month(ECHO_nonzero$month_year))))
TukeyHSD(lm1)


lmer1<-lmer(log(kg_N_TN_per_month)~
                as.factor(month(month_year)) + 
                  (1|permit_outfall), data=ECHO_nonzero)
summary(lmer1)


lm1<-aov(lm(log(ECHO_nonzero$kg_N_TN_per_month)~as.factor(year(ECHO_nonzero$date))))
TukeyHSD(lm1)

#plot observations by month
PCS_all %>% 
  filter(!is.na(kg_N_TN_per_month)) %>%
  ggplot(aes(x=
               month(date), y=kg_N_TN_per_month)) +
  geom_boxplot()+
  geom_point() +
  geom_smooth(method = "loess")+
  ggtitle('PCS data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))

#plot observations by year
PCS_all %>% 
  filter(!is.na(kg_N_TN_per_month)) %>%
  ggplot(aes(x=year(date), y=kg_N_TN_per_month)) +
  geom_point() +
  geom_smooth(method = "loess")+
  ggtitle('PCS data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))

hist(PCS_all$kg_N_TN_per_month)

PCS_nonzero<-filter(PCS_all,kg_N_TN_per_month>0)
summary(lm(log(PCS_nonzero$kg_N_TN_per_month)~month(PCS_nonzero$date)))

summary(lm(log(PCS_nonzero$kg_N_TN_per_month)~year(PCS_nonzero$date)))



# test for seasonality ----------------------------------------------------

#see https://online.stat.psu.edu/stat510/lesson/4/4.2
#and https://digitalcommons.wayne.edu/cgi/viewcontent.cgi?article=2030&context=jmasm
#https://www.r-bloggers.com/2021/04/timeseries-analysis-in-r/
#http://fable.tidyverts.org/


#these commands below are designed for time series of n=1, but I need to figure out how to
#analyze for the many different time series in our data for each permit/outfall
# ECHO_ts<-ts(ECHO_all)
# 
# diff4 = diff(ECHO_ts, 4)
# diff1and4 = diff(diff4,1)
# acf2(diff1and4,24)
