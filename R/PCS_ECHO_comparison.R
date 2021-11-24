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

#add column identifier (PCS or ECHO) and filter and join data
PCS_dup<-PCS_all %>% 
  filter(key %in% unlist(dup_keys)) %>%
  mutate(source=rep('PCS'))%>%
  select(key, kg_N_TN_per_month,month_year,source)
PCS_dup

ECHO_dup<-ECHO_all %>% 
  filter(key %in% unlist(dup_keys)) %>%
  mutate(source=rep('ECHO')) %>%
  select(key, kg_N_TN_per_month,month_year,source)
ECHO_dup

dat<-left_join(PCS_dup, ECHO_dup, by = 'key', suffix = c(".PCS", ".ECHO"))



ggplot(dat, aes(x=kg_N_TN_per_month.ECHO, y=kg_N_TN_per_month.PCS)) +
  geom_point() +
  geom_abline(slope=1)


dat$ECHO_minus_PCS<-dat$kg_N_TN_per_month.ECHO-
  dat$kg_N_TN_per_month.PCS

difference<-filter(dat, ECHO_minus_PCS>1 | ECHO_minus_PCS < -1)
difference
#there some important differences

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






#add column identifier (PCS or ECHO) and filter and join data
PCS_dup<-PCS_all %>% 
  filter(key %in% unlist(dup_keys)) %>%
  mutate(source=rep('PCS'))%>%
  select(key, kg_N_TN_per_month,month_year,source)
PCS_dup

ECHO_dup<-ECHO_all %>% 
  filter(key %in% unlist(dup_keys)) %>%
  mutate(source=rep('ECHO')) %>%
  select(key, kg_N_TN_per_month,month_year,source)
ECHO_dup

dat<-left_join(PCS_dup, ECHO_dup, by = 'key', suffix = c(".PCS", ".ECHO"))

ggplot(dat, aes(x=kg_N_TN_per_month.ECHO, y=kg_N_TN_per_month.PCS)) +
  geom_point() +
  geom_abline(slope=1)
#now there's pretty close agreement

dat$ECHO_minus_PCS<-dat$kg_N_TN_per_month.ECHO-
  dat$kg_N_TN_per_month.PCS
difference<-filter(dat, ECHO_minus_PCS>1 | ECHO_minus_PCS < -1)
difference


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
 ggplot(aes(x=month_year, y=kg_N_TN_per_month)) +
  geom_point(alpha=.5) +
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')#+

##free scales
#plot through time with permit facet for non CT states
ECHO_all %>% 
  filter(state!="CT") %>%
  ggplot(aes(x=ym(month_year), y=kg_N_TN_per_month)) +
  geom_point(alpha=.5) +
  # ylim(0,50000)+
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')+
scale_x_date(date_breaks =('5 years'))+
facet_wrap(~facility, scales = "free")

#plot through time with permit facet for CT
ECHO_all %>% 
  filter(state=="CT") %>%
  ggplot(aes(x=ym(month_year), y=kg_N_TN_per_month)) +
  geom_point(alpha=.5) +
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  scale_x_date(date_breaks =('5 years'))+
  facet_wrap(~facility, scales = "free")

#fixed scales
##same plots with same x/y axes
#plot through time with permit facet for non CT states
ECHO_all %>% 
  filter(state!="CT") %>%
  filter(!is.na(kg_N_TN_per_month)) %>%
  ggplot(aes(x=month_year, y=kg_N_TN_per_month)) +
  #geom_boxplot()+
  geom_point(alpha=.5) +
  # ylim(0,50000)+
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  #scale_x_date(date_breaks =('5 years'))+
  facet_wrap(~facility)

#plot through time with permit facet for CT
ECHO_all %>% 
  filter(state=="CT") %>%
  filter(!is.na(kg_N_TN_per_month)) %>%
  ggplot(aes(x=month_year, y=kg_N_TN_per_month)) +
  #geom_boxplot()+
  geom_point(alpha=.5) +
  # ylim(0,50000)+
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  #scale_x_date(date_breaks =('5 years'))+
  facet_wrap(~facility)

#
PCS_all %>% 
  filter(state!="CT") %>%
  filter(!is.na(kg_N_TN_per_month)) %>%
  ggplot(aes(x=ym(month_year), y=kg_N_TN_per_month)) +
  #geom_boxplot()+
  geom_point(alpha=.5) +
  ggtitle('PCS data')+
  ylab('Monthly TN load (kg N)')+
  scale_x_date(date_breaks =('5 years'))+
  facet_wrap(~facility, scales = "free")

PCS_all %>% 
  filter(state=="CT") %>%
  filter(!is.na(kg_N_TN_per_month)) %>%
  ggplot(aes(x=month_year, y=kg_N_TN_per_month)) +
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
  group_by(month=month(month_year),year=year(month_year)) %>%
  summarise(mean_kg_N_TN_per_month=mean(kg_N_TN_per_month, na.rm=T)) %>%
  ggplot(aes(x=as.factor(month), y=mean_kg_N_TN_per_month)) +
  geom_point() +
  geom_boxplot(alpha=.4)+
  ggtitle('ECHO data')+
  ylab('Monthly TN load (kg N)')+
  xlab('month')

#plot observations by year
ECHO_all %>% 
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


lm1<-aov(lm(log(ECHO_nonzero$kg_N_TN_per_month)~as.factor(year(ECHO_nonzero$month_year))))
TukeyHSD(lm1)

#plot observations by month
PCS_all %>% 
  filter(!is.na(kg_N_TN_per_month)) %>%
  ggplot(aes(x= as.factor(month(month_year)), y=kg_N_TN_per_month)) +
  geom_boxplot(alpha=.4)+
  geom_point() +
  ggtitle('PCS data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))

#plot observations by year
PCS_all %>% 
  filter(!is.na(kg_N_TN_per_month)) %>%
  ggplot(aes(x=year(month_year), y=kg_N_TN_per_month)) +
  geom_point() +
  geom_smooth(method = "loess")+
  ggtitle('PCS data')+
  ylab('Monthly TN load (kg N)')+
  theme(axis.text.x = element_text(angle = 60, hjust=1))

hist(PCS_all$kg_N_TN_per_month)

PCS_nonzero<-filter(PCS_all,kg_N_TN_per_month>0)
summary(lm(log(PCS_nonzero$kg_N_TN_per_month)~month(PCS_nonzero$month_year)))

summary(lm(log(PCS_nonzero$kg_N_TN_per_month)~year(PCS_nonzero$month_year)))
