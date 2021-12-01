
library(tidyverse)
library(lubridate)
library(here)

#load both datasets
ECHO_all<-read_csv(file = here("data", "ECHO_CT_data.csv"))

CT<-read_csv(file=here("data","CT_NX_data.csv"))
CT$key<-paste0(CT$facility,"_",CT$date)
CT$duplicated<-duplicated(CT)

CT<-distinct(CT) #remove a few duplicate rows

#editing the three outliers 

impute_value<-as.numeric(ECHO_all %>% filter(permit_outfall=='CT0100323_1' & month_year>'1999-06'
                                             & month_year <'1999-10') %>% #grabbing months before and after August
                           filter(!month_year=='1999-08') %>% #removing problematic date
                           summarize(mean(kg_N_TN_per_month)))

ECHO_all$kg_N_TN_per_month<-ifelse(
  ECHO_all$key == 'CT0100323_1_1999-08-31', impute_value, ECHO_all$kg_N_TN_per_month)
ECHO_all$kg_N_TN_per_month<-ifelse(
  ECHO_all$key == 'CT0100447_1_2005-02-28', ECHO_all$kg_N_TN_per_month/10, ECHO_all$kg_N_TN_per_month)
ECHO_all$kg_N_TN_per_month<-ifelse(
  ECHO_all$key == 'CT0100447_1_2005-02-28', ECHO_all$kg_N_TN_per_month/1000, ECHO_all$kg_N_TN_per_month)




#clean up a few problematic observations
 
CT$TN_kg_d<-ifelse(
  CT$TN_kg_d >3000000, CT$TN_lbs_d/2.205, CT$TN_kg_d) #this converts the lbs/day amount for a few outliers in flow and concentration that are clearly wrong

# CT<-CT %>%
#   mutate(date=mdy(date)) %>%
#   mutate(month_year=format(as.Date(date), "%Y-%m")) %>%
#   mutate(days = day(date)) %>%
#   mutate(Outfall=rep(1)) %>%
#   mutate(key_2 = paste0(permit, "_", Outfall, "_", month_year))

#formatting CT data to join with rest of ECHO data
CT_summary<-CT %>% 
  mutate(date=mdy(date)) %>%
  mutate(month_year=format(as.Date(date), "%Y-%m")) %>%
  mutate(days_date = day(date)) %>%
  mutate(Outfall=rep(1)) %>%
  group_by(permit,Outfall,month_year) %>%
  arrange(date)%>%
  mutate(date_sequence = row_number())%>%
  mutate(day_interval= case_when( 
    date_sequence == 1 ~ days_date,
    date_sequence > 1 ~ days_date-dplyr::lag(days_date, n=1)
    )) #computing the number of days in each reporting date interval

CT_summary<-CT_summary %>% 
  mutate(TN_kg_interval=day_interval*TN_kg_d) %>%#calculating N load per interval
  group_by(permit,Outfall,month_year) %>%
  summarise(kg_N_TN_per_month=sum(TN_kg_interval),
            day_interval_total=sum(day_interval)) %>%
  mutate(permit_outfall=paste0(permit,"_",Outfall)) %>%
  mutate(key_2 = paste0(permit, "_", Outfall, "_", month_year)) 

CT_summary

ECHO_all<- ECHO_all %>%  
  mutate(month_year=format(as.Date(date), "%Y-%m")) %>%
  select(-date) %>%
  mutate(key_2=paste0(permit,'_',Outfall,'_',month_year))

ECHO_all$Outfall<-as.character(ECHO_all$Outfall)
CT_summary$Outfall<-as.character(CT_summary$Outfall)



#organizing keys
CT_key<-list(unique(CT_summary$key_2))
ECHO_key<-tibble(key=as.character(unique(ECHO_all$key_2)))
CT_key
ECHO_key

#filtering for keys in both datasets
dup_keys<-ECHO_key %>% 
  filter(key %in% unlist(CT_key))
dup_keys<-list(dup_keys)
dup_keys

#add column identifier (CTDEEP or ECHO) and filter and join data
CT_dup<-CT_summary %>% 
  filter(key_2 %in% unlist(dup_keys)) %>%
  mutate(source=rep('CTDEEP'))%>%
  select(key_2, kg_N_TN_per_month,month_year,source)
CT_dup


ECHO_dup<-ECHO_all %>% 
  filter(key_2 %in% unlist(dup_keys)) %>%
  mutate(source=rep('ECHO')) %>%
  select(key_2, kg_N_TN_per_month,month_year,source)
ECHO_dup

dup_dat<-left_join(CT_dup, ECHO_dup, by = 'key_2', suffix = c(".CTDEEP", ".ECHO"))
dup_dat$permit<-substr(dup_dat$key_2,1,9)
unique(dup_dat$permit)


ggplot(dup_dat, aes(x=kg_N_TN_per_month.ECHO, y=kg_N_TN_per_month.CTDEEP,
                    col=permit)) +
  geom_point() +
  geom_abline(slope=1, col='red',linetype='dashed')+
  ylab('CTDEEP monthly total N loads (kg N/month)')+
  xlab('ECHO monthly total N loads (kg N/month)')+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  ggtitle('Comparison of monthly load data by data source')+
  xlim(0,50000)+
  theme_minimal()



dup_dat$abs_CTDEEP_minus_ECHO<-abs(dup_dat$kg_N_TN_per_month.CTDEEP-dup_dat$kg_N_TN_per_month.ECHO)
dup_dat$CTDEEP_minus_ECHO<-dup_dat$kg_N_TN_per_month.CTDEEP-dup_dat$kg_N_TN_per_month.ECHO

dup_dat %>% 
  filter(abs_CTDEEP_minus_ECHO<100000)%>%
  ggplot(aes(x=abs_CTDEEP_minus_ECHO))+
  geom_histogram()+
  theme_minimal()+
  xlab('Absolute value of differences between monthly total N loads (kg N/month) from ECHO and CTDEEP')

summary(dup_dat$CTDEEP_minus_ECHO)

dup_dat %>% 
  filter(kg_N_TN_per_month.ECHO==0) %>%
  group_by(permit) %>%
  tally()

dup_dat %>% 
  filter(kg_N_TN_per_month.ECHO==0) %>%
  group_by(permit) %>%
  tally() %>%
  summarize(sum(n)) #268 observations with "0" N load

# data quality issues observed when comparing ECHO CT data to CTDEEP data:

#permits that report many "0s" N concentrations in monitoring location 1, but may be conflated with N loads reported in monitoring location C, which is not considered effluent but rather, "Nitrogen, Removal Complete"
#CT0100081 
#


#these permits seems to just report lots of zeroes 
#CT0100315
#CT0100641
#CT0101087
#CT0101222
#CT0101273
#CT0101320
#CT0101681
#CT0101788

#this permit often missing flow
#CT0100714
