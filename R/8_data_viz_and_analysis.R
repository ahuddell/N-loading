library(tidyverse)
library(lubridate)
library(here)
library(lme4)
library(lmerTest)
library(zoo)
library(tsibble)
library(dataRetrieval)
library(broom)
library(imputeTS)
#library(fable)

#load data
dat<-read_csv(file=here("data","clean_PCS_ECHO_dat.csv"))
dat

dat<-dat %>%
  mutate(month=month(month_year)) %>%
  mutate(month=str_remove(month, "^0+"))%>%
  mutate(key2=paste0(permit_outfall,'_',month,'_',year))



# impute time series missing data -----------------------------------------

#rounding monthly N loads
dat$kg_N_TN_per_month<-round(dat$kg_N_TN_per_month,2)


#drop day from month year format
dat$month_year<-yearmonth(dat$month_year)
 
# #calculate rolling mean in separate column
# dat<-dat %>%
#   group_by(permit_outfall) %>% 
#   arrange(month_year) %>%
#   mutate(rollingmean=rollmean(kg_N_TN_per_month, k=6, fill=NA)) #calculate rolling mean
# 
# ggplot(dat, aes(x=kg_N_TN_per_month, y=rollingmean))+
#   geom_point()+
#   geom_abline(slope=1)+
#   annotate('text', x=10^5,y=10^5.8,label='R2= 0.99', fontface=2)
# 
# summary(lm(dat$rollingmean~dat$kg_N_TN_per_month))

#need to remove a few duplicate observations from different outfalls that were combined
dat %>%
  select(permit_outfall, Outfall, month_year, kg_N_TN_per_month) %>%
  duplicates(key = permit_outfall, index=month_year) 


dat<-dat %>%
  mutate(key_remove=paste0(permit_outfall,Outfall,month_year)) %>%
  filter(!key_remove=="NY0021750_122004 Apr") %>%
  filter(!key_remove=="NY0206644_122004 Mar") %>%
  filter(!key_remove=="NY0206644_122004 Apr") %>%
  filter(!key_remove=="NY0206644_122004 May") %>%
  select(-key_remove)

#create tsibble object
dat_ts<-as_tsibble(dat, key = permit_outfall, index=month_year)
dat_ts

#inspecting missingingness
has_gaps(dat_ts)
ts_gaps<-count_gaps(dat_ts)

#visualizing time gaps
ggplot(ts_gaps, aes(x = permit_outfall)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme_minimal()+
  ggtitle('Data gaps in time')+
  theme(legend.position = "bottom")


# identifying permits that stop reporting early ---------------------------
ts_end_date<-dat %>%
             summarize(max(month_year)) #the time series end date

permits_early_end_date<-dat %>%
  group_by(permit, facility) %>%
  summarise(end_date=max(month_year)) %>%
  filter(end_date<ts_end_date[[1]])

permits_early_end_date

#write_csv(permits_early_end_date, 'monitoring early end dates.csv')

# filling in time gaps with imputeTS ----------------------

#impute missing values within the bounds of the original time series for each outfall
#with imputeTS "na_kalman" function 
full_ts<- dat_ts %>%
  select(permit_outfall,month_year,kg_N_TN_per_month) %>% #removing unnecessary columns
  fill_gaps(.full = FALSE) %>% #fill in NAs in missing months within the bounds of each outfall's time series
  group_by_key() %>% 
  mutate(imputed_missing_value=if_else(is.na(kg_N_TN_per_month), 1, 0)) %>% #designate imputed values as "1"
  mutate(kg_N_TN_per_month_complete=kg_N_TN_per_month) %>% #first, copy over original data
  mutate(kg_N_TN_per_month_complete=na_kalman(kg_N_TN_per_month)) #impute missing data in "kalman imputed"


#impute missing values within the bounds of the original time series for each outfall
#with imputeTS "na_kalman" function 
end_dates_imputation<- full_ts %>%
  filter( permit_outfall %in% c('NY0022128_1','NY0023311_1', 'NY0026999_3')) %>%
  select(permit_outfall,month_year,kg_N_TN_per_month) %>% #removing unnecessary columns
  fill_gaps(.end = max(full_ts$month_year)) %>% #fill in NAs in missing months to the end of the time series
  group_by_key() %>% 
  mutate(imputed_missing_value=if_else(is.na(kg_N_TN_per_month), 1, 0)) %>% #designate imputed values as "1"
  mutate(kg_N_TN_per_month_complete=kg_N_TN_per_month) %>% #first, copy over original data
  mutate(kg_N_TN_per_month_complete=na_kalman(kg_N_TN_per_month)) #impute missing data in "kalman imputed"

end_dates_imputation

full_ts<- bind_rows(as_tibble(full_ts), as_tibble(end_dates_imputation))
full_ts<-distinct(full_ts) #drop duplicated rows caused by previous step
full_ts<-as_tsibble(full_ts, key = permit_outfall, index=month_year) #turn back into tsibble

#inspecting missingingness
summary(has_gaps(full_ts)$.gaps)
#now there are no time series gaps in full_ts object

# backfill each time series further back in time --------------------------------------------------------------------


#next we want to determine the length of data that need to be filled in for each
#permit_outfall time series for the beginning of the series

#bootstrapping approach to fill in missing data from before when data 
# was available;
set.seed(4561)

# #create small df with permit/outfall and date of first observation
# min_year<-as_tibble(full_ts) %>%
#           drop_na(kg_N_TN_per_month) %>% #drop NAs in kg_N_TN_per_month
#           group_by(permit_outfall) %>%
#           select(permit_outfall,month_year) %>%
#           summarise(min_time_step=min(month_year)) %>%
#           mutate(time_gap_interval=min_time_step-min(min_time_step)) #calculate length of time interval to fill in later
# min_year

#create empty rows for missing data from the beginning of the time series to fill later
start_ts_NA <-full_ts %>%
  fill_gaps(.full = start()) %>% #fill in NAs in missing months to the same starting point
  mutate(imputed_missing_value=if_else(is.na(kg_N_TN_per_month), 1, 0)) %>% #designate imputed values as "1"
  filter(is.na(kg_N_TN_per_month_complete)) %>%
  select(-kg_N_TN_per_month_complete)

#summarizes number of time gaps to fill per permit/outfall
start_ts_NA_gaps<-as_tibble(start_ts_NA) %>%
  group_by(permit_outfall) %>%
  summarise(nrow_fill_gaps=n())

start_ts_NA_gaps

#################################################
          
#sample from first 3 years of each distribution (n=nrow_fill_gaps from previous step)
data_first_3yr<-
  as_tibble(full_ts) %>%
  ungroup() %>%
  drop_na(kg_N_TN_per_month) %>% #drop NAs in kg_N_TN_per_month
  mutate(year=year(month_year)) %>%
  group_by(permit_outfall) %>% 
  filter(year>=min(year) & year<=min(year)+3) %>% #filter each group to first 3 years
  select(permit_outfall,kg_N_TN_per_month)  %>%
  left_join(start_ts_NA_gaps) 


nested_dat<-data_first_3yr %>%
  nest(data = (kg_N_TN_per_month))
  
nested_dat

bootstrap_first_3yr <- nested_dat %>%
  drop_na(nrow_fill_gaps) %>% #drop the one permit/outfall that isn't missing anything
  #sample the data column (observations from first three years of data) using the
  #nrow_fill_gaps column as the size to create the right size tibble to rejoin
  mutate(samp = map2(data, nrow_fill_gaps, sample_n, replace=T)) %>% 
  select(-data, -nrow_fill_gaps) %>% #remove unwanted columns
  unnest(cols=c(samp)) %>%
  group_by(permit_outfall) %>%
  mutate(n = row_number()) %>% #number rows by group
  mutate(permit_outfall_n=paste0(permit_outfall,'_',n)) %>% #add unique row ID %>%
  mutate(boostrapped_dat=kg_N_TN_per_month) %>%
  select(-n) 

#check the dimensions are the same between the bootstrapped values and the 
#time gap interval that we are supposed to fill
sum(start_ts_NA_gaps$nrow_fill_gaps)
dim(bootstrap_first_3yr)
dim(start_ts_NA)

summary(bootstrap_first_3yr$boostrapped_dat)

# insert bootstrapped data into empty start_ts_NA dataframe  --------------

#check the length of both dataframes and that all data are missing from start_ts_NA
nrow(bootstrap_first_3yr)
nrow(start_ts_NA)
summary(start_ts_NA$kg_N_TN_per_month)

unique(bootstrap_first_3yr$permit_outfall)
unique(start_ts_NA$permit_outfall)

#join bootstrapped data back to time series data data frame
start_ts_NA2<-
  as_tibble(start_ts_NA) %>%
  group_by(permit_outfall) %>%
  mutate(n = row_number()) %>%
  mutate(permit_outfall_n=paste0(permit_outfall,'_',n)) %>% #add unique row ID
  left_join(bootstrap_first_3yr, by="permit_outfall_n") %>% #join the bootstrap tibble
  mutate(kg_N_TN_per_month_complete=boostrapped_dat)  %>% #overwrite the NA values with the bootstrapped values 
  mutate(permit_outfall= permit_outfall.x,
         kg_N_TN_per_month=kg_N_TN_per_month.x) %>%
  select(permit_outfall, month_year, kg_N_TN_per_month,
          imputed_missing_value, kg_N_TN_per_month_complete)

start_ts_NA2

#checking that there are the same number of NAs in this column
summary(start_ts_NA$kg_N_TN_per_month)
summary(start_ts_NA2$kg_N_TN_per_month)

#confirming there are no NAs in this column
summary(start_ts_NA2$kg_N_TN_per_month_complete)


# rejoin the boostrapped, early observations to the later observations --------

full_ts<-as_tibble(full_ts) 
nrow(full_ts) #31k
summary(full_ts$kg_N_TN_per_month_complete) #no NAs


full_ts<-bind_rows(full_ts,start_ts_NA2)
nrow(full_ts) #53k
summary(full_ts$kg_N_TN_per_month_complete) #no NAs



#quantify how many data were imputed
((table(full_ts$imputed_missing_value)[[2]])/
    (table(full_ts$imputed_missing_value)[[1]]+
       table(full_ts$imputed_missing_value)[[2]]))*100
#48% of the data

full_ts



# rejoin other data to completed time series ------------------------------

#organizing permit, facility, Outfall, and state by each permit_outfall to rejoin 
data_to_rejoin<- dat %>%
  group_by(permit_outfall) %>%
  summarise(facility=first(facility),
            permit=first(permit),
            outfall=first(Outfall),
            state=first(state),
            huc8=first(huc8),
            name=first(name),
            long=first(LONGITUDE83),
            lat=first(LATITUDE83),
            water_body=first(STATE_WATER_BODY_NAME)
            )%>%
  mutate(facility_outfall=paste(facility,outfall))

#rejoin to full_ts
full_ts<-left_join(full_ts,data_to_rejoin)

#add column about outlier imputation 
data_to_rejoin2<-dat %>%
  select(permit_outfall, month_year, outlier, season, -name) %>%
  mutate(outlier= case_when(outlier == "FALSE" ~ 0,
                            outlier == "TRUE" ~ 1)
  )

#rejoin to full_ts
full_ts<-left_join(full_ts,data_to_rejoin2)
names(full_ts)


# exclude data before permit origination date ----------------------------


#load permit origination data
permit_dates<-read_csv(file=here("data","permit_origination_dates.csv"))
permit_dates<-permit_dates %>%
              mutate(orig_issue_dt=ymd(orig_issue_dt)) %>%           
              select(permit, orig_issue_dt) #select columns needed
permit_dates #some fail to parse because not all permits had origination dates

#join permit dates 
full_ts<-left_join(full_ts,permit_dates)

#check whether any observations from before the permit origin issue date
check_permit_dates<-full_ts %>%
  group_by(permit) %>%
  mutate(obs_date=as_date(month_year))%>%
  filter(obs_date<=orig_issue_dt) %>% #remove early data from before permit origination
  mutate(remove_ID=paste0(permit_outfall,month_year)) #create a tag to remove these rows
nrow(check_permit_dates)
#441 observations do come before permit observation date and should be excluded

observations_to_remove=check_permit_dates$remove_ID

#remove observations that precede permit origination date
nrow(full_ts) #50,776

full_ts<-full_ts %>%
  mutate(check_ID=paste0(permit_outfall,month_year)) %>%
  filter(!check_ID %in% observations_to_remove) %>% #remove observations from before or at permit origin issue date
  select(-check_ID) #then remove those columns

nrow(full_ts) #50,335    

#441 observations removed

# join TMDL zones to data and add water year-------------------------------------------------

TMDL_zones<-read_csv(file=here("data",'TMDL_zones.csv'))

full_ts<-left_join(full_ts,TMDL_zones)

#calculate water year
full_ts$water_year<-calcWaterYear(full_ts$month_year)


# check which outfalls end before present ---------------------------------
end_ts_all<-max(full_ts$month_year)

full_ts %>%
  group_by(permit_outfall) %>%
  summarise(end_ts=max(month_year)) %>%
  filter(end_ts<end_ts_all)


# plot time series --------------------------------------------------------

#plot CT facilities only with imputed vs. not in different color
full_ts %>%
  filter(state=="CT") %>%
  ggplot(aes(x=as.Date(month_year), y=kg_N_TN_per_month_complete/1000)) +
  geom_point(aes(col=as.factor(imputed_missing_value)),  shape = 20)+
  ylab('monthly total N load by outfall (1,000 kg N/mo)')+
  ggtitle("Monthly totals with missing data imputed for Connecticut's WWTPs")+
  theme(legend.position = "top")+
  xlab('Date')+
  scale_x_date(breaks=as.Date(x=c("1995-01-01", "2005-01-01", "2015-01-01"),
                              format = "%Y-%m-%d"), 
               date_labels='%Y')+
  theme_minimal()+
  scale_color_manual(name="data origin", 
                     labels=c("original value", "imputed value"), 
                     values = c('dodgerblue4','tomato4'))+
  theme(legend.position = "top")+
  guides(color = guide_legend(override.aes = list(size=3)))+
  facet_wrap(~facility_outfall, scale="free_y", labeller = label_wrap_gen(20))

#plot CT facilities only with simple lines
full_ts %>%
  filter(state=="CT") %>%
  ggplot(aes(x=as.Date(month_year), y=kg_N_TN_per_month_complete/1000)) +
  geom_line()+
  ylab('monthly total N load by outfall (1,000 kg N/mo)')+
  ggtitle("Monthly totals with missing data imputed for Connecticut's WWTPs")+
  theme(legend.position = "top")+
  xlab('Date')+
  scale_x_date(breaks=as.Date(x=c("1995-01-01", "2005-01-01", "2015-01-01"),
                              format = "%Y-%m-%d"), 
               date_labels='%Y')+
  theme_minimal()+
  scale_color_manual(name="data origin", 
                     labels=c("original value", "imputed value"), 
                     values = c('dodgerblue4','tomato4'))+
  theme(legend.position = "top")+
  guides(color = guide_legend(override.aes = list(size=3)))+
  facet_wrap(~facility_outfall, scale="free_y", labeller = label_wrap_gen(20))


#plot non-CT facilities with imputed vs. not in different color
full_ts %>%
  filter(state!="CT") %>%
  ggplot(aes(x=as.Date(month_year), y=kg_N_TN_per_month_complete/1000)) +
  geom_point(aes(col=as.factor(imputed_missing_value)),  shape = 20)+
  ylab('monthly total N load by outfall (1,000 kg N/mo)')+
  ggtitle("Monthly totals with missing data imputed for all other states' WWTPs")+
  theme(legend.position = "top")+
  xlab('Date')+
  scale_x_date(breaks=as.Date(x=c("1995-01-01", "2005-01-01", "2015-01-01"),
                              format = "%Y-%m-%d"), 
               date_labels='%Y')+
  theme_minimal()+
  scale_color_manual(name="data origin", 
                     labels=c("original value", "imputed value"), 
                     values = c('dodgerblue4','tomato4'))+
  theme(legend.position = "top")+
  guides(color = guide_legend(override.aes = list(size=3)))+
  facet_wrap(~facility_outfall, scale="free_y", labeller = label_wrap_gen(20))


#plot non-CT facilities with simple lines
full_ts %>%
  filter(state!="CT") %>%
  ggplot(aes(x=as.Date(month_year), y=kg_N_TN_per_month_complete/1000)) +
  geom_line(col="grey10")+
  ylab('monthly total N load by outfall (1,000 kg N/mo)')+
  ggtitle("Monthly totals with missing data imputed for all other states' WWTPs")+
  theme(legend.position = "top")+
  xlab('Date')+
  scale_x_date(breaks=as.Date(x=c("1995-01-01", "2005-01-01", "2015-01-01"),
                              format = "%Y-%m-%d"), 
               date_labels='%Y')+
  theme_minimal()+
  scale_color_manual(name="data origin", 
                     labels=c("original value", "imputed value"), 
                     values = c('dodgerblue4','tomato4'))+
  theme(legend.position = "top")+
  guides(color = guide_legend(override.aes = list(size=3)))+
  facet_wrap(~facility_outfall, scale="free_y", labeller = label_wrap_gen(20))


full_ts$date<-as.Date(full_ts$month_year)
full_ts$watershed_name<-full_ts$name
 
#remove one column 
full_ts<-select(full_ts, -name)

#reformat outlier column class
full_ts$outlier<-as.factor(full_ts$outlier)

zipfunc <- function(df, zippedfile) {
  # write temp csv
  temp_filename = 'complete_time_series_with_missing_data_imputed.csv'
  write_csv(df, file=temp_filename)
  # zip temp csv
  zip(zippedfile,temp_filename)
  # delete temp csv
  unlink(temp_filename)
}

zipfunc(df=full_ts,zippedfile=here("data",'complete_time_series_with_missing_data_imputed.zip'))


# write out summary of location data --------------------------------------
location_summary<-as.tibble(full_ts) %>%
  ungroup()%>%
  group_by(permit_outfall)%>%
  summarize(permit_outfall=first(permit_outfall),
            permit=first(permit),
            outfall=first(outfall),
            facility=first(facility),
            long=first(long),
            lat=first(lat),
            state=first(state),
            huc8=first(huc8),
            watershed_name=first(watershed_name),
            TMDL_zone=first(TMDL_zone))

location_summary

write_csv(location_summary,here("data",'complete_time_series_location_summary.csv'))
