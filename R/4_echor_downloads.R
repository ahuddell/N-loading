

library(echor)
library(tidyverse)
library(here)

#read in list of permits to request
ids<-read_csv(here('data','data_request_LIS_AH.csv'))


ids_1_1000<-ids[1:500,]
df1 <- downloadDMRs(ids_1_1000, NPDES_ID, verbose=TRUE)
df1 <- df1 %>%
  tidyr::unnest(dmr)


ids_501_1000<-ids[501:1000,]
df2 <- downloadDMRs(ids_501_1000, NPDES_ID, verbose=TRUE)
df2 <- df2 %>%
  tidyr::unnest(dmr)

ids_1001_1500<-ids[1001:1500,]
df3 <- downloadDMRs(ids_1001_1500, NPDES_ID, verbose=TRUE)
df3 <- df3 %>%
  tidyr::unnest(dmr)


ids_1501_1851<-ids[1501:1851,]
df4 <- downloadDMRs(ids_1501_1851, NPDES_ID, verbose=TRUE)
df4 <- df4 %>%
  tidyr::unnest(dmr) 

df4 <- df4 %>%  select(-"{")

dat1<-rbind(df1,df2) 
dat2<-rbind(df3,df4) 

dat1<-dat1%>% 
  filter(perm_feature_type_code=="EXO") %>%
  select(npdes_id,
         date=monitoring_period_end_date,
         outfall=perm_feature_nmbr,
         parameter_code,	
         parameter_desc,	
         monitoring_location_code,
         monitoring_location_desc,
         standard_unit_desc,
         statistical_base_code,	
         statistical_base_short_desc,	
         statistical_base_type_code,	
         statistical_base_type_desc,
         dmr_value_standard_units,
         dmr_value_qualifier_code)
#write_csv(dat1,'ECHOR_DL_1.csv')

dat2<-dat2%>% 
  filter(perm_feature_type_code=="EXO") %>%
  select(npdes_id,
         date=monitoring_period_end_date,
         outfall=perm_feature_nmbr,
         parameter_code,	
         parameter_desc,	
         monitoring_location_code,
         monitoring_location_desc,
         standard_unit_desc,
         statistical_base_code,	
         statistical_base_short_desc,	
         statistical_base_type_code,	
         statistical_base_type_desc,
         dmr_value_standard_units,
         dmr_value_qualifier_code)
write_csv(dat2,'ECHOR_DL_2.csv')          

#test<-echoGetEffluent(p_id='NY0201235')

names(dat)
min(dat$monitoring_period_end_date)
max(dat$monitoring_period_end_date)

echoWaterGetFacilityInfo(p_huc='01080101') #function that returns huc8 watershed codes: p_huc



#link to data dictionary https://echo.epa.gov/help/reports/effluent-charts-help

levels(as.factor(dat$npdes_id)) #want this
levels(as.factor(dat$perm_feature_id)) #outfall IDs
levels(as.factor(dat$perm_feature_nmbr)) #this is outfall number
levels(as.factor(dat$perm_feature_type_code)) #we should filter to "EXO"
levels(as.factor(dat$perm_feature_type_desc))
levels(as.factor(dat$parameter_code))
levels(as.factor(dat$parameter_desc))
levels(as.factor(dat$monitoring_location_code)) #we want to filter this to level 0 only, effluent gross
levels(as.factor(dat$monitoring_location_desc))
levels(as.factor(dat$standard_unit_desc)) #these are the standardized units
levels(as.factor(dat$statistical_base_short_desc)) #this is the stat
levels(as.factor(dat$dmr_event_id)) # The unique ID identifying the DMR Event. A DMR Event is a DMR submission with a DMR period end date and DMR due date
levels(as.factor(dat$monitoring_period_end_date)) #this is the month
levels(as.factor(dat$dmr_form_value_id)) #not sure if we need this 
levels(as.factor(dat$dmr_value_id)) #not sure if we need this
levels(as.factor(dat$dmr_value_nmbr)) #this might be the data?
levels(as.factor(dat$dmr_unit_desc)) #here are units
levels(as.factor(dat$dmr_value_standard_units)) #this might be data?
levels(as.factor(dat$dmr_value_qualifier_code)) #this shows <>? qualifier codes




# need to convert  # > levels(as.factor(test$standard_unit_desc)) #these are the standardized units
# [1] "%"         "d"         "deg F"     "kg"        "kg/d"      "mg/L"      "MGD"  = millions of gallons per day    
# [8] "MPN/100mL" "SU" 

#to our standardize units to kg per month, million L per month, or milligram per liter 
#eventually we may want to summarize as pounds per day


params <- c(
  "Flow",
  "Flow rate" ,
  "Flow, maximum during 24 hr period",
  "Nitrogen, ammonia total [as N]",
  "Nitrogen, Kjeldahl, total [as N]",
  "Nitrogen, nitrate total [as N]",
  "Nitrogen, nitrite total [as N]",
  "Nitrogen, organic total [as N]",
  "Nitrogen, Total",
  "Nitrogen, total [as N]"
)


test2 <- test %>%
  select(
    npdes_id,
    perm_feature_nmbr,
    parameter_desc,
    standard_unit_desc,
    monitoring_period_end_date,
    dmr_value_standard_units,
    statistical_base_short_desc,
    monitoring_location_code
  ) %>%
  filter(monitoring_location_code == "0") %>%
  filter(parameter_desc  %in% params)

#plot the frequency of different parameters
test2$stat<-as.factor(test2$"statistical_base_short_desc")
levels(test2$stat)

levels<-test2 %>%
  group_by(stat) %>%
  tally()

ggplot(levels, aes(stat,n))+
  geom_col() +
  ylab("count of observations")+
  theme(axis.text.x=element_text(angle = 30, hjust=1))


