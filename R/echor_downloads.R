

library(echor)
library(tidyverse)

df <- tibble::tibble("id" = c('CT0000086','CT0000434',
                              'CT0000582','CT0000744')
                     )
                      
df <- downloadDMRs(df, id)
df<-df$dmr
#df<-as_tibble(df)
# df<-unnest(df)


test<-df[[1]]
str(test)

#link to data dictionary https://echo.epa.gov/help/reports/effluent-charts-help

levels(as.factor(test$npdes_id)) #want this
levels(as.factor(test$perm_feature_id)) #outfall IDs
levels(as.factor(test$perm_feature_nmbr)) #this is outfall number
levels(as.factor(test$perm_feature_type_code))
levels(as.factor(test$perm_feature_type_desc))
levels(as.factor(test$parameter_code))
levels(as.factor(test$parameter_desc))
levels(as.factor(test$monitoring_location_code)) #we want to filter this to level 0 only, effluent gross
levels(as.factor(test$monitoring_location_desc))
levels(as.factor(test$standard_unit_desc)) #these are the standardized units
levels(as.factor(test$statistical_base_short_desc)) #this is the stat
levels(as.factor(test$dmr_event_id)) # The unique ID identifying the DMR Event. A DMR Event is a DMR submission with a DMR period end date and DMR due date
levels(as.factor(test$monitoring_period_end_date)) #this is the month
levels(as.factor(test$dmr_form_value_id)) #not sure if we need this 
levels(as.factor(test$dmr_value_id)) #not sure if we need this
levels(as.factor(test$dmr_value_nmbr)) #this might be the data?
levels(as.factor(test$dmr_unit_desc)) #here are units
levels(as.factor(test$dmr_value_standard_units)) #this might be data?
levels(as.factor(test$dmr_value_qualifier_code)) #this shows <>? qualifier codes




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
    outfall,
    perm_feature_nmbr,
    parameter_desc,
    standard_unit_desc,
    monitoring_period_end_date,
    dmr_value_standard_units
  ) %>%
  filter(monitoring_location_code == "0") %>%
  filter(parameter_desc  %in% params)



