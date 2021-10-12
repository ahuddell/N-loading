

library(echor)
library(tidyverse)
library(here)

requestURL <- function(query) {
  
  urlBuildList <- structure(list(scheme = "https",
                                 hostname = "echodata.epa.gov",
                                 port = NULL, 
                                 path = "echo/cwa_rest_services.get_download", 
                                 query = query), 
                            class = "url")
  return(httr::build_url(urlBuildList))
}

getDownload <- function(service, qid, qcolumns, col_types = NULL) {
  ## build the request URL statement
  if (service == "sdw") {
    path <- "echo/sdw_rest_services.get_download"
  } else if (service == "cwa") {
    path <- "echo/cwa_rest_services.get_download"
  } else if (service == "caa") {
    path <- "echo/air_rest_services.get_download"
  } else {
    stop("internal error in getDownload, incorrect service argument supplied")
  }
  qid <- paste0("qid=", qid)
  query <- paste(qid, qcolumns, sep = "&")
  getURL <- requestURL(path = path, query = query)
  
  ## Make the request
  request <- httr::RETRY("GET", getURL)
  
  ## Check for valid response for serve, else returns error
  resp_check(request)
  
  info <- httr::content(request, as = "raw")
  
  info <- readr::read_csv(info, col_names = TRUE,
                          col_types = col_types,
                          na = " ",
                          locale = readr::locale(date_format = "%m/%d/%Y"))
  
  return(info)
}

getDownload(service="cwa", qid=df$id, qcolumns=c(1:11,14,23,24,25,26,30,36,58,60,63,64,65,67,86,206))

#this was my old query that worked
df <- tibble::tibble("id" = c('CT0000086','CT0000434',
                              'CT0000582','CT0000744')
)

df <- downloadDMRs(df, id)
df<-df$dmr

echoGetEffluent(p_id = 'tx0119407', parameter_code = '50050')
#df<-as_tibble(df)
# df<-unnest(df)


library(httr)
r <- GET("http://httpbin.org/get")

ids<-read_csv(here('data','data_request_LIS_AH.csv'))

ids_1_10<-ids[30,]
df <- downloadDMRs(df=ids_1_10, iColumn=id, verbose=TRUE)
df<-df$dmr

library(purrr)    
out_lst <- map(df$url, pfun)
names(out_lst) <- df$ID 
pfun <- possibly(f1, otherwise = NA)
f1 <- function(urllink) {
  openxlsx::read.xlsx(urllink)
}

f2 <-  function(urllink) {
  
  tryCatch(openxlsx::read.xlsx(urllink), 
           error = function(e) message("error occured"))
}
out_lst2 <- lapply(df$url, f2)

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



