library(here)
## Read online table

### Download table (*.csv`)
#set variables
csv_url <- "http://merlin.dms.uconn.edu:8080/erddap/tabledap/DEEP_WQ.csv?cruise_name%2Cstation_name%2Ctime%2Clatitude%2Clongitude%2Cdepth_code%2Cdepth%2Csea_water_pressure%2Csea_water_electrical_conductivity%2Csea_water_temperature%2Coxygen_sensor_temp%2CPAR%2CChlorophyll%2CCorrected_Chlorophyll%2CpH%2Csea_water_salinity%2Coxygen_concentration_in_sea_water%2Cwinkler%2Ccorrected_oxygen%2Cpercent_saturation_100%2Cpercent_saturation%2Csea_water_density%2CStart_Date%2CEnd_Date%2CTime_ON_Station%2CTime_OFF_Station"
#dir_data<- "data"
csv_url <- "http://merlin.dms.uconn.edu:8080/erddap/tabledap/DEEP_WQ.csv"

library(data.table)
mydat <- fread(csv_url)


# Specify destination where file should be saved
dlfile <- here('data','DEEP_WQ.csv')
read.csv(url)

library(RCurl)
myfile <- getURL(csv_url)

#derived variables
csv <- file.path(dir_data, basename(csv_url))

#create directory
dir.create(dir_data, showWarnings = F)

#download file
if(!file.exists(csv))
  download.file(csv_url, csv)

# attempt to read csv
d <- read.csv(csv, skip = 2, header = FALSE)
#show data frame
d
# update data frame to original column names
names(d) <- names(read.csv(csv))
d