library(here)
## Read online table

### Download table (*.csv`)
#set variables
csv_url <- "http://merlin.dms.uconn.edu:8080/erddap/tabledap/DEEP_WQ.csv"
# Specify destination where file should be saved
destfile <- here('data','DEEP_WQ.csv')

#download file
if(!file.exists(destfile))
  download.file(url=csv_url, destfile=destfile)

library(rerddap)
rerddap::tabledap(x='DEEP_WQ',
                  fields = c('time','station_name', 'longitude', 'latitude', 'pH', 
                             'corrected_oxygen'),
                  url='http://merlin.dms.uconn.edu:8080/erddap/index.html?page=1&itemsPerPage=1000')

# #this didn't work for me
# library(data.table)
# mydat <- fread(csv_url)

# library(RCurl)
# myfile <- getURL(csv_url)

# attempt to read csv
d <- read.csv(csv, skip = 2, header = FALSE)
#show data frame
d
# update data frame to original column names
names(d) <- names(read.csv(csv))
d