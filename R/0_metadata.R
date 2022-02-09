library(EML)


#creating EML
#following this vingette https://cran.r-project.org/web/packages/EML/vignettes/creating-EML.html

#creating custom unit
custom_units <- 
data.frame(id = "kilogramsNitrogenPerMonth", 
           unitType = c("otherUnitType", "otherUnitType"), 
           parentSI = c('kilogram', 'degree'),
           multiplierToSI = c(NA, NA),
           abbreviation = c('kg_N/month','decimal_degree'),
           description = c("kilograms of nitrogen per month", 
                          "decimal degrees = degrees + (minutes/60) + (seconds/3600)")
            )


unitList <- set_unitList(custom_units)


#attribute metadata
attributes <-
tibble::tribble(
  ~attributeName, ~attributeDefinition,                                                                                                                     ~formatString,               ~definition,        ~unit,                            ~numberType,
    'permit_outfall', 'unique identifer for each outfall',                                                                                                  'EPA NPDES ID_outfall number',   NA,                NA,                             NA,
    'month_year', 'year (YYYY) and month (Jan, Feb, etc.) ',                                                                                                 'YYYY mth',                     NA,                NA,                             NA,          
    'kg_N_TN_per_month','total nitrogren load from outfall effluent',                                                                                          NA,                           NA,               'kg_N/month',                   'real',
    'rollingmean', 'rolling mean of total nitrogren load from outfall effluent, calculated using prior 3 and following 3 observations (when available)',      NA,                           NA,                'kg_N/month',                   'real',
    'imputed_missing_value',   'imputed value binary variable where missing data are designated as 1 and non-missing values are designated as 0,',            NA,                           NA,                'unitless',                              NA,
    'kg_N_TN_per_month_complete','column with complete data where original obeservations are combined with imputed data for missing data or outliers, with rolling mean used for missing data, and within-seasonal, within-outfall mean imputed for outliers',         NA,                          NA,                'kg_N/month',       'real',
    'facility','wastewater treatment plant or industrial site name',                                                                                           'name',                       NA,               NA,                              NA,
    'permit', 'U.S. Environmental Protection Agency (EPA) National Pollutant Discharge Elimination System (NPDES) permit number',                              'permit ID',                  NA,                  NA,                           NA, 
    'outfall','facility outfall number',                                                                                                                        'outfall number',             NA,                NA,                           NA,   
    'state', 'state abbreviation i.e. New York (NY)',                                                                                                          'state abbreviation',         NA,               NA,                             NA,
    'huc8',  'HUC 8 ID from the National Watershed Boundary Database',                                                                                         'HUC8 ID' ,                   NA,               NA,                              NA, 
    'long', 'longtitude coordinates in decimal degrees',                                                                                                        NA,                           NA,            'decimal_degree',                 NA,      
    'lat', 'latitude coordinates in decimal degrees',                                                                                                           NA,                           NA,            'decimal_degree',                 NA,   
    'water_body', 'water body that outfall discharges to from the EPA ICIS-NPDES Discharge Points dataset',                                                     'waterbody name',             NA,           NA,                                 NA,
    'facility_outfall', 'facility and outfall identifiers combined with underscore',                                                                            'facility name_outfall number', NA,         NA,                                 NA,   
    'outlier', 'binary variable where outliers are designated as 1 and non-outliers are designated as 0',                                                       NA,                            NA,           'unitless',                        NA,
    'season', 'calendar season, winter (Dec., Jan., Feb.), spring (Mar., Apr, May), summer (Jun., Jul., Aug.), fall (Sep., Oct., Nov.)',                        NA,                            NA,           'unitless',                        NA,
    'TMDL_zone','Total Maximum Daily Load (TMDL) zones for the Long Island Sound',                                                                              'TMDL zone number',            NA,           NA,                                NA, 
    'water_year', 'water year, calculated with the calcWaterYear function of the dataRetrieval R package',                                                       NA,                          'year',        NA,                                'year', 
    'date','date YYYY-MM-DD',                                                                                                                                    'YYYY-MM-DD year-month-day',  NA,           NA,                                NA,
    'watershed_name','HUC8 watershed name',                                                                                                                       'watershed name',            NA,           NA,                                NA,
    )

# define factor variables
imputed_missing_value <- c('0' = "not a missing value",
                           '1' = "missing value")

outlier <- c('0' = "not an outlier",
            '1' = "outlier")
season <- c(
  winter  = "winter season (Dec., Jan., Feb.)",
  spring  = "spring season (Mar., Apr, May)",
  summer  = "summer season (Jun., Jul., Aug.)",
  fall = "fall season (Sep., Oct., Nov.)")


## Write these into the data.frame format
factors <- rbind(
  data.frame(
    attributeName = "imputed_missing_value",
    code = names(imputed_missing_value),
    definition = unname(imputed_missing_value)
  ),
  data.frame(
    attributeName = "outlier",
    code = names(outlier),
    definition = unname(outlier)
  ),
  data.frame(
    attributeName = "season",
    code = names(season),
    definition = unname(season)
  )
)

#attribute list
attributeList <- 
  set_attributes(attributes, factors, 
                 col_classes = c("character", 
                                 "character", 
                                 "numeric", 
                                 "numeric", 
                                 "factor", 
                                 "numeric", 
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character", 
                                 "numeric", 
                                 "numeric",
                                 "character", 
                                 "character", 
                                 "factor", 
                                 "factor",
                                 "character", 
                                 "numeric",
                                 "Date", 
                                 "character")
                 )

physical <- set_physical("complete_time_series_with_missing_data_imputed.csv")


dataTable <- list(
  entityName = "complete_time_series_with_missing_data_imputed.csv",
  entityDescription = "Point source nitrogen loading Long Island Sound Watershed 1989-2021",
  physical = physical,
  attributeList = attributeList)

#coverage metadata
geographicDescription <- "Long Island Sound Watershed"


coverage <- 
  set_coverage(begin = '1989-01-01', end = '2021-09-31',
               geographicDescription = geographicDescription,
               west = -73.98, east = -71.53, 
               north = 44.59, south = 41.18)

#need to add a methods file in the future
# #methods file
# methods_file <- system.file("examples/hf205-methods.docx", package = "EML")
# methods <- set_methods(methods_file)

R_person <- person(given="Alexandra", 
                   family="Huddell", 
                   email="amh2284@columbia.edu", 
                   role="cre", 
                   comment=c(ORCID = "0000-0002-6289-6290")
                   )
alexandra <- as_emld(R_person)


#set keywords
keywordSet <- list(
    keyword = list("point source nitrogen loading",
                   "U.S. Environmental Protection Agency (EPA)",
                   "National Pollutant Discharge Elimination System (NPDES)",
                   "Long Island Sound Watershed")
  )
  
#header information
pubDate <- "2022" 

title <- "Point source nitrogen loading data for the Long Island Sound Study from 1989-2021"

abstract <- "NOTE: These are provisional data that have not yet undergone peer-review, and 
      improvements to the methods for outlier and missing data imputation are 
      ongoing. The raw data contains monthly estimates of point source nitrogen 
      loading data from wastewater treatment plants in the Long Island Sound Watershed.
      Data that were imputed are designated as such. Please see the metadata for detailed
      descriptions of the data and variable names. Please use this information with 
      caution, and contact amh2284 [at] columbia [dot] edu with any questions."  

intellectualRights <- "This dataset is released to the public and may be freely
  downloaded. Please keep the designated Contact person informed of any
plans to use the dataset. Consultation or collaboration with the original
investigators is strongly encouraged. Publications and data products
that make use of the dataset must include proper acknowledgement."

#compile dataset
dataset <- list(
  title = title,
  creator = alexandra,
  pubDate = pubDate,
  intellectualRights = intellectualRights,
  abstract = abstract,
  keywordSet = keywordSet,
  coverage = coverage,
 # methods = methods, #need to add methods file in future
  dataTable = dataTable,
 additionalMetadata = list(metadata = list(
   unitList = unitList
 )))

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset)

write_eml(eml, "metadata_eml.xml")

eml_validate("metadata_eml.xml")


