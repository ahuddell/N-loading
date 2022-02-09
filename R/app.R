

# load libraries ----------------------------------------------------------
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shiny)
library(tidyverse)
library(RColorBrewer)
library(bslib)
library(leaflet)
library(here)
library(lubridate)
library(sf)
library(viridis)
library(EML)


# read data, objects, and functions ---------------------------------------

dat<-read_csv(file=unzip(zipfile=here('data','complete_time_series_with_missing_data_imputed.zip')))
names(dat)

#read in the rest of the huc8 shapes 
huc8_combined<-st_read(here('data','huc_8_dat_join','huc8_combined.shp'))

#read in metadata XML file
eml <- read_eml(here('data','metadata_eml.xml'))


#group data in different ways
dat_annual<- dat %>%
  group_by(facility, year=year(date), lon=long, lat=lat) %>%
  summarise(kgN_facility_yr=sum(kg_N_TN_per_month_complete,na.rm=T)) %>%
  group_by(facility, lon, lat) %>%
  summarise(kgN_facility_yr=mean(kgN_facility_yr, na.rm=T))
  
dat_annual<- st_as_sf(dat_annual,coords=c('lon','lat'))
dat_annual$radius<-(dat_annual$kgN_facility_yr)/10^6

#join state water body to dat_annual
to_join<-dat %>% 
         group_by(facility) %>% 
         summarise(water_body=first(water_body)) %>%
         mutate(label = paste(
                'Facility:',facility, 
                'Water Body:',water_body), 
                sep =  "<br>")
                
dat_annual<-left_join(dat_annual,to_join)

#group facilities by HUC8 watershed
dat_HUC8<-dat %>%
  group_by(watershed_name, date) %>%
  summarise(kgN_mo=sum(kg_N_TN_per_month_complete,na.rm=T))

#group facilities by state
dat_state<-dat %>%
  group_by(state, date) %>%
  summarise(kgN_mo=sum(kg_N_TN_per_month_complete,na.rm=T))

#group facilities by water body
dat_water_body_map<-dat %>%
  filter(water_body %in% c('CONNECTICUT RIVER',
                           'HOUSATONIC RIVER',
                           'NAUGATUCK RIVER',
                           'FARMINGTON RIVER',
                           'LONG ISLAND SOUND',
                           'QUINEBAUG RIVER',
                           'QUINNIPIAC RIVER',
                           'THAMES RIVER',
                           'EAST R',
                           'MANHASSET BAY',
                           'PAWCATUCK RIVER',
                           'SMITHTOWN BAY')) %>%
  group_by(water_body, facility, date, lon=long, lat=lat) 


radius<-select(as_tibble(dat_annual), radius, facility)
dat_water_body_map<-left_join(dat_water_body_map,radius)
dat_water_body_map<- st_as_sf(dat_water_body_map,coords=c('lon','lat'))


dat_water_body<-dat %>%
  filter(water_body %in% c('CONNECTICUT RIVER',
                           'HOUSATONIC RIVER',
                           'NAUGATUCK RIVER',
                           'FARMINGTON RIVER',
                           'LONG ISLAND SOUND',
                           'QUINEBAUG RIVER',
                           'QUINNIPIAC RIVER',
                           'THAMES RIVER',
                           'EAST R',
                           'MANHASSET BAY',
                           'PAWCATUCK RIVER',
                           'SMITHTOWN BAY')) %>%
  group_by(water_body, date) %>%
  summarise(kgN_mo=sum(kg_N_TN_per_month_complete,na.rm=T))

#color palette
pal=c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

# ui ----------------------------------------------------------------------
ui <- function(request) {
  navbarPage(
    title = tags$a(
      href = 'https://www.longislandsoundstudy.net',
      icon('fish', lib = 'font-awesome'),
      'Long Island Sound Study'
    ),
    theme = shinytheme('lumen'),
     tabPanel(
       'Point Source N loading Aggregated by HUC8 Watershed',
      fluidRow(
        height = 4,
        tags$style(type = 'text/css', '.selectize-input{ z-index: 999; }'),
        #h3('This tool summarizes point source N loading in the Long Island Sound Watershed'),
        column(
          width = 5
              ),
        column(
          width = 7,
          height=4,
          selectizeInput(
            inputId = 'huc8',
            label = 'HUC8 Watershed',
            choices = unique(dat_HUC8$watershed_name),
            multiple = T,
            width='100%',
            selected = 'Outlet Connecticut River'
          )
        )
      ),
      fluidRow(splitLayout(
        cellWidths = c('40%', '60%'),
        leafletOutput('map'),
        plotOutput('plot1')
      )),

    hr(), # add spacer
    
    fluidRow(splitLayout(
      cellWidths = c('40%', '60%'),
      column(
        width = 4,
     # Button
     downloadButton('downloadMetaData', 'Download metadata'),
     downloadButton('downloadData', 'Download raw data')
      )))

      ),
    
    p("NOTE: These are provisional data that have not yet undergone peer-review, and 
      improvements to the methods for outlier and missing data imputation are 
      ongoing. The raw data contains monthly estimates of point source nitrogen 
      loading data from wastewater treatment plants in the Long Island Sound Watershed.
      Data that were imputed are designated as such. Please see the metadata for detailed
      descriptions of the data and variable names. Please use this information with 
      caution, and contact amh2284 [at] columbia [dot] edu with any questions.", 
      style = "font-si16pt"),
    
    
    tabPanel(
    'Point Source N loading Aggregated by State',
    fluidRow(
      height = 4,
      tags$style(type = 'text/css', '.selectize-input{ z-index: 999; }'),
      h3('This tool summarizes point source N loading in the Long Island Sound Watershed'),
      column(
        width = 5
      ),
      column(
        width = 7,
        height=4,
        selectizeInput(
          inputId = 'state_input',
          label = 'State',
          choices = unique(dat_state$state),
          multiple = T,
          width='100%',
          selected = 'NY'
        )
      )
    ),
    
    hr(), # add spacer
    
    fluidRow(splitLayout(
      cellWidths = c('40%', '60%'),
      leafletOutput('map2'),
      plotOutput('plot2')
    )),
    hr(),
    
    tags$h6('The points on the map show wastewater treatment plant locations with the radius of the points proportional to their mean annual nitrogen loads.')
    
  ),
  tabPanel(
    'Point Source N loading Aggregated by Select Rivers and Embayments',
    fluidRow(
      height = 4,
      tags$style(type = 'text/css', '.selectize-input{ z-index: 999; }'),
      # h3('This tool summarizes point source N loading in the Long Island Sound Watershed'),
      column(
        width = 5
      ),
      column(
        width = 7,
        height=4,
        selectizeInput(
          inputId = 'water_body_input',
          label = 'River or Embayment',
          choices = c('Connecticut River'='CONNECTICUT RIVER',
                      'Housatonic River'='HOUSATONIC RIVER',
                      'Naugatuck River'='NAUGATUCK RIVER',
                      'Farmington River'= 'FARMINGTON RIVER',
                      'Long Island Sound'='LONG ISLAND SOUND',
                      'Quinebaug River'= 'QUINEBAUG RIVER',
                      'Quinnipiac River'='QUINNIPIAC RIVER',
                      'Thames River'='THAMES RIVER',
                      'East River'='EAST R',
                      'Manhasset River'='MANHASSET BAY',
                      'Pawcatuck River'='PAWCATUCK RIVER',
                      'Smithtown Bay'='SMITHTOWN BAY'),
          multiple = T,
          width='100%',
          selected = 'CONNECTICUT RIVER'
        )
      )
    ),
    
    hr(), # add spacer
    
    fluidRow(splitLayout(
      cellWidths = c('40%', '60%'),
      leafletOutput('map3'),
      plotOutput('plot3')
    )),
    hr(),
    
    tags$h6('The points on the map show wastewater treatment plant locations with 
            the radius of the points proportional to their mean annual 
            nitrogen loads and colored by the water body discharged to.')
    
  )
  
  )
}






# server ------------------------------------------------------------------

# Server
server <- function(input, output, session) {

#tab 1
  #mouseover labels
  labs <- as.list(dat_HUC8$watershed_name)
  
  # render base map
  output$map <- renderLeaflet({
    leaflet(huc8_combined) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addPolygons( fillColor = ~ 'lightgrey',
                   weight = 1,
                   color = 'black',
                   opacity = 1,
                   fillOpacity = .8,
                   popup = paste('Watershed name:',huc8_combined$name),
                   label = paste('Watershed name:',huc8_combined$name)
                       )
  })
  
  # Reactive expression for the data filtered to what the user selected
  filtered_watershed_name <- reactive({
    dat_HUC8 %>%
    filter(watershed_name %in% input$huc8)
  })
  
  # render plot1
  output$plot1 <- renderPlot({
    ggplot(data = filtered_watershed_name(),
      aes(x = date, y = kgN_mo/1000, col = watershed_name,
          fill=watershed_name)) +
      ylab(expression(paste(
        'N load (1,000 kg N',~~ha^-1,month^-1,')')))+
      xlab('Date')+
      geom_line(alpha = 0.9, size=1) +
      #geom_smooth(method = 'lm', se = TRUE, linetype='dashed', alpha=.4, size=1.2) +
      theme_minimal() +
      theme(text = element_text(size=18),
            legend.position = 'bottom')+
      scale_color_manual(name='Watershed', 
                         breaks= c("Housatonic",                      
                                   "Outlet Connecticut River",        
                                    "Northern Long Island",         
                                    "Long Island Sound",               
                                    "East River",                      
                                    "Bronx",                           
                                    "Quinebaug River",                 
                                    "Chicopee River",
                                    "Millers River",                   
                                    "Deerfield River",                 
                                    "Ashuelot River-Connecticut River",
                                    "Westfield River",                 
                                    "Black River-Connecticut River",   
                                    "Headwaters Connecticut River",
                                    "Saugatuck",                       
                                    "Farmington River",                
                                    "Quinnipiac",                      
                                    "Thames",                          
                                    "Shetucket River",                 
                                    "Pawcatuck River"),
                         labels =  c("Housatonic River",                      
                                  "Outlet Connecticut River",        
                                  "Northern Long Island",         
                                  "Long Island Sound",               
                                  "East River",                      
                                  "Bronx River",                           
                                  "Quinebaug River",                 
                                  "Chicopee River",
                                  "Millers River",                   
                                  "Deerfield River",                 
                                  "Ashuelot River-Connecticut River",
                                  "Westfield River",                 
                                  "Black River-Connecticut River",   
                                  "Headwaters Connecticut River",
                                  "Saugatuck River",                       
                                  "Farmington River",                
                                  "Quinnipiac River",                      
                                  "Thames River",                          
                                  "Shetucket River",                 
                                  "Pawcatuck River"),
                         values = pal)
   })
  # Downloadable csv of selected dataset 
  output$downloadMetaData <- downloadHandler(
    filename = function() {'metadata_eml.xml'},
    content = function(file) {
      EML::write_eml(eml, file)
    })
      

  
  # Downloadable csv of selected dataset 
  output$downloadData <- downloadHandler(
    filename = function() {paste('Long_Island_Sound_WWTP_data', ' ',Sys.Date(),'.csv',sep='')},
    content = function(file) {
      write.csv(dat, 
                file, 
                row.names = FALSE)})
  
  #tab 2
  # render base map2
  output$map2 <- 
    renderLeaflet({
      leaflet(dat_annual) %>%
      addProviderTiles('CartoDB')  %>% 
      setView(lng = -73.52, lat = 42.98,  zoom = 6) %>%
        addCircleMarkers(data = dat_annual$geometry,
                         popup =paste(sep = "<br/>",
                                      paste('Facility:',  dat_annual$facility),
                                      paste('Water Body:',dat_annual$water_body)
                                      ),
                        label = paste('Facility:',  dat_annual$facility, 
                                      'Water Body:',dat_annual$water_body
                                      ),
                         radius = dat_annual$radius,
                        fillOpacity = 1
                        )
                })
  
  # Reactive expression for the data filtered to what the user selected
  filtered_state <- reactive({
    dat_state %>%
      filter(state %in% input$state_input)
  })
  # render plot2
  output$plot2 <- renderPlot({
    ggplot(data = filtered_state(),
           aes(x = date, y = kgN_mo/1000, col = state,
               fill=state)) +
      ylab(expression(paste(
        'N load (1,000 kg N',~ha^-1,mo^-1,')')))+
      xlab('Date')+
      geom_line(alpha = 0.7, size=1) +
      #geom_smooth(method = 'lm', se = FALSE, linetype='dashed', alpha=.4, size=.6) +
      theme_minimal() +
      theme(text = element_text(size=18),
            legend.position = 'bottom')+
      scale_color_viridis(name='State', discrete=TRUE) +
      scale_fill_viridis(name='State', discrete=TRUE)
  })
  
  #tab 3
  # render base map3
  output$map3 <- 
    renderLeaflet({
      leaflet(dat_water_body_map) %>%
        addProviderTiles('CartoDB')  %>% 
        setView(lng = -73.52, lat = 42.98,  zoom = 6) %>%
        addCircleMarkers(data = dat_water_body_map$geometry,
                         popup =paste(sep = "<br/>",
                                      paste('Facility:',  dat_water_body_map$facility),
                                      paste('Water Body:',dat_water_body_map$water_body)
                         ),
                         label = paste('Facility:',  dat_water_body_map$facility, 
                                       'Water Body:',dat_water_body_map$water_body
                         ),
                         radius = dat_water_body_map$radius,
                         fillOpacity = .01
                          )
    })

  
  # Reactive expression for the data filtered to what the user selected
  filtered_water_body<- reactive({
    dat_water_body %>%
      filter(water_body %in% input$water_body_input)
  })
  # render plot3
  output$plot3 <- renderPlot({
    ggplot(data = filtered_water_body(),
           aes(x = date, y = kgN_mo/1000, col = water_body,
               fill=water_body)) +
      ylab(expression(paste(
        'N load (1,000 kg N',~ha^-1,mo^-1,')')))+
      xlab('Date')+
      geom_line(alpha = 0.7, size=1) +
      #geom_smooth(method = 'lm', se = FALSE, linetype='dashed', alpha=.4, size=.6) +
      theme_minimal() +
      theme(text = element_text(size=18),
            legend.position = 'bottom')+
      scale_color_manual(name='Water Body',
                         breaks=c('CONNECTICUT RIVER',
                                  'HOUSATONIC RIVER',
                                  'NAUGATUCK RIVER',
                                  'FARMINGTON RIVER',
                                  'LONG ISLAND SOUND',
                                  'QUINEBAUG RIVER',
                                  'QUINNIPIAC RIVER',
                                  'THAMES RIVER',
                                  'EAST R',
                                  'MANHASSET BAY',
                                  'PAWCATUCK RIVER',
                                  'SMITHTOWN BAY'),
                        labels=c('Connecticut River',
                                   'Housatonic River',
                                   'Naugatuck River',
                                   'Farmington River',
                                   'Long Island Sound',
                                   'Quinebaug River',
                                   'Quinnipiac River',
                                   'Thames River',
                                   'East River',
                                   'Manhasset River',
                                   'Pawcatuck River',
                                   'Smithtown Bay'),
                         values = pal)
     
  })
  
}


# run app -----------------------------------------------------------------

#options(shiny.reactlog = TRUE)
shinyApp(ui, server)

