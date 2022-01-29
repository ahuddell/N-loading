

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


# read data, objects, and functions ---------------------------------------

dat<-read_csv(file=unzip(zipfile=here('data','complete_time_series_with_missing_data_imputed.zip')))
names(dat)

#read in the rest of the huc8 shapes 
huc8_combined<-st_read(here('data','huc_8_dat_join','huc8_combined.shp'))


#group data in different ways
dat_annual<- dat %>%
  group_by(facility, year=year(date), lon=long, lat=lat) %>%
  summarise(kgN_facility_yr=sum(kg_N_TN_per_month_complete,na.rm=T))

dat_annual<- st_as_sf(dat_annual,coords=c('lon','lat'))
dat_annual$radius<-(dat_annual$kgN_facility_yr)/10^6


#group facilities by HUC8 watershed
dat_HUC8<-dat %>%
  group_by(watershed_name, year=year(date)) %>%
  summarise(kgN_huc8_yr=sum(kg_N_TN_per_month_complete,na.rm=T))

#group facilities by state
dat_state<-dat %>%
  group_by(state, year=year(date)) %>%
  summarise(kgN_huc8_yr=sum(kg_N_TN_per_month_complete,na.rm=T))

#group facilities by HUC8 watershed
dat_TMDL<-dat %>%
  group_by(TMDL_zone, year=year(date)) %>%
  summarise(kgN_huc8_yr=sum(kg_N_TN_per_month_complete,na.rm=T))


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
       'Point Source N loading by Aggregated by HUC8 Watershed',
      fluidRow(
        height = 4,
        tags$style(type = 'text/css', '.selectize-input{ z-index: 999; }'),
        h3('This tool summarizes point source N loading in the Long Island Sound Watershed'),
        column(
          width = 5,
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
      downloadButton("downloadData", "Download raw data")
      ))),

      ),
    
  tabPanel(
    'Point Source N loading by Aggregated by State',
    fluidRow(
      height = 4,
      tags$style(type = 'text/css', '.selectize-input{ z-index: 999; }'),
      h3('This tool summarizes point source N loading'),
      column(
        width = 5,
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
        ),
    fluidRow(splitLayout(
        cellWidths = c('40%', '60%'),
        leafletOutput('map2'),
        plotOutput('plot2')
      )),
      )
    )
  )
}


# server ------------------------------------------------------------------

# Server
server <- function(input, output, session) {
  ## Tab 1
  # # Reactive expression for the data filtered to what the user selected
  # filtered_year <- reactive({
  #   dat_annual %>% filter(year == input$year)
  # })
  
  #mouseover labels
  labs <- as.list(dat_HUC8$name)
  
  # render base map
  output$map <- renderLeaflet({
    leaflet(huc8_combined) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addPolygons( fillColor = ~ "lightgrey",
                   weight = 1,
                   color = 'black',
                   opacity = 1,
                   fillOpacity = .8,
                   popup = paste('Watershed name:',huc8_combined$name),
                   label = paste('Watershed name:',huc8_combined$name)
                       )
  })
  
  # Reactive expression for the data subsetted to what the user selected
  filtered_watershed_name <- reactive({
    dat_HUC8 %>%
    filter(watershed_name %in% input$huc8)
  })
  
  # render plot1
  output$plot1 <- renderPlot({
    ggplot(data = filtered_watershed_name(),
      aes(x = year, y = kgN_huc8_yr/1000, col = watershed_name,
          fill=watershed_name)) +
      ylab(expression(paste(
        'Annual N load (1,000 kg N ',ha^-1,yr^-1,')')))+
      xlab('Date')+
      geom_line(alpha = 0.7) +
      geom_smooth(method = 'lm', se = FALSE, linetype='dashed', alpha=.4, size=.6) +
      theme_minimal() +
      theme(text = element_text(size=18),
            legend.position = 'bottom')+
      scale_color_viridis(name='Watershed', discrete=TRUE) +
      scale_fill_viridis(name='Watershed', discrete=TRUE)#+
      #facet_wrap(~facility)
   })
  
  # render base map2
  output$map2 <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Stamen.Toner)
          })
  
  # Reactive expression for the data subsetted to what the user selected
  filtered_state <- reactive({
    dat_state %>%
      filter(state %in% input$state_input)
  })
  # render plot2
  output$plot2 <- renderPlot({
    ggplot(data = filtered_state(),
           aes(x = year, y = kgN_huc8_yr/1000, col = state,
               fill=state)) +
      ylab(expression(paste(
        'Annual N load (1,000 kg N ',ha^-1,yr^-1,')')))+
      xlab('Date')+
      geom_line(alpha = 0.7) +
      geom_smooth(method = 'lm', se = FALSE, linetype='dashed', alpha=.4, size=.6) +
      theme_minimal() +
      theme(text = element_text(size=18),
            legend.position = 'bottom')+
      scale_color_viridis(name='State', discrete=TRUE) +
      scale_fill_viridis(name='State', discrete=TRUE)#+
  })

  # Downloadable csv of selected dataset 
  output$downloadData <- downloadHandler(
    filename = function() {paste("Long_Island_Sound_WWTP_data", " ",Sys.Date(),".csv",sep="")},
    content = function(file) {
      write.csv(dat, 
                file, 
                row.names = FALSE)
    }
  )
  
}


# run app -----------------------------------------------------------------

options(shiny.reactlog = TRUE)
shinyApp(ui, server)

