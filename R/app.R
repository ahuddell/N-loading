
# load libraries ----------------------------------------------------------
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shiny)
library(tidyverse)
library(RColorBrewer)
library(bslib)
# library(highcharter)
# library(shinyjs)
# library(tmap)
# library(tmaptools)
library(leaflet)
library(here)
library(lubridate)
library(sf)
library(viridis)


# read data, objects, and functions ---------------------------------------
# load in spatial dataframe with summarized monthly N loads by HUC8 watershed
dat_sf <- st_read(here(
  'data', 'huc_8_dat_join',
  'N_load_huc_join.shp'
))

dat_sf_annual<- dat_sf %>%
  group_by(huc8, year=year(date)) %>%
  summarise(kgN_huc8_yr=sum(kgN_huc8,na.rm=T))


dat_sf$year<-year(dat_sf$date)
# same data as data frame object for plots
dat <- as.data.frame(dat_sf)

# dat_WQ1<- read_csv(here(
#   'data', 'DEEP_WQ_2021.csv'
# ))
# dat_WQ2<- read_csv(here(
#   'data', 'DEEP_WQ_2015_present.csv'
# ))
# 
# dat_WQ<-rbind(dat_WQ1,dat_WQ2)
# rm(dat_WQ1)
# rm(dat_WQ2)
# #reducing size of dat_WQ for speed
# dat_WQ<-dat_WQ %>%select(pH, oxygen_concentration_in_sea_water,End_Date) 
# dat_WQ$oxygen_concentration_in_sea_water<-as.numeric(dat_WQ$oxygen_concentration_in_sea_water,na.rm=T)

# dat_WQ$End_Date<-as.character(map(strsplit(dat_WQ$End_Date, split = "T"), 1))
# dat_WQ$End_Date<-ymd(dat_WQ$End_Date)
# 
# dat_WQ1$End_Date<-as.character(map(strsplit(dat_WQ1$End_Date, split = "T"), 1))
# dat_WQ1$End_Date<-ymd(dat_WQ1$End_Date)

# color palette for choropleth map
bins <- c(0,2500,5000,10000,200000,300000,140000)
pal <- colorBin('Blues',bin=bins, na.color = '808080')
# in future make this colorQuantiles with more categories


# # labels for map
# labels <- sprintf(
#   '<strong>%s</strong><br/>%g kg total N / month<sup>-1</sup>',
#   dat_sf$name, dat_sf$kgN_h8
# ) %>% lapply(htmltools::HTML)

centroids <- dat_sf%>%
  group_by(name) %>%
  slice(1) %>%
  st_centroid()

# ui ----------------------------------------------------------------------
ui <- function(request) {
  navbarPage(
    title = tags$a(
     # href = 'https://www.longislandsoundstudy.net',
      icon('fish', lib = 'font-awesome'),
     # 'Long Island Sound Study'
     'Draft app'
    ),
    theme = shinytheme('lumen'),
    tabPanel(
      'Point Source N loading',
      fluidRow(
        height = 4,
        tags$style(type = 'text/css', '.selectize-input{ z-index: 999; }'),
        h3('N loading'),
        p('This is a data visualization tool summarizing monthly point source N loading in the Long Island Sound, and summer month hypoxia area and volume. Results from each analysis are in separate tabs'
        ),
        column(
          width = 6,
          sliderInput(
            inputId = 'year',
            label='Year',
            min = min(dat_sf$year),
            max = max(dat_sf$year),
            value = min(dat_sf$year),
           timeFormat = '%Y',
           sep = '',
          step = 1)
        ),
        column(width = 6,
          selectizeInput(
            inputId ='watershed_name',
            label = 'Watershed',
            choices = dat_sf$name,
            multiple = T,
            selected = 'Housatonic'))
        ),
      fluidRow(splitLayout(
          cellWidths = c('50%', '50%'),
          leafletOutput('map'),
          plotOutput('plot1')))),
  
# #Tab 2
#   tabPanel(
#     'pH and DO',
#     fluidRow(
#       height = 4,
#       tags$style(type = 'text/css', '.selectize-input{ z-index: 999; }'),
#       h3('N loading'),
#       p('This is a data visualization tool summarizing CT DEEP '
#       ),
#       column(
#         width = 6,
#         selectizeInput(
#           inputId = 'WQ_param',
#           label='Water Quality parameter',
#           choices =c('pH',
#           'Dissolved Oxygen mg/L'),
#           selected='pH')
#       ),
#       column(
#         width = 6,
#         selectizeInput(
#           inputId ='watershed_name',
#           label='Watershed',
#           choices = dat_sf$name,
#           multiple = T,
#           selected = 'Housatonic'
#         )
#       )
#     ),
#     fluidRow(
#       splitLayout(
#         cellWidths = c('50%', '50%'),
#         plotOutput('plot2'),
#         plotOutput('plot3')))
#   )
)}



# server ------------------------------------------------------------------

# Server
server <- function(input, output, session) {

  ## Tab 1
 # Reactive expression for the data subsetted to what the user selected
  filtered_year <- reactive({
    dat_sf_annual %>% filter(year == input$year) 
  })
  

  # filteredLabels <- reactive({
  #   filter(
  #     sprintf(
  #       '<strong>%s</strong><br/>%g kg total N / month<sup>-1</sup>',
  #       dat$name,
  #       dat$kgN_h8
  #     ) %>% lapply(htmltools::HTML)
  #   )
  # })

  # render base map
  output$map <- renderLeaflet({
    leaflet(dat_sf_annual) %>%
      addProviderTiles(providers$Esri.WorldPhysical) %>%
      addLegend(
        pal = pal,
        values = ~ density,
        opacity = 0.7,
        title = NULL,
        position = 'bottomright') %>%
      fitBounds(~ unname(st_bbox(dat_sf_annual)[1]),
                ~ unname(st_bbox(dat_sf_annual)[2]),
                ~ unname(st_bbox(dat_sf_annual)[3]),
                ~ unname(st_bbox(dat_sf_annual)[4])) %>%

      addLabelOnlyMarkers(
        data = centroids,
        lng = ~ unlist(map(centroids$geometry, 1)),
        lat = ~ unlist(map(centroids$geometry, 2)),
        label = ~ (name),
        labelOptions = labelOptions(textsize = 6,
          noHide = T,
          direction = 'center',
          textOnly = TRUE)
      )
  })

  # add reactive choropleth layer
  observe({
    leafletProxy('map', data = filtered_year()) %>%
      #clearShapes() %>%
      addPolygons(
        fillColor = ~ pal(kgN_huc8_yr),
        weight = 2,
        color = 'black',
        opacity = 1,
        fillOpacity = 0.7)
  })


  # Reactive expression for the data subsetted to what the user selected
  filtered_watershed_name <- reactive({
    dat %>%
      filter(name == input$watershed_name)
  })


  # render plot1
  output$plot1 <- renderPlot({
    ggplot(
      data = filtered_watershed_name(),
      aes(x = year, y = kgN_h8, col = name,
          fill=name)) +
      ylab(expression(paste(
        'Monthly N load (kg N ',ha^-1,yr^-1,')')))+
      xlab('Date')+
      geom_point(alpha = 0.7) +
      geom_smooth(method = 'loess', se = T) +
      theme_minimal() +
      theme(text = element_text(size=18),
            legend.position = 'bottom')+
      scale_color_viridis(name='Watershed', discrete=TRUE) +
      scale_fill_viridis(name='Watershed', discrete=TRUE)
  })

  # ## Tab 2
  # 
  #     output$plot2 <- renderPlot({
  #       yvarnames <- c('pH','oxygen_concentration_in_sea_water')
  #               ggplot(data = dat_WQ1,
  #              aes_string(x = 'End_Date', y = yvarnames[input$WQ_param])) +
  #         geom_point() +
  #                 labs(x = "Year", y = input$y) +
  #         geom_smooth(method = 'loess', se = T) +
  #         theme_minimal() +
  #         theme(text = element_text(size=18),
  #               legend.position = 'bottom')
  #     })
  #   
  # 
  # 
  #     # render plot3
  # # output$plot3 <- renderPlot(
  # #   ggplot(
  # #     data = filtered_watershed_name(),
  # #     aes(x = date, y = kgN_h8, col = name,
  # #         fill=name)) +
  # #     ylab(expression(paste(
  # #       'Monthly N load (kg N ',ha^-1,yr^-1,')')))+
  # #     xlab('Date')+
  # #     geom_point(alpha = 0.7) +
  # #     geom_smooth(method = 'loess', se = T) +
  # #     theme_minimal() +
  # #     theme(text = element_text(size=18),
  # #           legend.position = 'bottom')+
  # #     scale_color_viridis(name='Watershed', discrete=TRUE) +
  # #     scale_fill_viridis(name='Watershed', discrete=TRUE)
  # # )
}


# run app -----------------------------------------------------------------

#options(shiny.reactlog = TRUE)
shinyApp(ui, server)

