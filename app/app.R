
library(shinydashboard)
library(png)
library(shinydashboardPlus)
library(shiny)
library(tidyverse)
#library(highcharter)
library(shinyjs)
#library(tmap)
#library(tmaptools)
library(leaflet)
library(here)
library(sf)
#library(mapdeck)

#data and headers
title <- tags$a(href='https://www.longislandsoundstudy.net',
                icon("fish"), 'Long Island Sound Study')

N_load <- read_csv( file = here('data', 'ECHO_data_clean.csv'))

N_load_summary<- N_load %>%
    group_by(huc8,date) %>%
    summarise(sum(kg_N_TN_per_month,na.rm=T))

huc8 <- st_read(here('data','huc8 shapes',paste0('HUC8_','01080101'),'Shape'), 
         layer='WBDHU8')

huc8<-st_transform(huc8, crs=4326)

# 
# #read in each layer manually
# huc8_01080101<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080101'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080102<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080102'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080103<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080103'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080104<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080104'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080105<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080105'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080106<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080106'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080107<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080107'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080201<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080201'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080202<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080202'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080203<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080203'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080204<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080204'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080205<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080205'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080206<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080206'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080207<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080207'),'Shape'), 
#     layer='WBDHU8')
# huc8_01080207<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01080207'),'Shape'), 
#     layer='WBDHU8')
# huc8_01100001<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01100001'),'Shape'), 
#     layer='WBDHU8')
# huc8_01100002<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01100002'),'Shape'), 
#     layer='WBDHU8')
# huc8_01100003<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01100003'),'Shape'), 
#     layer='WBDHU8')
# huc8_01100004<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01100004'),'Shape'), 
#     layer='WBDHU8')
# huc8_01100005<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01100005'),'Shape'), 
#     layer='WBDHU8')
# huc8_01100006<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','01100006'),'Shape'), 
#     layer='WBDHU8')
# huc8_02030102<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','02030102'),'Shape'), 
#     layer='WBDHU8')
# huc8_02030201<-st_read(
#     here('data','huc8 shapes',paste0('HUC8_','02030201'),'Shape'), 
#     layer='WBDHU8')
# 
# #now transforming all CRS
# huc8_01080101<-st_transform(huc8_01080101, crs=4326)
# huc8_01080102<-st_transform(huc8_01080102, crs=4326)
# huc8_01080103<-st_transform(huc8_01080103, crs=4326)
# huc8_01080104<-st_transform(huc8_01080104, crs=4326)
# huc8_01080105<-st_transform(huc8_01080105, crs=4326)
# huc8_01080106<-st_transform(huc8_01080106, crs=4326)
# huc8_01080107<-st_transform(huc8_01080107, crs=4326)
# huc8_01080201<-st_transform(huc8_01080201, crs=4326)
# huc8_01080202<-st_transform(huc8_01080202, crs=4326)
# huc8_01080203<-st_transform(huc8_01080203, crs=4326)
# huc8_01080204<-st_transform(huc8_01080204, crs=4326)
# huc8_01080205<-st_transform(huc8_01080205, crs=4326)
# huc8_01080206<-st_transform(huc8_01080206, crs=4326)
# huc8_01080207<-st_transform(huc8_01080207, crs=4326)
# huc8_01100001<-st_transform(huc8_01100001, crs=4326)
# huc8_01100002<-st_transform(huc8_01100002, crs=4326)
# huc8_01100003<-st_transform(huc8_01100003, crs=4326)
# huc8_01100004<-st_transform(huc8_01100004, crs=4326)
# huc8_01100005<-st_transform(huc8_01100005, crs=4326)
# huc8_01100006<-st_transform(huc8_01100006, crs=4326)
# huc8_02030102<-st_transform(huc8_02030102, crs=4326)
# huc8_02030201<-st_transform(huc8_02030201, crs=4326)
start<-as.Date(min(N_load$date))
end<-as.Date(max(N_load$date))

ui <- fluidPage(
    
    titlePanel("Title"),
    
    sidebarLayout(
        
        sidebarPanel(
            tabsetPanel(id= "tabs",
                        
                        tabPanel("Map", id = "Map", 
                                 br(), 
                                 
                                 p("Choose options below to interact with the Map"), 
                                 
                                 sliderInput("date", "Select the date", 
                                             min = as.Date(start), 
                                             max = as.Date(end), 
                                             value = as.Date("2018-07-31"), 
                                             step = 1, 
                                             dragRange= TRUE)
                        )
            )
        ),
        
        mainPanel(
            
            tabsetPanel(type= "tabs",
                        
                        
                        tabPanel("Map", leafletOutput(outputId = "map"))
            )
        )
    )
)



server <- function(input, output) {
    
    
    layer <- reactive( {
        
        shp = st_read(
            here('data','huc8 shapes',paste0('HUC8_','01080101'),'Shape'),
             layer='WBDHU8')
        shp_p <- st_transform(shp, crs=4326)

    })
    
    output$map <- renderLeaflet({
        bins <- c(0, 2000, 4000, 8000, 16000, Inf)
        pal <- colorBin("YlOrRd", domain = layer()$h_7, bins = bins)
        
        
        leaflet(layer()) %>%
            setView(lat = 43,
                    lng = -73.2 ,
                    zoom = 7)
        addProviderTiles(providers$OpenStreetMap) %>%
            addCircleMarkers(data=dat_sf$geometry, col='red',
                         fillOpacity = .2, radius = .5) %>%
            setView( lat=43, lng=-73.2 , zoom=10) %>%
        addPolygons(data = shp_p$geometry) %>%
            
            addLegend(
                pal = pal,
                values = ~ h_7,
                opacity = 0.7,
                title = NULL,
                position = "bottomright"
            )
        
        
    })
    
    # observeEvent({input$hour},{
    #     hour_column <- paste0('h_',input$hour)
    #     data = layer()[hour_column]
    #     pal <- colorBin("YlOrRd", domain = as.numeric(data[[hour_column]]), bins = bins)
    #     leafletProxy("map", data=data)%>%
    #         clearShapes()%>%
    #         addPolygons( 
    #             fillColor = pal(as.numeric(data[[hour_column]])),  
    #             weight = 0.0,
    #             opacity = 1,
    #             color = "white",
    #             dashArray = "3",
    #             fillOpacity = 0.7 
    #         )  %>% clearControls() %>%
    #         addLegend(pal = pal, values =as.numeric(data[[hour_column]]), opacity = 0.7, title = NULL, position = "bottomright")
    # }
    # )
}

shinyApp(ui, server)
