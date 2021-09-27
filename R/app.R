
library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
library(tidyverse)
library(RColorBrewer)
library(bslib)
# library(highcharter)
#library(shinyjs)
# library(tmap)
# library(tmaptools)
library(leaflet)
library(here)
library(lubridate)
library(sf)
# library(mapdeck)

# data and headers
title <- tags$a(
  href = "https://www.longislandsoundstudy.net",
  icon("fish"), "Long Island Sound Study"
)

# load in spatial dataframe with summarized monthly N loads by HUC8 watershed
dat <- st_read(here(
  "data", "huc_8_dat_join",
  "N_load_huc_join.shp"
))

# using smaller subset of data for building app
# dat <-
#   filter(
#     dat,
#     dat$date > "2005-01-01",
#     dat$date < "2008-01-01",
#   )

st_crs(dat) # WGS 84 is coordinate reference system

labels <- sprintf(
  "<strong>%s</strong><br/>%g kg total N / month<sup>-1</sup>",
  dat$name, dat$kgN_huc8
) %>% lapply(htmltools::HTML)


# bins <- unname(quantile(one_month_test$kgN_huc8, c(.2,.6,.8,1)))
bins <- c(0, 2, 3, 6, 7000, 8000)
pal <- colorBin("Blues", bins = bins)
# in future make this colorQuantiles with more categories


ui <- fluidPage(
  theme=bs_theme(version=4,bootswatch = "lumen"),
  # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),

  )
  


server <- function(input, output, session) {

  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    filter(dat, date == input$month)
  })
  
  filteredLabels <- reactive({
      sprintf(
          "<strong>%s</strong><br/>%g kg total N / month<sup>-1</sup>",
          dat$name, dat$kgN_huc8
      ) %>% lapply(htmltools::HTML)
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(dat) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addLegend(
        pal = pal,
        values = ~density, opacity = 0.7, title = NULL,
        position = "bottomright"
      ) %>%
      fitBounds(
        ~ unname(st_bbox(dat)[1]), ~ unname(st_bbox(dat)[2]),
        ~ unname(st_bbox(dat)[3]), ~ unname(st_bbox(dat)[4])
      )
  })




  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(kgN_huc8),
        weight = 2,
        color = "black",
        opacity = 1,
        fillOpacity = 0.7,
        # label = filteredLabels(),
        # labelOptions = labelOptions(
        #     style = list("font-weight" = "normal", padding = "3px 8px"),
        #     textsize = "15px",
        #     direction = "auto"
        # )
      )
  })
}

shinyApp(ui, server)
