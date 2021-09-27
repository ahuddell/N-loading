
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

# read data, objects, and functions ---------------------------------------
# load in spatial dataframe with summarized monthly N loads by HUC8 watershed
dat_sf <- st_read(here(
  "data", "huc_8_dat_join",
  "N_load_huc_join.shp"
))

# same data as data frame object for plots
dat <- as.data.frame(dat_sf)

# color palette for choropleth map
bins <- c(0, 2, 3, 6, 7000, 8000)
pal <- colorBin("Blues", bins = bins)
# in future make this colorQuantiles with more categories


# labels for map
labels <- sprintf(
  "<strong>%s</strong><br/>%g kg total N / month<sup>-1</sup>",
  dat_sf$name, dat_sf$kgN_huc8
) %>% lapply(htmltools::HTML)


# ui ----------------------------------------------------------------------
ui <- function(request) {
  navbarPage(
    title = tags$a(
      href = "https://www.longislandsoundstudy.net",
      icon("fish", lib = "font-awesome"),
      "Long Island Sound Study"
    ),
    theme = shinytheme("lumen"),
    tabPanel(
      "Point Source N loading",
      fluidRow(
        height = 4,
        tags$style(type = "text/css", ".selectize-input{ z-index: 999; }"),
        h3("N loading"),
        p(
          "This is a data visualization tool summarizing monthly point source N loading in the Long Island Sound, and summer month hypoxia area and volume. Results from each analysis are in separate tabs"
        ),
        column(
          width = 6,
          sliderInput(
            "month",
            "Month",
            min = min(dat$date),
            max = max(dat$date),
            value = median(dat$date),
            timeFormat = "%b %Y",
            step = 1
          )
        ),
        column(
          width = 6,
          selectizeInput(
            "watershed_name",
            "Watershed",
            choices = dat$name,
            multiple = T,
            selected = "Bronx"
          )
        )
      ),
      fluidRow(
        splitLayout(
          cellWidths = c("50%", "50%"),
          leafletOutput("map"),
          plotOutput("plot1")
        )
      ),
    )
  )
}



# server ------------------------------------------------------------------


# Server
server <- function(input, output, session) {

  ### Tab 1
  # Reactive expression for the data subsetted to what the user selected
  filtered_month <- reactive({
    filter(dat_sf, date == input$month)
  })

  # filteredLabels <- reactive({
  #   sprintf(
  #     "<strong>%s</strong><br/>%g kg total N / month<sup>-1</sup>",
  #     dat$name, dat$kgN_huc8
  #   ) %>% lapply(htmltools::HTML)
  # })
  #
  # render base map
  output$map <- renderLeaflet({
    leaflet(dat_sf) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addLegend(
        pal = pal,
        values = ~density, opacity = 0.7, title = NULL,
        position = "bottomright"
      ) %>%
      fitBounds(
        ~ unname(st_bbox(dat_sf)[1]), ~ unname(st_bbox(dat_sf)[2]),
        ~ unname(st_bbox(dat_sf)[3]), ~ unname(st_bbox(dat_sf)[4])
      )
  })
  # add reactive choropleth layer
  observe({
    leafletProxy("map", data = filtered_month()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ pal(kgN_huc8),
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
        # popup = ~paste0("Site No: ", SiteID)
      )
  })


  # Reactive expression for the data subsetted to what the user selected
  filtered_watershed_name <- reactive({
    filter(dat, name == input$watershed_name)
  })


  # render plot
  output$plot1 <- renderPlot(
    ggplot(
      data = filtered_watershed_name(),
      aes(x = date, y = kgN_huc8, col = name)
    ) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "loess", se = T) +
      theme_minimal() +
      theme(legend.position = "right")
  )
}


# run app -----------------------------------------------------------------


shinyApp(ui, server)
