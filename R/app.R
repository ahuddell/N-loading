
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
  "data", "huc_8_dat_join",
  "N_load_huc_join.shp"
))

dat_sf_annual<- dat_sf %>%
  group_by(huc8, year=year(date)) %>%
  summarise(kgN_huc8_yr=sum(kgN_huc8,na.rm=T)) %>%
  ungroup()

# same data as data frame object for plots
dat <- as.data.frame(dat_sf)

# color palette for choropleth map
bins <- c(0,25000,50000,100000,200000,300000,550500)
pal <- colorBin("Blues",bin=bins, na.color = "808080",
)
# in future make this colorQuantiles with more categories


# # labels for map
# labels <- sprintf(
#   "<strong>%s</strong><br/>%g kg total N / month<sup>-1</sup>",
#   dat_sf$name, dat_sf$kgN_huc8
# ) %>% lapply(htmltools::HTML)

centroids <- dat_sf%>%
  group_by(name) %>%
  slice(1) %>%
  st_centroid()

# ui ----------------------------------------------------------------------
ui <- function(request) {
  navbarPage(
    title = tags$a(
     # href = "https://www.longislandsoundstudy.net",
      icon("fish", lib = "font-awesome"),
     # "Long Island Sound Study"
     "Draft app"
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
            inputId = "year",
            label="Year",
            min = min(dat_sf_annual$year),
            max = max(dat_sf_annual$year),
            value = min(dat_sf_annual$year),
           timeFormat = "%Y",
           sep = "",
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
            selected = "Housatonic"
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

  ## Tab 1
 # Reactive expression for the data subsetted to what the user selected
  filtered_year <- reactive({
    dat_sf_annual %>% filter(year == input$year) 
  })

  # filteredLabels <- reactive({
  #   filter(
  #     sprintf(
  #       "<strong>%s</strong><br/>%g kg total N / month<sup>-1</sup>",
  #       dat$name,
  #       dat$kgN_huc8
  #     ) %>% lapply(htmltools::HTML)
  #   )
  # })
  #
  # render base map
  output$map <- renderLeaflet({
    leaflet(dat_sf_annual) %>%
      addProviderTiles(providers$Esri.WorldPhysical) %>%
      addLegend(
        pal = pal,
        values = ~ density,
        opacity = 0.7,
        title = NULL,
        position = "bottomright") %>%
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
          textOnly = TRUE,
          
        )
      )

  })
  
  # add reactive choropleth layer
  observe({
    leafletProxy("map", data = filtered_year()) %>%
      #clearShapes() %>%
      addPolygons(
        fillColor = ~ pal(kgN_huc8_yr),
        weight = 2,
        color = "black",
        opacity = 1,
        fillOpacity = 0.7)
  })
  

  

  # Reactive expression for the data subsetted to what the user selected
  filtered_watershed_name <- reactive({
    dat %>%
      filter(name == input$watershed_name) %>%
      droplevels()
  })


  # render plot
  output$plot1 <- renderPlot(
    ggplot(
      data = filtered_watershed_name(),
      aes(x = date, y = kgN_huc8, col = name,
          fill=name)) +
      ylab(expression(paste(
        'Monthly N load (kg N ',ha^-1,yr^-1,')')))+
      xlab('Date')+
      geom_point(alpha = 0.7) +
      geom_smooth(method = "loess", se = T) +
      theme_minimal() +
      theme(text = element_text(size=18),
            legend.position = "bottom")+
      scale_color_viridis(name="Watershed", discrete=TRUE) +
      scale_fill_viridis(name="Watershed", discrete=TRUE)
  )
}


# run app -----------------------------------------------------------------

options(shiny.reactlog = TRUE)
shinyApp(ui, server)
