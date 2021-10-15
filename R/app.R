
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
dat_sf$year<-year(dat_sf$date) #add a year column

# same data as data frame object for plots
dat <- read_csv(here("data", "ECHO_data_clean.csv"))

dat_WQ1<- read_csv(here(
  "data", "DEEP_WQ_thru_2015.csv"
))
dat_WQ2<- read_csv(here(
  "data", "DEEP_WQ_2015_present.csv"
))

dat_WQ<-rbind(dat_WQ1,dat_WQ2)

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
            min = min(dat_sf$year),
            max = max(dat_sf$year),
            value = min(dat_sf$year),
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
    ),
  
#Tab 2
  tabPanel(
    "pH and DO",
    fluidRow(
      height = 4,
      tags$style(type = "text/css", ".selectize-input{ z-index: 999; }"),
      h3("N loading"),
      p(
        "This is a data visualization tool summarizing CT DEEP "
      ),
      column(
        width = 6,
        selectInput(
          inputId = "WQ_param",
          label="Water Quality parameter",
          choices =c('pH'="pH",
          'Dissolved Oxygen mg/L'="corrected_oxygen"),
          selected="corrected_oxygen"
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
        plotOutput("plot2"),
        plotOutput("plot3")
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
    dat_sf %>% filter(year == input$year) 
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

  # render base map
  output$map <- renderLeaflet({
    leaflet(dat_sf) %>%
      addProviderTiles(providers$Esri.WorldPhysical) %>%
      addLegend(
        pal = pal,
        values = ~ density,
        opacity = 0.7,
        title = NULL,
        position = "bottomright") %>%
      fitBounds(~ unname(st_bbox(dat_sf)[1]),
                ~ unname(st_bbox(dat_sf)[2]),
                ~ unname(st_bbox(dat_sf)[3]),
                ~ unname(st_bbox(dat_sf)[4])) %>%

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
        fillColor = ~ pal(kgN_huc8),
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


  # render plot1
  output$plot1 <- renderPlot(
    ggplot(
      data = filtered_watershed_name(),
      aes(x = date, y = kg_N_TN_per_month, col = name,
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

  # ## Tab 2
  # # Reactive expression for the data subsetted to what the user selected
  # # column_se<-reactive({
  # #   par_se<-column_par_se<-[input$WQ_param]
  # # })
  # # 
  # # select_dat_WQ <- reactive({
  # #   dat_WQ %>% select(input$WQ_param, station_name,Start_Date) 
  # # })
  # # 
  # # render plot2
  # output$plot2 <- renderPlot(
  #   ggplot(
  #     data = dat_WQ,
  #     aes(x = Start_Date, y = input$WQ_param, col = station_name,
  #         fill=station_name)) +
  #     # ylab(expression(paste(
  #     #   'Monthly N load (kg N ',ha^-1,yr^-1,')')))+
  #     xlab('Date')+
  #     geom_point(alpha = 0.7) +
  #     geom_smooth(method = "loess", se = T) +
  #     theme_minimal() +
  #     theme(text = element_text(size=18),
  #           legend.position = "none")+
  #     scale_color_viridis(name="Station Name", discrete=TRUE) +
  #     scale_fill_viridis(name="Station Name", discrete=TRUE)
  # )

  # render plot3
  # output$plot3 <- renderPlot(
  #   ggplot(
  #     data = filtered_watershed_name(),
  #     aes(x = date, y = kgN_huc8, col = name,
  #         fill=name)) +
  #     ylab(expression(paste(
  #       'Monthly N load (kg N ',ha^-1,yr^-1,')')))+
  #     xlab('Date')+
  #     geom_point(alpha = 0.7) +
  #     geom_smooth(method = "loess", se = T) +
  #     theme_minimal() +
  #     theme(text = element_text(size=18),
  #           legend.position = "bottom")+
  #     scale_color_viridis(name="Watershed", discrete=TRUE) +
  #     scale_fill_viridis(name="Watershed", discrete=TRUE)
  # )
}


# run app -----------------------------------------------------------------

#options(shiny.reactlog = TRUE)
shinyApp(ui, server)
