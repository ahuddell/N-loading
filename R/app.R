

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
dat<- read_csv(here('data','combined_top9_WLIS_full_ts.csv'))

dat$month_year<-parse_datetime(dat$month_year, format = "%Y %b")


dat_annual<- dat %>%
  group_by(facility, year=year(month_year), lon=LONGITUDE83, lat=LATITUDE83) %>%
  summarise(kgN_facility_yr=sum(kg_N_TN_per_month,na.rm=T))

dat_annual<- st_as_sf(dat_annual,coords=c('lon','lat'))
dat_annual$radius<-(dat_annual$kgN_facility_yr)/10^6




# ui ----------------------------------------------------------------------
ui <- function(request) {
  navbarPage(
    title = tags$a(
      href = 'https://www.longislandsoundstudy.net',
      icon('fish', lib = 'font-awesome'),
      'Long Island Sound Study'
    ),
    theme = shinytheme('lumen'),
    # tabPanel(
    #   'Point Source N loading',
      fluidRow(
        height = 4,
        tags$style(type = 'text/css', '.selectize-input{ z-index: 999; }'),
        h3('This tool summarizes monthly point source 
           N loading from top polluters to the Western section of the Long Island Sound'
        ),
        column(
          width = 5,
              ),
        column(
          width = 7,
          height=4,
          selectizeInput(
            inputId = 'facility',
            label = 'Facility',
            choices = unique(dat_annual$facility),
            multiple = T,
            width='100%',
            selected = "NYCDEP - NEWTOWN CREEK WPCP"
          )
        )
      ),
      fluidRow(splitLayout(
        cellWidths = c('40%', '60%'),
        leafletOutput('map'),
        plotOutput('plot1')
      )),
      # fluidRow(
      #   column(
      #     width = 8,
      #     height=4,
      #     selectizeInput(
      #       inputId = 'facility2',
      #       label = 'Facility',
      #       choices = unique(dat_annual$facility),
      #       multiple = T,
      #       width='100%',
      #       selected = "NYCDEP - NEWTOWN CREEK WPCP"
      #     )
      # )),
    
    hr(), # add spacer
    
    fluidRow(splitLayout(
      cellWidths = c('40%', '60%'),
      column(
        width = 4,
     # Button
      downloadButton("downloadData", "Download raw data")
      ),
      plotOutput('plot3')
    )),

     # )
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
  labs <- as.list(dat_annual$facility)
  
  # render base map
  output$map <- renderLeaflet({
    leaflet(dat_annual) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addCircleMarkers(data = dat_annual$geometry,
                       radius=(dat_annual$radius),
                       popup = paste('Facility:',dat_annual$facility),
                       label = paste('Facility:',lapply(labs, HTML))
                       )
  })
  
  # Reactive expression for the data subsetted to what the user selected
  filtered_facility_name <- reactive({
    dat %>%
    filter(facility %in% input$facility)
  })
  
  # render plot1
  output$plot1 <- renderPlot({
    ggplot(data = filtered_facility_name(),
      aes(x = month_year, y = kg_N_TN_per_month/1000, col = facility,
          fill=facility)) +
      ylab(expression(paste(
        'Monthly N load (1,000 kg N ',ha^-1,yr^-1,')')))+
      xlab('Date')+
      geom_line(alpha = 0.7) +
      geom_smooth(method = 'lm', se = FALSE, linetype='dashed', alpha=.4, size=.6) +
      theme_minimal() +
      theme(text = element_text(size=18),
            legend.position = 'bottom')+
      scale_color_viridis(name='Facility', discrete=TRUE) +
      scale_fill_viridis(name='Facility', discrete=TRUE)#+
      #facet_wrap(~facility)
   })
  # render plot3
  output$plot3 <- renderPlot({
    ggplot() +
      geom_jitter(data=filtered_facility_name(), aes(x=as.factor(season), y=log(kg_N_TN_per_month),
                                                     col = facility, fill=facility),width=.05,alpha=.4) +
      geom_violin(data=filtered_facility_name(), aes(x=as.factor(season), y=log(kg_N_TN_per_month)),fill='lightgrey', color='lightgrey',alpha=.4)+
      # geom_pointrange(data=meq1_outputs,
      #                 aes(x=season,y=coef,ymin = coef-SE, ymax =  coef+SE),
      #                 col='red',
      #                 size=1) +
      theme_minimal()+
      xlab('Monthly loads by season')+
      ylab('ln (monthly total N load \nby outfall (kg N/mo))')+
      #theme(axis.text.x = element_text(angle = 60, hjust=1))+
      ggtitle('Monthly loads grouped by season')+
      theme(text = element_text(size=18),
            legend.position = 'bottom',
            plot.title = element_text(hjust = 0.5)
      )+
      scale_color_viridis(name='Facility', discrete=TRUE) +
      scale_fill_viridis(name='Facility', discrete=TRUE)
    #facet_wrap(~facility)
  })
  
  # Downloadable csv of selected dataset 
  output$downloadData <- downloadHandler(
    filename = function() {paste("NY_CT_WWTP_data", " ",Sys.Date(),".csv",sep="")},
    content = function(file) {
      write.csv(dat, 
                file, 
                row.names = FALSE)
    }
  )
  
  # #reactive expression to filter seasonal data
  # filtered_dat_nonzero <-reactive({
  #   dat %>%
  #     filter(facility %in% input$facility) %>%
  #     filter(kg_N_TN_per_month>0 &
  #              !is.na(permit_outfall) &
  #              !is.na(season))
  # })

  # # Reactive expression for the data subsetted to what the user selected
  # filtered_facility_name <- reactive({
  #   dat %>%
  #     filter(facility %in% input$facility2)
  # })
  # 

  # #linear mixed effect model
  # lmer(log(kg_N_TN_per_month) ~ season+ (1|permit_outfall), data=dat_nonzero, 
  #      na.action=NULL)
  
  # # render plot3
  # output$plot3 <- renderPlot({
  #   ggplot() +
  #   geom_jitter(data=filtered_dat_nonzero(), aes(x=as.factor(season), y=log(kg_N_TN_per_month)),width=.05,alpha=.4,)+
  #              # col = facility, fill=facility) +
  #   geom_violin(data=filtered_dat_nonzero(), aes(x=as.factor(season), y=log(kg_N_TN_per_month)),fill='lightgrey', color='lightgrey',alpha=.4)+
  #   # geom_pointrange(data=meq1_outputs,
  #   #                 aes(x=season,y=coef,ymin = coef-SE, ymax =  coef+SE),
  #   #                 col='red',
  #   #                 size=1) +
  #   theme_minimal()+
  #   xlab('Monthly loads by season')+
  #   ylab('ln (monthly total N load by outfall (kg N/mo))')+
  #   theme(axis.text.x = element_text(angle = 60, hjust=1))+
  #   ggtitle('Monthly loads grouped by season')#+
  #   #facet_wrap(~facility)
  # })

}


# run app -----------------------------------------------------------------

#options(shiny.reactlog = TRUE)
shinyApp(ui, server)
