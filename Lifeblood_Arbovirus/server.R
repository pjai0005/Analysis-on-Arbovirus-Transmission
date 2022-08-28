library(shiny)
library(leaflet)
library(rgdal)
library(tidyverse)
library(tigris)
library(leaflet.extras)
library(shinydashboard)
library(plotly)
shapefile <- readOGR(dsn = paste0(getwd(), "/../Data/SA3_2011/"))
full_data <- read_csv(file.choose())

shinyServer(function(input, output) {
  observeEvent(input$type, {
    if (input$type == "LA"){
      choices <- c("Ross River" = "RRV",
                   "Dengue" = "DENV",
                   "Barmah Forest" = "BFV",
                   "Murray Valley Encephalitis" = "MVEV",
                   "West Nile/Kunjin" = "WNV")
    }
    else {
      choices <- c("Dengue" = "DENV",
                   "Zika" = "ZIKV",
                   "West Nile/Kunjin" = "WNV",
                   "Japanese Encephalitis" = "JEV",
                   "Chikungunya" = "CHIKV")
    }
    updateSelectInput(inputId = "virus", choices = choices)
  })
  
  output$ir_map <- renderLeaflet({
    if (input$groupYear == "1"){
      start_year <- 2007
      end_year <- 2011
    } else {
      start_year <- 2012
      end_year <- 2017
    }
    data_avg <- full_data %>%
      filter(Year >= start_year & Year <= end_year) %>%
      group_by(SA3_NAME_2011) %>%
      summarise_at(match(paste0(input$virus, "_IR", input$type),names(full_data))-1, list(mean = mean))
    
    content <- paste0(sep = "<br/>", "<b>SA3 Region: </b>",data_avg$SA3_NAME_2011, "<br>",
                      "<b>Avg. Incidence Rate: </b>", round(data_avg$mean, 4))
    
    shapefile_temp <- geo_join(shapefile, data_avg, "SA3_NAME11", "SA3_NAME_2011")
    pal <- colorNumeric("RdBu", shapefile_temp$mean, reverse = TRUE)
    leaflet(shapefile_temp) %>%
      addPolygons(weight = 1,
                  fillColor = ~pal(mean),
                  opacity = 0.5,
                  color = "white",
                  fillOpacity = 0.9,
                  smoothFactor = 0.5,
                  popup = content) %>%
      addLegend("topright", pal = pal, values = ~mean, opacity = 1, title = "Mean Incidence Rate") %>%
      setMapWidgetStyle(list(background= "#fcf9f2"))
  })
})
