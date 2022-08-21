library(shiny)
library(leaflet)
library(rgdal)
library(tidyverse)
library(tigris)
library(leaflet.extras)
library(shinydashboard)

shapefile <- readOGR(dsn = paste0(getwd(), "/../Data/SA3_2011/"))
full_data <- read_csv(file.choose())

sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem("Incidence Rate Comparison", 
             menuSubItem(tabName = "connection", text = "Connection Map"),
             menuSubItem(text = "Incidence Rate for Group Year" , tabName = "ir_group_year"),
             menuSubItem(tabName = "bubble_map", text = "Bubble Map for LA vs IMP Cases"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "connection",
            h2("Connection Map")
    ),
    tabItem(tabName = "ir_group_year",
            h2("Incidence Rate for Group Year"),
              sidebarLayout(
                sidebarPanel(
                      radioButtons("groupYear",
                                    "Select a Group Year:",
                                   c("2002-2006" = "1",
                                     "2007-2011" = "2",
                                     "2012-2017" = "3")),
              radioButtons("type",
                           "Locations: ",
                           c("Locally Acquired" = "LA",
                             "Imported" = "IMP")),
              selectInput("virus",
                           "Select a Virus:",
                           c("Ross River" = "RRV",
                             "Dengue" = "DENV",
                             "Barmah Forest" = "BFV",
                             "Zika" = "ZIKV",
                             "Murray Valley Encephalitis" = "MVEV",
                             "West Nile/Kunjin" = "WNV",
                             "Japanese Encephalitis" = "JEV",
                             "Chikungunya" = "CHIKV"))),
                    mainPanel(
                      leafletOutput("ir_map", height = 500))
            )
    ),
    tabItem(tabName = "bubble_map",
            h2("Animated Bubble Map for LA vs IMP Cases Comparison")
  ))
)

shinyApp(
  ui <- dashboardPage(
    dashboardHeader(title = "Analysis on spread of Arbovirus", titleWidth = 350),
    sidebar,
    body
  ),
  server = function(input, output) {
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
          start_year <- 2002
          end_year <- 2006
        } else if (input$groupYear == "2"){
          start_year <- 2007
          end_year <- 2011
        } else {
          start_year <- 2012
          end_year <- 2017
        }
        data_avg <- full_data %>%
          filter(Year >= 2007 & Year <= 2011) %>%
          group_by(SA3_NAME_2011) %>%
          summarise_at(match(paste0(input$virus, "_IR", input$type),names(full_data))-1, list(mean = mean))

        content <- paste0(sep = "<br/>", "<b>SA3 Region: </b>",data_avg$SA3_NAME_2011, "<br>",
                          "<b>Avg. Incidence Rate: </b>", round(data_avg$mean, 2))

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
          setMapWidgetStyle(list(background= "white"))
      })
  }
)