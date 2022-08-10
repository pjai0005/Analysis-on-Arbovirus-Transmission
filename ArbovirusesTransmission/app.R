library(shiny)
library(leaflet)
library(rgdal)
library(tidyverse)
library(tigris)
library(leaflet.extras)

shapefile <- readOGR(dsn = paste0(getwd(), "/../Data/SA3_2011/"))
full_data <- read_csv(file.choose())
ui <- fluidPage(

    # Application title
  titlePanel(h1("Analysis on Arbovirus Transmission in Australia",
                style="background-color:#43593F;
                     padding-left: 5px; color: #F0EBE5", align = "center")),
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
        # Show a plot of the generated distribution
        mainPanel(
          leafletOutput("ir_map", height = 500))
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
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
      summarise_at(match(paste0(input$virus, "_IR", input$type),names(full_data))-1, list(mean = mean)) %>% 
      mutate(label = paste0("SA3 Region: ", SA3_NAME_2011, " Avg. Incidence Rate: ", round(mean, 2)))
    
    shapefile_temp <- geo_join(shapefile, data_avg, "SA3_NAME11", "SA3_NAME_2011")
    leaflet(shapefile_temp) %>%
      addPolygons(weight = 1, 
                  fillColor = ~colorNumeric("RdBu", mean)(mean),
                  opacity = 0.5,
                  color = "white",
                  fillOpacity = 0.9,
                  smoothFactor = 0.5,
                  label = ~label) %>% 
      setMapWidgetStyle(list(background= "white"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
