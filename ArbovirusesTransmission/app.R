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

full_data <- cbind(full_data, full_data %>% select(contains("IR")) %>% 
                     rowSums())
names(full_data)[ncol(full_data)] <- "SUM_IR"
long_data <- full_data %>% pivot_longer(cols = c("donationrate1000", "SUM_IR"), names_to = "type", values_to = "count") %>% mutate(Year = as.Date(ISOdate(Year, 1, 1)))
get.centroid.bb <- function(x){
  N <- length(x)  # Number of polygons
  # Initialise data.frame
  Centroids.bb <- data.frame(matrix(NA, N, 2, dimnames = list(NULL, c("long", "lat"))))
  for(i in 1:N){
    # Bounding box of polygon
    bb <- bbox(x@polygons[[i]])
    # Compute centroid
    Centroids.bb[i,] <- c(
      0.5 * (bb[1,1] + bb[1,2]),
      0.5 * (bb[2,1] + bb[2,2]))
  }
  return(Centroids.bb)
}

centroid <- get.centroid.bb(shapefile) %>% cbind(shapefile$SA3_NAME11)
colnames(centroid) <- c("long", "lat", "SA3_NAME_2011")
sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem("Incidence Rate Comparison", 
             menuSubItem(tabName = "connection", text = "Connection Map"),
             menuSubItem(text = "Incidence Rate for Group Year" , tabName = "ir_group_year"),
             menuSubItem(tabName = "bubble_map", text = "Bubble Map for LA vs IMP Cases")),
    menuItem("Donation Rate",
             menuSubItem(tabName = "donation_bubble", text = "Donation Rate by SA3 Regions"),
             menuSubItem(tabName = "donation_rate_time", text = "Donation Rate vs Incidence Rate over time"))
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
                                   c("2007-2011" = "1",
                                     "2012-2017" = "2")),
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
  ),
  tabItem(tabName = "donation_bubble",
          h2("Donation Rate by SA3 Region"),
          sidebarLayout(sidebarPanel(
            selectInput("sa3_donation","Select SA3 Region: ",
                        unique(full_data$SA3_NAME_2011), multiple = TRUE)
          ),
          mainPanel(
            leafletOutput("dr_map", height = 500)
          ))
  ),
  tabItem(tabName = "donation_rate_time",
          h2("Donation Rate vs Incidence Rate over time"),
          sidebarLayout(sidebarPanel(
            selectInput("sa3_area","Select SA3 Region: ",
                        unique(full_data$SA3_NAME_2011), multiple = TRUE), 
            sliderInput("date_slider", "Date Range", min = min(full_data$Year), max = max(full_data$Year),
                        value = c(2007, 2017), sep = "")
            
          ),
          mainPanel(
            plotlyOutput("dr_graph", height = 500)
          ))
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
      
      output$dr_map <- renderLeaflet({
        if (is.null(input$sa3_donation)){
          data <- full_data %>% group_by(SA3_NAME_2011) %>%
            summarise(avg_donation_rate = mean(donationrate1000))
        } else {
          data <- full_data %>%
            filter(SA3_NAME_2011 %in% input$sa3_donation) %>%
            group_by(SA3_NAME_2011) %>%
            summarise(avg_donation_rate = mean(donationrate1000))
        }
        data <- full_join(centroid, data) %>% na.omit()
        print(data)
        pal <- colorNumeric("RdYlGn", data$avg_donation_rate)
        content <- paste0(sep = "<br/>", "<b>SA3 Region: </b>",data$SA3_NAME_2011, "<br>",
                          "<b>Avg. Donation Rate: </b>", round(data$avg_donation_rate, 2))
        data %>%
          leaflet() %>%
          addTiles() %>%
          setView(lat = -30, lng = 138, zoom = 4)%>%
            addProviderTiles("CartoDB.Positron")  %>%
          addCircleMarkers(~long, ~lat,
                           fillColor = ~pal(avg_donation_rate), fillOpacity = 0.7, color="white", radius=10, stroke=FALSE, popup = content) %>% addLegend( pal=pal, values=~avg_donation_rate, opacity=0.9, title = "Avg. Donation Rate", position = "topright")
      })
      
      output$dr_graph <- renderPlotly({
        if (is.null(input$sa3_area)){
          data <- long_data %>% group_by(Year, type)%>% 
            summarise(count = mean(count, na.rm = TRUE))
        } else {
          data <- long_data %>%
            filter(SA3_NAME_2011 %in% input$sa3_area) %>% 
            group_by(Year, type) %>% 
            summarise(count = mean(count, na.rm = TRUE))
        }
        plot <- data %>% 
          group_by(Year, type) %>% 
          summarise(count = mean(count, na.rm = TRUE)) %>% 
          ggplot(aes(x = Year, y = count, color = type)) +
          geom_line()
        
        ggplotly(plot)
      })
  }
)