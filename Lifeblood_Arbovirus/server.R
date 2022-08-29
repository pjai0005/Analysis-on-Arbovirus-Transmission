library(shiny)
library(tigris)
library(leaflet.extras)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(sf)
library(broom)
library(readxl)
library(rgdal)
library(themis)
library(leaflet)
library(openxlsx)
library(geosphere)
library(readr)
library(RColorBrewer)
library(plotly)
library(gganimate)

shapefile <- readOGR(dsn = paste0(getwd(), "/../Data/SA3_2011/"))

world_df <- readOGR( 
  dsn= paste0(getwd(),"/../Data/World_Shapefile/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE) %>% fortify(region = "NAME") %>% 
  rename(wlat = lat,
         wlong = long,
         wname = id)

full_data <- read_csv("../../Data/fulldata.csv") # read_csv(file.choose())
imported <-  "../../Data/dataset IMP viruses.xlsx"#file.choose()

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

virus_list <-  c("Dengue", 
                 "West_Nile_Kunjin", 
                 "Zika", 
                 "Chikungunya",
                 "Japanese_Encephalitis_Virus")

connection_data <- data.frame()

for(i in 1:5)
{
  connection_data <- rbind(connection_data, read_excel(imported, sheet = i) %>%
                             add_column(Virus = virus_list[i]))
}


connection_data <- connection_data %>% 
  filter(!`Country of Origin` %in% c("No data available", "Overseas - Country unknown", 
                                     "At Sea", "Unknown", "No data supplied", "Australia") &
           !`Residential location (SA3)` %in% c("No data available", "Overseas - Country unknown", 
                                                "At Sea", "No data supplied", "Unknown")) %>% 
  mutate(`Country of Origin` = 
           ifelse(`Country of Origin` =="China (excludes SARs and Taiwan)", "China",
                  ifelse(`Country of Origin` == "Venezuela, Bolivarian Republic of", "Venezuela",
                         ifelse(`Country of Origin` == "Samoa, American", "Samoa",
                                ifelse(`Country of Origin` == "Bolivia, Plurinational State of", "Bolivia",
                                       ifelse(`Country of Origin` == "Congo, Democratic Republic of", "Congo",
                                              ifelse(`Country of Origin` == "Macau (SAR of China)", "Macau",
                                                     ifelse(`Country of Origin` == "United Kingdom, Channel Islands and Isle of Man", "United Kingdom",
                                                            ifelse(`Country of Origin` == "Hong Kong (SAR of China)", "Hong Kong",
                                                                   ifelse(`Country of Origin` == "Congo, Republic of", "Congo", 
                                                                          ifelse(`Country of Origin` == "Myanmar, The Republic of the Union of", "Myanmar", `Country of Origin`)))))))))))

connection_data <- connection_data[!grepl("nfd|nec", connection_data$`Country of Origin`),] 



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
  
  output$connection_map <- renderLeaflet({
    final_map_data <- connection_data %>% 
      group_by(Virus, `Country of Origin`) %>% 
      summarise(count = sum(Count)) %>% 
      inner_join(. , world_df, by=c("Country of Origin"="wname"))%>% 
      group_by(Virus, `Country of Origin`) %>% 
      summarise(wlat = mean(wlat),
                wlong = mean(wlong),
                count  = mean(count)) %>% 
      rename(wname = "Country of Origin") %>% 
      filter(Virus == input$virus_connection)  
    
    content <- paste(sep = "<br/>",
                     "Virus Imported from: ", "<b>", final_map_data$wname, "</b>",
                     "Type of Virus: ",  "<b>", final_map_data$`Virus`, "</b>",
                     "Total Imported Cases: ", "<b>", final_map_data$`count`, "</b>")
    
    
    connection_map <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>% 
      addCircleMarkers(lng=final_map_data$wlong, lat=final_map_data$wlat, 
                       popup=content, radius = 4, opacity = final_map_data$count) %>%
      addMarkers(lng = 137, lat = -23.2,
                 labelOptions = labelOptions(noHide = T))
    
    for (i in 1:nrow(final_map_data)){
      connection_map <- connection_map %>% 
        addPolylines(lng = c(final_map_data$wlong[i],137),
                     lat = c(final_map_data$wlat[i],-23.2), weight=1.5, opacity=2, color="pink")
    }
    
    connection_map %>%  setView(0, 0, zoom = 2.4) 
  })
  
  output$dr_map <- renderLeaflet({
    if (is.null(input$sa3_donation)){
      data <- full_data %>% 
        filter (format(Year, format = "%Y") >= input$date_slider[1], 
                format(Year, format = "%Y") <= input$date_slider[2]) %>% 
        group_by(SA3_NAME_2011) %>%
        summarise(avg_donation_rate = mean(donationrate1000))
    } else {
      data <- full_data %>%
        filter(SA3_NAME_2011 %in% input$sa3_donation) %>%
        filter (format(Year, format = "%Y") >= input$date_slider[1], 
                format(Year, format = "%Y") <= input$date_slider[2]) %>% 
        group_by(SA3_NAME_2011) %>%
        summarise(avg_donation_rate = mean(donationrate1000))
    }
    data <- full_join(centroid, data) %>% na.omit()
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
    if (is.null(input$sa3_donation)){
      data <- long_data %>% 
        filter (format(Year, format = "%Y") >= input$date_slider[1], 
                format(Year, format = "%Y") <= input$date_slider[2]) %>% 
        group_by(Year, type)%>% 
        summarise(count = mean(count, na.rm = TRUE))
    } else {
      data <- long_data %>%
        filter(SA3_NAME_2011 %in% input$sa3_donation) %>%
        filter (format(Year, format = "%Y") >= input$date_slider[1], 
                format(Year, format = "%Y") <= input$date_slider[2]) %>% 
        group_by(Year, type) %>% 
        summarise(count = mean(count, na.rm = TRUE))
    }
    plot <- data %>% 
      group_by(Year, type) %>% 
      summarise(count = mean(count, na.rm = TRUE)) %>% 
      ggplot(aes(x = Year, y = count, color = type)) +
      geom_line() +
      scale_color_manual(values=c("#851e3e", "#009688")) +
      theme_light() +
      theme(legend.position="none")
    
    ggplotly(plot) %>%
      layout(plot_bgcolor  = "#fcf9f2",
             paper_bgcolor = "#fcf9f2",
             fig_bgcolor   = "#fcf9f2")
  })
})
