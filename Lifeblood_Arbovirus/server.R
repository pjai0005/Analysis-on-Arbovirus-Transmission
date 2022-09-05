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

full_data <- read_csv(file.choose())
imported <-  file.choose()

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

# pivoting full data to get virus names in a column
fulldata <- full_data %>% 
  pivot_longer(cols = c("BFV_CountLA" : "CHIKV_CountIMP"), #BFV_CountLA to CHIKV_CountIMP
               names_to = "Virus",
               values_to = "Value") 

# Count local or imported
fulldata$Commute_Count <- ifelse(grepl("_CountLA",fulldata$Virus),'local_count',
                                 ifelse(grepl("_CountIMP",fulldata$Virus),'imp_count','na'))

# IR local or imported
fulldata$Commute_IR <- ifelse(grepl("_IRIMP",fulldata$Virus),'imp_IR',
                              ifelse(grepl("_IRLA",fulldata$Virus),'local_IR','na'))


# transmission local or imported
fulldata$Transmission <- 
  ifelse(grepl("local_IR",fulldata$Commute_IR) | 
           grepl("local_count",fulldata$Commute_Count) ,'Local',
         
         ifelse(grepl("imp_IR",fulldata$Commute_IR) | 
                  grepl("imp_count",fulldata$Commute_Count),'Imported','na'))


# cleaning virus names
fulldata$Virus_Name <- 
  ifelse(grepl("BFV_",fulldata$Virus),'BFV',
         ifelse(grepl("RRV_",fulldata$Virus),'RRV',
                ifelse(grepl("DENV_",fulldata$Virus),'DENV',
                       ifelse(grepl("ZIKV_",fulldata$Virus),'ZIKV',
                              ifelse(grepl("MVEV_",fulldata$Virus),'MVEV',
                                     ifelse(grepl("JEV_",fulldata$Virus),'JEV',
                                            ifelse(grepl("WNV_",fulldata$Virus),'WNV',
                                                   ifelse(grepl("CHIKV_",fulldata$Virus),'CHIKV','na'))))))))



# extracting weather data froom full data
weather <- fulldata %>% 
  pivot_wider(names_from = c(Commute_Count, Commute_IR) ,
              values_from = Value) %>% 
  
  pivot_longer(cols = c(local_count_na, imp_count_na),
               names_to = "Count_Type",
               values_to = "Count")%>% 
  
  pivot_longer(cols = c(na_local_IR, na_imp_IR),
               names_to = "IR_Type",
               values_to = "IR")
# group_by(Year) %>% 
# summarise(Rainfall = round(mean(Rainavg), 2),
#           Min_Temprature = round(mean(meanMinTavg), 2),
#           Max_Temprature = round(mean(meanMaxTavg), 2),
#           `Incidence Percent` = round(mean(IR, na.rm = TRUE), 2)*100)

w_graph <- weather%>% 
  group_by(Year) %>% 
  summarise(Rainfall = round(mean(Rainavg), 2),
            Min_Temprature = round(mean(meanMinTavg), 2),
            Max_Temprature = round(mean(meanMaxTavg), 2),
            `Average Temperature` = round((Min_Temprature+Max_Temprature)/2, 2),
            `Incidence Percent` = round(mean(IR, na.rm = TRUE), 2)*100)

graph <- weather%>% 
  group_by(MonthYear) %>% 
  summarise(Rainfall = round(mean(Rainavg), 2),
            Min_Temprature = round(mean(meanMinTavg), 2),
            Max_Temprature = round(mean(meanMaxTavg), 2),
            `Average Temperature` = round((Min_Temprature+Max_Temprature)/2, 2),
            `Incidence Percent` = round(mean(IR, na.rm = TRUE), 2)*100)


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
    updateSelectInput(inputId = "virus", choices = choices, selected = "DENV")
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
  
  
  
  output$rain_overview <- renderPlotly({
    ggplotly(ggplot(graph, aes(x = Rainfall, y = `Incidence Percent`)) +
               geom_point() +
               stat_smooth(method = "lm")+
               theme_bw()+
               labs(title = "Incedence is predicted in terms of rainfall",
                    x = "Rainfall (in mm)"))%>%
      layout(plot_bgcolor  = "#fcf9f2",
             paper_bgcolor = "#fcf9f2",
             fig_bgcolor   = "#fcf9f2")
  })
  
  # rainfall map
  observeEvent(input$rain_map, {
    
    # if (input$rain_map == "LA"){
    #   choices <- c("Ross River" = "RRV",
    #                "Dengue" = "DENV",
    #                "Barmah Forest" = "BFV",
    #                "Murray Valley Encephalitis" = "MVEV",
    #                "West Nile/Kunjin" = "WNV")
    # }
    # else {
    # choices <- c("Dengue" = "DENV",
    #              "Zika" = "ZIKV",
    #              "West Nile/Kunjin" = "WNV",
    #              "Japanese Encephalitis" = "JEV",
    #              "Chikungunya" = "CHIKV")
    #}
    updateSelectInput(inputId = "rain_virus", choices = choices, selected = "DENV")
  })
  
  
  output$rainfall_map <- renderLeaflet({
    
    # rainfall map data 
    if (length(input$sa3_rainfall) != 0){
      sa3_region <- input$sa3_rainfall
    } else {
      sa3_region <- unique(fulldata$SA3_NAME_2011)
    }
    rain_map <- fulldata%>% 
      filter(SA3_NAME_2011 %in% sa3_region) %>% 
      group_by(SA3_NAME_2011, Virus_Name) %>% 
      summarise(Rainfall = round(mean(Rainavg), 2))
    
    shapefile_rain <- geo_join(shapefile, rain_map,
                               "SA3_NAME11", "SA3_NAME_2011")
    
    content_rain <- paste0(sep = "<br/>", "<b>SA3 Region: </b>",shapefile_rain$SA3_NAME_2011, "<br>",
                           "<b>Avg. Incidence Rate: </b>", round(shapefile_rain$Rainfall, 2))
    
    pal_rain <- colorNumeric("RdBu", rain_map$Rainfall, reverse = TRUE)
    
    leaflet(shapefile_rain) %>%
      addPolygons(weight = 1,
                  fillColor = ~pal_rain(Rainfall),
                  opacity = 0.5,
                  color = "white",
                  fillOpacity = 0.9,
                  smoothFactor = 0.5,
                  popup = content_rain) %>%
      addLegend("topright", pal = pal_rain, 
                values = ~Rainfall, opacity = 1, 
                title = "Average of Rainfall") %>%
      setMapWidgetStyle(list(background= "#fcf9f2"))
    
  })
  
  output$rainfall_graph <- renderPlotly({
    # rainfall map data 
    if (length(input$sa3_rainfall) != 0){
      sa3_region <- input$sa3_rainfall
    } else {
      sa3_region <- unique(fulldata$SA3_NAME_2011)
    }
    ggplotly(ggplot(w_graph)+
               theme_bw() +
               labs(title = "Comaparing Rainfall and Incidence Percent",
                    y = " ") + 
               scale_y_continuous(breaks = seq(0, 100, 10))+
               scale_x_continuous(breaks = c(2007:2020)) +
               
               geom_line(aes(x = Year, y = Rainfall), linetype = "dotdash",
                         color = "blue", alpha = 0.4, size = 1) + 
               annotate(geom="text", x=2008.5, y=65, 
                        label="Rainfall (in mm)", color="blue") +
               
               
               geom_line(aes(x = Year, y = `Incidence Percent`), 
                         color = "orange", alpha = 0.8, size = 1) +
               geom_point(aes(x = Year, y = `Incidence Percent`), 
                          color = "orange", alpha = 1, size = 1.5) +
               annotate(geom="text", x=2008.5, y=10, 
                        label="Incidence Percent", color="orange")) %>%
      layout(plot_bgcolor  = "#fcf9f2",
             paper_bgcolor = "#fcf9f2",
             fig_bgcolor   = "#fcf9f2")
  })
  
  
  
  output$temp_overview <- renderPlotly({
    ggplotly(ggplot(graph, aes(x = Max_Temprature, y = `Incidence Percent`)) +
               geom_point() +
               stat_smooth(method = "lm")+
               theme_bw()+
               labs(title = "Incedence is predicted in terms of temperature"))%>%
      layout(plot_bgcolor  = "#fcf9f2",
             paper_bgcolor = "#fcf9f2",
             fig_bgcolor   = "#fcf9f2")
  })
  
  
  observeEvent(input$tempmap, {
    
    # if (input$tempmap == "LA"){
    #   choices <- c("Ross River" = "RRV",
    #                "Dengue" = "DENV",
    #                "Barmah Forest" = "BFV",
    #                "Murray Valley Encephalitis" = "MVEV",
    #                "West Nile/Kunjin" = "WNV")
    # }
    # else {
    # choices <- c("Dengue" = "DENV",
    #              "Zika" = "ZIKV",
    #              "West Nile/Kunjin" = "WNV",
    #              "Japanese Encephalitis" = "JEV",
    #              "Chikungunya" = "CHIKV")
    #}
    updateSelectInput(inputId = "temp_virus", choices = choices, selected = "DENV")
  })
  
  output$temp_map <- renderLeaflet({
    
    # rainfall map data 
    if (length(input$sa3_temp) != 0){
      sa3_region <- input$sa3_temp
    } else {
      sa3_region <- unique(fulldata$SA3_NAME_2011)
    }
    # temperature map data 
    temp_map <- fulldata%>% 
      filter(SA3_NAME_2011 %in% sa3_region) %>% 
      group_by(SA3_NAME_2011, Virus_Name) %>% 
      summarise(Max_temp = round(mean(meanMaxTavg), 2),
                Min_temp = round(mean(meanMinTavg), 2),
                `Average Temperature` = round((Max_temp+Min_temp)/2, 2))
    
    shapefile_temp <- geo_join(shapefile, temp_map,
                               "SA3_NAME11", "SA3_NAME_2011")
    
    content_temp <- paste0(sep = "<br/>", "<b>SA3 Region: </b>",shapefile_temp$SA3_NAME_2011, "<br>",
                           "<b>Avg. Incidence Rate: </b>", round(shapefile_temp$Max_temp, 2))
    
    pal_temp <- colorNumeric("RdBu", temp_map$Max_temp, reverse = TRUE)
    
    leaflet(shapefile_temp) %>%
      addPolygons(weight = 1,
                  fillColor = ~pal_temp(Max_temp),
                  opacity = 0.5,
                  color = "white",
                  fillOpacity = 0.9,
                  smoothFactor = 0.5,
                  popup = content_temp) %>%
      addLegend("topright", pal = pal_temp, 
                values = ~Max_temp, opacity = 1, 
                title = "Average of Temperature")  %>%
      setMapWidgetStyle(list(background= "#fcf9f2"))
    
  })
  
  output$temp_graph <- renderPlotly({
    if (length(input$sa3_temp) != 0){
      sa3_region <- input$sa3_temp
    } else {
      sa3_region <- unique(fulldata$SA3_NAME_2011)
    }
    ggplotly(ggplot(w_graph)+
      theme_bw() +
      labs(title = "Comaparing Temperature and Incidence Percent",
           y = " ") + 
      scale_y_continuous(breaks = seq(0, 30, 5))+
      scale_x_continuous(breaks = c(2007:2020)) +
      
      geom_line(aes(x = Year, y = Min_Temprature), linetype = "twodash",
                color = "dark green", alpha = 0.4, size = 1) +
      annotate(geom="text", x=2008.9, y=14,
               label="Min Temperature (in °C)", color="dark green")+
      
      geom_line(aes(x = Year, y = Max_Temprature), linetype = "twodash",
                color = "red", alpha = 0.4, size = 1) +
      annotate(geom="text", x=2008.9, y=25,
               label="Max Temperature (in °C)", color="red")+
      
      geom_line(aes(x = Year, y = `Incidence Percent`), 
                color = "orange", alpha = 0.8, size = 1) +
      geom_point(aes(x = Year, y = `Incidence Percent`), 
                 color = "orange", alpha = 1, size = 1.5) +
      annotate(geom="text", x=2008.5, y=10, 
               label="Incidence Percent", color="orange"))%>%
      layout(plot_bgcolor  = "#fcf9f2",
             paper_bgcolor = "#fcf9f2",
             fig_bgcolor   = "#fcf9f2")
  })
  
})
