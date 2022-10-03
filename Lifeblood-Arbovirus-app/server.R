library(shiny)
library(tigris)
library(leaflet.extras)
library(rgeos)
library(plotly)

source("DataPreprocessing.R")
source("modules.R")

shinyServer(function(input, output, session) {

  output$connection_map <- renderLeaflet({
    final_map_data <- connection_data %>% 
      filter(Virus == input$connection_input)  

    content <- paste(sep = "<br/>",
                     "Virus Imported from: ", "<b>", final_map_data$`origin_country`, "</b>",
                     "Type of Virus: ",  "<b>", final_map_data$`Virus`, "</b>",
                     "Total Imported Cases: ", "<b>", final_map_data$`count`, "</b>")
    
    connection_map <- leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.DE) %>% 
      addCircleMarkers(lng=final_map_data$wlong, lat=final_map_data$wlat, 
                       popup=content, radius = 4, opacity = final_map_data$count, color = "#6cacbd") %>%
      addMarkers(lng = 137, lat = -23.2,
                 labelOptions = labelOptions(noHide = T),
                 icon = mapPickerIcon)
    
    for (i in 1:nrow(final_map_data)){
      connection_map <- connection_map %>% 
        addPolylines(lng = c(final_map_data$wlong[i],137),
                     lat = c(final_map_data$wlat[i],-23.2), weight=1.5, opacity=2, color="#025D75")
    }
    connection_map %>%  setView(0, 0, zoom = 2.4) 
  })
  
  toListen <- reactive({
    list(input$ir_virus, input$ir_type, input$ir_groupYear)
  })
  
  observeEvent(toListen(),{
    if (input$ir_type == "Local"){
      choices <- c("Ross River" = "RRV",
                   "Dengue" = "DENV",
                   "Barmah Forest" = "BFV",
                   "Murray Valley Encephalitis" = "MVEV",
                   "West Nile/Kunjin" = "WNV")
      updateSelectInput(inputId = "ir_virus", choices = choices, selected = input$ir_virus)
    }
    else {
      choices <- c("Dengue" = "DENV",
                   "Zika" = "ZIKV",
                   "West Nile/Kunjin" = "WNV",
                   "Japanese Encephalitis" = "JEV",
                   "Chikungunya" = "CHIKV")
      updateSelectInput(inputId = "ir_virus", choices = choices, selected = input$ir_virus)
    }
    
    if (input$ir_groupYear == "1"){
      start_year <- 2007
      end_year <- 2011
    } else {
      start_year <- 2012
      end_year <- 2017
    }
    
    data_avg <- full_data %>%
      filter(Year >= start_year & Year <= end_year &
               Virus_name == input$ir_virus & 
               Transmission == input$ir_type) %>%
      group_by(SA3_NAME_2011) %>%
      summarise(mean = mean(SUM_IR))
    
    if (length(data_avg > 0)){
      output$ir_map <- renderUI({renderLeaflet({
        
        content <- paste0(sep = "<br/>", "<b>SA3 Region: </b>",data_avg$SA3_NAME_2011, "<br>",
                          "<b>Avg. Incidence Rate: </b>", round(data_avg$mean, 4),
                          "<br>", "<b>Virus Name: </b>", input$ir_virus,
                          "<br>", "<b>Transmission Type: </b>", input$ir_type,
                          "<br>", "<b>Group Year: </b>", start_year, "-", end_year)
        
        shapefile_temp <- geo_join(shapefile, data_avg, "SA3_NAME11", "SA3_NAME_2011")
        shapefile_temp@data <- shapefile_temp@data %>% replace(is.na(.), 0)
        
        pal <- colorNumeric("RdBu", shapefile_temp$mean, reverse = TRUE)
        leaflet(shapefile_temp, height="100%") %>%
          addPolygons(weight = 1,
                      fillColor = ~pal(mean),
                      opacity = 0.5,
                      color = "white",
                      fillOpacity = 0.9,
                      smoothFactor = 0.5,
                      popup = content) %>%
          addLegend("topright", pal = pal, values = ~mean, opacity = 1, title = "Mean Incidence Rate") %>%
          setMapWidgetStyle(list(background= "white"))
      })})
    } else {
      output$ir_map <- renderUI({renderImage(
        list(src = "www/data_not_found.jpg", alt = "Data not found"
             , width = 800, height = 400, align = "center")
      )})
    }
  })
  
  output$dr_sa3_ui <- renderUI({
    selectInput("dr_sa3","Select SA3 Region: ",
                unique(full_data$SA3_NAME_2011), multiple = TRUE)
  })
  
  output$dr_graph <- renderUI({
    plot <- long_data
    if (!is.null(input$dr_sa3)){
      plot <- plot %>%
        filter(SA3_NAME_2011 %in% input$dr_sa3)
    }
    plot <- plot %>% 
      filter (format(Year, format = "%Y") >= input$dr_date[1], 
              format(Year, format = "%Y") <= input$dr_date[2]) %>% 
      group_by(Year, type) %>% 
      summarise(count = mean(count, na.rm = TRUE))
    
    if (length(plot) != 0){
      renderPlotly({
        plot <- plot %>%
          ggplot(aes(x = Year, y = count, color = type)) +
          geom_line() +
          geom_point()+
          scale_color_manual(values=c("#851e3e", "#009688")) +
          theme_light() +
          theme(legend.position="none")
          
        ggplotly(plot)%>%
          config(displayModeBar = FALSE)
      })
    } else {
      renderImage(
        list(src = "www/data_not_found.jpg", alt = "Data not found"
             , width = 600, height = 400, align = "center")
      )
    }
    })
  
  output$dr_map <- renderUI({
    data <- full_data
    if (!is.null(input$dr_sa3)){
      data <- data %>%
        filter(SA3_NAME_2011 %in% input$dr_sa3)
    } 
    data <- data %>% 
      filter (format(Year, format = "%Y") >= input$dr_date[1], 
              format(Year, format = "%Y") <= input$dr_date[2]) %>% 
      group_by(SA3_NAME_2011) %>%
      summarise(avg_donation_rate = mean(donationrate1000))
    
    if (length(data) != 0){
      renderLeaflet({
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
    } else {
      renderImage(
        list(src = "www/data_not_found.jpg", alt = "Data not found"
             , width = 600, height = 400, align = "center")
      )
    }
  })
  
  # Weather conditions
  
  ## temperature
  
  ### temperature lm overview
  output$Temperature_overview <- renderPlotly({
    ggplotly(ggplot(weather_data, aes(x = `Average Temperature`, y = `Incidence Percent`)) +
               geom_point() +
               stat_smooth(method = "lm")+
               theme_bw()+
               labs(title = "Incidence is predicted in terms of temperature"))%>%
      config(displayModeBar = FALSE)
  })
  
  ### temperature map
  output$Temperature_map <- renderUI({
    temperature_map_data <- temperature_map_data %>% filter(Virus_name == input$Temperature_virus)
    if (length(temperature_map_data) != 0){
      renderLeaflet({
        shapefile_temp <- geo_join(shapefile, temperature_map_data,
                                   "SA3_NAME11", "SA3_NAME_2011")
        shapefile_temp@data <- shapefile_temp@data %>% replace(is.na(.), 0)
        
        content_temp <- paste0(sep = "<br/>", "<b>SA3 Region: </b>",shapefile_temp$SA3_NAME_2011, "<br>",
                               "<b>Avg. Incidence Rate: </b>", round(shapefile_temp$Max_temp, 2))
        
        pal_temp <- colorNumeric("RdBu", temperature_map_data$Max_temp, reverse = TRUE)
        
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
                    title = "Average of Temperature") %>% 
          setMapWidgetStyle(list(background= "white"))
      })
    } else {
      renderImage(
        list(src = "www/data_not_found.jpg", alt = "Data not found"
             , width = 600, height = 400, align = "center")
      )
    }
    
  })
  
  ### temperature line graph
  output$Temperature_graph <- renderUI({
    
    ggplotly(ggplot(weather_graph)+
               theme_bw() +
               labs(title = "Comparing Temperature and Incidence ",
                    y = " ") + 
               scale_y_continuous(breaks = seq(0, 30, 5))+
               scale_x_continuous(breaks = c(2007:2020)) +
               
               geom_line(aes(x = Year, y = `Average Temperature`),
                         color = "red", alpha = 0.4, size = 1) +
               geom_point(aes(x = Year, y = `Average Temperature`), 
                          color = "red", alpha = 1, size = 1.5)+
               annotate(geom="text", x=2011, y=16,
                        label="Average Temperature (in °C)", color="red")+

               geom_line(aes(x = Year, y = `Incidence Percent`), 
                         color = "orange", alpha = 0.8, size = 1) +
               geom_point(aes(x = Year, y = `Incidence Percent`), 
                          color = "orange", alpha = 1, size = 1.5) +
               annotate(geom="text", x=2009, y=7, 
                        label="Incidence Percent", color="orange"))%>%
      config(displayModeBar = FALSE)
  })
  
  ## rainfall
  
  ### rainfall lm overview
  output$Rainfall_overview <- renderPlotly({
    ggplotly(ggplot(weather_data, aes(x = Rainfall, y = `Incidence Percent`)) +
               geom_point() +
               stat_smooth(method = "lm")+
               theme_bw()+
               labs(title = "Incidence is predicted in terms of Rainfall"))%>%
      config(displayModeBar = FALSE)
  })
  
  ### rainfall map
  output$Rainfall_map <- renderUI({
    map_data <- rain_map_data %>% filter(Virus_name == input$Rainfall_virus)
    if (length(map_data) != 0){
      renderLeaflet({
        shapefile_temp <- geo_join(shapefile, map_data,
                                   "SA3_NAME11", "SA3_NAME_2011")
        shapefile_temp@data <- shapefile_temp@data %>% replace(is.na(.), 0)
        
        content_temp <- paste0(sep = "<br/>", "<b>SA3 Region: </b>",shapefile_temp$SA3_NAME_2011, "<br>",
                               "<b>Avg. Incidence Rate: </b>", round(shapefile_temp$Rainfall, 2))
        
        pal_temp <- colorNumeric("RdBu", map_data$Rainfall, reverse = TRUE)
        
        leaflet(shapefile_temp) %>%
          addPolygons(weight = 1,
                      fillColor = ~pal_temp(Rainfall),
                      opacity = 0.5,
                      color = "white",
                      fillOpacity = 0.9,
                      smoothFactor = 0.5,
                      popup = content_temp) %>%
          addLegend("topright", pal = pal_temp, 
                    values = ~Rainfall, opacity = 1, 
                    title = "Average of Rainfall") %>% 
          setMapWidgetStyle(list(background= "white"))
      })
    } else {
      renderImage(
        list(src = "www/data_not_found.jpg", alt = "Data not found"
             , width = 600, height = 400, align = "center")
      )
    }
    
  })
  
  ### rainfall graph
  output$Rainfall_graph <- renderUI({
    
    ggplotly(ggplot(weather_graph)+
               theme_bw() +
               labs(title = "Comaparing Rainfall and Incidence",
                    y = " ") + 
               scale_x_continuous(breaks = c(2007:2020)) +
               
               geom_line(aes(x = Year, y = Rainfall), 
                         color = "blue", alpha = 0.4, size = 1) +
               geom_point(aes(x = Year, y = Rainfall), 
                          color = "blue", alpha = 1, size = 1.5) +
               annotate(geom="text", x=2009, y=65, 
                        label="Rainfall (in mm)", color="blue") +
               
               geom_line(aes(x = Year, y = `Incidence Percent`), 
                         color = "orange", alpha = 0.8, size = 1) +
               geom_point(aes(x = Year, y = `Incidence Percent`), 
                          color = "orange", alpha = 1, size = 1.5) +
               annotate(geom="text", x=2009, y=10, 
                        label="Incidence Percent", color="orange"))%>%
      config(displayModeBar = FALSE)
  })
  
  
  ## humidity
  
  ### humidity lm overview
  output$Humidity_overview <- renderPlotly({
    ggplotly(ggplot(weather_data, aes(x = `Average Humidity`, y = `Incidence Percent`)) +
               geom_point() +
               stat_smooth(method = "lm")+
               theme_bw()+
               labs(title = "Incidence is predicted in terms of Humidity"))%>%
      config(displayModeBar = FALSE)
  })
  
  ### humidity map
  output$Humidity_map <- renderUI({
    humidity_map_data <- humidity_map_data %>% filter(Virus_name == input$Temperature_virus)
    if (length(humidity_map_data) != 0){
      renderLeaflet({
        shapefile_temp <- geo_join(shapefile, humidity_map_data,
                                   "SA3_NAME11", "SA3_NAME_2011")
        shapefile_temp@data <- shapefile_temp@data %>% replace(is.na(.), 0)
        
        content_temp <- paste0(sep = "<br/>", "<b>SA3 Region: </b>",shapefile_temp$SA3_NAME_2011, "<br>",
                               "<b>Avg. Incidence Rate: </b>", round(shapefile_temp$Max_humd, 2))
        
        pal_temp <- colorNumeric("RdBu", humidity_map_data$Max_humd, reverse = TRUE)
        
        leaflet(shapefile_temp) %>%
          addPolygons(weight = 1,
                      fillColor = ~pal_temp(Max_humd),
                      opacity = 0.5,
                      color = "white",
                      fillOpacity = 0.9,
                      smoothFactor = 0.5,
                      popup = content_temp) %>%
          addLegend("topright", pal = pal_temp, 
                    values = ~Max_humd, opacity = 1, 
                    title = "Average of Humidity") %>% 
          setMapWidgetStyle(list(background= "white"))
      })
    } else {
      renderImage(
        list(src = "www/data_not_found.jpg", alt = "Data not found"
             , width = 600, height = 400, align = "center")
      )
    }
    
  })
  
  ### humidity line graph
  output$Humidity_graph <- renderUI({
    
    ggplotly(ggplot(weather_graph)+
               theme_bw() +
               labs(title = "Comaparing Humidity and Incidence ",
                    y = " ") + 
               scale_x_continuous(breaks = c(2007:2020)) +
               
               geom_line(aes(x = Year, y = `Average Humidity`), 
                         color = "dark green", alpha = 0.4, size = 1) +
               geom_point(aes(x = Year, y = `Average Humidity`), 
                          color = "dark green", alpha = 1, size = 1.5) +
               annotate(geom="text", x=2009, y=60,
                        label="Average Humidity", color="dark green")+

               geom_line(aes(x = Year, y = `Incidence Percent`), 
                         color = "orange", alpha = 0.8, size = 1) +
               geom_point(aes(x = Year, y = `Incidence Percent`), 
                          color = "orange", alpha = 1, size = 1.5) +
               annotate(geom="text", x=2009, y=10, 
                        label="Incidence Percent", color="orange"))%>%
      config(displayModeBar = FALSE)
  })
})
