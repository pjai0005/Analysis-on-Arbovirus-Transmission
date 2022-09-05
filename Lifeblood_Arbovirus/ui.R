library(shiny)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(plotly)

mycss <- "
#plot-container {
  position: relative;
}
#loading-spinner {
  position: absolute;
  left: 50%;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
"

full_data <- read_csv(file.choose())

ui <- fluidPage(
  tags$head(tags$style(HTML(mycss))),
  actionButton("btn", "Plot (takes 2 seconds)"),
  div(id = "plot-container",
      tags$img(src = "spinner.gif",
               id = "loading-spinner"),
      plotOutput("plot")
  )
)

shinyUI(fluidPage(
  tags$head(tags$style(HTML(mycss))),
  theme = shinytheme("flatly"),
  navbarPage(position = "fixed-top","Arbovirus Transmission",
             tabPanel("Incidence Rate",
                      fluidRow(
                        HTML('<img src="Arbovirus.gif" width="100%" height="auto">'), 
                        setBackgroundColor("#fcf9f2"),
                        HTML('<p style = "text-align: justify; text-justify: inter-word;"><br><br>Arboviruses remain a significant concern for public health in Australia and worldwide. Three of the most common and clinically important arboviruses in Australia of today include Ross River virus (RRV), Barmah Forest virus (BFV), and dengue virus (DENV). These mosquito-borne diseases have exhibited an upward trend in Australia since 2002. Concern has arisen that these numbers will only continue to increase as a result of climate change. Arboviral disease risk is “a function of spatial and temporal patterns of vector breeding habitats and factors that affect distribution” (Dale et al 1998). The identification of environmental patterns and parameters that affect vector distribution, as well as spatio-temporal dynamics of disease transmission, are necessary to predict and prevent future transmission of arboviruses in Australia. Given that arboviruses are of concern due to their potential to impact on both the safety and the supply of product, possible implications for blood transfusion safety will be discussed.<br><p>'),
                        HTML("<br><h2>Imported Viruses Overview</h2>"),
                        column(width = 9, 
                               div(id = "plot-container",
                                   tags$img(src = "spinner.gif",
                                            id = "loading-spinner"),
                                   leafletOutput("connection_map", height = 500)
                               )),
                        column(width = 2, radioButtons("virus_connection",
                                                       "Select a Virus: ",
                                                       c("Dengue" = "Dengue", 
                                                         "West Nile Kunjin" = "West_Nile_Kunjin", 
                                                         "Zika" = "Zika", 
                                                         "Chikungunya" = "Chikungunya",
                                                         "Japanese Encephalitis Virus" = "Japanese_Encephalitis_Virus"
                                                       )))),
                      fluidRow(
                        HTML("<br><h2>Incidence Rate for Group Year by SA3 Region</h2>"),
                        column(width = 2, radioButtons("groupYear",
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
                        column(width = 2),
                        column(width = 8,
                               div(id = "plot-container",
                                   tags$img(src = "spinner.gif",
                                            id = "loading-spinner"),
                                   leafletOutput("ir_map", height = 500)
                               ))
                      ),
                      fluidRow(HTML("<br><h2>Comparison of Cummulative Incidence Rate</h2>"),
                               column(width = 4, HTML('<img src="gganim_imp.gif" width="600" height="500">')),
                               column(width = 2),
                               column(width = 4, HTML('<img src="gganim_local.gif" width="600" height="500">'))
                      )),
             tabPanel("Donation Rate",
                      setBackgroundColor("#fcf9f2"),
                      fluidRow(HTML('<img src="Donation.gif" width="100%" height="auto">'), 
                               setBackgroundColor("#fcf9f2"),
                               
                        column(width = 5, 
                               HTML("<br><br><br><h2>Donation Rate by SA3 Region</h2>"),
                               div(id = "plot-container",
                                   tags$img(src = "spinner.gif",
                                            id = "loading-spinner"),
                                   leafletOutput("dr_map", height = 500)
                               )),
                        column(width = 2,
                               HTML("<br><br><br><br><br><br><br><br>
                                    <br><br><br><br><br><br>"),
                               sliderInput("date_slider", "Date Range", min = min(full_data$Year), max = max(full_data$Year),
                                           value = c(2007, 2017), sep = ""),
                               selectInput("sa3_donation","Select SA3 Region: ",
                                           unique(full_data$SA3_NAME_2011), multiple = TRUE)
                        ),
                        column (width = 5,
                                HTML("<br><br><br><h2>Donation Rate vs Incidence Rate</h2>"),
                                div(id = "plot-container",
                                    tags$img(src = "spinner.gif",
                                             id = "loading-spinner"),
                                    plotlyOutput("dr_graph", height = 500)
                                )
                        )
                      )),
             tabPanel("Weather Conditions",
                      setBackgroundColor("#fcf9f2"),
                      fluidRow( # overview of rainfall
                        column(width = 3),
                        column(width = 5, 
                               HTML("<br><br><br><h2>Overview of Rainfall in SA3 Regions</h2>"),
                               div(id = "plot-container",
                                   tags$img(src = "spinner.gif",
                                            id = "loading-spinner"),
                                   plotlyOutput("rain_overview", height = 500)
                               )),
                        
                        column(width = 3)),
                      
                      fluidRow(# rainfall map
                        column(width = 5, 
                               HTML("<br><br><br><h2>Rainfall by SA3 Region</h2>"),
                               div(id = "plot-container",
                                   tags$img(src = "spinner.gif",
                                            id = "loading-spinner"),
                                   leafletOutput("rainfall_map", height = 500)
                               )),
                              # rainfall selectors
                        column(width = 2,
                               HTML("<br><br><br><br><br><br><br><br>
                                    <br><br><br><br><br><br>"),
                               # radioButtons("rain_map",
                               #              "Locations: ",
                               #              c("Locally Acquired" = "LA",
                               #                "Imported" = "IMP")),
                               # selectInput("virus",
                               #             "Select a Virus:",
                               #             c("Ross River" = "RRV",
                               #               "Dengue" = "DENV",
                               #               "Barmah Forest" = "BFV",
                               #               "Zika" = "ZIKV",
                               #               "Murray Valley Encephalitis" = "MVEV",
                               #               "West Nile/Kunjin" = "WNV",
                               #               "Japanese Encephalitis" = "JEV",
                               #               "Chikungunya" = "CHIKV")),
                               selectInput("sa3_rainfall","Select SA3 Region: ",
                                           unique(full_data$SA3_NAME_2011), multiple = TRUE)
                        ),
                        # rainfall graph
                        column(width = 5, 
                               HTML("<br><br><br><h2> Rainfall and Incidence Percent </h2>"),
                               div(id = "plot-container",
                                   tags$img(src = "spinner.gif",
                                            id = "loading-spinner"),
                                   plotlyOutput("rainfall_graph", height = 500)
                               ))
                        ),
                      
                      
                      
                      
                      fluidRow( # overview of temperature
                        column(width = 3),
                        column(width = 5, 
                               HTML("<br><br><br><h2>Overview of Temperature in SA3 Regions</h2>"),
                               div(id = "plot-container",
                                   tags$img(src = "spinner.gif",
                                            id = "loading-spinner"),
                                   plotlyOutput("temp_overview", height = 500)
                               )),
                        
                        column(width = 3)),
                      
                      fluidRow(# temperature map
                        column(width = 5, 
                               HTML("<br><br><br><h2>Temperature by SA3 Region</h2>"),
                               div(id = "plot-container",
                                   tags$img(src = "spinner.gif",
                                            id = "loading-spinner"),
                                   leafletOutput("temp_map", height = 500)
                               )),
                        # temperature selectors
                        column(width = 2,
                               HTML("<br><br><br><br><br><br><br><br>
                                    <br><br><br><br><br><br>"),
                               # radioButtons("rain_map",
                               #              "Locations: ",
                               #              c("Locally Acquired" = "LA",
                               #                "Imported" = "IMP")),
                               # selectInput("virus",
                               #             "Select a Virus:",
                               #             c("Ross River" = "RRV",
                               #               "Dengue" = "DENV",
                               #               "Barmah Forest" = "BFV",
                               #               "Zika" = "ZIKV",
                               #               "Murray Valley Encephalitis" = "MVEV",
                               #               "West Nile/Kunjin" = "WNV",
                               #               "Japanese Encephalitis" = "JEV",
                               #               "Chikungunya" = "CHIKV")),
                               selectInput("sa3_temp","Select SA3 Region: ",
                                           unique(full_data$SA3_NAME_2011), multiple = TRUE)
                        ),
                        # temperature graph
                        column(width = 5, 
                               HTML("<br><br><br><h2> Temperature and Incidence Percent </h2>"),
                               div(id = "plot-container",
                                   tags$img(src = "spinner.gif",
                                            id = "loading-spinner"),
                                   plotlyOutput("temp_graph", height = 500)
                               ))
                      )
    
                      
                      
             )
  )
)
)