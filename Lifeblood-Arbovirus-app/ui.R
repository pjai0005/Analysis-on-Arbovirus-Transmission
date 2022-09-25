# Loading Libraries
library(shiny)
library(leaflet)
library(plotly)
source("modules.R")

min_year = 2007
max_year = 2017

shinyUI(fluidPage(includeCSS("www/theme.css")
                  , navbarPage(position = "fixed-top", "Arbovirus Transmission", selected = "Incidence Rate"
                               , theme = "www/theme.css"
                               , tabPanel("Incidence Rate"
                                          , fluidRow(HTML('<img src="Arbovirus.gif" width="100%" height="auto">'),
                               HTML('<p style = "text-align: justify; text-justify: inter-word;"><br><br>Arboviruses remain a significant concern for public health in Australia and worldwide. Three of the most common and clinically important arboviruses in Australia of today include Ross River virus (RRV), Barmah Forest virus (BFV), and dengue virus (DENV). These mosquito-borne diseases have exhibited an upward trend in Australia since 2002. Concern has arisen that these numbers will only continue to increase as a result of climate change. Arboviral disease risk is “a function of spatial and temporal patterns of vector breeding habitats and factors that affect distribution” (Dale et al 1998). The identification of environmental patterns and parameters that affect vector distribution, as well as spatio-temporal dynamics of disease transmission, are necessary to predict and prevent future transmission of arboviruses in Australia. Given that arboviruses are of concern due to their potential to impact on both the safety and the supply of product, possible implications for blood transfusion safety will be discussed.<br></p>')
                             )
                             , HTML('<h2>Imported Cases Overview</h2>')
                             , fluidRow(column(width = 9, load_spinner(leafletOutput("connection_map"
                                                                                    , height = 500)))
                                     , column(width = 2, virus_input("connection_input")))
                             , HTML('<h2>Incidence Rate for Group Year by SA3 Region</h2>')
                             , fluidRow(column(width = 2
                                               , radioButtons("ir_groupYear", "Select a Group Year:",
                                                                       c("2007-2011" = "1",
                                                                         "2012-2017" = "2"))
                                               , radioButtons("ir_type",
                                                            "Transmission Type: ",
                                                            c("Locally Acquired" = "Local",
                                                              "Imported" = "Imported"))
                                               , selectInput("ir_virus",
                                                             "Select a Virus:",
                                                             c("Ross River" = "RRV",
                                                               "Dengue" = "DENV",
                                                               "Barmah Forest" = "BFV",
                                                               "Murray Valley Encephalitis" = "MVEV",
                                                               "West Nile/Kunjin" = "WNV")))
                                        , column(width = 9, load_spinner(uiOutput("ir_map"))))
                             , HTML('<h2>Comparison of Cummulative Incidence Rate</h2>')
                             , fluidRow(column(width = 5, HTML('<img src="gganim_imp.gif" width="600" height="500">')),
                                        column(width = 5, HTML('<img src="gganim_local.gif" width="600" height="500">')))
                            )
                   , tabPanel("Donation Rate"
                              , fluidRow(HTML('<img src="Donation.gif" width="100%" height="auto">')
                                         , HTML('<h2>Donation Rate by SA3 Region</h2>'))
                              , fluidRow(column(width = 5, load_spinner(uiOutput("dr_map")))
                                         ,column(width = 2
                                                 , sliderInput("dr_date", "Date Range"
                                                  , min = 2007, max = 2017,
                                                                        value = c(2007, 2017), sep = "")
                                                 , uiOutput("dr_sa3_ui")
                                                 )
                                         , column(width = 5, load_spinner(uiOutput("dr_graph")))
                                         )
                              )
                   , tabPanel("Weather Conditions", 
                              HTML("<br><br><br>"),
                    tabsetPanel(
                     tabPanel("Temperature", weather_cond_ui("Temperature")),
                     tabPanel("Rainfall", weather_cond_ui("Rainfall")),
                     tabPanel("Humidity", weather_cond_ui("Humidity"))
                   ))
                   , footer = HTML('<div class = "footer">©2021-2022 Australian Red Cross Lifeblood</div>')
                  )
                  ))
