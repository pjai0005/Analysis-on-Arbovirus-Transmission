library(shiny)
library(shinyWidgets)
library(shinythemes)
library(leaflet)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Arbovirus Transmission",
             tabPanel("Incidence Rate"),
             tabPanel("Donation Rate"),
             tabPanel("Weather Conditions")
  ),
  HTML('<img src="huge.gif" width="1400" height="650">'), 
  setBackgroundColor("#fcf9f2"),
  HTML('<p style = "text-align: justify; text-justify: inter-word;"><br><br>Arboviruses remain a significant concern for public health in Australia and worldwide. Three of the most common and clinically important arboviruses in Australia of today include Ross River virus (RRV), Barmah Forest virus (BFV), and dengue virus (DENV). These mosquito-borne diseases have exhibited an upward trend in Australia since 2002. Concern has arisen that these numbers will only continue to increase as a result of climate change. Arboviral disease risk is “a function of spatial and temporal patterns of vector breeding habitats and factors that affect distribution” (Dale et al 1998). The identification of environmental patterns and parameters that affect vector distribution, as well as spatio-temporal dynamics of disease transmission, are necessary to predict and prevent future transmission of arboviruses in Australia. Given that arboviruses are of concern due to their potential to impact on both the safety and the supply of product, possible implications for blood transfusion safety will be discussed.<br><p>'),
  fluidRow(
    HTML("<br><h2>Incidence Rate for Group Year by SA3 Region</h2>"),
    sidebarPanel(style = "background-color: #fcf9f2;",
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
))
