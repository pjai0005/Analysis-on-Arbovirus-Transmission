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
                                                     HTML('<p style = "text-align: justify; text-justify: inter-word;"><br><br>Arboviruses continue to be a major threat to public health in Australia and around the 
                                                          world. Emerging or re-emerging vector-borne diseases are a significant concern for global
                                                          health. A thorough understanding of diseases and interactions in their natural setting is 
                                                          crucial for developing successful diagnostics, vaccinations, and other control methods. 
                                                          The purpose of this report is to present the role of arboviruses in Australias SA3 regions.
                                                          The project is created entirely with R studio and analyzes data using the programming language
                                                          R. This work catalyzes the implementation of an R Shiny App that will provide future insights
                                                          into the analysis of Arbovirus transmission within and outside of Australia under various
                                                          conditions. Given that arboviruses are of concern due to their potential to impact both 
                                                          the safety and the supply of products, possible implications for blood transfusion safety
                                                          will be discussed. Various weather conditions like temperature, humidity, and rainfall and 
                                                          their implications on the donation rate and incidence rate will also be analyzed.<br></p>
                                                          
                                                          <p>Although Australia has documented more than 70 arboviruses, only a few are human diseases, and 
                                                          even fewer are of serious concern.Each has its own unique ecological and epidemiological 
                                                          properties, and each has shown an inclination to expand and settle in new locations, which is 
                                                          causing increasing worry. The goal of this review is to summarize the activities of these 
                                                          viruses as well as to comment on new information on their ecology and spread.<br></p>')
                                          )
                                          , HTML('<h2>Imported Cases Overview</h2>')
                                          , fluidRow(column(width = 9, load_spinner(leafletOutput("connection_map"
                                                                                                  , height = 500)))
                                                     , column(width = 2, virus_input("connection_input"))),
                                          HTML('<p style = "text-align: justify; text-justify: inter-word;"><br><br>
                                               Out of the 70 arboviruses (mentioned earlier in the background section), we have Japanese 
                                               Encephalitis, Zika Virus, Chikungunya, West Nile Kunjin, and, most significantly, Dengue Virus, 
                                               which are a few of them addressed in our dataset. These viruses are either currently or in the 
                                               past and have caused severe human diseases in Australia. Over the last decade, all of these viruses 
                                               have been linked to human illnesses. And this can be confirmed from the map above, where we can see 
                                               that the dengue virus has the most significant infection rate of any virus. With the increasing 
                                               frequency of dengue virus introductions over the past decade, there has been some concern expressed 
                                               that dengue might become endemic in the north-east of Australia.<br></p>
                                               
                                               <p>According to the map above, dengue has one of the greatest numbers of viruses entering Australia, 
                                               with a total of 9,218 cases. Countries such as Indonesia (6,545 cases) and Thailand (1,387) exported
                                               almost 86% of the total number of dengue viruses in Australia. Another virus-like Chikungunya is the 
                                               second most imported virus, with 241 cases imported from Indonesia and India (476 cases). In 
                                               comparison, the West Nile Kunjin virus has one of the lowest numbers of occurrences in Australia, 
                                               with only 6 cases reported. As a result, the nations with the most instances of the virus in 
                                               Australia are Indonesia, Thailand, and India, which contribute 6,667, 1,367, and 591 cases, 
                                               respectively.<br></p>')
                                          
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
                                                     column(width = 1),
                                                     column(width = 5, HTML('<img src="gganim_local.gif" width="600" height="500">'))),
                                          HTML('<p style = "text-align: justify; text-justify: inter-word;"><br><br>
                                          There are eight different viruses in all that we are dealing with, five of which are classified
                                          as imported and five as locally transmitted. The reason for this is that both groups contain the 
                                          dengue virus and the West Nile Kunjin virus. As a result, locally transmitted viruses are Ross 
                                          River Virus, Barmah Forest Virus, Murray Valley Encephalitis Virus, Dengue Virus, and West Nile 
                                          Kunjin. And for the foreign acquired viruses, we have Chikungunya, Japanese Encephalitis Virus, 
                                          Zika Virus, Dengue Virus, and West Nile Kunjin Virus. From the animation graph above, we can see
                                          that for locally transmitted viruses, Ross River Virus and Barmah Forest Virus are much higher 
                                          than the other viruses, and in contrast to this, we have West Nile Kunjin Virus with the lowest 
                                          sum incidence rate for virtually all the years.<br></p>
                                               
                                          <p>Moving on to the foreign-acquired viruses, we notice that Dengue Virus has been continuously high from 
                                          2007 to 2017, followed by the competition between Chikungunya and Japanese Encephalitis Virus. When it 
                                          Nile Kunjin Virus has the lowest occurrence rate for both categories. We discovered that the highest 
                                          occurrence of locally transmitted viruses occurred in 2015, and the highest incidence of viruses acquired
                                          abroad occurred in 2016, after breaking down the data by year for the animation above. The prevalence of 
                                          viruses acquired abroad is lowest in 2007, whereas that of locally transmitted viruses was lowest in 2016. 
                                          Its interesting to note that 2013 has the second-highest total incidence rate for viruses that are acquired
                                          locally and abroad.</p>')
                                          
                                          
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
                                          ),
                                          
                                          fluidRow(HTML('<h2>Statistical Analysis</h2>')),
                                          fluidRow(column(width = 3),
                                                   column(width = 6,dataTableOutput('overview_table')),
                                                   column(width = 3)),
                                          HTML('<p style = "text-align: justify; text-justify: inter-word;"><br><br>
                                               We performed some statistical analysis in order to uncover patterns, trends, and other significant details 
                                               in the data. We specifically selected negative binomial regression, which is a technique very close to 
                                               multiple regression. There is one difference, though: in negative binomial regression, the dependent variable
                                               follows the negative binomial. The variables can therefore be positive or negative integers. These 
                                               regressions are one of the most popular modelling techniques for over-dispersed count outcome variables.
                                               Negative binomial regression can be used when the conditional variance is greater than the conditional mean,
                                               as is the case for over-dispersed count data. And as seen from the table below, the variance is not so much
                                               greater but yet justifies negative binomial regression.<br></p>'),
                                          
                                          
                                          
                                          fluidRow(HTML('<h3>Negative Binomial</h3>')),
                                          fluidRow(column(12,dataTableOutput('aic_table'))),
                                          HTML('<p style = "text-align: justify; text-justify: inter-word;"><br><br>
                                               We conducted the regressions by keeping the donation rate as our predictor variable and the incidence rate as 
                                               the dependent variable for all the models. In addition to several negative binomial regressions, we also used 
                                               Poisson regression to see if the data contained count data, which are discrete data with non-negative integer 
                                               values. Also, negative binomial regression shares many common assumptions with Poisson regression, such as 
                                               linearity in model parameters, independence of individual observations, and the multiplicative effects of 
                                               independent variables.<br></p>
                                               
                                               <p>Negative Binomial (Weather) and Negative Binomial (Full Data) have additional parameters like weather conditions 
                                               (rainfall, temperature, and humidity) and all variables respectively as their dependent variables. We discovered that 
                                               expanding the number of parameters improves the AIC values of negative binomial regression. The Akaike information criterion 
                                               is a metric used to evaluate the quality of a model. Low complexity and a good fit are indications of a model with a low AIC.
                                               As a result, we chose the basic negative binomial model (donation rate as our predictor variable and the incidence rate as 
                                               the dependent variable) for further analysis.<br></p>'),
                                          
                                          
                                          fluidRow(HTML('<h3>Summary Statistics of Negative Binomial</h3>')),
                                          fluidRow(column(width = 3),
                                                   column(width = 6, HTML('<img src="SummaryTable.png" width="120%" height="auto">')),
                                                   column(width = 3)),
                                          HTML('<p style = "text-align: justify; text-justify: inter-word;"><br><br>
                                               The table above shows the summary statistics of the chosen negative binomial model.
                                               <ul>
                                               <li>Median: Because the median deviance residual is near to zero, our model is not biased in either
                                               way which means the outcome is neither over- nor underestimated). </li>
                                               <li>Coefficients: </li>
                                               <li>Null Deviance: It is implied by a low null deviation that the data can be adequately described by 
                                               utilising the intercept alone. Consider utilising a few data-modelling features if the null deviance 
                                               is minimal. If the null deviation is significant, then fitting the model with more than one parameter 
                                               makes sense. </li>
                                               <li>Residual Deviance: A low residual deviation indicates that your trained model is suitable.</li>
                                               <li>Degree of freedom: The residual deviation should be near to the degrees of freedom for a well-fitting 
                                               model, which is not the case in this instance.</li></ul><br></p>'),
                                          
                                          
                                          
                                          fluidRow(HTML('<h3>Underfitting and Overfitting Data </h3>')),
                                          fluidRow(column(12,load_spinner(plotOutput('rootogram')))),
                                          HTML('<p style = "text-align: justify; text-justify: inter-word;"><br><br>
                                               The theoretical Negative Binomial fit is represented by the red curved line. The height indicates the gap 
                                               between expected and observed numbers is "hanging" from each position on the bar. A hanging bar below 0 suggests
                                               underfitting, whereas a hanging bar above 0 indicates overfitting. And, because the bars do not lie in just about 
                                               any direction, the data is a well-fitted model.</p>')
                                          
                               )
                               , tabPanel("Weather Conditions", 
                                          HTML("<br><br><br>"),
                                          fluidRow(HTML('<h2> Weather Conditions </h2>')),
                                          HTML('<p style = "text-align: justify; text-justify: inter-word;">
                                               Considerable attention has been drawn over the past few years to the possible effects of global warming on human 
                                               health. Out of these infectious diseases, those most likely to be affected by global warming are diseases that are 
                                               transmitted by insect vectors, especially mosquito-borne arboviruses. The importance of weather in the genesis of 
                                               outbreaks of human arboviral disease in Australia has been widely recognised. In particular, heavy rainfall and 
                                               flooding may result in outbreaks of these viruses. Let us have look at each weather condition below.</p>'),
                                          
                                          tabsetPanel(
                                            tabPanel("Temperature", weather_cond_ui("Temperature")),
                                            tabPanel("Rainfall", weather_cond_ui("Rainfall")),
                                            tabPanel("Humidity", weather_cond_ui("Humidity"))
                                          ))
                               , footer = HTML('<div class = "footer">©2021-2022 Australian Red Cross Lifeblood</div>')
                  )
))