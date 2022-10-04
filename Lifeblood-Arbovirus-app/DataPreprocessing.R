# Loading Required Libraries----------------------------------------------------
library(readxl)
library(rgdal)
library(tidyverse)
library(tigris)

# Loading required datasets---------------------------------------------------------
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

# List of viruses
virus_list <-  c("Dengue", "West_Nile_Kunjin", "Zika", "Chikungunya", "Japanese_Encephalitis_Virus")

# Preprocessing of full data---------------------------------------------------------

# Get sum of all incidence rates from all viruses
full_data <- cbind(full_data, full_data %>% select(contains("IR")) %>% rowSums())
names(full_data)[ncol(full_data)] <- "SUM_IR"

# Convert data to long form for easy graph plotting
long_data <- full_data %>% pivot_longer(cols = c("donationrate1000", "SUM_IR")
                                        , names_to = "type", values_to = "count") %>% 
  mutate(Year = as.Date(ISOdate(Year, 1, 1)))

# Get Center of all states-------------------------------------------------------
centroid <- get_centroid(shapefile) %>% cbind(shapefile$SA3_NAME11)
colnames(centroid) <- c("long", "lat", "SA3_NAME_2011")

# Getting the data for connection map--------------------------------------------
# accessing all the sheets 
sheet = excel_sheets(imported)

# applying sheet names to dataframe names
connection_data = lapply(setNames(sheet, sheet), 
                    function(x) read_excel(imported, sheet=x))

# attaching all dataframes together
connection_data = bind_rows(connection_data, .id="Virus")

# Country lookup table for replacing country names that are not in the world_df
country_lu <- data.frame(original_name = c("China (excludes SARs and Taiwan)"
                           , "Venezuela, Bolivarian Republic of"
                           , "Samoa, American"
                           , "Bolivia, Plurinational State of"
                           , "Congo, Democratic Republic of"
                           , "Macau (SAR of China)"
                           , "United Kingdom, Channel Islands and Isle of Man"
                           , "Hong Kong (SAR of China)"
                           , "Congo, Republic of")
                         , replace_with = c("China", "Venezuela", "Samoa", "Bolivia", "Congo"
                             , "Macau", "United Kingdom", "Hong Kong", "Congo")
                           )

replace_country <- function(x){
  if (x %in% country_lu$original_name){
    country_lu$replace_with[match(x, country_lu$original_name)]
  }
  else x
}
connection_data <- connection_data %>% rename("origin_country" = "Country of Origin")
connection_data$origin_country <- lapply(connection_data$origin_country, 
                               replace_country)

connection_data <- connection_data %>% 
  filter(!origin_country %in% c("No data available", "Overseas - Country unknown", 
                                     "At Sea", "Unknown", "No data supplied", "Australia") &
           !`Residential location (SA3)` %in% c("No data available", "Overseas - Country unknown", 
                                                "At Sea", "No data supplied", "Unknown")) %>% 
  mutate(Virus = str_remove(Virus, " virus"))

connection_data <- connection_data[!grepl("nfd|nec", connection_data$origin_country),]

connection_data$origin_country <- as.character(connection_data$origin_country) 
connection_data <- connection_data %>% 
  group_by(Virus, origin_country) %>% 
  summarise(count = sum(Count)) %>% 
  inner_join(world_df, by= c("origin_country" = "wname"))

connection_data <- connection_data %>% 
  group_by(Virus, origin_country) %>% 
  summarise(wlat = mean(wlat),
            wlong = mean(wlong),
            count  = mean(count))  

# Pivoting full data to get virus names in a column----------------------------------------
full_data <- full_data %>% 
  pivot_longer(cols = c("BFV_CountLA" : "CHIKV_CountIMP")
               , names_to = "Virus_Type",values_to = "Value") %>% 
  separate(col = Virus_Type, into = c("Virus_name", "Type"), sep = "\\_") %>% 
  mutate(Transmission = ifelse(grepl("LA", Type), "Local", "Imported"))

# Data for temperature overview graph---------------------------------------------------
weather_data <- full_data %>% 
  filter(str_detect(Type, "IR")) %>% 
  group_by(MonthYear) %>% 
  summarise(Rainfall = round(mean(Rainavg), 2),
            Min_Temprature = round(mean(meanMinTavg), 2),
            Max_Temprature = round(mean(meanMaxTavg), 2),
            Min_Humidity = round(mean(meanRHTMinavg), 2),
            Max_Humidity = round(mean(meanRHTMaxavg), 2),
            `Average Humidity` = round((Min_Humidity+Max_Humidity)/2, 2),
            `Average Temperature` = round((Min_Temprature+Max_Temprature)/2, 2),
            `Incidence Percent` = round(mean(Value, na.rm = TRUE), 2)*100)

# temperature map data 
temperature_map_data <- full_data %>% 
  group_by(SA3_NAME_2011, Virus_name) %>% 
  summarise(Max_temp = round(mean(meanMaxTavg), 2),
            Min_temp = round(mean(meanMinTavg), 2),
            `Average Temperature` = round((Max_temp+Min_temp)/2, 2))

# Humidity map data
humidity_map_data <- full_data %>% 
group_by(SA3_NAME_2011, Virus_name) %>% 
  summarise(Max_humd = round(mean(meanRHTMaxavg), 2),
            Min_humd = round(mean(meanRHTMinavg), 2),
            `Average Humidity` = round((Max_humd+Min_humd)/2, 2))

weather_graph <- full_data%>% 
  filter(str_detect(Type, "IR")) %>% 
  group_by(Year, Virus_name) %>% 
  summarise(Rainfall = round(mean(Rainavg), 2),
            `Average Temperature` = round((mean(meanMinTavg)+mean(meanMaxTavg))/2, 2),
            `Average Humidity` = round((mean(meanRHTMaxavg)+mean(meanRHTMinavg))/2, 2),
            `Incidence Percent` = round(mean(Value, na.rm = TRUE), 2)*100) %>% 
  pivot_longer(cols = -c('Year', 'Virus_name'), 
               names_to = "Type", values_to = "Value")

rain_map_data <- full_data %>% 
  group_by(SA3_NAME_2011, Virus_name) %>% 
  summarise(Rainfall = round(mean(Rainavg), 2))

# 3d weather lm
weather_data_3d <- full_data %>% 
  filter(str_detect(Type, "IR")) %>% 
  group_by(Year) %>% 
  summarise(Rainfall = round(mean(Rainavg), 2),
            Min_Temprature = round(mean(meanMinTavg), 2),
            Max_Temprature = round(mean(meanMaxTavg), 2),
            Min_Humidity = round(mean(meanRHTMinavg), 2),
            Max_Humidity = round(mean(meanRHTMaxavg), 2),
            `Average Humidity` = round((Min_Humidity+Max_Humidity)/2, 2),
            `Average Temperature` = round((Min_Temprature+Max_Temprature)/2, 2),
            `Incidence Percent` = round(mean(Value, na.rm = TRUE), 2)*100)

# Statistical Analysis - Negative Bimonial
nb_data <- long_data %>% 
  filter(str_detect(Type, "IR")) 


nb_data <- data.frame(Year = as.integer(nb_data$Year),
                      Rainfall = as.integer(nb_data$Rainavg),
                      Temperature = (as.integer(nb_data$meanMaxTavg + nb_data$meanMinTavg)/2),
                      Humidity = as.integer((nb_data$meanRHTMinavg + nb_data$meanRHTMaxavg)/2),
                      Donation = as.integer(nb_data$donationrate),
                      Value = as.integer(nb_data$Value))

# setting NA to 0
nb_data[is.na(nb_data)] <- 0 

