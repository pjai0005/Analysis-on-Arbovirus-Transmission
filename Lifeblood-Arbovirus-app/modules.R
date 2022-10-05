load_spinner <- function(ui_element){
  tagList(div(id = "plot-container",
              tags$img(src = "spinner.gif",
                       id = "loading-spinner"),
              ui_element
  ))
}

get_centroid <- function(x){
  N <- length(x)  # Number of polygons
  # Initialise data.frame
  Centroids <- data.frame(matrix(NA, N, 2, dimnames = list(NULL, c("long", "lat"))))
  for(i in 1:N){
    # Bounding box of polygon
    bb <- bbox(x@polygons[[i]])
    # Compute centroid
    Centroids[i,] <- c(
      0.5 * (bb[1,1] + bb[1,2]),
      0.5 * (bb[2,1] + bb[2,2]))
  }
  return(Centroids)
}

mapPickerIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/344/place-marker.png",
  iconWidth = 30, iconHeight = 30
)

virus_input <- function(id){
  radioButtons(id,
               "Select a Virus: ",
               c("Dengue" = "Dengue", 
                 "West Nile Kunjin" = "West Nile - Kunjin", 
                 "Zika" = "Zika", 
                 "Chikungunya" = "Chikungunya",
                 "Japanese Encephalitis Virus" = "Japanese encephalitis"
               ))
}

weather_cond_ui <- function(id){
  tagList(column(width = 3),
          column(width = 6, 
                 HTML(paste0("<h2>Overview of ", id, " in SA3 Regions</h2>")),
                 load_spinner(plotlyOutput(paste0(id, "_overview"))),
                 HTML(paste0("<h2>", id, " by SA3 Region</h2>")),
                 column(width = 3)),
          
          fluidRow(column(width = 5, 
                          load_spinner(uiOutput(paste0(id, "_map"), height = 500))),
                   
                   column(width = 2, selectInput(paste0(id, "_virus"),
                                                 "Select a Virus:",
                                                 c("Ross River" = "RRV",
                                                   "Dengue" = "DENV",
                                                   "Barmah Forest" = "BFV",
                                                   "Murray Valley Encephalitis" = "MVEV",
                                                   "West Nile/Kunjin" = "WNV",
                                                   "Zika" = "ZIKV", 
                                                   "Japanese Encephalitis" = "JEV",
                                                   "Chikungunya" = "CHIKV"))),
                   
                   column(width = 5, HTML("<br>")
                          , load_spinner(uiOutput(paste0(id, "_graph"), height = 500)))
                   
                   # fluidRow(column(width = 3),
                   #          column(width = 6, 
                   #                 load_spinner(uiOutput(paste0(id, "_map")))),
                   #          column(width = 3))
          )
  )
  
}