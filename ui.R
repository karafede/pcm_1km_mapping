library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(sp)

############################################################

shinyUI(fluidPage(
  
  # Title
 titlePanel("Modelled background pollution data (from UK-AIR)"),
fixedRow(
   column(10,
       sidebarLayout(
    sidebarPanel(
      selectInput("type", "Pollutant",
                  c("PM10" = "PM10", "PM2.5"= "PM2.5", "NO2" = "NO2", "NOx" = "NOx")),

#      conditionalPanel(
#        condition = "input.type == 'PM10'",
#         selectInput("variable_raster_PM10", "Year", c("2001" = "mappm102001",
#                                                          "2002" = "mappm102002",
#                                                       "2003" = "mappm102003s15a",
#                                                       "2004" = "mappm102004g",
#                                                       "2005" = "mappm1005ac",
#                                                       "2006" = "mappm102006gh",
#                                                       "2007"= "mappm102007g",
#                                                       "2008" = "mappm102008g",
#                                                       "2009" = "mappm102009g",
#                                                       "2010" = "mappm102010g",
#                                                       "2011" = "mappm102011g",
#                                                       "2012" = "mappm102012g",
#                                                       "2013" = "mappm102013g",
#                                                       "2014" = "mappm102014g"))),
     
     conditionalPanel(
       condition = "input.type == 'PM10'",

#         lapply(X = seq(2001, 2014,1), FUN = function(i) {
#                     conditionalPanel(# condition = "input.type == 'PM10'",
#                            condition = paste0("input.year == ", i),
#                            img(PM10_raster = paste("C:/RICARDO-AEA/pcm_1km_mapping/pm10_pcm_regular/",
#                                             "pm10",i, ".tif",sep = ""))
#                     )
#           }),

#          conditionalPanel(condition = "input.type == 'PM10'"),

       sliderInput("year",
                   "Year 2010", 
                   min = 2001, max = 2014, 
                   value = 2001, step =1,
                   pre = "", sep = "" ,
                   animate = FALSE)),
      # animate=animationOptions(interval = 5000, loop = FALSE, playButton = NULL, pauseButton = NULL)
#         sliderInput(input = "year", label = "Year 2010 prova", min = 2001, max = 2014,
#                     value = 1, pre = "", sep = "",
#             animate = animationOptions(interval = 1000, loop = FALSE)),


     conditionalPanel(
       condition = "input.type == 'PM2.5'",
       selectInput("variable_raster_PM25", "Year", c("2002" = "mappm252002",
                                                     "2003" = "mappm252003grav",
                                                     "2004" = "mappm252004g"
                                                                            ))),
     
     conditionalPanel(
       condition = "input.type == 'NO2'",
       selectInput("variable_raster_NO2", "Year", c("2001" = "mapno22001_3",
                                                    "2002" = "mapno22002_1",
                                                    "2003" = "mapno22003"))),
     
     conditionalPanel(
       condition = "input.type == 'NOx'",
       selectInput("variable_raster_NOx", "Year", c("2001" = "mapnox2001_3",
                                                    "2002" = "mapnox2002_1",
                                                    "2003" = "mapnox2003"))),

column(5,
       mainPanel(
         imageOutput("myImage")))
    ),

    # Show leaflet map with a text div reporting the selected date and extents 

fixedRow(
  column(7,
         mainPanel(
           leafletOutput('myMap', height = 600, width = 800)))
  
)
    
)

))

)
)

