library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(sp)
library(RCurl)
library(devtools)
# devtools::install_github("rstudio/shinydashboard")
library(shinydashboard)


############################################################

 shinyUI(dashboardPage(
   dashboardHeader(title = "Modelled background pollution data (from UK-AIR)"),
  
    dashboardSidebar(
     sidebarMenu(
       menuItem("Dashboard", tabName = "Pollutants", icon = icon("dashboard")),
       menuItem("Widgets", tabName = "widgets", icon = icon("th"))
     )
   ),
    
dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "Pollutants",
           fluidRow(
 #            column(width = 6,
                    box( height = 625, width = 5,
                      selectInput("type", "Pollutant",
                  c("PM10" = "PM10", "PM2.5"= "PM2.5", "NO2" = "NO2", "NOx" = "NOx")),
                  
                  conditionalPanel(
                    condition = "input.type == 'PM10'", imageOutput("myImage"),
                    
                    sliderInput("year",
                   "PM10", 
                   min = 2001, max = 2014, 
                   value = 2001, step =1,
                   pre = "", sep = "" ,
                   animate = FALSE)),
                  
                  conditionalPanel(
                    condition = "input.type == 'PM2.5'",
                    selectInput("variable_raster_PM25", "Year", c("2002" = "mappm252002",
                                                                  "2003" = "mappm252003grav",
                                                                  "2004" = "mappm252004g"))),
                  
                  conditionalPanel(
                    condition = "input.type == 'NO2'",
                    selectInput("variable_raster_NO2", "Year", c("2001" = "mapno22001_3",
                                                                 "2002" = "mapno22002_1",
                                                                 "2003" = "mapno22003"))),
                  
                  conditionalPanel(
                    condition = "input.type == 'NOx'",
                    selectInput("variable_raster_NOx", "Year", c("2001" = "mapnox2001_3",
                                                                 "2002" = "mapnox2002_1",
                                                                 "2003" = "mapnox2003")))
     #             )
           ),


    # Show leaflet map with a text div reporting the selected date and extents 
    
#    column(width = 6,
         box( height = 625, width = 7,
             leafletOutput('myMap', height = 600 , width = 650)
             )
#         )
    )
    ),
    
           

# Second tab content
tabItem(tabName = "widgets",
        h2("Widgets tab content")
        )
  ))
 ))

