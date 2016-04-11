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
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("type", "Pollutant",
                  c("PM10" = "PM10", "PM2.5"= "PM2.5")),

     conditionalPanel(
       condition = "input.type == 'PM10'",
        selectInput("variable_raster_PM10", "Year", c("2001" = "mappm102001",
                                                         "2002" = "mappm102002",
                                                      "2003" = "mappm102003s15a",
                                                      "2004" = "mappm102004g",
                                                      "2005" = "mappm1005ac",
                                                      "2006" = "mappm102006gh",
                                                      "2007"= "mappm102007g",
                                                      "2008" = "mappm102008g",
                                                      "2009" = "mappm102009g",
                                                      "2010" = "mappm102010g",
                                                      "2011" = "mappm102011g",
                                                      "2012" = "mappm102012g",
                                                      "2013" = "mappm102013g",
                                                      "2014" = "mappm102014g")))
        ),
   
    # Show leaflet map with a text div reporting the selected date and extents 
    mainPanel(
      leafletOutput('myMap', height = 600, width = 800)
    )
    
  )
))
