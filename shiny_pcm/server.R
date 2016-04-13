library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(sp)

# setwd("C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM10_output")

#### Load rasters (only .tif or .rds files format)

# mappm102001_tif <- raster::raster("pm102001.tif")
# mappm102001 <- readRDS("pm102001.rds")
# mappm102002 <- readRDS("pm102002.rds")
# mappm102003s15a <- readRDS("pm102003s15a.rds")
# mappm102004g <- readRDS("pm102004g.rds")
# mappm1005ac <- readRDS(("pm1005ac.rds"))
# mappm102006gh <- readRDS("pm102006gh.rds")
# mappm102007g <- readRDS("pm102007g.rds")
# mappm102008g <- readRDS ("pm102008g.rds")
# mappm102009g <- readRDS("pm102009g.rds")
# mappm102010g <- readRDS ("pm102010g.rds")
# mappm102011g <- readRDS("pm102011g.rds")
# mappm102012g <- readRDS("pm102012g.rds")
# mappm102013g <- readRDS("pm102013g.rds")
# mappm102014g <- readRDS("pm102014g.rds")

mappm102001 <- raster::raster("pm102001.tif")
mappm102002 <- raster::raster("pm102002.tif")
mappm102003s15a <- raster::raster("pm102003s15a.tif")
mappm102004g <- raster::raster("pm102004g.tif")
mappm1005ac <- raster::raster(("pm1005ac.tif"))
mappm102006gh <- raster::raster("pm102006gh.tif")
mappm102007g <- raster::raster("pm102007g.tif")
mappm102008g <- raster::raster ("pm102008g.tif")
mappm102009g <- raster::raster("pm102009g.tif")
mappm102010g <- raster::raster ("pm102010g.tif")
mappm102011g <- raster::raster("pm102011g.tif")
mappm102012g <- raster::raster("pm102012g.tif")
mappm102013g <- raster::raster("pm102013g.tif")
mappm102014g <- raster::raster("pm102014g.tif")



###############################################################

shinyServer(function(input, output) {
  
  finalMap <- reactive({
    
    # Raster data (by year)
    PM10_raster <- input$variable_raster_PM10
    
      rast_pal <- colorNumeric(c("#9999ff", "#ffffcc", "#ff0000"), 
                              getValues(get(PM10_raster)),
                      na.color = "transparent")
      
    
#      rast_pal <- colorNumeric(
#                               palette = "Reds",
#                               getValues(get(PM10_raster)),
#                               na.color = "transparent")
     
     
     ## use values from 2007 raster layer for PM10
     ## c("#9999FF", "#9999FF", "#9999FF","#FFFF00", "#FF0000", "#b30000")
  
    # pollutant
    TYPE <- input$type 
    
    # Create base map
    map <- leaflet() %>% 
      addTiles() %>% 
      setView(-2, 53.5, 6)
    

    if (TYPE == "PM10" & PM10_raster == input$variable_raster_PM10) {
      
      map <- map %>% 
        addRasterImage(get(PM10_raster), 
                       colors = rast_pal, 
                       opacity = 0.6) %>%
        addLegend("bottomright", pal = rast_pal, values = getValues(get(PM10_raster)),
                  title = "<br><strong>PM<sub>10</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6)
      
    }
    
    # Return
    map
    
  })
  

  # Return to client
  output$myMap = renderLeaflet(finalMap())
  
})

