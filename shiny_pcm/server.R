library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(sp)
library(shinydashboard)

# setwd("C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM10_output")
# setwd("C:/RICARDO-AEA/pcm_1km_mapping/shiny_pcm")

#### Load rasters (only .tif files format)

# PM10
mappm102001 <- raster::raster("pm102001.tif")
mappm102002 <- raster::raster("pm102002.tif")
  # mappm102003s15a <- raster::raster("pm102003s15a.tif")
mappm102003 <- raster::raster("pm102003s15a.tif")
   #mappm102004g <- raster::raster("pm102004g.tif")
mappm102004 <- raster::raster("pm102004g.tif")
   #mappm1005ac <- raster::raster(("pm1005ac.tif"))
mappm102005 <- raster::raster(("pm1005ac.tif"))
#mappm102006gh <- raster::raster("pm102006gh.tif")
mappm102006 <- raster::raster("pm102006gh.tif")
# mappm102007g <- raster::raster("pm102007g.tif")
# mappm102008g <- raster::raster ("pm102008g.tif")
# mappm102009g <- raster::raster("pm102009g.tif")
# mappm102010g <- raster::raster ("pm102010g.tif")
# mappm102011g <- raster::raster("pm102011g.tif")
# mappm102012g <- raster::raster("pm102012g.tif")
# mappm102013g <- raster::raster("pm102013g.tif")
# mappm102014g <- raster::raster("pm102014g.tif")
mappm102007 <- raster::raster("pm102007g.tif")
mappm102008 <- raster::raster ("pm102008g.tif")
mappm102009 <- raster::raster("pm102009g.tif")
mappm102010 <- raster::raster ("pm102010g.tif")
mappm102011 <- raster::raster("pm102011g.tif")
mappm102012 <- raster::raster("pm102012g.tif")
mappm102013 <- raster::raster("pm102013g.tif")
mappm102014 <- raster::raster("pm102014g.tif")
df_PM10 <- as.data.frame(c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                           2010, 2011, 2012, 2013, 2014),
                          c("mappm102001","mappm102002","mappm102003",
                            "mappm102004","mappm102005", "mappm102006", 
                            "mappm102007","mappm102008","mappm102009",
                            "mappm102010", "mappm102011", "mappm102012",
                            "mappm102013", "mappm102014"))    
df_PM10$layers <- rownames(df_PM10)
rownames(df_PM10)=NULL
colnames(df_PM10) <- c("year_PM10", "layer_PM10")
PM10_raster <- as.character(filter(df_PM10, year_PM10 == 2002))[2]

MIN_PM10 <- min(minValue(mappm102001), minValue(mappm102002),  minValue(mappm102003), minValue(mappm102004), minValue(mappm102005),
           minValue(mappm102006), minValue(mappm102007), minValue(mappm102008), minValue(mappm102009), minValue(mappm102010),
           minValue(mappm102011), minValue(mappm102012), minValue(mappm102013), minValue(mappm102014))

MAX_PM10 <- max(maxValue(mappm102001), maxValue(mappm102002),  maxValue(mappm102003), maxValue(mappm102004), maxValue(mappm102005),
           maxValue(mappm102006), maxValue(mappm102007),maxValue(mappm102008),maxValue(mappm102009),maxValue(mappm102010),
           maxValue(mappm102011),maxValue(mappm102012),maxValue(mappm102013),maxValue(mappm102014))


# PM2.5
mappm252002 <- raster::raster("pm252002.tif")
mappm252003grav <- raster::raster("pm252003grav.tif")
mappm252004g <- raster::raster("pm252004g.tif")

# NO2
mapno22001_3 <- raster::raster("no22001_3.tif")
mapno22002_1 <- raster::raster("no22002_1.tif")
mapno22003 <- raster::raster("no22003.tif")

# NOx
mapnox2001_3 <- raster::raster("nox2001_3.tif")
mapnox2002_1 <- raster::raster("nox2002_1.tif")
mapnox2003 <- raster::raster("nox2003.tif")

###############################################################

shinyServer(function(input, output) {
  
  finalMap <- reactive({
    
    # Raster data (by year)
     PM10_raster <- as.character(filter(df_PM10, year_PM10 == input$year))[2]
  #  PM10_raster <- input$year
   # PM10_raster <- input$variable_raster_PM10
#       rast_pal_PM10 <- colorNumeric(c("#9999ff", "#ffffcc", "#ff0000"), 
#                               getValues(get(PM10_raster)),
#                       na.color = "transparent")
     
      ## create an unique legend for PM10
     rast_pal_PM10 <- colorNumeric(c("#9999ff", "#ffffcc", "#ff0000"), 
                               c(MIN_PM10,MAX_PM10),
                               na.color = "transparent")
      
    
    PM25_raster <- input$variable_raster_PM25
      rast_pal_PM25 <- colorNumeric(c("#9999ff", "#ffffcc", "#ff0000"), 
                                    getValues(get(PM25_raster)),
                                    na.color = "transparent")
      
    NO2_raster <- input$variable_raster_NO2
      rast_pal_NO2 <- colorNumeric(c("#9999ff", "#ffffcc", "#ff0000"), 
                                    getValues(get(NO2_raster)),
                                    na.color = "transparent")
      
    NOx_raster <- input$variable_raster_NOx
      rast_pal_NOx <- colorNumeric(c("#9999ff", "#ffffcc", "#ff0000"), 
                                   getValues(get(NOx_raster)),
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
    

  #  if (TYPE == "PM10" & PM10_raster == input$variable_raster_PM10) {
      if (TYPE == "PM10" &
          PM10_raster == as.character(filter(df_PM10, year_PM10 == input$year))[2]) {
        # PM10_raster == input$year) {
        
        # define popup for PM10 
      #  "h1 { font-size: 4px;}"
        content <- paste('<h3><strong>', input$year,'(PM<sub>10</sub>)', sep = "")
        
      map <- map %>% 
        addPopups(-1.4, 54.9, content,
                  options = popupOptions(closeButton = FALSE)) %>%
        addRasterImage(get(PM10_raster), 
                       colors = rast_pal_PM10, 
                       opacity = 0.6) %>%
        addLegend("bottomright", pal = rast_pal_PM10, values = c(MIN_PM10, MAX_PM10),
                  title = "<br><strong>PM<sub>10</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6)
      
    }
    
    
    if (TYPE == "PM2.5" & PM25_raster == input$variable_raster_PM25) {
      
      map <- map %>% 
        addRasterImage(get(PM25_raster), 
                       colors = rast_pal_PM25, 
                       opacity = 0.6) %>%
        addLegend("bottomright", pal = rast_pal_PM25, values = getValues(get(PM25_raster)),
                  title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6)
      
    }
    
    if (TYPE == "NO2" & NO2_raster == input$variable_raster_NO2) {
      
      map <- map %>% 
        addRasterImage(get(NO2_raster), 
                       colors = rast_pal_NO2, 
                       opacity = 0.6) %>%
        addLegend("bottomright", pal = rast_pal_NO2, values = getValues(get(NO2_raster)),
                  title = "<br><strong>NO<sub>2</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6)
      
    }
    
    
    if (TYPE == "NOx" & NOx_raster == input$variable_raster_NOx) {
      
      map <- map %>% 
        addRasterImage(get(NOx_raster), 
                       colors = rast_pal_NOx, 
                       opacity = 0.6) %>%
        addLegend("bottomright", pal = rast_pal_NOx, values = getValues(get(NOx_raster)),
                  title = "<br><strong>NO<sub>x</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6)
      
    }
    
    # Return
    map
    
  })
  
  
  #### add lleaflet map and animated image

  # Return to client
  output$myMap = renderLeaflet(finalMap())
  
  output$myImage <- renderImage({
    
    #  image_file <- paste("www/",input$image.type,".jpeg",sep="")
    # image_file <- "C:/RICARDO-AEA/pcm_1km_mapping/shiny_pcm/animated.gif"
    image_file_PM10 <- "animated_PM10_pcm.gif"
    
    # animated image for PM10
    return(list(
      src = image_file_PM10,
      filetype = "image/gif",
      
      height = 380, #520
      width = 456 #696
    ))
    
  }, deleteFile = FALSE)
  
})

