
# Load packages
library(threadr)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(sp)
library(raster) 
library(gstat)
library(ncdf4)
library(mapview)
library(leaflet)


# Clear all objects
rm(list = ls(all = TRUE))

options(stringsAsFactors = FALSE)

# Set working directory
setwd("C:/RICARDO-AEA/pcm_1km_mapping")
dir_UK <- "C:/RICARDO-AEA/pcm_1km_mapping/UK_shp"
dir <- "C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM10"

# Variables for holding the coordinate system types (see:
# http://www.epsg.org/ for details)
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"
shp_UK <- readOGR(dsn = dir_UK, layer = "GBR_adm0")
# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UK <- spTransform(shp_UK, CRS("+init=epsg:4326"))
shp_UK@data$name <- 1:nrow(shp_UK)

##############################################################################

# Load data
pcm_PM10_2001 <- read_csv("mappm102001.csv")
UK_Grid_Code <- read_csv("uk.gridcode.csv")  ### codes for matching Lon, Lat, with Easting and Northing
name <- as.character(pcm_PM10_2001[5,4])
pcm_PM10_2001 <- pcm_PM10_2001[6:nrow(pcm_PM10_2001),]
colnames(pcm_PM10_2001) <- c("ukgridcode", "x", "y", "pm102001")

 pcm_PM10_2001$pm102001 <- ifelse(grepl("MISSING", pcm_PM10_2001$pm102001, ignore.case = TRUE), 
                                   "" ,pcm_PM10_2001$pm102001)

colnames(UK_Grid_Code)[3] <- "ukgridcode"
colnames(UK_Grid_Code)[6] <- "Lon"
colnames(UK_Grid_Code)[7] <- "Lat"
UK_Grid_Code <- as.data.frame(UK_Grid_Code)

# pcm_PM10_2001 <- pcm_PM10_2001 %>%
#                filter(pm102001 != "MISSING")
# pcm_PM10_2001 <- pcm_PM10_2001[,-1]
write_csv(pcm_PM10_2001, "pcm_PM10_2001.csv")
pcm_PM10_2001 <- read_csv("pcm_PM10_2001.csv")
str(pcm_PM10_2001)

##########################################################################

# Join data with British coordinates with the one Lat, Lon file by ukgridcode as common indicator

# pcm_PM10_2001 <- pcm_PM10_2001 %>% 
#   left_join(UK_Grid_Code, c("ukgridcode"))
# 
# pcm_PM10_2001 <- cbind(pcm_PM10_2001$Lon, pcm_PM10_2001$Lat, pcm_PM10_2001$pm102001)
# colnames(pcm_PM10_2001) <- c("Lon", "Lat", "pm102001")
# pcm_PM10_2001 <- as.data.frame(pcm_PM10_2001)
# write_csv(pcm_PM10_2001, "pcm_PM10_2001.csv")

########################################################################
#### create a GeoJson for pcm data #####################################

# Create coordinates variable
coords <- cbind(Easting = as.numeric(as.character(pcm_PM10_2001$x)),
                Northing = as.numeric(as.character(pcm_PM10_2001$y)))

# # Create a unique ID for each GP
# pcm_PM10_2001$ID <- 1:nrow(pcm_PM10_2001)
# 
# 
# # Create the SpatialPointsDataFrame 
# pcm_PM10_2001_SP <- SpatialPointsDataFrame(coords,
#                                            data = data.frame(pcm_PM10_2001$pm102001,
#                     pcm_PM10_2001$ID), proj4string = CRS("+init=epsg:27700"))
# head(pcm_PM10_2001_SP@coords)


# Create the SpatialPointsDataFrame
pcm_PM10_2001_SP <- SpatialPointsDataFrame(coords,
                                           data = data.frame(pcm_PM10_2001$pm102001),
                                           proj4string = CRS("+init=epsg:27700"))
head(pcm_PM10_2001_SP@coords)

# plot(pcm_PM10_2001_SP)

# Convert from Eastings and Northings to Latitude and Longitude
pcm_PM10_2001_SP_LL <- spTransform(pcm_PM10_2001_SP, CRS(latlong))
# we also need to rename the columns
colnames(pcm_PM10_2001_SP_LL@coords)[colnames(pcm_PM10_2001_SP_LL@coords) == "Easting"] <- "Longitude"
colnames(pcm_PM10_2001_SP_LL@coords)[colnames(pcm_PM10_2001_SP_LL@coords) == "Northing"] <- "Latitude"
colnames(pcm_PM10_2001_SP_LL@data) <- name
# plot(pcm_PM10_2001_SP_LL)
head(pcm_PM10_2001_SP_LL@data)
head(pcm_PM10_2001_SP_LL@coords)
head(pcm_PM10_2001_SP_LL)


# Generate a shp file
writeOGR(pcm_PM10_2001_SP_LL, dsn=dir,
         layer=name, driver="ESRI Shapefile")

#### Write GeoJSON for Leaflet application ############################
# ----- Write data to GeoJSON

leafdat<-paste(dir, "/",name,".PM10_geojson", sep="") 

####  ATT !!!!! erase existing .geojson file when re-runing code ######
writeOGR(pcm_PM10_2001_SP_LL, leafdat, layer="", driver="GeoJSON")  ## erase existing .geojson file when re-runing code 
file <- readOGR(paste(dir,"/",name,".PM10_geojson",sep=""), "OGRGeoJSON")
# save file into rds format
saveRDS(file, paste(dir,"/", name,".rds",sep = ""))
# file <- readOGR("C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM10/pm102001.PM10_geojson", "OGRGeoJSON")
file <- readRDS(paste(dir,"/",name,".rds", sep = ""))


# r <- raster(extent(file))
# res(r)=0.01
# r <- rasterize(file, field="pm102001", r)
# 
# plot(r)
# plot(shpfile,lwd=10,add=TRUE)





 
 # pcm_PM10_2001$x <- pcm_PM10_2001$Lon # define x & y as longitude and latitude
 # pcm_PM10_2001$y <- pcm_PM10_2001$Lat
 # 
 # coordinates(pcm_PM10_2001) = ~x + y  ## Set spatial coordinates to create a Spatial object:
 # 
 # x.range <- as.numeric(c(-8.633, 1.762003))  # min/max longitude of the interpolation area
 # y.range <- as.numeric(c(49.88647, 60.84472))  # min/max latitude of the interpolation area
 # 
 # ### expand points to grid 1km resolution
 # grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
 #                    y = seq(from = y.range[1], to = y.range[2], by = 0.01))  
 # 
 # coordinates(grd) <- ~x + y
 # gridded(grd) <- TRUE
 # 
 # # tolerance = 0.79000
 # coordinates(pcm_PM10_2001) = ~Lon + Lat
 # pixels <- SpatialPixelsDataFrame(pcm_PM10_2001, tolerance = 0.999, pcm_PM10_2001@data)
 # 
 # raster <- raster(pixels[,'pm102001'])
 # plot(raster)
 # mapview(raster)
 # 
 # 
 # 
 # # ### apply idw model for the data (interpolation)
 # idw <- idw(formula = pm102001 ~ 1, locations = pcm_PM10_2001, 
 #            newdata = grd)  
 # 
 # idw.output = as.data.frame(idw)  # output is defined as a data table
 # names(idw.output)[1:3] <- c("Lon", "Lat", "pcm_PM10_2001")  # give names to the modelled variables
 # 
 # write.csv(idw.output, file = "pcm_PM10_2001_1km_interp.csv", row.names=FALSE)
 # 
 # ###################################################################################
 # 
 # # Load Interpolated pcm data
 # 
 # pcm_PM10_2001 <- read_csv("pcm_PM10_2001_1km_interp.csv")
 # pcm_PM10_2001 <- as.data.frame(pcm_PM10_2001)
 # str(pcm_PM10_2001)
 # 
 # #### create a raster for PM25 UK-AIR 1Km in England #######
 # 
 # coordinates(pcm_PM10_2001) <- ~ Lon + Lat
 # # coerce to SpatialPixelsDataFrame
 # gridded(pcm_PM10_2001) <- TRUE
 # raster_pcm_PM10_2001 <- raster(pcm_PM10_2001)
 # projection(raster_pcm_PM10_2001) <- CRS("+proj=longlat +datum=WGS84")
 # plot(raster_pcm_PM10_2001)
 # plot(shp)
 # 
 # #### crop the raster over the UK shp file ###########################
 # raster_pcm_PM10_2001_cropped <- crop(raster_pcm_PM10_2001, extent(shp_UK))
 # raster_pcm_PM10_2001_cropped <- mask(raster_pcm_PM10_2001_cropped, shp_UK)
 # 
 # plot(raster_pcm_PM10_2001_cropped)
 # # plot(shp_UK, add=TRUE, lwd=2)
 # 
 # pcm_PM10_2001_nc <- writeRaster(raster_pcm_PM10_2001_cropped,
 #                               filename="pcm_PM10_2001.nc",
 #                               format="CDF", overwrite=TRUE) 
 # pcm_PM10_2001_nc <- raster("pcm_PM10_2001.nc")
 # 
 # 
 # ## Output: a GeoTIFF file
 # file.tiff <- 'pcm_PM10_2001.tif'
 # 
 # ## Save to disk as GeoTIFF
 # pcm_PM10_2001_tiff <- writeRaster(pcm_PM10_2001_nc, filename = file.tiff, format = 'GTiff', overwrite = T)
 # plot(pcm_PM10_2001_tiff)
 # 
 # # Load raster
 # pcm_PM10_2001_tiff <- raster("pcm_PM10_2001.tif")
 # plot(pcm_PM10_2001_tiff)
 # # save file into rds format
 # saveRDS(pcm_PM10_2001_tiff, "pcm_PM10_2001.rds")
 # 
 
 ##############################################################
 ##############################################################
 ##############################################################
 
 
 
 
 
 #############################################################################
 ########################################################################
 ########################################################################
 
 ### create a function
 
 pcm_data_mgt <- function (file) {
   
   dir <- "C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM10"
   # Load data
   df <- readr::read_csv(file, progress = FALSE)
   name <- as.character(df[5,4])
   df <- df[6:nrow(df),]
   colnames(df) <- c("ukgridcode", "x", "y", name)
   
   df <- as.data.frame(df)
   
   df <- df %>%
     filter(df[4]!= "MISSING")
   df <- df[,-1]
   
   #### create a GeoJson for pcm data
   
   # Create coordinates variable
   coords <- cbind(Easting = as.numeric(as.character(df$x)),
                   Northing = as.numeric(as.character(df$y)))
   
   # Create the SpatialPointsDataFrame
   df_SP <- SpatialPointsDataFrame(coords, data = data.frame(df[3]),
                                   proj4string = CRS("+init=epsg:27700"))
   
   # Convert from Eastings and Northings to Latitude and Longitude
   df_SP_LL <- spTransform(df_SP, CRS(latlong))
   # we also need to rename the columns
   colnames(df_SP_LL@coords)[colnames(df_SP_LL@coords) == "Easting"] <- "Longitude"
   colnames(df_SP_LL@coords)[colnames(df_SP_LL@coords) == "Northing"] <- "Latitude"
   colnames(df_SP_LL@data) <- name
   
   # Generate a shp file
   writeOGR(df_SP_LL, dsn=dir,
            layer=name, driver="ESRI Shapefile")
   
   
   #### Write GeoJSON for Leaflet application ############################
   # ----- Write data to GeoJSON
   
   leafdat<-paste(dir, "/",name,".PM10_geojson", sep="") 
   
   ####  ATT !!!!! erase existing .geojson file when re-runing code ######
   writeOGR(df_SP_LL, leafdat, layer="", driver="GeoJSON")  ## erase existing .geojson file when re-runing code 
   file <- readOGR(paste(dir,"/",name,".PM10_geojson",sep=""), "OGRGeoJSON")
   # save file into rds format
   saveRDS(file, paste(dir,"/", name,".rds",sep = ""))
   # file <- readOGR("C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM10/pm102001.PM10_geojson", "OGRGeoJSON")
   file <- readRDS(paste(dir,"/",name,".rds", sep = ""))
 }
 
 ########################################################################################  
 
 
 ## list all pcm files in specific folder
 
 file_list <- list.files(path = "C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM10/", pattern = "\\.csv$",
                         full.names = TRUE)
 
 # Apply function to all files
 data_pcm <- lapply(file_list, pcm_data_mgt)
 
 
 
 
 
 
 
 
 
 
 
 
 
 






######################################################################################
# Leafleft 
 
name <- "pm102001"
file <- readRDS(paste(dir,"/",name,".rds", sep = ""))

pal_pm102001 <- colorBin(
  palette = "Reds",
  domain = file$pm102001)

map_pcm <- leaflet(file)

map = map_pcm %>% 
  setView(lng = 1.25, lat = 52.40, zoom = 7) %>%
  
  # Add tiles
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  

  addCircles(lng=file$coords.x1, lat=file$coords.x2,  
            # popup=as.vector(popup_info),
             weight = 3, radius= ~pm102001, 
             color="#ffffff", stroke = FALSE, fillOpacity = 0, 
             group='Time & Position') 










