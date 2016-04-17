
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
# setwd("C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM10_input")
# setwd("C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM2.5_input")
# setwd("C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_NO2_input")
setwd("C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_NOx_input")

# dir <- "C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM10_output"
# dir <- "C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM25_output"
# dir <- "C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_NO2_output"
dir <- "C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_NOx_output"

# Variables for holding the coordinate system types (see:
# http://www.epsg.org/ for details)
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

# .....start the function here ######################

pcm_data_mgt <- function (file) {
  
  # Load data
  df <- readr::read_csv(file, progress = FALSE)
  name <- as.character(df[5,4])
  df <- df[6:nrow(df),]
  colnames(df) <- c("ukgridcode", "x", "y", name)
  df <- as.data.frame(df)
  df[4][df[4] == "MISSING"] <- ""
  df <- df[,-1]

  write_csv(df, paste(dir,"/", name,".csv", sep = ""))
  df <- read_csv(paste(dir, "/", name,".csv", sep =""))
  df <- as.matrix(df)
  df <- as.data.frame(df)
  
  coordinates(df)=~x+y
  proj4string(df)=CRS("+init=epsg:27700") 
  
  gridded(df) = TRUE
  
  r = raster(df)
  projection(r) = CRS("+init=epsg:27700")
  writeRaster(r, paste(dir,"/",name, ".tif",sep =""), overwrite = TRUE)
  
  # reload geoTiff file in UK British coordinates
  pcm_tif <- raster::raster(paste(dir,"/",name, ".tif",sep =""))
  
  # convert "pcm_pm102001_tif" to longlat (WGS84) EPSG:4326
  # pcm_tif = projectRaster(pcm_tif, crs = ('+proj=longlat'))
  pcm_tif_longlat = projectRaster(pcm_tif, crs=("+init=epsg:4326"))

  writeRaster(pcm_tif_longlat,paste(dir,"/",name, ".tif",sep =""), overwrite = TRUE)
  # reload geoTiff file in WGS84 coordinates
  # pcm_tif_longlat <- raster::raster(paste(dir,"/",name, ".tif",sep =""))
  # saveRDS(pcm_tif_longlat, paste(dir,"/", name,".rds",sep = ""))
}

###############################################################  
## RUN the function here.... ##################################

## list all .csv pcm files in specific folder

# file_list <- list.files(path = "C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM10_input/", pattern = "\\.csv$",
#                         full.names = TRUE)
# file_list <- list.files(path = "C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_PM2.5_input/", pattern = "\\.csv$",
#                         full.names = TRUE)
# file_list <- list.files(path = "C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_NO2_input/", pattern = "\\.csv$",
#                         full.names = TRUE)
file_list <- list.files(path = "C:/RICARDO-AEA/pcm_1km_mapping/data_pcm_NOx_input/", pattern = "\\.csv$",
                        full.names = TRUE)

# Apply function to all files
data_pcm <- lapply(file_list, pcm_data_mgt)




  

# Original scipt...this is not a function ##################################
##############################################################################

# Load data
pcm_PM10_2001 <- read_csv("mappm102001.csv")
UK_Grid_Code <- read_csv("uk.gridcode.csv")  ### codes for matching Lon, Lat, with Easting and Northing
name <- as.character(pcm_PM10_2001[5,4])
pcm_PM10_2001 <- pcm_PM10_2001[6:nrow(pcm_PM10_2001),]
colnames(pcm_PM10_2001) <- c("ukgridcode", "x", "y", "pm102001")

# pcm_PM10_2001$pm102001 <- ifelse(grepl("MISSING", pcm_PM10_2001$pm102001, ignore.case = TRUE), 
#                                  "" ,pcm_PM10_2001$pm102001)

pcm_PM10_2001[4][pcm_PM10_2001[4] == "MISSING"] <- ""

colnames(UK_Grid_Code)[3] <- "ukgridcode"
colnames(UK_Grid_Code)[6] <- "Lon"
colnames(UK_Grid_Code)[7] <- "Lat"

UK_Grid_Code <- as.data.frame(UK_Grid_Code)

#  pcm_PM10_2001 <- pcm_PM10_2001 %>%
#    filter(pm102001 != "MISSING")
pcm_PM10_2001 <- pcm_PM10_2001[,-1]

write_csv(pcm_PM10_2001, "pcm_PM10_2001.csv")
pcm_PM10_2001 <- read_csv("pcm_PM10_2001.csv")
pcm_PM10_2001 <- as.matrix(pcm_PM10_2001)
pcm_PM10_2001 <- as.data.frame(pcm_PM10_2001)
str(pcm_PM10_2001)

coordinates(pcm_PM10_2001)=~x+y
proj4string(pcm_PM10_2001)=CRS("+init=epsg:27700") 

gridded(pcm_PM10_2001) = TRUE

r = raster(pcm_PM10_2001)
projection(r) = CRS("+init=epsg:27700")

plot(r)
writeRaster(r,"pm102001.tif", overwrite = TRUE)

# reload geoTiff file in UK British coordinates
pcm_pm102001_tif <- raster::raster("pm102001.tif")
plot(pcm_pm102001_tif)
mapview(pcm_pm102001_tif)

# convert "pcm_pm102001_tif" to longlat (WGS84) EPSG:4326
# pcm_pm102001_tif_longlat = projectRaster(pcm_pm102001_tif, crs = ('+proj=longlat'))
pcm_pm102001_tif_longlat = projectRaster(pcm_pm102001_tif, crs=("+init=epsg:4326"))
plot(pcm_pm102001_tif_longlat)
mapview(pcm_pm102001_tif_longlat)
writeRaster(pcm_pm102001_tif_longlat,"pm102001.tif", overwrite = TRUE)

# reload geoTiff file in WGS84 coordinates (Lat , Lon)
pcm_pm102001_tif <- raster::raster("pm102001.tif")
plot(pcm_pm102001_tif)

################################################################################


