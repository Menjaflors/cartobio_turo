
# SCRIPT MODELITZACIÓ TURÓ

# OBRIM PAQUETS ----

options(warn=1)
list.of.packages <- c("terra", "readxl", "writexl", "mapview", "dplyr", "usdm", "getPass", "httr", "jsonlite",
                      "geojsonio", "geojsonR", "rgdal", "sp", "ggplot2", "RColorBrewer", "raster", "geodata", "ranger")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
install.packages(new.packages)
invisible(lapply(list.of.packages, library, character.only = T, logical.return=T))
rm(list.of.packages, new.packages)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Li donem el directori de l'script. 
getwd()

# OBSERVACIONS ----

obs_CAT <- read_xlsx("../../OBSERVACIONS/Polecat presence SE Pyrenees.xlsx", sheet="Punts turo Catalunya")
obs_NE <- read_xlsx("../../OBSERVACIONS/Polecat presence SE Pyrenees.xlsx", sheet="Punts_NE_CAT")
obs_emporda<-read_xlsx("../../OBSERVACIONS/Polecat presence SE Pyrenees.xlsx", sheet="Punts Plana Empordà")

obs_CAT<-vect(obs_CAT, geom=c("UTM X", "UTM Y"), crs="EPSG:25831")
obs_NE<-vect(obs_NE, geom=c("UTM X", "UTM Y"), crs="EPSG:25831")
obs_emporda<-vect(obs_emporda, geom=c("UTM X", "UTM Y"), crs="EPSG:25831")

mapview(obs_CAT, zcol="Polecat presence")+mapview(obs_emporda, zcol="Polecat presence")+mapview(obs_NE, zcol="Polecat presence")


# VARIABLES ----