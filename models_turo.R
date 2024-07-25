
# SCRIPT MODELITZACIÓ TURÓ

# OBRIM PAQUETS ----

options(warn=1)
list.of.packages <- c("terra", "readxl", "writexl", "mapview", "dplyr", "usdm", "getPass", "httr", "jsonlite","stars","tictoc",
                      "geojsonio", "geojsonR", "rgdal", "sp", "ggplot2", "RColorBrewer", "raster", "geodata", "ranger")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
install.packages(new.packages)
invisible(lapply(list.of.packages, library, character.only = T, logical.return=T))
rm(list.of.packages, new.packages)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Li donem el directori de l'script. 
getwd()

# ÀMBIT D'ESTUDI

CAT<-vect("C:/Users/david.munoz/OneDrive - ctfc.cat/Limits/COMARQUES/LIMADM_COMARCA.shp")
CAT<-project(CAT, "EPSG:25831")

ambit<-ext(c(370000, 528000, 4620000, 4710000)) 
ambit<-vect(ambit)
crs(ambit)<-"EPSG:25831"

ambit<-crop(ambit, CAT)
ambit_r<-rast(ambit, res=100)

# OBSERVACIONS ----

obs_CAT <- read_xlsx("../../OBSERVACIONS/Polecat presence SE Pyrenees.xlsx", sheet="Punts turo Catalunya")
obs_NE <- read_xlsx("../../OBSERVACIONS/Polecat presence SE Pyrenees.xlsx", sheet="Punts_NE_CAT")
obs_emporda<-read_xlsx("../../OBSERVACIONS/Polecat presence SE Pyrenees.xlsx", sheet="Punts Plana Empordà")

obs_CAT<-vect(obs_CAT, geom=c("UTM X", "UTM Y"), crs="EPSG:25831")
obs_NE<-vect(obs_NE, geom=c("UTM X", "UTM Y"), crs="EPSG:25831")
obs_emporda<-vect(obs_emporda, geom=c("UTM X", "UTM Y"), crs="EPSG:25831")

mapview(obs_CAT, zcol="Polecat presence")+mapview(obs_emporda, zcol="Polecat presence")+mapview(obs_NE, zcol="Polecat presence")

# VARIABLES ----

# Rius
rius<-vect("C:/Users/david.munoz/OneDrive - ctfc.cat/CARTOBIO_TURO/VARIABLES/rius/rius_detallat/RiusMM_CAT13_ETRS89.shp")
rius<-crop(rius, ambit)
mapview(rius)

d_rius<-distance(ambit_r, rius, unit="m")
mapview(d_rius)
d_rius<-mask(d_rius, ambit)

writeRaster(d_rius, "../../variables/definitives/dist_rius.tif")

# VAR BIOFISIQUES

biof<-list.files("../../Variables/biofisiques_arbrat", pattern=".tif", full.names = T)
biof<-rast(biof)  
names(biof)<-c("area_basal", "dbhm", "densitat_peus")

biof<-crop(biof, ambit) #Així anirà més ràpid
biof<-resample(biof, ambit_r, "average") #Adaptem els píxels de 20x20 fent la mitjana a 100x100m.

biof <- subst(biof, NA, 0) #Abans de fer el mask, omplim els NA amb 0

biof<-mask(biof, ambit) #I així els NA seran els píxels que queden fora de l'àmbit d'estudi. 
writeRaster(biof, paste0("../../variables/definitives/",names(biof), ".tif"), overwrite=TRUE)

# HÀBITATS

habitats<-rast("../../Variables/habitats/chcv3_CORINE_20x20.tif") #Obrim la capa a 20m. 
levels_r <- levels(habitats)[[1]] #Com que els valors no acaben de coincidir, farem un pas previ per a fer la reclassificació

t_habitats<-read_xlsx("../../Variables/habitats/taula_correspondencia_CORINE_predictor_David.xlsx") #Reclassificació preparada prèviament
t_habitats<-t_habitats[!is.na(t_habitats$class_TURO),]
t_habitats<-t_habitats[,c(5,12)] # Ens quedem només amb les columnes de CODI CORINE i codi que ens interessa actualment

matriu_reclasss<-full_join(t_habitats, levels_r, by="COD_CORINE") #ajuntem amb els nivells que tenim al ràster
matriu_reclasss$Codi_turo<-ifelse(is.na(matriu_reclasss$Codi_turo), 0, matriu_reclasss$Codi_turo) #eliminem NA dels hàbitats q no incloem a la classificació
matriu_reclasss<-matriu_reclasss[,c(3,2)] #I retallem i ordenem. Hi havia pas de as.matrix, me l'he carregat

prova<-classify(habitats, matriu_reclasss) #Reclassifiquem

raster_habitats<-list()

for (i in 1:12) { #Separem en diferents capes i sense NA, només 0. 
  raster_habitats[[i]] <- ifel(prova == i, 1, 0)
  print(i)
}

raster_habitats<-rast(raster_habitats)
raster_100m<-list()

for (i in 1:nlyr(raster_habitats)) { #Ho passem a 100m amb la cobertura i adaptem (similar al mask) a l'àmbit d'estudi
  tic()
  raster_100m[[i]] <- aggregate(raster_habitats[[i]], fact=5, fun="mean", na.rm=TRUE) # el raster de 20 ha de tenir els NAs=0s, ho passem a 100m. 
  raster_100m[[i]] <- resample(raster_100m[[i]], ambit_r)
  toc()
}

raster_100m<-rast(raster_100m) #Hi posem noms
names<-c("caducifolis", "esclerofiles", "agricola", "plantacions_forestals", "prats", "coniferes_med", 
                      "matollars_med", "matollars_mon", "bosc_mixt_mon", "coniferes_mon", "veg_ribera", "urba")

for (i in 1:nlyr(raster_100m)) { #I guardem. 
  tic()
  writeRaster(raster_100m[[i]], paste0("../../VARIABLES/definitives/", names[i],".tif"))
  toc()
}

