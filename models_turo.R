
# SCRIPT MODELITZACIÓ TURÓ

# OBRIM PAQUETS ----
options(java.parameters = "-Xmx10g")
options(warn=1)
list.of.packages <- c("terra", "readxl", "writexl", "mapview", "dplyr", "usdm", "getPass", "httr", "jsonlite","stars","tictoc",
                      "geojsonio", "geojsonR", "rgdal", "sp", "ggplot2", "RColorBrewer", "raster", "geodata", "ranger", "ecospat")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
install.packages(new.packages)
invisible(lapply(list.of.packages, library, character.only = T, logical.return=T))
rm(list.of.packages, new.packages)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Li donem el directori de l'script. 
getwd()

fun_categories_SDM <- function(r) {
  valor<-list()
  valorq10<-list()
  min_points<-list()
  r1<-r
  
  valor <- terra::extract(r, occs.xy) # extract the values of all the points
  valorq10 <- valor[valor < quantile(valor[,2], 0.1, na.rm=T)] # values lower of threhold Quantile 0.1 [,2] is the column of the values
  min_points <- mean(valorq10, na.rm=T) # mean of the values lower of Q10
  
  min_points<-mean(unlist(min_points))
  
  r2 <- r
  r2[r2[] < min_points] <- 0 # set values lower of the mean of Q10 to 0
  m2 <- mean(r2[r2!=0]) # m2 is the mean of the non-zero new values (adequat-bo)
  
  r3 <- r
  r3[r3[] < m2] <- 0  # set values lower of the mean2 to 0
  m3 <- mean(r3[r3!=0]) # m3 is the mean of the non-zero new values (bo-optim)
  
  r[r[] <= min_points]  <- 0 # here we transform the original raster
  r[r[] >= m3] <- 3 # here we transform the original raster
  r[r[] > min_points & r[] <= m2]  <- 1 # here we transform the original raster
  r[r[] > m2 & r[] < m3]  <- 2 # here we transform the original raster
  return(r)
}

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
obs_NE <- read_xlsx("../../OBSERVACIONS/Polecat NE Catalonia.xlsx", sheet="Punts Munt NE Catalunya")
obs_NE <- obs_NE[,c(3:7)]
obs_CAT <- obs_CAT[,c(3:7)]

obs_emporda<-read_xlsx("../../OBSERVACIONS/Polecat presence SE Pyrenees.xlsx", sheet="Punts Plana Empordà")

obs_CAT<-vect(obs_CAT, geom=c("UTM X", "UTM Y"), crs="EPSG:25831")
obs_NE<-vect(obs_NE, geom=c("UTM X", "UTM Y"), crs="EPSG:25831")
obs_emporda<-vect(obs_emporda, geom=c("UTM X", "UTM Y"), crs="EPSG:25831")

#mapview(obs_CAT, zcol="Polecat presence")+mapview(obs_emporda, zcol="Polecat presence")+mapview(obs_NE, zcol="Polecat presence")

names(obs_CAT) <- trimws(names(obs_CAT))
names(obs_NE) <- trimws(names(obs_NE))
names(obs_emporda) <- trimws(names(obs_emporda))

obs_tot <- rbind(obs_CAT, obs_NE, obs_emporda)
#writeVector(obs_tot, "../../observacions/observacions_tot.gpkg")

# VARIABLES ----

# Rius

## Detallat
rius<-vect("../../VARIABLES/rius/rius_detallat/RiusMM_CAT13_ETRS89.shp")
rius<-crop(rius, ambit)
mapview(rius)

d_rius<-distance(ambit_r, rius, unit="m")
mapview(d_rius)
d_rius<-mask(d_rius, ambit)

writeRaster(d_rius, "../../variables/definitives/dist_rius.tif")

## Grans rius
rius<-vect("../../VARIABLES/rius/rius_cat_poc_detallat/AIGUA_PDG_SUPERF_CIC_22_27.shp")
rius<-crop(rius, ambit)
mapview(rius)

d_rius<-distance(ambit_r, rius, unit="m")
mapview(d_rius)
d_rius<-mask(d_rius, ambit)

writeRaster(d_rius, "../../variables/definitives/dist_rius_principals.tif")

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

# DHI 

dhi<-list.files("../../variables/DHI/", full.names = T, pattern = ".tif")
dhi<-rast(dhi)

dhi<-project(dhi, crs(ambit_r))
names<-c("acc_NDVI", "min_NDVI", "var_NDVI")
dhi<-terra::resample(dhi, ambit_r, "bilinear")

for (i in 1:nlyr(dhi)) { #I guardem. 
  writeRaster(dhi[[i]], paste0("../../VARIABLES/definitives/", names[i],".tif"))

}

#MDE

mde<-rast("C:/Users/david.munoz/OneDrive - ctfc.cat/CARTOBIO_OSBRU/2022/PREDICTORS/PREDICTORS_OS_ORIGINAL/MDE/dem_WGS84_31N_Clip.tif")
mde<-project(mde, crs(ambit_r))
mde<-terra::resample(mde, ambit_r, "bilinear")

writeRaster(mde, "../../variables/definitives/MDE.tif")

# PREC

prec<-rast("C:/Users/david.munoz/OneDrive - ctfc.cat/REPORTING_2019_2024/VARIABLES/CLIMATIQUES/METEOCAT/CLIMA_ATLES9120_PPTANUAL.tif")
prec<-resample(prec, ambit_r)
writeRaster(prec, "../../VARIABLES/DEFINITIVES/prec.tif")

# MIDA TAQUES DE BOSC

habitats<-list.files("../../VARIABLES/DEFINITIVES", full.names = T)
habitats<-habitats[c(4:7,11,16,21)]
habitats<-rast(habitats)
habitats<- app(habitats, fun = sum)
habitats<- ifel(habitats < 1, 0, habitats)

patches<-patches(habitats, 4, zeroAsNA=T)

z <- cellSize(patches) |> zonal(patches, sum) 
z$area<-z$area/10000 #Calculate the area of each patch. Now z$area is the number of pixels

reclassification_matrix <- as.matrix(z[, c("patches", "area")])

patches <- classify(patches, reclassification_matrix, others = 0)
patches<- ifel(patches < 20, 0, patches)
patches <- ifel(is.na(patches), 0, patches)

writeRaster(patches, "../../VARIABLES/DEFINITIVES/taques_bosc.tif")

# PRESÈNCIA TURÓ, CONTAGI

subconques<-vect("../../variables/rius/subconques/AIGUA_CON50M_REGION_SUBCON_G0.shp")
presencies<-obs_emporda[obs_emporda$`Polecat presence`==1,]
presencies_NE<-obs_NE[obs_NE$`Polecat presence`==1,]

presencies<-rbind(presencies, presencies_NE)
rm(presencies_NE)
presencies<-vect(presencies, geom=c("UTM X", "UTM Y"), crs="EPSG:25831")

presencies<-terra::intersect(subconques, presencies)
subconques$presencia<-ifelse(subconques$CODI_SUBCO %in% presencies$CODI_SUBCO, 1, 0)

presencies<-rasterize(subconques, ambit_r, field="presencia", background=0)

writeRaster(presencies, "../../VARIABLES/DEFINITIVES/presencia_turo.tif")

# ESFORÇ MOSTREIG

# esforc<-obs_tot
# esforc<-rasterize(esforc, field="Trap.nights", ambit_r)
# esforc[is.na(esforc)]<-0
# 
# pesos<-matrix(nrow = 33, ncol = 33, data=1)
# esforc<-terra::focal(esforc, w=pesos, fun="sum")
# 
# 
# writeRaster(esforc, "../../esforc_33km.tif", overwrite=T)

## Generar variable mitjana amb l'esforç de mostreig
# 
#  esforc<-rast("../../VARIABLES/DEFINITIVES/ESFORC_1km.tif")
#  global(esforc, fun="mean", na.rm=T)
# # 
#  esforc_constant<-ambit_r
#  values(esforc_constant)<-30.34857
#  plot(esforc_constant)
# # 
#  writeRaster(esforc_constant, "../../VARIABLES/DEFINITIVES/PROJECCIO_3km/ESFORC_CONSTANT.tif")

## Generar variable mitjana amb esforç de mostreig ESTANDARDITZADA
#Primer cal tenir la variable estandarditzada més avall

global(variables[[10]], fun="mean", na.rm=T)
# # 
  esforc_constant<-ambit_r
  values(esforc_constant)<-0.002771612
#  plot(esforc_constant)
# # 
  writeRaster(esforc_constant, "../../VARIABLES/DEFINITIVES/PROJECCIO_1km/ESFORC_CONSTANT_estandard.tif")




# ENTREN VARIABLES ----
variables<-list.files("../../VARIABLES/definitives", full.names = T, pattern = ".tif")
variables<-variables[-c(10,13)] # eliminem la variable de mostreig que no toca i dist rius
noms<-list.files("../../VARIABLES/definitives", pattern = ".tif")
noms<-noms[-c(10, 13)] #eliminem la variable de mostreig que no toca
noms <- substr(noms,1,nchar(noms)-4)
variables<-rast(variables)
names(variables)<-noms
#usdm::vif(variables) # Valors màxims de 12, ho deixem així, hi ha correlació però no és increïblement forta. Només treuríem les esclerofiles

# Estandardització de les variables
for (i in 1:nlyr(variables)) {
  min<-minmax(variables[[i]])
  variables[[i]]<-(variables[[i]]-min[1])/(min[2]-min[1])
  print(i)
}

# Passem un VIF per a disminuir la multicolinealitat 
vif_analisys<-vifstep(variables, th=7)

variables<-terra::subset(variables, c(3,10,15), negate=T) #I eliminem les variables amb pitjors valors.
noms<-names(variables)

# MODELITZACIÓ EMPORDÀ ----

obs_tot<-obs_emporda[obs_emporda$Polecat.presence==1,]
occs.xy<-obs_tot |> geom() |> as.data.frame()
occs.xy<-occs.xy[,c(3,4)]

bg.xy <- spatSample(variables, 10000, method="random", xy=T, values=F, na.rm=T)
bg.xy <- as.data.frame(bg.xy)

tune.args <- list(fc = c("L", "LQ"), rm = seq(1,2, 0.5)) # + fc=LQH = 17minuts !! ##rm treure opcions?

model <-  ENMeval::ENMevaluate(occs.xy, envs=variables, bg=bg.xy, algorithm = "maxent.jar", 
                                tune.args = tune.args, partitions = "randomkfold") # user.grp necessary?
evalTbl <- model@results
evalMods <- model@models
names(evalMods) <- model@tune.settings$tune.args
evalPreds <- model@predictions
evalPreds<-rast(evalPreds)

best_model <- which.min(evalTbl$delta.AICc) #No tenim prous observacions

#best_model<-5 #Posem com a millor model el menys complex: L RM=2

mapa<-evalPreds[[best_model]]
importancia<-as.data.frame(model@variable.importance[[best_model]])
#dismo::response(model@models[[best_model]], col="black", lwd=0.5, expand=0, range="p")

writeRaster(mapa, "../../MODELS/NOUS/EMPORDA/model_emporda.tif", overwrite= T)
write.csv(importancia, "../../MODELS/NOUS/EMPORDA/importancia_emporda.csv")
write.csv(evalTbl, "../../MODELS/NOUS/EMPORDA/evaluacio_models_emporda.csv")

pdf("../../MODELS/NOUS/EMPORDA/corbes_resposta_emporda.pdf", width=20, height=10)
dismo::response(model@models[[best_model]], col="black", lwd=0.5, expand=0, range="p")
dev.off()

zones<-fun_categories_SDM(mapa) #Reclassify the continuous map to 3 levels of suitability. 
writeRaster(zones, "../../MODELS/NOUS/EMPORDA/zones_emporda.tif")

# PROJECCIÓ

var_projeccio<-list.files("../../VARIABLES/DEFINITIVES/PROJECCIO_1km", full.names = T, pattern = ".tif")
var_projeccio<-rast(var_projeccio)

noms<-list.files("../../VARIABLES/definitives/projeccio_1km", pattern = ".tif")
noms <- substr(noms,1,nchar(noms)-4)

names(var_projeccio)<-noms
esforc<-var_projeccio[[13]]

# Estandarditzar variables de projecció.

for (i in 1:nlyr(var_projeccio)) {
  min<-minmax(var_projeccio[[i]])
  var_projeccio[[i]]<-(var_projeccio[[i]]-min[1])/(min[2]-min[1])
  print(i)
}

var_projeccio[[13]]<-esforc
#vif_analisys<-vifstep(var_projeccio, th=7)

var_projeccio<-terra::subset(var_projeccio, c(3,10,11,12,17), negate=T)
names(var_projeccio)[9]<-"esforc_1km"

projected_map <- dismo::predict(model@models[[best_model]], var_projeccio)

writeRaster(projected_map, "../../MODELS/NOUS/EMPORDA/projectat_emporda.tif")
zones_projectat<-fun_categories_SDM(projected_map)
writeRaster(zones_projectat, "../../MODELS/NOUS/EMPORDA/zones_projectat_emporda.tif")


# MODELITZACIÓ NOMÉS NE ----

obs_NE<-obs_NE[obs_NE$'Polecat.presence'==1,]
occs.xy<-obs_NE |> geom() |> as.data.frame()
occs.xy<-occs.xy[,c(3,4)]

bg.xy <- spatSample(variables, 10000, method="random", xy=T, values=F, na.rm=T)
bg.xy <- as.data.frame(bg.xy)

tune.args <- list(fc = c("L", "LQ"), rm = seq(1,2, 0.5)) # + fc=LQH = 17minuts !! ##rm treure opcions?

model <-  ENMeval::ENMevaluate(occs.xy, envs=variables, bg=bg.xy, algorithm = "maxent.jar", 
                               tune.args = tune.args, partitions = "randomkfold") # user.grp necessary?
evalTbl <- model@results
evalMods <- model@models
names(evalMods) <- model@tune.settings$tune.args
evalPreds <- model@predictions
evalPreds<-rast(evalPreds)

best_model <- which.min(evalTbl$delta.AICc)
mapa<-evalPreds[[best_model]]
importancia<-as.data.frame(model@variable.importance[[best_model]])
#dismo::response(model@models[[best_model]], col="black", lwd=0.5, expand=0, range="p")

writeRaster(mapa, "../../MODELS/NOUS/NE/model.tif", overwrite= T)
write.csv(importancia, "../../MODELS/NOUS/NE/importancia_tot.csv")
write.csv(evalTbl, "../../MODELS/NOUS/NE/eval_tot.csv")

pdf("../../MODELS/NOUS/NE/corbes.pdf", width=20, height=10)
dismo::response(model@models[[best_model]], col="black", lwd=0.5, expand=0, range="pa", rug=F)
dev.off()

zones<-fun_categories_SDM(mapa) #Reclassify the continuous map to 3 levels of suitability. 
writeRaster(zones, "../../MODELS/NOUS/NE/zones_tot.tif")

# PROJECCIÓ

var_projeccio<-list.files("../../VARIABLES/DEFINITIVES/PROJECCIO_1km", full.names = T, pattern = ".tif")
var_projeccio<-rast(var_projeccio)

noms<-list.files("../../VARIABLES/definitives/projeccio_1km", pattern = ".tif")
noms <- substr(noms,1,nchar(noms)-4)

names(var_projeccio)<-noms
esforc<-var_projeccio[[13]]

# Estandarditzar variables de projecció.

for (i in 1:nlyr(var_projeccio)) {
  min<-minmax(var_projeccio[[i]])
  var_projeccio[[i]]<-(var_projeccio[[i]]-min[1])/(min[2]-min[1])
  print(i)
}

var_projeccio[[13]]<-esforc
#vif_analisys<-vifstep(var_projeccio, th=7)

var_projeccio<-terra::subset(var_projeccio, c(3,10,11,17), negate=T)
names(var_projeccio)[10]<-"esforc_1km"

projected_map <- dismo::predict(model@models[[best_model]], var_projeccio)

writeRaster(projected_map, "../../MODELS/NOUS/NE/projectat.tif")
zones_projectat<-fun_categories_SDM(projected_map)
writeRaster(zones_projectat, "../../MODELS/NOUS/NE/zones_projectat.tif")
