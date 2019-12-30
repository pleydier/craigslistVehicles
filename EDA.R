
#################################################################################
##                          ININITIALISATION                                   ##
#################################################################################

###### installation packages necessaires ########
list.of.packages= c("data.table", "naniar", "ggplot2", "dplyr", "tidyr", "rworldmap"
                    , "corrplot", "rstudioapi", "VIM")
install.packages(list.of.packages)
#################################################

library(rstudioapi)
# Set active directory to the document currently opened in RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(data.table)
library(naniar) #pour plot les valeurs manquantes
library(ggplot2)
#library(tidyverse)#PROBLEME AVEC CETTE LIBRAIRIE!!!!!!!!!
#install.packages("tidyverse")
library(dplyr) # pour le recode
library(tidyr) #drop na
#install.packages("zoo")
library(rworldmap)
#install.packages("rworldmap")
library(corrplot)
library(VIM)
#install.packages("corrplot")

#data= read.csv("data/craigslistVehicles.csv", na.strings= "", sep= ",")
data= fread("data/craigslistVehicles.csv", header = T, sep = ',', na.strings= "")
dim(data)
colnames(data)
head(data)

#################################################################################
#################################################################################
#################################################################################

#informations sur les features de notre table
str(data)

#on plot les valeurs manquantes pour les 100 000 premières lignes
vis_miss(data[c(0:100000)], warn_large_data=F)
#REMARQUE: il semble que certaines que le manque de données dans certaines variables
          #ne soit pas aléatoire ==> on observe des patterns
navar= colSums(is.na(data))/nrow(data) #taux de na dans les colonnes (variable)
navar

naind= rowSums(is.na(data))#/ncol(data)#taux na par individu
max(naind)

table(naind) #TODO: Dropper les mecs qui ont trop de nan ?

data= setDF(data)

col_names= colnames(Filter(is.character, data))
col_names
data[col_names] <- lapply(data[col_names] , factor)

str(data)



##############################################################################
##                              EXPLORATION                                 ##
##############################################################################


#on va séparer notre df en 2; quali et numériques
dim(data)
nums= Filter(is.numeric, data)
#nums$year= NULL
quali= Filter(is.factor, data)
#on rajoute year ici aussi
quali$year= data$year
dim(nums)
dim(quali)



bar_freq= function(data, variable_name, x){
  ploted= ggplot(data.frame(data), aes(x=data[, x]), na.rm= T) +
    geom_bar() +
    ggtitle(variable_name)
  print(ploted)
}

box_plot= function(data, variable_name, x){
  ploted= ggplot(data, aes(x= data[, x]), na.rm= T) +
    geom_boxplot() +
    ggtitle(variable_name)
  print(ploted)
  
}

#
#étude variables nums
#

par(mfrow=c(2, 3))
for (x in seq(1, length(nums)))
  boxplot(nums[,x], main= names(nums[x]))


#boxplot bizarre pour la variable price et odomètre
summary(nums)
#on se rend compte qu'il ya une valeur très élévé pour la feature price (3milliards)
#qui nous empèche de parfaitement tracer notre boxplot.
#on a aussi un prix = 0
quantile(nums$price, c(.75, .99))
quantile(nums$price, c(.99, .9997))
#on va donc supprimer toutes les lignes tels que le prix est supérieurs à notre quantile
#99,9% (~250 000€)
quantile(nums$price, c(.1, .05))
#on va aussi forcer le prix à au moins être supérieur à 0
#car on se rend compte que ceux qui mettent en vente une voiture le font pour mettre
#en description une pub

quantile(nums$odometer, c(.75, .99), na.rm= T)
quantile(nums$odometer, c(.75, .999), na.rm= T)
#une voiture a apparement au compteur 999 999 de milles US (limite compteur)

#on remarque que notre distribution de la longitude est très concentré entre -150 et 
#-50 ==> normal on est aux USA (longitude US entre -60 et -160(alaska))
#latitude négative ==> liée aux îles ? (Hawaii etc.)

quantile(nums$year, c(.001, .01), na.rm=T)
quantile(nums$year, c(.0001, .001), na.rm=T)
#j'ai une valeur de 1914.44 au quantile .0001 !!!!!
sum(nums$price == 1914.44) #ca doit être la fonction quantile qui a un problème
#valeur max de year est 1900, cela ne semble pas être une valeur aberrante étant donné
#que la première voiture date de 1884
#mais le pic de vente c'est les 30 glorieuses, et quand on regarde l'histogramme
#c'est à peu près a partir de la que ca commence
#on remarque aussi une année 2020 qui semble aberrante
quantile(nums$year, c(.75, .99), na.rm= T)
quantile(nums$year, c(.75, .997), na.rm= T)
#on va supprimer les années qui sont supérieures à 2019!

#
#on va plot nos coordonnées
#
dev.off(dev.list()["RStudioGD"]) #on nettoie les images
newmap <- getMap(resolution = "low")
xlim= c(min(nums$long, na.rm=T), max(nums$long, na.rm=T)) #long 
ylim= c(min(nums$lat, na.rm=T), max(nums$lat, na.rm=T)) #lat


plot(newmap, xlim = xlim, ylim = ylim, asp = 1)
points(nums$long, nums$lat, col = "red", cex = .6)

# ==> on observe des valeurs aberrantes sur le plotting des coord
summary(nums)

#on va donc se restreindre aux USA
#on verifie que nos limites correspondent aux USA
xlim_us= c(-180, -66.9) #long
ylim_us= c(5.87, 71.39) #lat
dev.off(dev.list()["RStudioGD"]) #on nettoie les images
plot(newmap, xlim = xlim_us, ylim = ylim_us, asp = 1)
points(nums$long, nums$lat, col = "red", cex = .6)
#on va garder ces limites pour nos données
#TODO: Ameliorer les frontieres pour rendre la data plus propre
# => mettre en NA les outliers
# Reconstruire les NA en fonction de la ville



#
#étude variables quali
#

#peut prendre un peu de temps
col_names= colnames(quali)
for (x in seq(1, length(quali)))
  bar_freq(quali, col_names[x], x)

#variable condition a beaucoup de nan ==> remplacé par non-rensigné ? ou supprimer ?

unique(quali$cylinders)
unique(quali$fuel)
unique(quali$title_status) #peu de valeur autre de "clean"
unique(quali$transmission) #peu de vaeur autre que "automatic"
unique(quali$drive) #bcp de 4wd et de nan
unique(quali$size) #bcp de nan
unique(quali$type) #bcp de nan
unique(quali$paint_color) #bcp de nan
#pk pas regrouper des couleurs entre elles (grey /silver)
unique(quali$manufacturer)
unique(quali$city)
unique(quali$condition)

#peut être que les NaN dans conditions sont liés à l'année 
bar_freq(subset(quali, is.na(condition)), "year", 13)
#la distribution est similaire à celle de toute la pop donc non


#################################################################################
#################################################################################
#################################################################################


#################################################################################
##                                RETRAITEMENT                                 ##
#################################################################################


###################################### NAN ######################################

######### manufacturer #########
## Strategie 1 : recherche du manufacturer dans le champ "make"
sum(is.na(data$manufacturer)) # 24 579 NA
unique_manufacturers <- unique(data$manufacturer) # These are the known manufacturers
unique_manufacturers <- unique_manufacturers[-3] # remove the NA value
# Pour chaque ligne ou manufacturer est NA, chercher un manufacturer connu dans make et desc
data[is.na(data$manufacturer)]$manufacturer = apply(
  data[is.na(data$manufacturer)],
  1, 
  function(row, count){
    return <- NA
    for (pattern in unique_manufacturers){
      if(grepl(pattern, row["make"], ignore.case=TRUE)){
        return <- pattern
      }else if(grepl(pattern, row["desc"], ignore.case=TRUE)){
        return <- pattern
      }
    }
    return
  }
)
sum(is.na(data$manufacturer)) # 12 213 NA (12 366 valeurs trouvees)

## Strategie 2 : assigner � "unknown"
data$manufacturer[is.na(data$manufacturer)] = "unknown"
sum(is.na(data$manufacturer)) # 0 NA (12 213 valeurs remplacees)

## Strategie 3 : knn avec le champ make
# bag of words sur le champ make
# knn sur le champ make
#TODO




retraitement= function(x) {
  if (is.numeric(x)) {
    x[is.na(x)]= mean(x, na.rm = TRUE)
    x
  } else {
    x[is.na(x)]= names(which.max(table(x)))
    x
  }
} #remplace par mean si numeric soit par la valeur la plus frequente pour les factor



#paint_color ==> fusion modalité grey et silver et remplacer NaN par "Not Documented"
data_new= copy(data)

data_new$paint_color= recode(data_new$paint_color, silver= "grey")
data_new$paint_color= as.character(data_new$paint_color)
data_new$paint_color[is.na(data_new$paint_color)]= "Not Documented"
data_new$paint_color= as.factor(data_new$paint_color)
data_new$paint_color = droplevels(data_new$paint_color)
unique(data_new$paint_color)

#size ==> manque des classes dans le segment automobile (classe A B C ... F)
#on va remplacer NaN par "Not Documented"
#ou on peut droper
data_new$size= as.character(data_new$size)
data_new$size[is.na(data_new$size)]= "Not Documented"
data_new$size= as.factor(data_new$size)
data_new$size = droplevels(data_new$size)
unique(data_new$size)

#pattern de valeur manquante entre condition, drive, type
#drive ==> remplacement NaN par "awd" (autre type de drive) (courant aux US)
data_new$drive= as.character(data_new$drive)
data_new$drive[is.na(data_new$drive)]= "Not Documented"
data_new$drive= as.factor(data_new$drive)
data_new$drive = droplevels(data_new$drive)
unique(data_new$drive)

#type
data_new$type= as.character(data_new$type)
data_new$type[is.na(data_new$type)]= "Not Documented"
data_new$type= as.factor(data_new$type)
data_new$type = droplevels(data_new$type)
unique(data_new$type)

#condition
data_new$condition= recode(data_new$condition, new= "like new")
data_new$condition= as.character(data_new$condition)
data_new$condition[is.na(data_new$condition)]= "Not Documented"
data_new$condition= as.factor(data_new$condition)
data_new$condition = droplevels(data_new$condition)
unique(data_new$condition)

#cycinder, pas sûr par cete transformation
data_new$cylinders= as.character(data_new$cylinders)
data_new$cylinders[is.na(data_new$cylinders)]= "Not Documented"
data_new$cylinders= as.factor(data_new$cylinders)
data_new$cylinders = droplevels(data_new$cylinders)
unique(data_new$cylinders)


#year
data_new$year= as.factor(data_new$year)#on passe en factor au cas ou
data_new$year= retraitement(data_new$year)
data_new$year= as.numeric(as.character(data_new$year))
mean(is.na(data_new$year)) #vérification

#odometer
data_new$odometer= retraitement(data_new$odometer)

#manufacter
#peut être regrouper des marques entres elles 
data_new$manufacturer= retraitement(data_new$manufacturer)

#make, on va probablement devoir suppimer cette varaible (colinéaire avec manufacturer ?????)
data_new$make= retraitement(data_new$make)

#fuel
data_new$fuel= retraitement(data_new$fuel)

#title
data_new$title_status= retraitement(data_new$title_status)

#transmission
data_new$transmission= retraitement(data_new$transmission)


#la vérificaiton
navar= colSums(is.na(data_new))/nrow(data_new) #taux de na dans les colonnes (variable)
navar

vis_miss(setDT(data_new)[c(0:200000)], warn_large_data=F)


#
#suppression valeurs aberrantes
#
data_abe= copy(data_new)
data_abe= data_abe %>% filter(year <= 2019
                              , year >= 1950
                              , odometer <= 999999
                              , odometer <= 999999
                              , price > 0
                              , price <= 250000)
                              #, long >= -180  #limite US
                              #, long <= -66.9
                              #, lat >= 5.87
                              #, lat <= 71.39) #ca a drop les NaN de lat et long

summary(data_abe)
dim(data_abe)

rm(data)
rm(data_new)
rm(nums)
rm(quali)

write.csv(data_abe
          , file = "data_abe.csv")
#
#on va plot nos coordonnées pour voir
#
#dev.off(dev.list()["RStudioGD"]) #on nettoie les images
#newmap <- getMap(resolution = "low")
#xlim= c(min(data_abe$long, na.rm=T), max(data_abe$long, na.rm=T)) #long 
#ylim= c(min(data_abe$lat, na.rm=T), max(data_abe$lat, na.rm=T)) #lat


#plot(newmap, xlim = xlim, ylim = ylim, asp = 1)
#points(data_abe$long, data_abe$lat, col = "red", cex = .6)




##################################################################################
##                                CREATION VARIABLES                            ##
##################################################################################


#on modifie la valeurs des lat / long par la moyenne des lat / long par ville
data_general_localisation= aggregate(data_abe[, 16:17]
                                     , list(data_abe$city)
                                     , mean
                                     , na.rm= TRUE)

#
#on va plot nos coordonnées pour voir
#
dev.off(dev.list()["RStudioGD"]) #on nettoie les images
newmap <- getMap(resolution = "low")
xlim= c(min(data_general_localisation$long, na.rm=T), max(data_general_localisation$long, na.rm=T)) #long 
ylim= c(min(data_general_localisation$lat, na.rm=T), max(data_general_localisation$lat, na.rm=T)) #lat
plot(newmap, xlim = xlim, ylim = ylim, asp = 1)
points(data_general_localisation$long, data_general_localisation$lat, col = "red", cex = .6)
# on remarque que le fait d'aggréger nos lat / long a supprimé nos valeurs aberrantes


#on va chercher à déterminer de quel ville US, la ville dans notre DF se rapproche le plus
#(utilisation des lat / long de data_general_localisation)

localisation_us= read.csv("data/us-zip-code-latitude-and-longitude.csv", sep= ";")
dim(localisation_us)
localisation_us$geopoint= NULL
localisation_us$Timezone= NULL
localisation_us$Zip= NULL
localisation_us$Daylight.savings.time.flag= NULL

#on a plusieurs fois la même ville avec des coordonnéees uasi égale (différence liéau zip code qu'on a delete)
#on va donc faire récupérer la première occurence par ville (1er ligne)

#d'abord on range par ordre de ville ET d'état
localisation_us= localisation_us[order(localisation_us$City, localisation_us$State),]
#puis on supprime les dupliquées
localisation_us= localisation_us[!duplicated(localisation_us[c(1, 2)]),]
dim(localisation_us)

#on a enfin des données de ville / etat exploitable
#maintenant on va s'amuser à chercher la ville qui est la plus proche des coordonnées
#que nous avons calculé pour lui associé un Etat (pour la partie Tableau)

#trouvé sur internet
#permet de faire un merge par rapport à la distance minimale
greatCircleDistance <- function(lat1, long1, lat2, long2, radius=6372.795){
  sf <- pi/180
  lat1 <- lat1*sf
  lat2 <- lat2*sf
  long1 <- long1*sf
  long2 <- long2*sf
  lod <- abs(long1-long2)
  radius * atan2(
    sqrt((cos(lat1)*sin(lod))**2 +
           (cos(lat2)*sin(lat1)-sin(lat2)*cos(lat1)*cos(lod))**2),
    sin(lat2)*sin(lat1)+cos(lat2)*cos(lat1)*cos(lod)
  )
}


dist.merge <- function(x, y, xlongnme, xlatnme, ylongnme, ylatnme){
  tmp <- t(apply(x[,c(xlongnme, xlatnme)], 1, function(x, y){
    dists <- apply(y, 1, function(x, y) greatCircleDistance(x[2],
                                                            x[1], y[2], y[1]), x)
    cbind(1:nrow(y), dists)[dists == min(dists),,drop=F][1,]
  }
  , y[,c(ylongnme, ylatnme)]))
  tmp <- cbind(x, min.dist=tmp[,2], y[tmp[,1],-match(c(ylongnme,
                                                       ylatnme), names(y))])
  row.names(tmp) <- NULL
  tmp
}

#attention c'est un peu long
data_merge_localisation= dist.merge(data_general_localisation
           , localisation_us
           , 'long', 'lat', 'Longitude', 'Latitude')
#on change le nom de nos features
data_merge_localisation= data_merge_localisation %>% 
  dplyr::rename(city_neighbour= City, city= Group.1)

write.csv(data_merge_localisation
          , file = "data_merge_localisation.csv")



##################################################################################
##################################################################################
##################################################################################







######################################
####  CORR et contingence ############
######################################
navar= colSums(is.na(data_abe))/nrow(data_abe) #taux de na dans les colonnes (variable)
navar

dim(data_abe)
nums= Filter(is.numeric, data_abe)
quali= Filter(is.factor, data_abe)
#quali$year= data$year
dim(nums)
dim(quali)
summary(data_abe)

par(mfrow=c(1,1))
m= cor(nums)
corrplot(m)





#
#enregistrement nouvelle table propre
#
write.csv(data_abe, file = "data_abe.csv")


