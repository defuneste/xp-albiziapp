##.###################################################################################33
## I. Chargement des données de l'xp ====
##.#################################################################################33

## 1 - Les bibliotheques ================

library(jsonlite) # pour les json
library(sp) # ancien package spatial toujours présent
library(sf) # package spatial
library(lubridate) # les dates 
library(forcats) # pour les facteurs
library(dplyr) # manip données


### stream du json
history_brut <- stream_in(file("data/history.json"))

# ici il faut filtrer sur la date 

attr(history_brut$date, "tzone") <- "Europe/Paris"

# je filtre à partir du 15 à voir si c'est bon 
history_16_09 <- subset(history_brut, date > ymd_hms("2019-09-15T09:00:00Z", tz = "Europe/Paris"))

# ici juste un changement de nom et un code pour les activités qui commence par par 0
history_16_09$code_activ <- history_16_09$activity$index # avec un nom plus parlant
history_16_09$code_activ <- history_16_09$code_activ+1

# on ne garde que newobservation, pe regarder pour validation
newObservation <- history_16_09[history_16_09$event == "newObservation",]
# ici on vire Pierre-Yves qui fait pas partie de l'xp
newObservation<- newObservation[newObservation$username != "gickhub",]

### zone de l'xp
zone.shp <- st_read("data/zone_se.shp") # ouverture du fichier de zone 
# st_crs(zone.shp) # vérifie le le CRS de la couche
# summary(zone.shp) # verif de base

# on ne garde qu'une zone
zone.shp <- zone.shp[zone.shp$id ==1, ] # il y avait deux zones à la base au final je n'ai pris que la première


newObservation.shp <- newObservation[,c("username","date","code_activ")] 
newObservation.shp$date <- ymd_hms(newObservation.shp$date)


for(i in 1:length(newObservation$event)) {
  newObservation.shp$point[i] <- st_sfc(st_point(newObservation$object[[i]]$location$coordinates))}

newObservation.shp <- st_sf(newObservation.shp, geom = newObservation.shp$point) # la bonne colone pour le champs geom
newObservation.shp <- newObservation.shp[,-4] ## attention ici j'ai fait des selections par num de colonnes

st_crs(newObservation.shp) = 4326 # le bon scr


df_bota <- as.data.frame(t(sapply(newObservation[["object"]], `[`, c("authorName", "common" , "specie", "genus"))))
names(df_bota) <- c("authorName", "common", "specie", "genus")

# on a un pb avec le sapply on genere des listes il faut le corriger, pe le fonctionnaliser
df_bota$common[df_bota[["common"]] == "NULL"] <- NA
df_bota$specie[df_bota[["specie"]] == "NULL"] <- NA
df_bota$genus[df_bota[["genus"]] == "NULL"] <- NA

df_bota$authorName <- unlist(df_bota$authorName)
df_bota$common <- unlist(df_bota$common)
df_bota$specie <- unlist(df_bota$specie)
df_bota$genus <- unlist(df_bota$genus)

newObservation.shp <- bind_cols(newObservation.shp, df_bota) %>% select(-"authorName")


# point de départ de l'experimentation 

mixeur.shp <- st_point(c(4.3860717, 45.4496287), dim = "XY") # localisation du mixeur

# on prend les arbres que l'on connait 
arbre_xp.shp <- st_read("data/arbres_se_final.geojson")
# limité à la zone
arbre_xp_zone.shp <- arbre_xp.shp[zone.shp,]

newObservation.shp <- bind_cols(newObservation.shp, df_bota) %>% select(-"authorName")
# ici on delimite la zone, dans ce cas cela à peu d'effet mais il faut faire attention 
# pour la suite notament sur les extraction à partir de newObservation qui peut avoir une longueur différente

# newObservation.shp <- newObservation.shp[zone.shp,] je decoupe par la zone sinon je perds un point en bordure ...

# on sauve 
#st_write(newObservation.shp, "data/xp_16_09_2019v3.geojson")

xp_16_09_bota.shp <- st_read("data/xp_16_09_2019.geojson")

xp_16_09_bota.shp <- xp_16_09_bota.shp[!is.na(xp_16_09_bota.shp$verif),]

## 2 - Modification du fichier avec bota ================

f_unlist <- function(une_list_un_niveau, un_nom_de_champ) {
  unlist( # on déroule la liste 
    sapply(une_list_un_niveau, # sapply sur la colonne qui contient les variables d'intéret
           `[`,  # `[` est la foction d'indexation
           un_nom_de_champ)) # on indexe sur une variable de nom confidence}
}

xp_16_09_bota.shp$hasImage <- f_unlist(newObservation[["object"]], "hasImage") #has image
xp_16_09_bota.shp$confiance <- f_unlist(newObservation[["object"]], "confidence") #confidence
xp_16_09_bota.shp$id <- unlist(newObservation[1]) #id de mongoDB

## 3 - on "annonymise" ============================ confidence

# dans ce cas transform va passer le tibble en df
xp_16_09_bota.shp <- transform(xp_16_09_bota.shp,Participant=as.numeric(factor(username)))


## 4 - onroganise le tout ======================
xp_16_09_bota.shp <- xp_16_09_bota.shp[,c(15,1,16,2:11, 13,14,12)]

