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

## 2 - Les données ================

### stream du json
exp_brut <- stream_in(file("data/tracesBrutesStEtienne29_06_19.json"))

### zone de l'xp
zone.shp <- st_read("data/zone_se.shp") # ouverture du fichier de zone 
# st_crs(zone.shp) # vérifie le le CRS de la couche
# summary(zone.shp) # verif de base

# on ne garde qu'une zone
zone.shp <- zone.shp[zone.shp$id ==1, ] # il y avait deux zones à la base au final je n'ai pris que la première
# plot(zone.shp)

# point de départ de l'experimentation 

mixeur.shp <- st_point(c(4.3860717, 45.4496287), dim = "XY") # localisation du mixeur

# on prend les arbres que l'on connait 
arbre_xp.shp <- st_read("data/arbres_se_final.geojson")
# limité à la zone
arbre_xp_zone.shp <- arbre_xp.shp[zone.shp,]

# plot(st_geometry(arbre_xp_zone.shp))

#### rapides descriptions/exploration des données
# summary(exp_brut)
# str(exp_brut, max.level = 2) # faire fluctuer le level pour descendre dans list/df 
# names(exp_brut)
# "_id"      "event"    "object"   "userId"   "username" "date"     "__v"      "activity"
# dim(exp_brut)
# class(exp_brut)

##.###################################################################################33
## II. Preprocess des données  ====
##.#################################################################################33

## 1 - Suppresion de données ================
# userId et username pareil
# unique(exp_brut$username)

# il faut me virer (defuneste)  car je n'ai pas participé, juste montré directement sur mon tel parfois
exp_brut <- exp_brut[exp_brut$username != "defuneste",]

# correspondance entre champs 
# activity$index : activity$name
# NA : verifier
# 0 : "Faites 5 relevés"
# 1 : "Faites 3 relevés de genres différents" 
# 2 : "Vérifiez 3 relevés d'un autre utilisateur (cercle vert clignotant)"
# 3 : "Identifiez 3 relevés d'expert (cercle bleu clignotant)"
# 4 : "Faites un maximum de relevé en un temps limite"
# il n'est utile que d'en garder que un
# on recode activité
exp_brut$code_activ <- exp_brut$activity$index # avec un nom plus parlant
exp_brut$code_activ <- exp_brut$code_activ+1

# je met de coté "event" Pierre-Yves et Ludovic doivent être plus apte pour travailler avec ce type d' info
# mais je vais pe l'utiliser pour filtrer 
# table(exp_brut$event, exp_brut$activity$index)
# sum(table(exp_brut$event, exp_brut$activity$index)) 
# 935 on a des NA

# unique(exp_brut$"__v")  #ne renvoie que 0 penser à le filtrer 

# sélection de newObservation
newObservation <- exp_brut[exp_brut$event == "newObservation",]

newObservation.df <- newObservation[,c("username","date","code_activ")] ## attention ici j'ai fait des selections par noms de colonnes

## 2 - Formatage et ajout de données ================
# on met la bonne tz 
attr(newObservation.df$date, "tzone") <- "Europe/Paris"

#### c'est assez hideux mais je fais vite
# on prend la date via une boucle, pe le changer 
newObservation$point <- NULL

for(i in 1:length(newObservation$event)) {
  newObservation.df$point[i] <- st_sfc(st_point(newObservation$object[[i]]$location$coordinates))}

newObservation.df <- st_sf(newObservation.df, geom = newObservation.df$point) # la bonne colone pour le champs geom
newObservation.df <- newObservation.df[,-4] ## attention ici j'ai fait des selections par num de colonnes

st_crs(newObservation.df) = 4326 # le bon scr

### ici c'est une solution R : on applique la fonction '[' (qui indexe la list), en fonction d'un vecteur de nom 
# le t() est juste pour un transpose
# as.data.frame est pour en faire un df

df_bota <- as.data.frame(t(sapply(newObservation[["object"]], `[`, c("authorName", "common" , "specie", "genus"))))
names(df_bota) <- c("authorName", "common", "specie", "genus")

# on a un pb avec le sapply on genere des listes il faut le corriger
df_bota$common[df_bota[["common"]] == "NULL"] <- NA
df_bota$specie[df_bota[["specie"]] == "NULL"] <- NA
df_bota$genus[df_bota[["genus"]] == "NULL"] <- NA

df_bota$authorName <- unlist(df_bota$authorName)
df_bota$common <- unlist(df_bota$common)
df_bota$specie <- unlist(df_bota$specie)
df_bota$genus <- unlist(df_bota$genus)

newObservation.df <- bind_cols(newObservation.df, df_bota) %>% select(-"authorName")

# on sauve 
#st_write(newObservation.df, "data/newObservation.geojson")

# ici lecture du fichier après vérification de la bota dans qgis
xp_bota.shp <- st_read("data/newObservation.geojson") 

## 3 - Modification du fichier avec bota ================

# anonimisation 
xp_bota.shp$participant <- "A"
xp_bota.shp$participant[xp_bota.shp$username == "tjoliveau"] <- "B"
xp_bota.shp$participant[xp_bota.shp$username == "JitenshaNiko"] <- "C"
xp_bota.shp$participant[xp_bota.shp$username == "MathDu"] <- "D"
xp_bota.shp$participant[xp_bota.shp$username == "Yoann Duriaux"] <- "E"
xp_bota.shp$participant[xp_bota.shp$username == "pofx"] <- "F"
xp_bota.shp$participant[xp_bota.shp$username == "Catherine JHG"] <- "G"

# attribution de 6 pour un relevé hors activité
xp_bota.shp$code_activ[is.na(xp_bota.shp$code_activ)] <- 6

# un factor
xp_bota.shp$code_activ <- as.factor(xp_bota.shp$code_activ)

xp_bota.shp <- xp_bota.shp %>% 
  mutate(participant = fct_infreq(participant))



