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

newObservation.shp <- newObservation[,c("username","date","code_activ")] 
newObservation.shp$date <- ymd_hms(newObservation.shp$date)

for(i in 1:length(newObservation$event)) {
  newObservation.shp$point[i] <- st_sfc(st_point(newObservation$object[[i]]$location$coordinates))}

newObservation.shp <- st_sf(newObservation.shp, geom = newObservation.shp$point) # la bonne colone pour le champs geom
newObservation.shp <- newObservation.df[,-4] ## attention ici j'ai fait des selections par num de colonnes

st_crs(newObservation.shp) = 4326 # le bon scr

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

newObservation.shp <- bind_cols(newObservation.shp, df_bota) %>% select(-"authorName")

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


newObservation.shp <- newObservation.shp[zone.shp,]


# . -------------------------------------------------------------------------- =============
# II - une carte animée ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

## 1 - Fond de cartes  =======
# ici je prend chez stamen, il faut une bbox bb() de sf 
# xp_st_e <- ggmap(get_stamenmap(bb(zone.shp, output = "matrix"),zoom = 16, maptype = "terrain-lines"))


# str(xp_st_e)
# bb(x = xp_st_e$data$lon, y = xp_st_e$data$lat)

arbre_xp_zone.coord <-st_coordinates(arbre_xp_zone.shp)
arbre_xp_zone.coord <- as.data.frame(arbre_xp_zone.coord)

arbre_xp_zone.shp[bb(x = xp_st_e$data$lon, y = xp_st_e$data$lat),]

obs_timing <- st_coordinates(newObservation.shp)
obs_timing <- as.data.frame(obs_timing)
obs_timing$date <- newObservation.shp$date
obs_timing$username <- newObservation.shp$username

# obs_timing$participant <- "Particpant 1"
# obs_timing$participant[obs_timing$username == "tjoliveau"] <- "Particpant 2"
# obs_timing$participant[obs_timing$username == "JitenshaNiko"] <- "Particpant 3"
# obs_timing$participant[obs_timing$username == "MathDu"] <- "Particpant 4"
# obs_timing$participant[obs_timing$username == "Yoann Duriaux"] <- "Particpant 5"
# obs_timing$participant[obs_timing$username == "pofx"] <- "Particpant 6"
# obs_timing$participant[obs_timing$username == "Catherine JHG"] <- "Particpant 7"

unique(obs_timing$username)

xp_st_e_anim <- xp_st_e + 
  geom_point(aes(x = 4.3860717, y = 45.4496287), size = 4, pch = "M") +# localisation mixeur
  geom_point(data = arbre_xp_zone.coord, aes(x = X, y = Y), size = 0.75, col = "#208842", alpha = 0.5) +
  #geom_point(data = obs_timing, aes(x = X, y = Y), size = 2.5) + 
  geom_point(data = obs_timing, aes(x = X, y = Y, colour = username), size = 2.5) + 
  xlab("") + ylab("") +
  ggtitle("Test d'Albiziapp",
          subtitle = 'time:{frame_time}') +
  transition_components(date) +
  shadow_mark() 

xp_st_e_anim

anim_save("xp_st_e_16-09.gif" , animation = last_animation())

unique(newObservation.shp$username)
