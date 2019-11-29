##.###################################################################################33
## I. Chargement des données de l'xp v 11/2019 ====
##.#################################################################################33

### zone de l'xp
zone.shp <- st_read("data/zone_se.shp") # ouverture du fichier de zone 
# st_crs(zone.shp) # vérifie le le CRS de la couche
# summary(zone.shp) # verif de base

# on ne garde qu'une zone
zone.shp <- zone.shp[zone.shp$id ==1, ] # il y avait deux zones à la base au final je n'ai pris que la première
zone.shp <- sf::st_transform(zone.shp, 2154) 

# point de départ de l'experimentation 
mixeur.shp <-st_sfc(st_point(c(4.3860717, 45.4496287)), crs = 4326) # localisation du mixeur
mixeur.shp <- sf::st_transform(mixeur.shp ,2154)

# chargement des arbres que l'on connait 
arbre_xp.shp <- sf::st_read("data/arbres_se_final.geojson")
arbre_xp.shp <- st_transform(arbre_xp.shp, 2154)
# limité à la zone
arbre_xp_zone.shp <- arbre_xp.shp[zone.shp,]
rm(arbre_xp.shp)

# 2 - données d' xp ==================

xp_total.dat <- readRDS("data/xp_total.rds")
xp_total.shp <- st_as_sf(xp_total.dat, sf_column_name = "geometry")
rm(xp_total.dat)

