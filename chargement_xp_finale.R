##.###################################################################################33
## I. Chargement des données de l'xp v 11/2019 ====
##.#################################################################################33

# 1 - zone de l'xp ==================
### zone de l'xp
zone.shp <- st_read("data/zone_se.shp") # ouverture du fichier de zone 
# st_crs(zone.shp) # vérifie le le CRS de la couche
# summary(zone.shp) # verif de base

# on ne garde qu'une zone
zone.shp <- zone.shp[zone.shp$id ==1, ] # il y avait deux zones à la base au final je n'ai pris que la première
zone.shp <- sf::st_transform(zone.shp, 2154) 

# point de départ de l'experimentation 
mixeur.shp <-st_sfc(st_point(c(4.3860717, 45.4496287)), crs = 4326) # localisation du mixeur
mixeur.shp <- sf::st_transform(mixeur.shp, 2154)

# chargement des arbres que l'on connait 
# ici il peut y avoir un gain de temps si je le sauve en RDS
arbre_xp.shp <- sf::st_read("data/arbres_se_final.geojson")
arbre_xp.shp <- st_transform(arbre_xp.shp, 2154)
# limité à la zone
arbre_xp_zone.shp <- arbre_xp.shp[zone.shp,]
str(arbre_xp_zone.shp)
# correction de facteur en trop 
arbre_xp_zone.shp$genus <- factor(arbre_xp_zone.shp$genus)
arbre_xp_zone.shp$species <- factor(arbre_xp_zone.shp$species)
arbre_xp_zone.shp$species.fr <- factor(arbre_xp_zone.shp$species.fr)
rm(arbre_xp.shp)

# 2 - données d' xp ==================

xp_total.shp <- sf::st_read("data/xp_total.geojson")

# 3 - renforcement des données ================

# ici on va prendre les mois des xp et les passer en facteur
# permet d'avoir un facteur mois qui distingue les trois xp
xp_total.shp$mois <- as.factor(lubridate::month(xp_total.shp$date))

# distance parcourue
# ici on va faire une distance à vole d'oiseau, il faut aussi prendre le point de départ qui est le mixeur

# etape 1 : on fait un tableau point de départ 
username <- unique(xp_total.shp$username)
date <- rep(min(xp_total.shp$date) - 60, length(unique(xp_total.shp$username)))
point_depart <- data.frame(username, date)
# dont le point de départ est le mixeur
st_geometry(point_depart) <- rep(mixeur.shp, length(username))
rm(username, date)


# c'est un peu gourmant en ressource ces 400 calculs de distance pe pas optimisé
# on obtient ainsi 400 distance en m dont la première est calculée par rapport au mixeur
temp_dist <- xp_total.shp %>% 
    # je suis passé en metre et pas en degré
    st_transform(2154) %>% 
    dplyr::select(username, date) %>% 
    rbind(st_transform(point_depart,  2154)) %>% 
    group_by(username) %>% 
    # on part de la date min qui est celle definit dans point de départ 
    arrange(date) %>% 
    mutate(
        # on ne peut pas passer un lag par contre on peut indexer une colonne 
        lead = geometry[row_number() + 1],
        # st_distance peut travailler par element
        dist = st_distance(geometry, lead, by_element = T)
    ) %>% 
    # les NA sont un artefact que l'on peut virer
    filter(!is.na(dist))

# rajout à xt_total.shp
xp_total.shp$dist_m <- temp_dist$dist
rm(temp_dist, point_depart)
