##.###################################################################################33
## I. zone de l'experimentation ====
##.#################################################################################33

library(maptools)
library(jsonlite) # pour les json
library(sp) # ancien package spatial toujours présent
library(sf) # package spatial
library(lubridate) # les dates 
library(forcats) # pour les facteurs
library(dplyr) # manip données
library(leaflet)
library(ggplot2)
library(ggmap)
library(tmaptools)
library(spatstat)

## 1 - Les  données de la zone  ================
#Ici On va ne garder qu'une partie des données pour l'experimentation d'albiziapp

source("chargement_xp_finale.R")

sum(as.numeric(st_area(zone.shp)))/10000 # surface de la zone ici en ha car je suis en m2 en 2154

#une carte rapide
plot(st_geometry(zone.shp))
plot(st_geometry(arbre_xp_zone.shp), pch = 16, add = T, col = "forestgreen") # un plot pour verifier (on passe par le geom car on veut juste verifier les points pas toutes les variables)

############## attention il faut changer pas mal de truc ========================
## ici j'ai utilisé ggmap mais les API sur les site de tuiles sont de moins en moins open donc il faudra pe un jour le changer
# bb est une fonction de tmaptools qui permet de retourner une bounding box sous un format matrix
# on va chez stamen, on prends toner-lite, préciser le zoom aide pour le dl des tuiles
xp_st_e <- ggmap(get_stamenmap(bb(st_transform(zone.shp, 4326), output = "matrix"),zoom = 16, maptype = "terrain-lines"))
xp_st_e +
  geom_sf(data = st_transform(arbre_xp_zone.shp, 4326), inherit.aes = FALSE, pch = 16, alpha = 0.7, col = "forestgreen", size = 0.5) +
  xlab("") + ylab("") +
  labs(caption = "Fonds OSM, rendu Stamen") +
  scale_color_manual(values = c("forestgreen" = "forestgreen"), name = "", labels = "Arbres préalablement identifiés")

save(xp_st_e, file = "data/xp_st_e.RData")

# 2 - Un leaflet =============
carto_se <- leaflet() %>% 
  addTiles() %>%
  addCircleMarkers(data = st_transform(arbre_xp_zone.shp, 4326),         # repasser en wgs84 
                   radius = 2, label = ~species, col = "forestgreen")    # cosmetiques + label avec nom latin
carto_se

## 3 - des stats ==========

# On a un peu trop de genre pour que cela soit lisible sur un graph/carte en dessous de 10 individus species va devenir "autre". Je sais pas encore si je vais le garder. 

#nombre d'arbre dans la zone
nrow(arbre_xp_zone.shp)
# densité
nrow(arbre_xp_zone.shp)/(sum(as.numeric(st_area(zone.shp)))/10000)

# un tableau du nombre d'arbre par genre
comptage_xp_reduit <- arbre_xp_zone.shp %>% # pour le sous ensemble
    st_set_geometry(value = NULL) %>% # drop la geometrie
    group_by(genus) %>%  # groupe par genus ou species
    summarize(comptage = n()) %>% # comptage
    arrange(desc(comptage)) # range juste pour la lisibilité

arbre_xp_zone.shp %>%  
    st_set_geometry(value = NULL) %>% # drop la geometrie
    group_by(genus) %>% # group par genus
    tally() %>%  # compte
    left_join (arbre_xp_zone.shp, by =c ("genus" = "genus" )) %>%  # merge le compte obtenu sur chaque arbre, c'est pas sexy mais bon 
        ggplot(aes(x = reorder(genus, n))) + #  fill = as.factor(id))) + # ici on reoder avec n et on met id n factor pour le remplissage
            geom_bar(fill = "#8BBFDD") + 
            labs(x ="Genre",
                ylab ="Nombre d'arbres") +
            coord_flip() + 
            theme(axis.text=element_text(size=8)) 

# fct_explicit_na(species) si on veut avoir les espèces
# mutate(espece = if_else(comptage_xp$comptage >= 10, comptage_xp$genus, "Autres")) un bout de code au besoin si on veut éviter la longue traine

## des stats de distance et NN

## ici on passe en sp avec sf
xp_sp <- as(arbre_xp_zone.shp, "Spatial")
class(xp_sp)
## ici on passe en ppp avec maptools
xp_ppp <- as(xp_sp, "ppp") 
class(xp_ppp)

## on verifie 

## on plot
# plot(xp_ppp$x, xp_ppp$y)
# ici juste dans un veteur
# nndist vient de spatstat 
# et calcul la distance la plus proche dans un objet ppp
arbre_xp_zone.shp$dist <- nndist(xp_ppp)

# un graph
arbre_xp_zone.shp %>% 
  ggplot(aes(dist)) + 
  geom_freqpoly(binwidth = 1) + # freqpoly avec un binwidth de 1 m 
  xlab("distance (m)") +
  ylab("Nombres d'arbres") + 
  theme_bw()

#### La distance à un arbre de meme genre

source("fonction_xp_albiziapp.R")

nom_genre <- c("Platanus", "Prunus", "Sorbus", "Corylus", "Pinus", "Magnolia") 

dist_same_tree("Platanus", arbre_xp_zone.shp)

for(i in 1:length(nom_genre)){
    arbre_xp_zone2.shp <- dist_same_tree(nom_genre[i], arbre_xp_zone.shp)
    print(nom_genre[i])} 


arbres_xp_dist %>% 
  filter(!is.na(ndistidem)) %>% # ici c'est un filtre des NA cf ligne plus
  ggplot(aes(ndistidem, fill = genus)) + # il faut un facteur pour utiliser colours
  geom_histogram(binwidth = 1) + # freqpoly avec un binwidth de 1 m 
  scale_x_continuous(limits = c(0,30)) +
  facet_wrap(~genus, ncol = 2) +
  xlab("distance (m)") +
  ylab("Nombres d'arbres")

