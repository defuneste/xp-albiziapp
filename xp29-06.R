##.###################################################################################33
## Script rapide pour regarder le json de l'xp ====
##.#################################################################################33



# . -------------------------------------------------------------------------- =============
# I - Chargement de packages supplementaire et des données  ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============


## 1 - Données  =======

source("chargement_xp.R")

## 2 - Package supplemtaire  =======

#attention certaines fonctions sont masquées 
library(tidyverse) # envt pour des donnees de type tidy pas trop volumineuse
library(rgdal) # 
library(tmap) # cartes statics
library(tmaptools) # outils de carte dont palette_color
library(leaflet) # cartes dynamiques
library(ggmap) # outils pour les cartes statics avec tuiles
library(gganimate) # animation avec ggplot et map
library(spatstat) # outils d'analyse point pattern
library(maptools) # des outils principalements de conversion
library(purrr) # prog fonctionnel en tidyverse

# . -------------------------------------------------------------------------- =============
# II - Carte + stats   ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

## 1 - Une carte des nouvelle obs  =======

pal <- colorFactor(palette =c(get_brewer_pal("Set2", n = 7)),
                   levels = c("Catherine JHG", "JitenshaNiko", "Kasia Ozga", "MathDu", "pofx", "tjoliveau",  "Yoann Duriaux"),
                   na.color = "black")

carto_SE <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = arbre_xp_zone.shp, popup = arbre_xp_zone.shp$species, radius = 1) %>% 
    addCircleMarkers(data = newObservation.df, radius = 2, opacity = 0.7, 
                     popup = paste("Nom commun:", newObservation.df$common, "<br>",
                                   "Sp.:", newObservation.df$specie, "<br>",
                                   "Genre:", newObservation.df$genus),
                     color = ~pal(username)) %>% 
    addLegend(position = "bottomright",
          pal = pal,
          values = c("Catherine JHG", "JitenshaNiko", "Kasia Ozga", "MathDu", "pofx", "tjoliveau",  "Yoann Duriaux"),
          na.label = "abs d'info")

carto_SE

## 2 - Une carte annimée  =======

# il faut aller dans gif_xp.R

## 3 - Temporalité ==============
# à travailler

plot(newObservation.df$date)

strftime(newObservation.df$date, format="%H:%M:%S")

## 4 - Activité/personne ================

apply(table(newObservation.df$username, newObservation.df$code_activ), 1, sum)


newObservation.df %>% 
ggplot(aes(x = code_activ, fill = username)) +
    geom_bar(position = "dodge") +
    labs(x ="Activités",
         y ="Nombre de relevés") 

## 5 - stats de base  ================

### 5.1 Info sur la zone de test ================
st_area(zone.shp) # surface de la zone
nrow(arbre_xp_zone.shp) # nombre d'arbre 

summary(arbre_xp_zone.shp)

# un tableau du nombre d'arbre par genre
comptage_xp_reduit <- arbre_xp_zone.shp %>% # pour le sous ensemble
  st_set_geometry(value = NULL) %>% # on drop la geometrie
  group_by(genus) %>%  # groupe par genus ou species
  summarize(comptage = n()) %>% # on compte
  arrange(desc(comptage)) # on range juste pour la lisibilité

# combien d'espèce de prunus

unique(arbre_xp_zone.shp$species[arbre_xp_zone.shp$genus == "Prunus" ])

unique(arbre_xp_zone.shp$species)

### 5.2 Info sur l'xp ================
summary(xp_bota.shp)
dim(xp_bota.shp)
names(xp_bota.shp)

# un tableau du nombre de relvé par participants

tableau_participants <- as.data.frame(table(xp_bota.shp$participant))

xp_bota.shp %>%  
  st_set_geometry(value = NULL) %>%  # on drop la geométrie
  group_by(Participant) %>% # on groupe par participants
  summarise(nb_releve = n(), # nombre de nouvelles observations par participants
            nb_genre = sum(!is.na(common)), # nombre de nom commun renseignés 
            nb_commun = sum(!is.na(genus)), # nombre de genre renseignés
            nb_espece = sum(!is.na(specie)) # nombre d'especes latin renseigné
            )  

# nombre de relevé avec au moins une info. Attention utilise NA dans bota pour cela
sum(!is.na(xp_bota.shp$bota))

## 6 - Distances entre les points ================

### 6.1 Distances à l'arbres le plus proches ====================

## ici on passe en sp avec sf
xp_sp <- as(st_transform(arbre_xp_zone.shp , 2154), "Spatial")
## ici on passe en ppp avec maptools
xp_ppp <- as.ppp(xp_sp) 

## on verifie 
class(xp_ppp)
str(xp_ppp)

## on plot
plot(xp_ppp$x, xp_ppp$y)
# ici juste dans un veteur
# nndist vient de spatstat 
arbre_plus_proche <- nndist(xp_ppp)
class(arbre_plus_proche)
length(arbre_plus_proche)
# une serie de stats de verif
head(arbre_plus_proche)
mean(arbre_plus_proche)
summary(arbre_plus_proche)
library(rethinking)
PI(arbre_plus_proche) # necessite rethinking 

## on sauve comme une nouvelle variable
arbre_xp_zone.shp$dist <- nndist(xp_ppp)

# un graph
arbre_xp_zone.shp %>% 
    ggplot(aes(dist)) + # il faut un facteur pour utiliser colours
    geom_freqpoly(binwidth = 1) + 
    xlab("distance (m)") +
    ylab("Nombres d'arbres")

### 6.2 Distances à l'arbres d'un genre différent le plus proches ====================

# retourne un tableau avec la distance aà tous les genre

data(ants)
plot(ants)
nnda <- nndist(ants, by=marks(ants)) 
head(nnda)

# ici retourne un tableau de l'arbre le plus proche par genre 
# droplevel a été utilisé pour virer les levels non utilisés je devrais le faire avant
# des genre contenu dans marks 
arbre_plus_proche_genre <- nndist(xp_ppp, by=droplevels(marks(xp_ppp)$genus))

apply(arbre_plus_proche_genre, 1, min)

# . -------------------------------------------------------------------------- =============
# III - Différents effets de l'envt possible  ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

## 1 - Effet du point de départ  ================

# on peut penser ici le modifier en commencant par une autre activité

# tester différentes distance par rapport à lui

## 2 - Effet par rapport au dernier point  ================


## 3 - Effet par rapport à la densité arborée  ================

## 4 - Effet par rapport à la diversité arborée  ================

# . -------------------------------------------------------------------------- =============
# IV - Différents effets des activités  ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============
