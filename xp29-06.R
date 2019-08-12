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

table(newObservation.df$username, newObservation.df$code_activ)

newObservation.df %>% 
ggplot(aes(x = code_activ, fill = username)) +
    geom_bar(position = "dodge") +
    labs(x ="Activités",
         y ="Nombre de relevés") 

## 5 - stats de base  ================

summary(xp_bota.shp)

ggplot(data = xp_bota.shp, aes(x = username)) +
  geom_bar(aes(fill = code_activ))

## 6 - nndist ================

newObservation_zone.df <- newObservation.df[zone.shp,]

## ici on passe en sp avec sf
xp_sp <- as(st_transform(newObservation_zone.df, 2154), "Spatial")
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
head(arbre_plus_proche)

## on sauve comme une nouvelle variable
newObservation_zone.df$dist <- nndist(xp_ppp)

# un graph
newObservation_zone.df %>% 
    ggplot(aes(dist)) + # il faut un facteur pour utiliser colours
    geom_freqpoly(binwidth = 1) + 
    xlab("distance (m)") +
    ylab("Nombres d'arbres")

# . -------------------------------------------------------------------------- =============
# III - Différents effets de l'envt possible  ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

## 1 - Effet du point de départ  ================



# tester différentes distance par rapport à lui

## 2 - Effet par rapport au dernier point  ================


## 3 - Effet par rapport à la densité arborée  ================

## 4 - Effet par rapport à la diversité arborée  ================

# . -------------------------------------------------------------------------- =============
# IV - Différents effets des activités  ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============
