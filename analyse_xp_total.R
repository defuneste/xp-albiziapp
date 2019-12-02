# . -------------------------------------------------------------------------- =============
# I - Chargement de packages supplementaire et des données  ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============



## 1 - Package supplemtaire  =======

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
library(sp)
library(sf)
library(plotly)

## 2 - Données  =======

source("chargement_xp_finale.R")

# . -------------------------------------------------------------------------- =============
# II - Cartes + stats   ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

summary(xp_total.shp)

# relevé par participant 

xp_total.shp %>% 
  st_drop_geometry() %>% 
  group_by(Participant) %>% 
  tally() %>% 
  ggplot(aes(y = n)) +
  geom_boxplot()


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