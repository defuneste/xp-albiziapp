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
library(lubridate) # un wrapper pour des fonctions plus simples de dates
library(sp)
library(sf)
library(plotly)

## 2 - Données  =======

source("chargement_xp_finale.R")

# . -------------------------------------------------------------------------- =============
# II - Cartes + stats   ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

summary(xp_total.shp)

# une facteur pour les xp
# ici on va prendre les mois des xp et les passer en facteur
xp_total.shp$mois <- as.factor(lubridate::month(xp_total.shp$date))

# nb de participants par xp
table(xp_total.shp$mois)

xp_total.shp %>% 
  st_drop_geometry() %>% # drop de la geometry 
  group_by(Participant, mois) %>% # group par participants et par xp
  summarize(n = n()) %>% # on compte les relevés par xp et participants
  ggplot(aes(x = n)) + # un plot rapide
  geom_histogram(binwidth = 1)

# relevés par participant 

xp_total.shp %>% 
  st_drop_geometry() %>% 
  group_by(Participant, mois) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(y = n, color = mois)) +
  geom_boxplot(alpha = .5) + 
  # ici je veux ajouter les points par dessus
  labs(y = "nbr de relevés") +
  # il me faut virer l'axes de x et le grid de x
  theme_bw()

#  indicateurs thierry 

xp_total.shp %>% 
  st_drop_geometry() %>% 
  group_by(username) %>% 
            # nombre de genre bon 
  dplyr::summarize(indic_genre = sum(genre_bon, na.rm = T),
            # nombre de nom commun bon       
            indic_commun = sum(commun_bon, na.rm = T),
            # nombre d'especes bon
            indic_sp = sum(espece_bon, na.rm = T),
            # comptage des relevés
            n = dplyr::n(),
            # attention on est en minute et en decimal de minute pas des secondes 
            # si on mets 0 on aura que des minutes 
            # ce temps correspond à l'ecart entre le premier relevé et le second
            # si il n'y a qu'un relevé sa valeur ne peut donc qu'être que de 0
            # c'est le cas pour deux utilisateurs
            # on ne devrait pe les garder ?
            temps_min = round((max(date) - min(date)),2))


names(xp_total.shp)

xp_total.shp[xp_total.shp$username == "GradelerM", ]

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

# . -------------------------------------------------------------------------- =============
# V - Différents effets de la botaniques  ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============



