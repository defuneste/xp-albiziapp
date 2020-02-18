# . -------------------------------------------------------------------------- =============
# I - Chargement de packages supplementaire et des données  ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

## 1 - Package supplemtaire  =======

#attention certaines fonctions sont masquées 
# manipulation des données 
library(tidyverse) # envt pour des donnees de type tidy pas trop volumineuse
library(lubridate) # un wrapper pour des fonctions plus simples de dates
library(purrr) # prog fonctionnel en tidyverse

# données spatiales
library(rgdal) # 
library(sp) # ancien package de gestion de donnees spatiales 
library(sf) # le nouveau
library(spatstat) # outils d'analyse point pattern
library(maptools) # des outils principalements de conversion

# visu plus outils
library(tmap) # cartes statics
library(tmaptools) # outils de carte dont palette_color
library(leaflet) # cartes dynamiques
library(plotly) # graph interactif 
library(profvis) # profilage de code

## 2 - Chargement Données  =========

source("chargement_xp_finale.R")

# . -------------------------------------------------------------------------- =============
# II - Cartes + stats   ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

summary(xp_total.shp)


# 1 Nombre de participants par xp ====================


xp_total.shp %>% 
  st_drop_geometry() %>% # drop de la geometry 
  group_by(Participant, mois) %>% # group par participants et par xp
  dplyr::summarize(n = n()) %>% # on compte les relevés par xp et participants
  ggplot(aes(x = n)) + # un plot rapide
  geom_histogram(binwidth = 1)

# relevés par participant 

xp_total.shp %>% 
  st_drop_geometry() %>% 
  group_by(Participant, mois) %>% 
  dplyr::summarize(n = dplyr::n()) %>% 
  ggplot(aes(y = n, color = mois)) +
  geom_boxplot() + 
  # ici je veux ajouter les points par dessus
  labs(y = "nbr de relevés") +
  # il me faut virer l'axes de x et le grid de x
  theme_bw() 

# je comprend pas la moustache du premier boxplot
xp_total.shp %>% 
    st_drop_geometry() %>% 
    group_by(Participant, mois) %>% 
    dplyr::summarize(n = dplyr::n()) %>% 
plot_ly(y = ~n, color = ~mois, type = "box")


xp_summarize <- xp_total.shp %>% 
  st_drop_geometry() %>% 
  # on rajoute la distance    
  group_by(username) %>% 
            # nombre de genre bon 
  dplyr::summarize(indic_genre = sum(genre_bon, na.rm = T),
            # nombre de nom commun bon, attention si le genre n'est pas present il n'est pas compté comme bon (ni comme faux)     
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
            temps_min = min(date),
            temp_max = max(date),
            # ici c'est la somme de la distance
            dist_cumul = round(sum(dist_m),2)) %>% 
    mutate(durée_secs = round((temp_max - temps_min),2)) %>% 
    select(-c(temp_max, temps_min))

xp_total.shp %>% 
    group_by(username, mois) %>% 
    ggplot(aes(y = dist_m, color = mois)) +
    geom_boxplot(alpha = .5) + 
    # ici je veux ajouter les points par dessus
    labs(y = "distance (m)") +
    # il me faut virer l'axes de x et le grid de x
    theme_bw()

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


# des arbres isolés aurait plus de chance d'etre pris ?
# cf tilleuls 

## 4 - Effet par rapport à la diversité arborée  ================

# . -------------------------------------------------------------------------- =============
# IV - Différents effets des activités  ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============


xp_activite <- xp_total.shp %>% 
  st_drop_geometry() %>% 
  # on rajoute la distance    
  group_by(username, code_activ) %>% 
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
                   temps_min = min(date),
                   temp_max = max(date),
                   # ici c'est la somme de la distance
                   dist_cumul = round(sum(dist_m),2)) %>% 
  mutate(durée_secs = round((temp_max - temps_min),2)) %>% 
  select(-c(temp_max, temps_min)) 


#une verif des atcivités, le passer en plotly
# 75 % des cas suivent la regle 
xp_activite %>% 
  filter(code_activ <= 2) %>% 
ggplot(aes(code_activ, y = n)) +
  geom_boxplot()



# . -------------------------------------------------------------------------- =============
# V - Différents effets de la botaniques  ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============



