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

## 2 - Données  =========

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
  dplyr::summarize(n = n()) %>% # on compte les relevés par xp et participants
  ggplot(aes(x = n)) + # un plot rapide
  geom_histogram(binwidth = 1)

# relevés par participant 

xp_total.shp %>% 
  st_drop_geometry() %>% 
  group_by(Participant, mois) %>% 
  dplyr::summarize(n = dplyr::n()) %>% 
  ggplot(aes(y = n, color = mois)) +
  geom_boxplot(alpha = .5) + 
  # ici je veux ajouter les points par dessus
  labs(y = "nbr de relevés") +
  # il me faut virer l'axes de x et le grid de x
  theme_bw()

## 1 - Indicateurs Thierry  =========

# distance parcourue
# ici on va faire une distance à vole d'oiseau, il faut aussi prendre le point de départ qui est le mixeur

# on fait un tableau point de départ 
username <- unique(xp_total.shp$username)
date <- rep(min(xp_total.shp$date) - 60, length(unique(xp_total.shp$username)))
point_depart <- data.frame(username, date)

# dont le point de départ est le mixeur
st_geometry(point_depart) <- rep(mixeur.shp, length(username))

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

# je le rajoute à xt_total.shp
xp_total.shp$dist_m <- temp_dist$dist

xp_summarize <- xp_total.shp %>% 
  st_drop_geometry() %>% 
  # on rajoute la distance    
  mutate(dist_m = temp_dist$dist) %>% 
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


# des arbres isolés dans aurait plus de chance d'etre pris ?
# cf tilleuls 

## 4 - Effet par rapport à la diversité arborée  ================

# . -------------------------------------------------------------------------- =============
# IV - Différents effets des activités  ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============


xp_activite <- xp_total.shp %>% 
  st_drop_geometry() %>% 
  # on rajoute la distance    
  mutate(dist_m = temp_dist$dist) %>% 
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



