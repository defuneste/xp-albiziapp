##.###################################################################################33
## Script rapide pour regarder le json de l'xp ====
##.#################################################################################33

#### script plus à jour à suuprimer une fois tout repomper

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
            nb_espece = sum(!is.na(specie)), # nombre d'especes latin renseigné
            ok_commun = sum(nb_commun))  

# nombre de relevé vérifiés
sum(!is.na(xp_bota.shp$verif))




