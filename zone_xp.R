##.###################################################################################33
## I. zone de l'experimentation ====
##.#################################################################################33

library(jsonlite) # pour les json
library(sp) # ancien package spatial toujours présent
library(sf) # package spatial
library(lubridate) # les dates 
library(forcats) # pour les facteurs
library(dplyr) # manip données

## 1 - Les  données de la zone  ================
#Ici On va ne garder qu'une partie des données pour l'experimentation d'albiziapp


zone.shp <- st_read("data/zone_se.shp") # ouverture du fichier de zone 
st_crs(zone.shp)
summary(zone.shp)
sum(st_area(zone.shp))/10000 # surface de la zone ici en ha car je suis en m2 en 4326
sum(st_area(zone.shp[1,]))/10000 # surface de la zone reduite ici 

#On ne garde que ce qui dans zone. 


# attention ici on est en lat/long et st_intersetcs fait comme si on était projeté
# dans notre cas c'est pas grave car on est sur une petite surface mais du coup si on utilise un 
# st_intersect c'est apparament mieux d'être en projeté
arbres_xp <- arbres_se.final.shp[zone.shp,] # un clip sur la zone totale
arbres_xp_reduit <- arbres_se.final.shp[zone.shp[1,],] # un clip sur la zone réduite
summary(arbres_xp)
summary(arbres_xp_reduit)

#st_write(arbres_xp, dsn = "data/arbres_se_xp.geojson")


plot(st_geometry(arbres_xp )) # un plot pour verifier (on passe par le geom car on veut juste verifier les points pas toutes les variables)
plot(st_geometry(zone.shp))
plot(st_geometry(zone.shp[1,]))



## une carte


arbres_xp$arbre <- 1
st_crs(arbres_xp)

############## attention il faut changer pas mal de truc ========================
## ici j'ai utilisé ggmap mais les API sur les site de tuiles sont de moins en moins open donc il faudra pe un jour le changer
# bb est une fonction de tmaptools qui permet de retourner une bounding box sous un format matrix
# on va chez stamen, on prends toner-lite, préciser le zoom aide pour le dl des tuiles
xp_st_e <- ggmap(get_stamenmap(bb(zone.shp, output = "matrix"),zoom = 16, maptype = "terrain-lines"))
xp_st_e +
  geom_sf(data = arbre_xp_zone.shp, inherit.aes = FALSE, pch = 16, alpha = 0.7, col = "forestgreen", size = 0.5) +
  xlab("") + ylab("")

save(xp_st_e, file = "data/xp_st_e.RData")

carto_se <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = arbres_xp, radius = 2) 

carto_se



## des stats


comptage_xp <-  arbres_xp %>% # pour l'ensemble
  st_set_geometry(value = NULL) %>% # on drop la geometrie
  group_by(genus) %>% # groupe par genus ou species
  summarize(comptage = n()) %>% # on compte
  arrange(desc(comptage)) # on range juste pour la lisibilité

comptage_xp_reduit <- arbres_xp_reduit %>% # pour le sous ensemble
  st_set_geometry(value = NULL) %>% # on drop la geometrie
  group_by(genus) %>%  # groupe par genus ou species
  summarize(comptage = n()) %>% # on compte
  arrange(desc(comptage)) # on range juste pour la lisibilité




# On a un peu trop de genre pour que cela soit lisible sur un graph/carte en dessous de 10 individus species va devenir "autre". Je sais pas encore si je vais le garder. 

```{r}
comptage_xp %>% 
  # ici on reprend le fichier de comptage et on va attraibuer Autres si on est inf à 10
  # puis on regroupe et recompte
  mutate(espece = if_else(comptage_xp$comptage >= 10, comptage_xp$genus, "Autres")) %>% 
  group_by(espece) %>% 
  summarize(comptage = sum(comptage)) %>% 
  arrange(desc(comptage))

```



# On regarde la répartition des genres. On va attribuer une zone aux arbres ce qui est sans doute l'option la plus simple (et avec laquelle j'aurais du commencer)


```{r}
arbres_xp <- st_join(arbres_xp, zone.shp) # une jointure spatiale, ici on prend tous le fichier comme il est presque vide
table(arbres_xp$id) # une verif

# un rableau temporaire qui compte l'occurence par genre
temp_order <- arbres_xp %>%  
  st_set_geometry(value = NULL) %>% 
  group_by(genus) %>% 
  tally()

# on le reinjecte dans arbres_xp pour ordonner le futur graph par le nombre d'occurence
arbres_xp <- left_join (arbres_xp, temp_order, by =c ("genus" = "genus" ))
```



```{r}
arbres_xp %>% 
  st_set_geometry(value = NULL) %>% # on drop la geométrie
  ggplot(aes(x= reorder(genus, n))) + #  fill = as.factor(id))) + # ici on reoder avec n et on met id n factor pour le remplissage
  geom_bar(fill = "#8BBFDD") + 
  labs(x="Genre",
       ylab="Nombre d'arbres") +
  coord_flip() + 
  #scale_fill_manual(values = c("#90D18D", "#8BBFDD"),  # des couleurs moins piquantes
  #                                 name = "Zonage", labels = c("Elargi", "Reduit")) + # la legende
  theme(axis.text=element_text(size=8)) # on diminue la taille des labels


```

## des stats de distance et NN

```{r}
## ici on passe en sp avec sf
xp_sp <- as(st_transform(arbres_xp, 2154), "Spatial")
## ici on passe en ppp avec maptools
xp_ppp <- as(xp_sp, "ppp") 

## on verifie 
class(xp_ppp)
str(xp_ppp)

## on plot
plot(xp_ppp$x, xp_ppp$y)
# ici juste dans un veteur
# nndist vient de spatstat 
arbre_plus_proche <- nndist(xp_ppp)
class(arbre_plus_proche )
length(arbre_plus_proche )
head(arbre_plus_proche )

## on sauve comme une nouvelle variable
arbres_xp$dist <- nndist(xp_ppp)

# un graph
arbres_xp %>% 
  ggplot(aes(dist)) + #colour = as.factor(id))) + # il faut un facteur pour utiliser colours
  geom_freqpoly(binwidth = 1) + # freqpoly avec un binwidth de 1 m 
  # scale_colour_manual(values = (c("#90D18D", "#8BBFDD")),  # des couleurs moins piquantes
  #                                  name = "Zonage", labels = c("Elargi", "Reduit")) + # la legende
  xlab("distance (m)") +
  ylab("Nombres d'arbres")



#### La distance à un arbre de meme genre



arbres_xp_dist <- arbres_xp
arbres_xp_dist$ndistidem <- NA

## attention il faut plus de deux arbres sinon nndist va retourner un vecteur de taille 0 alors que 1 est le minimum pour l'indexation
dist_same_tree <- function(nom_genre, arbres_sf) {
  # on filtre sur un genre
  arbres_xp_filter <- filter(arbres_sf, genus == nom_genre) %>% 
    select('ref:FR:Saint-Etienne:tree', geometry)
  # on converti ce sf en ppp filtrer, on passe aussi en lambert
  arbres_xp_filter.ppp <- as(as(st_transform(arbres_xp_filter, 2154), "Spatial"), "ppp")
  # on passe nndist dans le tible filtré
  arbres_sf$ndistidem[arbres_sf$genus == nom_genre] <- nndist(arbres_xp_filter.ppp)
  return(arbres_sf)
}

nom_genre <- c("Platanus", "Prunus", "Acer", "Pinus", "Tilia", "Sorbus", "Quercus", "Cedrus", "Alnus", "Populus") 



for(i in 1:length(nom_genre)){
  arbres_xp_dist <- dist_same_tree(nom_genre[i], arbres_xp_dist)
  print(nom_genre[i])} 

# arbres_xp_dist <- dist_same_tree(nom_genre, arbres_xp_dist)



arbres_xp_dist %>% 
  filter(!is.na(ndistidem)) %>% 
  ggplot(aes(ndistidem, fill = genus)) + # il faut un facteur pour utiliser colours
  geom_histogram(binwidth = 1) + # freqpoly avec un binwidth de 1 m 
  scale_x_continuous(limits = c(0,30)) +
  facet_wrap(~genus, ncol = 2) +
  xlab("distance (m)") +
  ylab("Nombres d'arbres")

