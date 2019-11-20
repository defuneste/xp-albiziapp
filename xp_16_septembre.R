##.###################################################################################33
## I. Chargement des données de l'xp ====
##.#################################################################################33
ha
## 1 - les données ================

# ici les données nous sont arrivées par bout et pas toujours sous la même forme ce qui explique le 
# chargement de plusieurs scripts qui les prépare

# xp du 26/09/2019
source("chargement_xp.R")
summary(xp_bota.shp)
dim(xp_bota.shp)

# xp du 16/09/2019
source("chargement_xp_16_09.R")
#### verif rapide 
summary(xp_16_09_bota.shp)
dim(xp_16_09_bota.shp)

# xp du 01/10/2019
source("chargement_xp_01_10.R")
summary(xp_01_10_bota.shp)
dim(xp_01_10_bota.shp)



##.###################################################################################33
## II. une seule db ====
##.#################################################################################33

# on vérifie nom et ordres des colonnes
names(xp_bota.shp) == names(xp_16_09_bota.shp)
names(xp_bota.shp) == names(xp_01_10_bota.shp)

# on fusionne le tout
# ici je suis resté en Rbase pe faire en dplyr pour ne pas perdre la géométrie
xp.dat <- rbind(as.data.frame(xp_16_09_bota.shp), as.data.frame(xp_bota.shp), as.data.frame(xp_01_10_bota.shp))

# on vérifie 
summary(xp.shp)
dim(xp.shp)

# on en refait un objet sf
xp.shp <- st_as_sf(xp.dat, sf_column_name = "geometry")


xp.shp[xp.shp$genus == "Platanus",]


##.###################################################################################33
## III. Première stats ====
##.#################################################################################33

summary(xp_16_09_bota.shp)
dim(xp_16_09_bota.shp)

length(unique(xp_16_09_bota.shp$username))

# pe faire une fonction annonyme
compteNA <- function(x) {sum(is.na(x))}
# comptage des na par colonne
sapply(xp_16_09_bota.shp, compteNA)

table(newObservation$username)
table(xp_16_09_bota.shp$username)

names(xp_bota.shp )