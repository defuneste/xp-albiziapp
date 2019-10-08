##.###################################################################################33
## I. Chargement des données de l'xp ====
##.#################################################################################33

## 1 - les données ================

source("chargement_xp_16_09.R")


#### verif rapide 

##.###################################################################################33
## II. Première stats ====
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

