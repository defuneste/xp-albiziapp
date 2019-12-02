###### scripts de fonctions pour les xp d'albiziapp 

# 1- fonctions de distances 

## la fonction prend un genre, un objet de point_sf et necessite pas mal de package spatiaux, surtout pour les conversions
## elle retourne un objet sf que l'on peut ensuite sauver 
## attention il faut plus de deux arbres sinon nndist va retourner un vecteur de taille 0 alors que 1 est le minimum pour l'indexation

dist_same_tree <- function(nom_genre, arbres_sf) {
    if(require("sf")==FALSE)  
        install.packages("sf",  dependencies=c("Depends", "Suggests"))
    if(require("sp")==FALSE)  
        install.packages("sf",  dependencies=c("Depends", "Suggests"))
    if(require("dplyr")==FALSE)  
        install.packages("sf",  dependencies=c("Depends", "Suggests"))
    if(require("maptools")==FALSE)  
        install.packages("sf",  dependencies=c("Depends", "Suggests"))
    if(require("spatstat")==FALSE)  
        install.packages("sf",  dependencies=c("Depends", "Suggests"))
    # on filtre sur un genre
    arbres_xp_filter <- filter(arbres_sf, genus == nom_genre) %>% 
        select('ref.FR.Saint.Etienne.tree', geometry)
    # on converti ce sf en ppp filtrer, on passe aussi en lambert
    arbres_xp_filter.ppp <- as(as(arbres_xp_filter, "Spatial"), "ppp")
    # on passe nndist dans le tible filtré
    arbres_sf$ndistidem[arbres_sf$genus == nom_genre] <- nndist(arbres_xp_filter.ppp)
    return(arbres_sf)
}


print("dist_same_tree() va calculer la distance entre les arbres de meme genre" )