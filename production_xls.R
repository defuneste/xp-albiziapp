##.###################################################################################33
## I. Petit script pour faire de l'xls ====
##.#################################################################################33

# on charge les données de l'xp
source("chargement_xp.R")

# il y a d'autre packages pour faire du xls
# xlsx demande java jdk
# XLconnect est dit un peu plus lent
# pour juste de l'export openxlsx semble suffisant
# penser à modifier le lancement des library par un if et require
library(openxlsx)

xp_bot.df <- as.data.frame(xp_bota.shp) # le tibe est moins bien supporté que le df
xp_bot.df <- subset(xp_bot.df,select = -geometry) # xls aime pas le champ géométrie
list_xp_bot.df <- split(xp_bot.df, xp_bot.df$username) # verifier que username est un facteur


write.xlsx(file = "xp_29_96.xlsx", list_xp_bot.df)