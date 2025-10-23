
##################################
# TP2 : Statistiques bivariées
##################################


# Nous allons travailler sur des données de résultats d'une compétition en 2020 d'e-sport sur le  jeu  "Leagues of Legend",
# qui oppose deux équipes de 5 joueurs.
# Source : https://oracleselixir.com/tools/downloads


# Récupération des données

# le package readr est compatible avec dplyr et permet de lire directement depuis une url
library(readr)
d <- read_csv("http://nextcloud.iscpif.fr/index.php/s/eegwmt29kimWgdz/download") # url du fichier csv transféré sur une nextcloud (pour éviter l'authentification google drive, tout de même possible en R avec le package googledrive)


##################
#1 Exercice : préparation des données


# 1. Décrire le jeu de données : nombre de variables, d'observations
#    Nb : une ligne contient les variables qui décrivent un match d'un joueur 
#     -> 10 lignes par match, plus 2 lignes (1 d'aggrégation par équipe)
dim(d)
d

# example d'un match
data.frame(d[1:12,1:20])

# Nombre de matchs ?
d %>% group_by(gameid) %>% summarise(count=n())


# 2. Quel est le type de la colonne `position` et de la colonne  `dpm`  du jeu de données ? 
d %>% select(position,dpm) 

# 3. Faire un graphe de la frequence d'apparition des champions
library(ggplot2)

g = ggplot(d %>% select(champion) %>% na.omit())
g+geom_bar(mapping = aes(x=champion))


# 4. Calculer la valeur moyenne de la colonne `totalgold`


# 5. Comparer les moyennes de `totalgold` selon le résultat de la partie (colonne result)



# 6. Calculer puis ajouter au dataframe l'or par minute (en utilisant `totalgold` et `gamelength`)


# 7. Faire la même chose avec les kills par minute


# 8. Tracer un nuage de points avec or par minute vs kills par minute, en colorant par victoire ou défaite.





