
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


# 2. Quel est le type de la colonne `position`  et de la colonne  `dpm`  du jeu de données ? 


# 3. Faire un graphe de la frequence d'apparition des champions


# 4. Calculer la valeur moyenne de la colonne `totalgold`


# 5. Comparer les moyennes de `totalgold` selon le résultat de la partie (colonne result)



# 6. Calculer puis ajouter au dataframe l'or par minute (en utilisant `totalgold` et `gamelength`)


# 7. Faire la même chose avec les kills par minute


# 8. Tracer un nuage de points avec or par minute vs kills par minute, en colorant par victoire ou défaite.





#######################
#2 Corrélations



# Corrélation de Pearson
cor(d$gamelength, d$totalgold)

# Avec test statistique de significativité
cor.test(d$gamelength, d$totalgold)


# selection des colonnes numériques
dnum <-  select(d, where(is.numeric))

# supression des variables numériques mais ne capturant pas un processus dans la partie
dnum <-  select(dnum,-c(year, playoffs,patch, participantid, game, result  ))




#######################
## Exercice

# 1. Variables les plus corrélées
#   -> Déterminer les 4 couples de variables numériques les plus corrélées au sens de Pearson





# 2. Variables les moins corrélées 
#    -> même traitement avec les corrélations les plus faibles 





# 3. Confirmer la forte/faible corrélation par un nuage de points pour quelques couples de variables identifiées ci-dessus




# 4. Faire une fonction qui génère  les nuages de points de tous les couples de variables dont la corrélation
#     est inférieure ou supérieure à un certain seuil (en valeur absolue)





# 5. Détecter un couple de variable pour lesquelles la corrélation de Pearson
#     est inférieure à la corrélation de Spearman; interpréter.






####################
#3 Régression linéaire 



############################
## Exercice

# 1. Choisir un couple de variables fortement corrélées et afficher son nuage de points




# 2. Réaliser la régression linéaire entre les deux variables ci-dessus; interpréter les résultats 




# 3. Pour les variables `monsterkills` et `totalgold`, tracer le nuage de points, puis proposer des conditions
#   pour décomposer les observations en 2 groupes




# 4. Refaire une regression linéaire pour chaque groupe, interpréter



#################################
#4 Test d indépendance du chi2


#########################
# Exercice

# 1. Chercher des variables qualitatives qui semblent avoir un lien avec la variable qualitative `result`


# 2. Proposer d autres variables discrètes construites à partir de seuils sur des variables numériques et tester leur relation avec result


# 3. Quelle est la variable discrète la plus liée au résultat du match ?



