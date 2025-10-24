

###########################
# TP 4 : reduction de dimension
###########################

library(dplyr)
library(ggplot2)

#######################
#1 ACP basique

irisnum <- iris[,1:4]

acp <- prcomp(irisnum,scale. = TRUE)

summary(acp)


# Examiner les champs de l'objet acp, interpréter les résultats







#######################
#2 ACP avec des packages avancés

# Utilisation des packages `ade4` qui réalise l'ACP et 
#   `factoextra`  qui fournit des outils pour visualiser les résultats (à installer)

# On travaille sur les données des penguins

library(ade4)
library(factoextra)
library(palmerpenguins)




#2.1) Préparation des données : filtrer les valeurs manquantes, sélectionner les variables numériques qui décrivent la morphologie des pingouins.



#2.2) Affichage des données pour appréhender les distributions
#  -> scatterplots (pour le faire avec ggplot, utiliser `ggpairs` du package `GGally`)
#  -> corrélations
#  -> densités et/ou histogrammes



#2.3) Calculer l'inertie des points (cf formule du cours)






#2.4) Question 5 : Réaliser une ACP avec `dudi.pca` du package `ade4`
#  et explorer les résultats avec le package `factoextra` (compatible avec `ade4`)
#    - `get_eigenvalue` : Extraction des valeurs propres / variances des composantes principales
#    - `fviz_eig` : Visualisation des valeurs propres
#    - `get_pca_ind`, `get_pca_var`: Extraction des résultats pour les individus et les variables, respectivement.
#    - `fviz_pca_ind`, `fviz_pca_var`: visualisez les résultats des individus et des variables, respectivement.



#2.5) Quel est le pourcentage d'inertie capturée  par les deux premières composantes ?



#2.6) Quelle est la coordonnée de la deuxième composante dans l'espace de départ ?



#2.7) Diagnostic global 



#2.8) Visualiser les variables dans le plan des PCs



#2.9)  Quelle est la contribution des variables `bill_length`  et `bill_depth` à la 3ème composante ? 


#2.10) projeter les individus dans le plan formé par les deux premières composantes et interpréter



#2.11) Colorer les pingouins par espèce sur un scatterplot; l'ACP permet-elle de mieux séparer les espèces?





#######################
# 3) Reduction de dimensionalité non-linéaire
#######################

# Prendre en main les méthodes t-SNE (package Rtsne) et UMAP (package umap)



# Comparer PCA, t-SNE, et UMAP sur un ensemble de variables quantitatives






