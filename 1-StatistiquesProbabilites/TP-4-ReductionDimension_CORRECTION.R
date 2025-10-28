

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
plot(irisnum)
plot(acp$x)
plot(data.frame(acp$x))

acp$rotation

plot(acp$x,col=iris$Species)
plot(irisnum,col=iris$Species)


#######################
#2 ACP avec des packages avancés

# Utilisation des packages `ade4` qui réalise l'ACP et 
#   `factoextra`  qui fournit des outils pour visualiser les résultats (à installer)

# On travaille sur les données des penguins

library(ade4)
library(factoextra)
library(palmerpenguins)




#2.1) Préparation des données : filtrer les valeurs manquantes, sélectionner les variables numériques qui décrivent la morphologie des pingouins.
dpengs <- penguins %>% na.omit() %>% select(bill_length_mm,bill_depth_mm,flipper_length_mm,body_mass_g)

#2.2) Affichage des données pour appréhender les distributions
#  -> scatterplots (pour le faire avec ggplot, utiliser `ggpairs` du package `GGally`)
#  -> corrélations
#  -> densités et/ou histogrammes

library(GGally)
ggpairs(dpengs, title="Correlogram of penguins numeric variables",
        lower = list(continuous=wrap("points",  color="darkcyan",   size=0.3)),
        diag = list(continuous=wrap("densityDiag", size=0.5,color="darkcyan"))
) +theme_light()


library(corrplot)
corrplot(cor(dpengs))


#2.3) Calculer l'inertie des points (cf formule du cours)
mu = colMeans(dpengs)

sum((dpengs - matrix(data=rep(mu,nrow(dpengs)),nrow = nrow(dpengs),byrow = T))^2)



#2.4) Question 5 : Réaliser une ACP avec `dudi.pca` du package `ade4`
#  et explorer les résultats avec le package `factoextra` (compatible avec `ade4`)
#    - `get_eigenvalue` : Extraction des valeurs propres / variances des composantes principales
#    - `fviz_eig` : Visualisation des valeurs propres
#    - `get_pca_ind`, `get_pca_var`: Extraction des résultats pour les individus et les variables, respectivement.
#    - `fviz_pca_ind`, `fviz_pca_var`: visualisez les résultats des individus et des variables, respectivement.

acp <- ade4::dudi.pca(dpengs, scannf = FALSE, nf=4)

# rotation
acp$co

#2.5) Quel est le pourcentage d'inertie capturée  par les deux premières composantes ?
fviz_eig(acp)
get_eigenvalue(acp)

#2.6) Quelle est la coordonnée de la deuxième composante dans l'espace de départ ?
acp$co

#2.7) Diagnostic global 
summary(acp)


#2.8) Visualiser les variables dans le plan des PCs
fviz_pca_var(acp)

#2.9)  Quelle est la contribution des variables `bill_length`  et `bill_depth` à la 3ème composante ? 
acp$co[,3]

#2.10) projeter les individus dans le plan formé par les deux premières composantes et interpréter
fviz_pca_ind(acp)


#2.11) Colorer les pingouins par espèce sur un scatterplot; l'ACP permet-elle de mieux séparer les espèces?
fviz_pca_ind(acp,col.ind = (penguins%>%na.omit())$species)




#######################
# 3) Reduction de dimensionalité non-linéaire
#######################


# Prendre en main les méthodes t-SNE (package Rtsne) et UMAP (package umap)
library(Rtsne)
library(umap)

vignette("umap")

# Comparer PCA, t-SNE, et UMAP sur un ensemble de variables quantitatives

tsnePenguins = Rtsne(dpengs)
tsnePenguinsCoords = data.frame(tsnePenguins$Y)
names(tsnePenguinsCoords) = c("X1","X2")

ggplot(data = cbind(tsnePenguinsCoords,species = (penguins%>% na.omit())$species))+
  geom_point(aes(x= X1, y=X2, color=species))


umapPenguins = umap(dpengs)
umapPenguinsCoords = data.frame(umapPenguins$layout)

ggplot(data = cbind(umapPenguinsCoords,species = (penguins%>% na.omit())$species))+
  geom_point(aes(x= X1, y=X2, color=species))


pcaPenguinsCoords = data.frame(get_pca_ind(acp)$coord)
names(pcaPenguinsCoords) = c("X1","X2")

ggplot(data = cbind(pcaPenguinsCoords,species = (penguins%>% na.omit())$species))+
  geom_point(aes(x= X1, y=X2, color=species))

