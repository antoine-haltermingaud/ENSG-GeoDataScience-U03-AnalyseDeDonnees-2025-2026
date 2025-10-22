
##################################
# TP0 : Introduction au language R
#   (CORRECTION)
##################################




#########################################
#01 Hello World

# La fonction `print` affiche une chaîne de caractère ou une valeur numérique passé en argument 
print("Hello World!")

#La fonction `cat` permet d'afficher plusieurs éléments , séparés par des virgules
cat("Hello", "World", "!")

#`cat` accepte des éléments de type différent. Ici , une chaîne de caractère, un nombre, le résultat d'une fonction et un booléen  :
cat("abc", 12.3, sqrt(49), TRUE)




#########################################
#02 Déclarer une variable 

# Une variable se déclare avec l'opérateur `<-` (ou `=`, déconseillé car moins général en portée: limité à la scope d'une fonction si passé en argument)
a <-  42
a
a + 10
a = 42

test<-function(x){print(x)}
test(x<-42)
x
rm(x) # supprimer la variable
test(x=42)
x # erreur : variable définie par '=' dans le scope de la fonction uniquement


#########################################
#03 Types primitifs 

#Les types primitifs en R sont : `numeric` , `character` (string) , `logical` (booléen), et `factor`
n <-  42.2
ch <-  "text"
b <-  FALSE
f <- factor(c("pomme", "banane", "kiwi"))

# définir les niveaux d'un facteur indépendamment de ses valeurs
f <- factor(c("pomme", "banane", "kiwi","banane"), levels = c("pomme", "banane", "kiwi", "poire"))




#########################################
#04 Vecteur 

# La fonction `vector` permet d'initialiser un vecteur d'une certaine longueur et d'un certain type atomique (`numeric`, `logical` , ou `character`)

v <- vector(mode="numeric", length= 10 )
v

# Les opérations classiques appliquées à un vecteur sont «vectorisées», c'est-à-dire qu'elles sont appliquées terme à terme au vecteur
v + 15 


# On peut également utiliser la fonction `c()` pour créer des collections en combinant des objets 
co <-  c(10,20,30,40,50,60,70,80,90)
co


# Modifier un élément : connaissant son index,  la modification de la valeur d'un élément  se fait à l'aide de l'opérateur d'affectation: 
co[5] <- 42
co


# Ajouter une élément à la fin d'un vecteur avec la fonction `c()` qui combine une valeur avec un objet
c(co, pi)
c(pi, co) # ajouter au début

# Supprimer un élément : exclure l'élément avec un signe "moins" devant son index.
# pour supprimer effectivement le cinquième élément , il faut mettre à jour la variable vecteur avec une affectation 
co[-5] 
co <- co[-5]



# Retrouver un élément par sa valeur: fonction `match`, qui renvoie le premier index auquel on trouve la valeur dans le vecteur.
tortues_ninja <-  c("Leonardo", "Raphaël", "Donatello", "Michelangelo", "Donatello")
match("Donatello", tortues_ninja )


# Pour trouver **tous les emplacements** d'une valeur dans un vecteur, utiliser la fonction `which` appliquée à une expression booléenne.
tortues_ninja <-  c("Leonardo", "Raphaël", "Donatello", "Michelangelo", "Donatello")
tortues_ninja == "Donatello"
which(tortues_ninja == "Donatello")






#########################################
#05 Liste 

#On utilise la fonction `list()`  pour initialiser une liste : 
li <-  list(1,2,3) 
li



# La fonction `list()` accepte des éléments de type différents : 
li2 <- list("abc", 15==3*5, 12.3)
li2


# Une liste peut être nommée (équivalent à une HashMap), c'est à dire stocker un nom pour chaque élément qu'elle contient, avec la fonction `names()` qui, suivant son utilisation, renvoie ou affecte des noms aux éléments d'une liste 
# affection des noms 
names(li2) <- c("premier élement", "deuxième élément", "troisième élément")
#affichage standard de la liste en console
li2
#affichage des  noms
names(li2)



# Modifier un élément : pour modifier un élément, à un index donné, on utilise les doubles crochets et l'affectation :
li2[[2]] <- "nouvelle valeur"
li2



# Ajouter des éléments : on peut ajouter un élément à la fin d'une liste avec la fonction `c()` qui combine une valeur avec un objet et renvoie un élément de même type  que l'objet (liste ou vecteur).
c(lili,4)

# Supprimer un élément : affecter la valeur `NULL` à un élément de la liste a pour effet de le supprimer. Supprimer un élément d'une liste a pour effet de réduire la taille de cette liste d'une unité et de "recoller les morceaux , comme le montre la séquence suivante : 
li <- list(1,2,3,4,5)
li[[3]] <-  NULL
li


# Retrouver un élément dans une list : les fonctions `match` et `which` fonctionnent également sur les listes (cf. section 3.4).
turtles_color <-  list(Leonardo="blue", Raphael="red", Donatello="purple", Michelangelo="orange")
turtles_color
match("orange", turtles_color)
which(turtles_color == "red") 



#########################################
#06 Exercice 1   

# 1. Créer une liste de 3 éléments 
# 2. Changer la valeur du deuxième élément de votre objet
# 3. Affecter à la liste  les trois noms suivants : "A" , "B" et "C"
# 4. Ajouter un élément à votre liste 
# 5. Nommer cet élément ajouté
# 6. Supprimer le troisième élément de cette liste
# 7. Afficher la liste finale et sa longueur






#########################################
#07 Matrices

# les matrices sont des tableaux à deux dimensions pour lesquels sont définies plusieurs fonctions de calcul matriciel.
# une matrice se définit par exemple avec la fonction `matrix()`, en renseignant le contenu de la matrice (argument `data`),
# et la dimension de celle-ci (arguments `nrow` et `ncol` qui donnennt respectivment le nombre de lignes et de colonnes).
m <-  matrix(data = 1:9, nrow = 3, ncol=3)
m


# Éléments, vecteurs lignes et vecteurs colonnes

# On extrait les vecteurs d'une matrice à l'aide de l'opérateur d'indexation , en spécifiant les indices des lignes ou colonnes , entre crochets `[]`, séparés par une virgule.
m[2,3]


# Pour accéder à toute une ligne ou toute une colonne , on laisse vide l'index de l'autre dimension 
m[1,] # première ligne
m[,3] #troisième colonne



# Opérations matricielles

#La transposée d'une matrice s'obtient avec la fonction `t()` :
t(m)

#Le produit matriciel est obtenu avec l'opérateur binaire  `%*%` : 
m2 <- matrix(data = seq(from=10, by=10, length.out=12  ), nrow = 3, ncol = 4)
m %*% m2


# Pour une matrice carrée , le déterminant est obtenu avec la fonction `det()`
# la décomposition en vecteurs propres avec la fonction `eigen()`
# la diagonale s'obtient avec la fonction `diag()`
det(m)
eigen(m)
diag(m)


# Opérations termes à termes 

#Les opérateurs arithmétiques ( `+,-,*,/,%`) classiques sont appliqués termes à terme aux élément de la matrice : 
m + 1 
m * 7


## Juxtaposer des matrices (ou des tableaux)

# Pour juxtaposer verticalement deux matrices de même nombre de colonnes, on utilise la fonction `rbind` , qui lie les lignes (row bind)
rbind(m, t(m2))



# Pour juxtaposer horizontalement deux matrices de même nombre de lignes, on utilise la fonction `cbind`, qui lie les colonnes (column bind)
cbind(m, m2)



#########################################
#08 Dataframes

# Le type d'objet le plus courant dans R est le dataframe :  un tableau de données dont les colonnes ont des noms et peuvent être de différents types. 
# Par convention, les variables qui décrivent la population des données sont en colonnes.


# R intègre un jeu de données appelé `iris` décrivant les caractéristiques de fleurs de différentes espèces d'iris
head(iris) # affichagee des premières lignes

# les noms des colonnes d'un dataframe sont obtenus avec la fonction `names()`
names(iris)

# dimensions et types des colonnes
str(iris)


# Accès aux valeurs : un dataframe est un tableau à deux dimensions, on peut donc accéder à la valeur d'un élément avec deux indices placés entre crochets et séparé par une virgule
iris[12, 4]

#On peut également utiliser le nom d'une colonne pour indiquer la colonne désirée :
iris[12, "Petal.Width"]



# Toutes les lignes ou toutes les colonnes
iris[, "Petal.Width"]
iris[12, ]


# Exclure des lignes ou des colonnes
iris[, -c(3,4)]
iris[ -(10:150), ]

# Dimensions
nrow(iris)
ncol(iris)
dim(iris)


# L'opérateur `$` : il est courant de vouloir utiliser les colonnes d'un dataframe : pour cela, on peut utiliser l'opérateur `$` sur les objets de type dataframe (autocomplétion des noms dans RStudio) :
iris$Species
iris$Petal.Length * 100 
mean(iris$Sepal.Width)


## Filtrage : lors de l'accès à des éléments d'un dataframe, on peut faire appliquer une condition booléenne pour ne sélectionner que les éléments qui remplissent cette condition. 
iris[iris$Species=="virginica","Sepal.Width"]
iris$Species=="virginica" # indexation booléenne

# Filtrage multi-conditions avec opérateurs booléens
iris[iris$Species != "versicolor" & iris$Petal.Width <= 1.5  & iris$Sepal.Length == 5.0, ]



# Filtrage avec `dplyr`
# si besoin, installer le package avec install.packages("dplyr")
library(dplyr)
# le package `dplyr` propose de nombreuses fonction très utiles , dont `filter()` et la transmission de données en mode "pipe shell" avec %>%
iris %>%  filter(Species =="versicolor") %>%  filter(Petal.Length>= 2) %>%  filter(Sepal.Width>3)



# Modifier une valeur : la modification de valeurs se fait par une affectation à l'endroit voulu du tableau
iriscopy <-  iris
iriscopy[1, "Sepal.Length"] <-  11111.1111
iriscopy[1,]


# Supprimer une valeur : pour supprimer une valeur il suffit de la remplacer par la valeur `NA` (signifiant "non attribué)
iriscopy[1, "Sepal.Width"] <-  NA
iriscopy[1,]




# Ajouter de nouvelles valeurs

# Ajout de colonnes : déclarer une colonne à l'aide de l'opérateur `$`
iriscopy$color <- NA
names(iriscopy)
iriscopy$color <- "purple"
iriscopy$color

# Ajout de ligne : ajouter la ligne à l'indice correpondant au nombre de lignes existantes + 1
# ou utiliser la fonction `rbind`
nrow(iriscopy)
iriscopy[151,] <- c(1,2,3,4,"versicolor", "purple")
iriscopy[151,]

my_line <- c(8,8,8,8,"virginica", "blue")
iriscopy <- rbind(iriscopy, my_line)
iriscopy[151:152, ]






#########################################
#09 Opérations sur des vecteurs ou listes

# Moyenne , médiane , écart-type
mean(iris$Petal.Length)
median(c(1,2,3,4,5,6))
sd(rnorm(500))


# Somme et Produit
sum(iris$Petal.Length)
prod(c(1,2,3,4,5,6))




#########################################
#10 Graphiques simples

# Histogramme : toute série de valeurs numériques peut être affichée en histogramme.
hist(iris$Sepal.Length)

# Nuage de points : pour tracer un nuage de points , il faut deux séries de valeurs de la même taille.
plot(iris$Sepal.Length, iris$Sepal.Width)






#########################################
#11 Exercice 2


# 0. Installer et charger le jeu de données du package `palmerpenguins`
install.packages("palmerpenguins")
library(palmerpenguins)

# -> Le jeu de données de ce package se nomme `penguins` 


# 1. Quelles sont les variables de ce jeu de données ? 
names(penguins)
str(penguins)

# 2. Quel est le poids moyens des pingouins ?
mean(penguins$body_mass_g)

# 3. Créer un dataframe qui ne contienne que les pingouins de l'espèce Gentoo 
gentoo <- penguins %>% filter(species == 'Gentoo')

# 4. Quelle est le poids moyen d'un pingouin Gentoo ?
gentoo %>% summarise(poids_moyen = mean(body_mass_g, na.rm=T)) # attention à filter les valeurs manquantes (NA) avec l'argument na.rm
mean(gentoo$body_mass_g, na.rm = T)

# 5. Sur quelle(s) île(s) trouve-t-on  les pingouins de l'espèce Adélie?
islands <- unique(penguins$island[penguins$species=="Adelie"]) # -> toutes les iles, plus intéressant avec l'espèce Chinstrap
islands <- unique(penguins$island[penguins$species=="Chinstrap"])


# 6. Y-a-t' il des autres pingouins que des pingouins Adélie sur les îles déterminées à la question 5 ? 
unique(penguins$species[penguins$island%in%islands]) # attention à regarder les valeurs du facteur et non pas les levels
# pour enlever le facteur:
unique(as.character(penguins$species[penguins$island%in%islands]))




#########################################
#12 Fonctions

#Pour définir une fonction , la syntaxe est la suivante  : 
my_func <- function(arg1, arg2){
  result = 42+arg1*arg2
  return(result)
}


#  Exemple de fonction : renvoyer le ou les indices des valeurs maximales d'une liste de nombres
index_of_max <- function(my_list){
  ind <- which(my_list==max(my_list))
  return(ind)
}
index_of_max(c(24,5,9,78,12,45,78,23,47,-75))




#########################################
#13 Exercice 3


# 1. Écrire une fonction qui prend en argument un vecteur de valeurs numériques et qui le centre et le réduit , c'est-à-dire qui soustrait à chaque valeur la moyenne du vecteur et qui divise par la valeur de l'écart-type . Vous pouvez tester votre fonction avec un vecteur initialisé par vos soins, un dataframe de votre choix ou le dataframe `iris`.


# 2. Afficher l'histogramme du vecteur centré et réduit.





#########################################
#14 Générer des données


# répéter un motif 
rep(c(1,5,10),10)

# discrétiser un intervalle en précisant le quantum 
seq(from= 25 , to = 250, by = 22 )

# discrétiser un intervalle en précisant la longueur 
seq(from= 25 , to = 250, length.out = 10 )

# échantillonner une loi uniforme entre deux valeurs
runif(25, min=  -15 , max = 8 )

# échantilloner une loi normale de moyenne et d'écart-type donné 
rnorm(25, m = 12 , sd = 2)




#########################################
#15 Entrées/sorties : fichier CSV

# D'autres fonctions et packages existent pour à peu près tous les formats de données

# Lecture de fichier : `data/pokemon.csv`
#  ! vérifier que vous êtes dans le bon répertoire de travail pour utiliser le chemin relatif avec getwd()
#  Le changer avec setwd(...) si ce n'est pas le cas
pokemon_df  <-  read.csv("data/pokemons.csv")

# Affichage
str(pokemon_df)


# Par défaut , les colonnes qui contiennent des chaînes de caractères sont traitées comme des facteurs (i.e. des variables modales, qui ne peuvent prendre qu'un nombre fini de valeurs).
# Pour changer ce comportement , il faut passer en argument à `read.csv()` l'argument `stringsAsFactors=FALSE`
str(read.csv("data/pokemons.csv",stringsAsFactors = F))                                                                                                 


# Type des colonnes d'un fichier CSV 
# Ici par exemple , le nom des pokemons est considéré comme un facteur à ... 800 modalités (pour 800 individus)  : ce qui est peu pertinent, chaque nom étant a priori unique et propre à chaque individu de la population de pokemons ; par ailleurs  il est peu probable de devoir faire des regrouppements par nom.
# Au contraire, les facteurs  `Type.1` et `Type.2` sont des facteurs à respectivement 18 et 19 modalités, ce qui peut s'entendre , vu le nom de la variable, et le nombre de modalités significativement plus faibles que le nombre d'individus.
#Nous pouvons changer le type de la variable `Name` en utilisant la fonction `as.character()` sur la colonne du dataframe pour la remplacer.
pokemon_df$Name <-  as.character(pokemon_df$Name)


#Autre problème : les valeurs de la variable `Legendary` sont reconnues comme des facteurs à deux modalités `"True"` et `"False"` alors que leurs valeurs sont clairement booléennes.
#La fonction `as.logical()` est capable de transformer automatiquement les valeurs `"T", "TRUE", "True", "true"` en la valeur `TRUE` du langage R (de meme pour les variations de la valeurs `False`)
pokemon_df$Legendary <-  as.logical(pokemon_df$Legendary)



# Écriture de fichier CSV
# L'écriture d'un dataframe dans le format CSV se fait avec la fonction  `write.csv()` , qui prend en argument un dataframe et un chemin d'accès vers un nouveau fichier : 
write.csv( iris, file = "test_writecsv.csv")






#####################
#16 Programation avec R

# Nous savons déjà comment définir une fonction , il nous reste à voir comment écrire les traitements conditionnels, et les boucles. 


## Bloc `if`
# Un bloc conditionnel s'écrit de la façon suivante : 

# if (condition) {
# code  éxécuté si condition vraie 
#}else{
# code si condition fausse
#}

#La condition doit renvoyer une valeur **booléenne**, TRUE ou FALSE.



### Variante `ifelse`


#Cette variante permet d'écrire une instruction conditionnelle sous une forme un peu plus compacte.
#ifelse(condition, valeur si vraie , valeur si fausse)


#Voilà un exemple avec les penguins

#Nous allons tester si la masse de chaque pingouin est inférieure ou supérieure à la moyenne de tous les pingouins  du dataset : 

masse_moyenne <- mean(penguins$body_mass_g, na.rm = T)
ifelse(penguins$body_mass_g < masse_moyenne, "plus léger" , "plus lourd")


masse_moyenne <- mean(penguins$body_mass_g, na.rm = T)
ifelse(penguins$body_mass_g < masse_moyenne, "plus léger" , "plus lourd") %>% head



## Boucle `for`


#La boucle for s'écrit de la façon suivante : 

#for (variable in collection) {
#  #code éxécuté pour chaque élément de la collection 
#}





### Sortie de boucle prématurée 

#pour interrompre une boucle for , on utilise le mot clé `break`

#for (x in c(1,2,3,4,5,6,7)) {
#  print(x)
#  if(x==4) {break}
#}



### Passer une itération
#Pour passer une itération sans interrompre la boucle , on utilise le mot clé `next`

#for (x in c(1,2,3,4,5,6,7)) {
#  if(x==3) {next}
#  print(x)
#}




## Boucle `while`
#La boucle `while` s'écrit de la façon suivante  :

#while (condition) {
#code 
#}



## Applications vectorielles
#les application vectorielles sont des variantes des traitements itératifs (boucle `for`),en R elles sont plus efficaces.
#Elles s'apparentent à de la programmation fonctionnelle.  Les fonctions vectorielles de bases appliquent une fonction à tous les termes d'une collection ou d'une matrice. 
#Les fonctions de base se nomment  `sapply`, `lapply`, `vapply` pour traiter des vecteurs, et `apply`,`mapply` pour traiter des tableaux et des matrices.


my_vec <-  seq(from=10, to=100, by=5)

my_func <- function(x){return(x+2)}
sapply(my_vec, my_func)
lapply(my_vec, my_func)




################
#17 Exercice 4


#Implémentez en R le [crible d'ératosthène](https://fr.wikipedia.org/wiki/Crible_d%27%C3%89ratosth%C3%A8ne)

#Fonction Eratosthène(Limite)
#L = tableau de booléen de taille Limite, initialisé à Vrai
#Mettre à Faux les cases d'indice pair > 2
#    L[1] = Faux
#    i=3
#    Tant que i*i≤Limite
#        Si L[i]
#            Pour j de i*i à Limite par pas de 2*i
#                L[j] = Faux
#            Fin pour
#        Fin si
#        i=i+1
#    Fin tant que
#    Retourner L
#Fin fonction







