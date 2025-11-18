

###########################
# TP 5 : Bootstrap
###########################




#######################
#1 Echantillonage Gaussien


# Rappel : échantillonage aléatoire avec une loi normale
x <- rnorm(10000, mean = 0.3,sd = 0.05)
mean(x)



# Estimation de la moyenne d'échantillons de taille variable
samples=seq(from=10, to= 100000, by=100)
moyenne <-  c()
for (n in samples){
  echantillon <- rnorm(n,0,1)
  moyenne <-  c(moyenne, mean(echantillon))
}


# Faire le même calcul avec sapply
moyenne = sapply(samples,function(n){mean(rnorm(n))})



# Faire un graphique moyenne=f(n), interpréter
plot(samples,moyenne,type='l')
 # -> convergence de l'amplitude en sqrt(n) (TCL)



#######################
#2 Exercice : somme d'échantillons tirés dans une loi normale 


# Simuler la moyenne de la somme de deux échantillons normaux
moyenne_somme = sapply(samples,function(n){mean(rnorm(n)+rnorm(n))})
plot(samples,moyenne_somme,type='l')

sd(rnorm(10000)+rnorm(10000))
# -> toute combinaison linéaire de lois normales est une loi normale


# Même chose avec le quotient
moyenne_quotient = sapply(samples,function(n){mean(rnorm(n)/rnorm(n))})
moyenne_quotient_2 = sapply(samples,function(n){mean(rnorm(n))/mean(rnorm(n))})
plot(samples,moyenne_quotient,type='l')
plot(samples,moyenne_quotient_2,type='l')
# -> quantité qui n'existe pas, "estimateurs" convergent pas





#######################
#3 Bootstrap

library(dplyr)
library(readr)
library(ggplot2)

# On travaille sur les données des pokemons de type Feu
pokemons <- read_csv("../0-Introduction/data/pokemons.csv")
fire_pokemons <- pokemons %>% filter(`Type 1`=="Fire")


# Rappel : échantilloner les données
sample(pokemons$Name,size =  10)

# Tirage avec remise
sample(fire_pokemons$Name, size=20, replace = T)



# Bootstrap sur la moyenne :
#   - évaluer la moyenne de points de vie (HP) un nombre $B$ de fois,
#     sur un échantillon (avec remise) de même taille que fire_pokemons
#   - afficher l'histogramme des moyennes estimées
B = 10000

n = length(fire_pokemons)

bootstrapped_hp_means = sapply(1:B,function(b){mean(sample(fire_pokemons$HP,size=n,replace = T))})

hist(bootstrapped_hp_means)


# Calcul d'un intervalle de confiance 
#  ->  En considérant que la distribution des moyennes observée est gaussienne, calculer l'intervalle de confiance à 95%.
#   Rappel:  [mu - sigma ; mu+ sigma]$ est un CI à ~ 68% pour une Gaussienne
#            [mu - 2*sigma ; mu+ 2*sigma]$ est un CI à ~ 95%
#            [mu - 3*sigma ; mu+ 3*sigma]$ est un CI à ~ 99.7%




# Même calcul en utilisant la fonction t.test (argument conf.level)





# Calcul direct de l'intervalle de confiance avec les quantiles empiriques des statistiques bootstrapées
#   (valide pour des distributiosn symétriques)



# Calcul direct de l'intervalle de confiance avec la distribution cumulée (fonction ecdf) (valide dans tous les cas)
#   -> inverser "à la main" la CDF







############################
#4 Exercice : sensibilité aux paramètres du bootstrap



#4.1 Encapsuler la boucle de bootstrap précédente dans une fonction permettant de varier nombre et taille des samples



#4.2 Calculer pour un ensemble de paramètres les estimations d'un intervalle de confiance pour la moyenne des points de vie



#4.3 Faire des graphiques et interpréter



################################
#5 Autres bootstraps 


# Recommencer la procédure avec une autre variable ou une autre catégorie de pokemons



# Calculer un intervalle de confiance sur la corrélation entre attaque et défense en utilisant un bootstrap sur l'ensemble des pokemons




