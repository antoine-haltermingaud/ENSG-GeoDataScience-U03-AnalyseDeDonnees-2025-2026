
##################################
# TP1 : Statistiques univariées
##################################





##################
#01 Statistiques univariées

# Génération de données synthétiques
x <-  rnorm(10000, 10,5)
hist(x, breaks=50, main="Histogramme de x", xlab="valeurs de x")

# Rappel: statistiques descriptives
summary(x)
quantile(x,seq(0.1,0.9,0.1))
mean(x)
sd(x)

###
# Exercice 1 : Calculer l'écart interquartile
quantile(x, 0.75) - quantile(x, 0.25)
IQ(x)


###
# Exercice 2 : Écrire une fonction qui calcule le mode d'une variable continue,
# prenant en argument un vecteur de valeurs, et un nombre d'intervalles qui discrétise son étendue. 


mode <- function(x, breaks = nclass.Sturges(x)) {
  h <- hist(x, breaks = breaks, plot = FALSE)
  mode_value <- h$mids[which.max(h$counts)]
  return(mode_value)
}






########################################
#02 Manipulation de dataframe avec le package dplyr

library(dplyr)


## Rappel : Enchainer les traitements avec les pipes
#`dplyr` utilise le pipe noté ` %>% ` pour enchainer deux traitements sur le même objet. 

valeurs <-  rnorm(10000,mean = 20, sd=1) #10000 échantillons dans une loi normale
summary(valeurs)
                
# arrondi puis moyenne                                                                                                                    
valeurs %>% round() %>% mean()
                                                                                                                                    
# écritures alternatives "classiques"
mean(round(valeurs))

arrondis <-  round(valeurs)
mean(arrondis)


## Rappel : Filtrage

# Filtrage d'un dataset selon une variable 
                                                                                                                                    
                                                                                                                                    
# `starwars` est un dataset sur les personnages de la franchise de films Star Wars. 
# (chargé automatiquement avec dplyr)                                                                                                                         
head(starwars) 

                                                                                                                                    
# Filtrage suivant une valeur numérique : height
filter(starwars, height < 100)

# Même chose en utilisant un pipe
starwars %>% filter(height < 100)

# Avec la syntaxe classique (garde les NAs, tandis que filter de dplyr les retire)                                                                                                                                   
starwars[starwars$height < 100,]


# Filtrage d'un dataset sur plusieurs variables
starwars %>% filter(height >= 180, species=="Human")
                                                                                                                                    
                                                                                                                       
## Exercice 3 :
# Déterminer quel est le plus grand personnage d'espèce humaine (variable `species`)
# dont le monde de résidence (variable `homeworld`) est Tatooine.
# indice : utiliser la fonction `slice_max` de dplyr.

starwars %>% filter(species == "Human", homeworld == "Tatooine") %>% slice_max(height, n=10)





## Regroupement 

#Pour regrouper selon des variables, on utilise la fonction`group_by`. 
starwars %>% group_by(homeworld)


## Agregéation
# Il est très courant d'opérer des agrégations juste après un regroupement avec summarise

starwars %>% filter(gender=="masculine") %>% 
          group_by(homeworld) %>% 
          summarise(mean_size=mean(height, na.rm=T))

                                                                                                                               
## Regroupement et agrégation multiples




## Exercice 4 : quel traitement effectue la commande ci-dessous ?
starwars %>% filter (height >150) %>% 
group_by(homeworld,species,) %>% 
summarize(nombre = n()) %>%  na.omit() %>% 
 arrange(-nombre) 
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
## Création de nouvelles variables agrégées
#La création de nouvelles variables se fait à l'aide de la fonction `mutate` de `dplyr`. 

starwars %>%  
          group_by(homeworld,species,) %>% 
          mutate( nombre = n(),
                  classe_taille= ifelse(height>140, "grand", "petit")
                  ) %>%  
          select(homeworld,species,nombre,classe_taille)



                                                                                                                                    
## Exercice 5: Calculer la masse moyenne des personnages par planète

starwars %>%
        group_by(homeworld) %>%
        summarise(avg_mass = mean(mass, na.rm = T)) %>%
        select(homeworld, avg_mass)

 





## Sélectionner des colonnes

# Sélection par nom 
 starwars %>% select(name, homeworld)

 

#On peut également retirer des variables en les préfixant par un signe moins `-`
names(starwars)
starwars %>% select(-films, -starships, - birth_year,-sex,-gender,-vehicles)


# Selection par pattern
starwars %>% select(name, matches("color"))


# Sélection par type
starwars %>% select(where(is.numeric))
starwars %>% select(where(is.character))
                                                                        
              

                                                                        
## Fonctions pratiques diverses 

# "top or bottom n"  par variable
starwars %>% slice_min(order_by = height, n=5)

 
# Enlever des doublons avec "distinct()"
doublons <- starwars %>% select(homeworld, species) # creation de doublons
doublons
doublons %>%  distinct()
                                                                                      
                                                                                      
                                                                                      
