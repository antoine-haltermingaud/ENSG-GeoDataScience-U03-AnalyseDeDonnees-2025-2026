
##################################
# TP2 : Statistiques bivariées
##################################


# Nous allons travailler sur des données de résultats d'une compétition en 2020 d'e-sport sur le  jeu  "Leagues of Legend",
# qui oppose deux équipes de 5 joueurs.
# Source : https://oracleselixir.com/tools/downloads


# Récupération des données

# le package readr est compatible avec dplyr et permet de lire directement depuis une url
library(readr)
library(dplyr)
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
g+geom_bar(mapping = aes(x=champion))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))


# 4. Calculer la valeur moyenne de la colonne `totalgold`
mean(d$totalgold)


# 5. Comparer les moyennes de `totalgold` selon le résultat de la partie (colonne result)
d %>% group_by(result) %>% summarise(meangold = mean(totalgold))


# 6. Calculer puis ajouter au dataframe l'or par minute (en utilisant `totalgold` et `gamelength`)
d <- d %>% mutate(gold_per_min = totalgold/(gamelength/60))

# 7. Faire la même chose avec les kills par minute
d <- d %>% mutate(kill_per_min = kills/(gamelength/60))

# 8. Tracer un nuage de points avec or par minute vs kills par minute, en colorant par victoire ou défaite.
plot(x = d$gold_per_min, y = d$kill_per_min, col = ifelse(d$result==1,"green","red"), pch=10)

g=ggplot(d)
g+geom_point(aes(x=gold_per_min,y=kill_per_min, color = position))
# -> ! les lignes de TEAM aggrégées ne sont pas comparables à celles des joueurs


# -> filtrage sur les joueurs uniquement

d <- d %>% filter(position!="team")


g=ggplot(d)
g+geom_point(aes(x=gold_per_min,y=kill_per_min, color = position))





#######################
#2 Corrélations



# Corrélation de Pearson
cor(d$gamelength, d$totalgold)

# Avec test statistique de significativité
cor.test(d$gamelength, d$totalgold)


# selection des colonnes numériques
dnum <-  select(d, where(is.numeric))

# supression des variables numériques mais ne capturant pas un processus dans la partie
dnum <-  select(dnum,-c(year, playoffs,patch, participantid, game, result))


dnum %>% na.omit() # ! aucune observation -> réduire les variables

# on utilise une regex pour enlever les variables observées dans le temps
dnum <- dnum %>% select(!matches("\\d"))

# supprimer colonnes avec plus de 50% de NAs
dnum <- dnum[,-which(apply(dnum,2,function(x){length(which(is.na(x)))/length(x)})>0.5)]

# supprimer les NAs
dnum <- dnum %>% na.omit()



# matrice complete
cor(dnum)

# visualisation de la matrice des corrélations
library(corrplot)
corrplot(cor(dnum))


#######################
## Exercice

# 1. Variables les plus corrélées
#   -> Déterminer les 4 couples de variables numériques les plus corrélées au sens de Pearson
cormat <- cor(dnum)
P = nrow(cormat)
rawcorrs <- abs(cormat[upper.tri(x = cormat,diag=F)])
maxcorrs <- data.frame(rho=rawcorrs) %>% slice_max(rho,n=4)
flat_indices = which(rawcorrs%in% maxcorrs$rho)

vars = rownames(cormat)

varsimax = vars[matrix(rep(1:P),nrow = P,ncol=P)[upper.tri(x = cormat,diag=F)][flat_indices]]
varsjmax = vars[matrix(rep(1:P),nrow = P,ncol=P,byrow = T)[upper.tri(x = cormat,diag=F)][flat_indices]]

corrplot(cormat[unique(c(varsimax,varsjmax)),unique(c(varsimax,varsjmax))])

# 2. Variables les moins corrélées 
#    -> même traitement avec les corrélations les plus faibles 

mincorrs <- data.frame(rho=rawcorrs) %>% slice_min(rho,n=10)
flat_indices = which(rawcorrs%in% mincorrs$rho)

varsimin = vars[matrix(rep(1:P),nrow = P,ncol=P)[upper.tri(x = cormat,diag=F)][flat_indices]]
varsjmin = vars[matrix(rep(1:P),nrow = P,ncol=P,byrow = T)[upper.tri(x = cormat,diag=F)][flat_indices]]

corrplot(cormat[unique(c(varsimin,varsjmin)),unique(c(varsimin,varsjmin))])


# 3. Confirmer la forte/faible corrélation par un nuage de points pour quelques couples de variables identifiées ci-dessus
plot(dnum[,unique(c(varsimax,varsjmax))])



# 4. Faire une fonction qui génère  les nuages de points de tous les couples de variables dont la corrélation
#     est inférieure ou supérieure à un certain seuil (en valeur absolue)

draw_corrplot_threshold <- function(d){
  cormat <- cor(dnum)
  P = nrow(cormat)
  rawcorrs <- abs(cormat[upper.tri(x = cormat,diag=F)])
  maxcorrs <- data.frame(rho=rawcorrs) %>% slice_max(rho,n=4)
  flat_indices = which(rawcorrs%in% maxcorrs$rho)
  
  vars = rownames(cormat)
  
  varsimax = vars[matrix(rep(1:P),nrow = P,ncol=P)[upper.tri(x = cormat,diag=F)][flat_indices]]
  varsjmax = vars[matrix(rep(1:P),nrow = P,ncol=P,byrow = T)[upper.tri(x = cormat,diag=F)][flat_indices]]
  
  # TODO
  
}




# 5. Détecter un couple de variable pour lesquelles la corrélation de Pearson
#     est inférieure à la corrélation de Spearman; interpréter.

corrspearman = cor(dnum,method="spearman")

diffcorr = abs(corrspearman) - abs(cormat)
ind = which(diffcorr==max(diffcorr))[1]
rownames(cormat)[floor(ind/P)] # TODO fix
rownames(cormat)[ind - P*floor(ind/P)] 
diffcorr["totalgold","monsterkills"]

plot(dnum$totalgold,dnum$monsterkills) # -> du à la bimodalité, à prendre en compte



####################
#3 Régression linéaire 



############################
## Exercice

# 1. Choisir un couple de variables fortement corrélées et afficher son nuage de points

plot(dnum$monsterkills,dnum$monsterkillsownjungle)


# 2. Réaliser la régression linéaire entre les deux variables ci-dessus; interpréter les résultats 
model = lm(dnum$monsterkills ~ dnum$monsterkillsownjungle)

model = lm(data = dnum, monsterkills ~ monsterkillsownjungle)

summary(model)

# 3. Pour les variables `monsterkills` et `totalgold`, tracer le nuage de points, puis proposer une méthode
#   pour décomposer les observations en 2 groupes

plot(dnum$monsterkills,dnum$totalgold)
# TODO clustering pour splitter



# 4. Refaire une regression linéaire pour chaque groupe, interpréter



#################################
#4 Test d indépendance du chi2


#########################
# Exercice

ddiscr <- d %>% select(!matches("\\d"))
ddiscr <-  select(ddiscr,-c(year, playoffs,patch, participantid))
ddiscr <- select(ddiscr,c(game,side,champion,playername,firstblood,firstbloodvictim,result))


# 1. Chercher des variables qualitatives qui semblent avoir un lien avec la variable qualitative `result`
chisq.test(ddiscr$side,ddiscr$result)
chisq.test(ddiscr$champion,ddiscr$result)
chisq.test(ddiscr$firstblood,ddiscr$result)
chisq.test(ddiscr$game,ddiscr$result)


# 2. Proposer d autres variables discrètes construites à partir de seuils sur des variables numériques et tester leur relation avec result
chisq.test(ifelse(d$totalgold>10000,"1","0"),ddiscr$result)


# 3. Quelle est la variable discrète la plus liée au résultat du match ?

res = data.frame()
for(v in names(ddiscr)){
  if(v != "result"){
    stat = chisq.test(ddiscr[,v],ddiscr$result)$statistic
    res=rbind(res,c(stat,v))
  }
}



