
###########################
# TP 3 : analyse multivariée
###########################

#######################
#1 Regression multiple


# 1.1) Préparation des données

# Charger à nouveau les données des matches de LOL,
#   filtrer les résultats d'équipe,
#   selectionner quelques variables quantitatives (dont result,gamelength)

library(readr)
library(dplyr)
d <- read_csv("http://nextcloud.iscpif.fr/index.php/s/eegwmt29kimWgdz/download") # url du fichier csv transféré sur une nextcloud (pour éviter l'authentification google drive, tout de même possible en R avec le package googledrive)

d <- d %>% filter(position=="team")
dnum <-  select(d, where(is.numeric))
dnum <-  select(dnum,-c(year, playoffs,patch, participantid, game))
dnum <- dnum %>% select(!matches("\\d"))
dnum <- dnum[,-which(apply(dnum,2,function(x){length(which(is.na(x)))/length(x)})>0.5)]
dnum <- dnum %>% na.omit()

names(dnum)[13] <- "team_kpm"
names(dnum)[49] <- "earned_gpm"

# 1.2) Modèles linéaires multivariés

# Utiliser des modèles linéaires pour expliquer dpm,result,gamelength (fonction lm)
summary(lm(data=dnum,dpm~totalgold+teamdeaths))
summary(lm(data=dnum,result~totalgold+teamdeaths))
summary(lm(data=dnum,gamelength~totalgold+teamdeaths+dragons))

# Comparer ces modèles en termes de R2, de AIC
m1 <- lm(data=dnum,gamelength~totalgold+teamdeaths)

formula2 <- paste0("gamelength~",paste(names(dnum)[-1],collapse = "+"))
m2 <- lm(data=dnum,formula = formula2)

summary(m1)$adj.r.squared
summary(m2)$adj.r.squared

AIC(m1)
AIC(m2)


# autres modèles pour dpm
m1 <- lm(data=dnum,dpm~totalgold+teamdeaths)
m2 <- lm(data=dnum,dpm~totalgold+teamdeaths+kills+minionkills+monsterkills+goldspent)

summary(m1)$adj.r.squared
summary(m2)$adj.r.squared
AIC(m1)
AIC(m2)



# Idem en séparant victoires et défaites
mwin <- lm(data=dnum[dnum$result==1,],dpm~totalgold+teamdeaths+kills+minionkills+monsterkills+goldspent)
mloose <- lm(data=dnum[dnum$result==0,],dpm~totalgold+teamdeaths+kills+minionkills+monsterkills+goldspent)

summary(mwin)
summary(mloose)


# Expliquer une variable composite qui vaut -gamelength en cas de défaite, gamelength en cas de victoire




# Trouver un modèle optimal en utilisant une "Stepwise Regression" (fonction step)
m2 <- lm(data=dnum,dpm~totalgold+teamdeaths+kills+minionkills+monsterkills+goldspent)
summary(step(m2))
# -> besoin que de 5 variables plutôt que 6




# 1.3) Régression logistique

# Expliquer la variable binaire "result" avec une régression logistique (fonction glm, en utilisant family = binomial(link = 'logit'))
#    - pour obtenir les effets marginaux du modèle logit, utiliser la fonction logitmfx du package mfx
#    - comparer le modèle logit avec un modèle linéaire, en utilisant le pseudo R2 de McFadden = 1 - logLik(logit)/logLik(model nul avec result~1)

logit <- glm(data=dnum,result~totalgold+teamdeaths+kills+minionkills+monsterkills+goldspent,
             family = binomial(link = 'logit'))
summary(logit)

# modele logit avec calcul des effets marginaux
library(mfx)
logitmfx(data=dnum,result~totalgold+teamdeaths+kills+minionkills+monsterkills+goldspent)

# McFadden pseudoR2
model_null = glm(formula = result~1 , data = d, family = binomial(link = 'logit'))
pseudoR2 = 1 - logLik(logit)/logLik(model_null)

m2 <- lm(data=dnum,result~totalgold+teamdeaths+kills+minionkills+monsterkills+goldspent)
summary(m2)$adj.r.squared



#######################
# 2) Séries temporelles


# Vérifier s'il existe une causalité de Granger entre les variables observées à étapes intermédiaires
#   (kills, golddiff, etc at 10, 15) et le résultat
granger_full_result = lm(data=d, result ~ goldat10 + goldat15 + killsat10 + killsat15)
summary(granger_full_result)

granger_full_logit_result = glm(data=d, result ~ goldat10 + goldat15 + killsat10 + killsat15,
                                family = binomial(link = 'logit'))
summary(granger_full_logit_result)











