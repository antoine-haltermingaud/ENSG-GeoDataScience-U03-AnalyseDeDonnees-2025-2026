###############################
# TP 7 : Statistiques spatiales
###############################

#########
#  1) Préparation des données


library(readr)
library(sf)
library(dplyr)
library(mapsf)

rawdvf2021 <- read_csv(file = 'https://files.data.gouv.fr/geo-dvf/latest/csv/2021/full.csv.gz')

# enlever super-outlier (si travail sur 2024)
toremove=unique(rawdvf2021[is.na(rawdvf2021$valeur_fonciere)|
                             rawdvf2021$valeur_fonciere>200000000,c("id_mutation")])
# length(which(rawdvf2021$valeur_fonciere>200000000))
#rawdvf2021$valeur_fonciere[rawdvf2021$valeur_fonciere>200000000]


# agrégation au niveau départemental (via code_departement)
depdvf = rawdvf2021 %>%
  filter(type_local%in%c('Appartement','Maison')) %>%
  filter(!id_mutation%in%unlist(toremove)) %>%
  group_by(code_departement) %>% 
  summarise(
    prix = median(valeur_fonciere, na.rm=T),
    surface_bati = median(surface_reelle_bati, na.rm=T),
    surface_terrain = median(surface_terrain, na.rm=T)
  )
summary(depdvf$prix)

# autres données
deps = read_sf(dsn='data/departements/',layer='DEPARTEMENT')

popdeps = read_delim('data/insee/Departements.csv', delim=";")

# Insee: Filosofi 2020
insee_filosofi <- read_delim(file = 'data/filosofi/cc_filosofi_2020_DEP.csv', delim = ";")

# jointure
deps = left_join(deps,popdeps[,c("CODDEP","PTOT")],
                 by=c("CODE_DEPT"="CODDEP"))
deps = left_join(deps,insee_filosofi[,c("CODGEO","MED20","PPEN20","PPAT20")],
                 by=c("CODE_DEPT"="CODGEO"))
deps = left_join(deps,depdvf,by=c("CODE_DEPT"="code_departement"))
deps = na.exclude(deps)

d = st_drop_geometry(deps)

# exploration des données: cartes
mf_map(deps,var="prix",type="choro",nbreaks = 15)

# exploration des données: PCA / corrélations
cors = cor(d[,c("PTOT","MED20","PPEN20","PPAT20","prix","surface_bati","surface_terrain")])
library(corrplot)
corrplot(cors)

# modele lineaire non spatialise
simplemodel = lm(data=d, prix ~ PTOT+surface_bati)
linearmodel = lm(data=d, prix ~ PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain)
summary(simplemodel)
summary(linearmodel)
AIC(simplemodel) - AIC(linearmodel) # -> modele complet


# cartographier résidus du modèle linéaire
deps$residus = residuals(linearmodel)
mf_map(deps,var="residus",type="choro")



# tests autocorrelation spatiale
library(spdep)
depsnb = poly2nb(deps)
w = nb2listw(depsnb)

lm.morantest(linearmodel, w, alternative="two.sided")

#lm.LMtests(linearmodel, w, test=c("LMerr","LMlag"))




#########
#  2 ) Geographically weighted regression
#


# 2.1) Tester des modèles GWR à bandwidth fixe
# package GWmodel

library(GWmodel)



# GWR simple
gwbasic <- gwr.basic(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain,
                     data=deps,
                     bw=10,
                     #kernel="bisquare", # noyau par default
                     adaptive=T # a mettre a F pour un voisinage de taille fixe (bw, en unités de mesure des geometry)
)
print(gwbasic)


# cartographier coefficients
coefs = gwbasic$SDF

deps$localR2=coefs$Local_R2
deps$residuals = coefs$residual
deps$alpha_population = coefs$PTOT
deps$alpha_surface = coefs$surface_bati

mf_map(deps,var='localR2',type='choro')
mf_map(deps,var='residuals',type='choro')
mf_map(deps,var='alpha_population',type='choro')
mf_map(deps,var='alpha_surface',type='choro')


# 2.2) Optimiser la bandwidth selon un critère d'AIC
# bandwidth adaptative en nombre de voisins



# 2.3) Selection de modèle (méthode de "forward selection")






#####
## 3 ) Auto-regression spatiales

library(spatialreg)

# modèle de Durbin spatial


# modèle avec erreur spatiale



# changer la matrice de poids, les spécifications des modèles



#####
##  4 ) Regressions multi-niveaux


library(lme4)

# modèle simple avec intercepts variables


# modèle simple avec coefficients variables


# comparer les modèles et en tester d'autres


# comparaison à GWR et interprétation

