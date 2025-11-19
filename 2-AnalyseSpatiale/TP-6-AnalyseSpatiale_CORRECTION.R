
#########################
# TP 6 : Analyse spatiale
#########################

#########
# 1 ) Analyse d'un semis de points : statistiques de synthèse
# Aires urbaines françaises (50 plus grandes) - evolution du centre de gravité - population (1831 - 2000)

#' Données: coordonnées et nom des villes (data/FRUrbanAreas/coords.csv), 
#' populations dans le temps (data/FRUrbanAreas/pop50.csv),
#' dates (data/FRUrbanAreas/dates.csv)
#'  * pas de header
#'  * utiliser de préférence dplyr et readr (tidyverse) 

library(dplyr)
library(readr)

# 1.1) charger les données
# - Question : système de coordonnées?

coords <- read_csv("data/FRUrbanAreas/coords.csv",col_names = F)
colnames(coords)<-c("name","x","y")
# -> LambertII hectometrique

coords[,c("x","y")]=coords[,c("x","y")]/10 # passage en km

populations <- read_csv("data/FRUrbanAreas/pop50.csv",col_names = F)
dates <- read_csv("data/FRUrbanAreas/dates.csv", col_names = F)

colnames(populations)<- as.character(dates$X1)

# -  coordonnees cohérentes ?
plot(coords$x,coords$y)

#  - population totale par annee?
totpop = colSums(populations)
plot(dates$X1,totpop,type='l')



# 1.2) calculer point moyen, point moyen pondéré, distance-type pour chaque date

#  - point moyen
mean(coords$x)
mean(coords$y)
apply(coords[,c("x","y")],2,mean)

#  - point moyen pondéré pour 1831
apply(coords[,c("x","y")],2,function(x){sum(x*populations$`1831`)/sum(populations$`1831`)})


#  - boucle pour chaque année
point_moyen_pondere <- function(w){
  apply(coords[,c("x","y")],2,function(x){sum(x*w)/sum(w)})
}

for(year in colnames(populations)){
  show(point_moyen_pondere(populations[,year]))
}

#  -  meme chose avec apply
point_moyen = t(apply(populations,2,point_moyen_pondere))


# 1.3) cartographier les villes et l'évolution du point moyen
#  Question : quel package pour cartographier "simplement" ? -> ggplot (function geom_sf)
#  Données supplémentaires: limites des régions: data/regions/regions_2015_metropole_region.shp
#   ! systèmes de coordonnées à gérer

library(sf)
library(ggplot2)

regions <- st_read(dsn='data/regions/',layer='regions_2015_metropole_region')


#  - voir les drivers disponibles (formats de fichiers)
st_drivers()
st_drivers()$name

# - plot point moyen uniquement
ggplot(point_moyen)+geom_point(aes(x=x,y=y))

#  - "carte" avec point moyen et regions
summary(st_coordinates(regions))

ggplot(point_moyen)+geom_sf(data=regions)+geom_point(aes(x=x*1000,y=y*1000))

# -  meme carte "zoomée"
zoom = regions[regions$RégION%in%c("Bourgogne et Franche-Comté","Centre"),]
ggplot(point_moyen)+geom_sf(data=zoom)+geom_point(aes(x=x*1000,y=y*1000,color=dates$X1))


#  - distance type ponderée (en 1831)
xm = sum(coords$x*populations$`1831`)/sum(populations$`1831`)
ym = sum(coords$y*populations$`1831`)/sum(populations$`1831`)
sigma_pondere <- sqrt(sum(((coords$x-xm)^2+(coords$y-ym)^2)*populations$`1831`)/sum(populations$`1831`))


# 1.4) faire de même avec le point médian et point médian pondéré
#  package pour calculer le point median en 2d:

library(ICSNP)

#  - point median
ICSNP::spatial.median(coords[,c("x","y")])

#  (difficile) - point median "pondéré" ?
#  -> utiliser une technique type bootstrap en générant des points synthétiques
#     en quantité proportionelle aux populations

k=1000

points = data.frame()
for(i in 1:nrow(populations)){
  synth_points =
    matrix(data=
      jitter(
          unlist(rep(coords[i,c("x","y")],k*populations[i,"1831"]/sum(populations[,"1831"]))),
        factor = 0.1),
      ncol=2,byrow = T)
  points = rbind(points,synth_points)
}

ICSNP::spatial.median(points)




#######
#  2 ) Analyse d'un semis de points: 


# 2.1) Charger les données d'OpenStreetMap:
#  * données gpkg
#  * fichiers disponibles: data/osmdata/
#   c("Architecte.gpkg","Courtier_immobilier.gpkg","Hôtels.gpkg",
#    "Auberge.gpkg","École_élémentaire.gpkg","Lycée.gpkg",
#    "Cabinet_avocats.gpkg","École_maternelle.gpkg","Motel.gpkg",
#    "Chambredhôte.gpkg","Ecole_primaire.gpkg","Notaires.gpkg",
#    "Collège.gpkg","Enseignement_Supérieur.gpkg","Salon_de_coiffure.gpkg",
#     "Comptable.gpkg","Géomètre.gpkg")
#   -> a regrouper par type d'activité: éducation, prof. libérales, logements, coiffeurs
#   (choisir une activité ou ne charger qu'un seul fichier pour l'instant)

library(sf)
coiffeurs = st_read("data/osmdata/Salon_de_coiffure.gpkg")
facs = st_read("data/osmdata/Enseignement_Supérieur.gpkg")

st_read(dsn = 'data/regions/',layer = 'regions_2015_metropole_region')

# - systeme de coordonnees?
st_crs(coiffeurs)
st_crs(regions)

#  - reprojection vers "EPSG:2154"
coiffeurs <- st_transform(coiffeurs,"EPSG:2154")
regions <- st_transform(regions,"EPSG:2154")
facs <- st_transform(facs,"EPSG:2154")

# filtrage des facs en metropole
facs = st_filter(facs,regions)


# 2.2) Calculer l'indice de plus proche voisin dans le cas d'un faible nombre de points
#      (universités par exemple)
dmat = st_distance(facs)
diag(dmat)<-NA
mean(apply(dmat,1,function(row){min(row,na.rm = T)}))


# 2.3) Cartographier la densité des points
g=ggplot(regions)
g+geom_sf()+geom_density2d_filled(
  data=data.frame(st_coordinates(facs)),
  mapping=aes(x=X,y=Y),alpha=0.5
)


# 2.4) Charger le recensement 2017 au niveau départemental
#      (niveau d'agrégation pour l'analyse statistique)
#   * fichier csv population data/insee/Departements.csv
#   * fichier shapefile data/departements/DEPARTEMENT.shp
#  puis agréger les aménités au niveau départemental

library(readr)
library(dplyr)
library(ggplot2)

popdeps = read_delim('data/insee/Departements.csv', delim=";")
deps = read_sf(dsn='data/departements/',layer='DEPARTEMENT')

# - jointure
deps = left_join(deps,popdeps,by = c("CODE_DEPT"="CODDEP"))

# - comptage des amenités dans chaque dep
joinfacs = st_join(facs, deps)
aggrfacs = joinfacs %>% group_by(CODE_DEPT) %>%
  summarise(numfacs = n(), population = PTOT[1])

joincoiffeurs = st_join(coiffeurs, deps)
aggrcoiffeurs = joincoiffeurs %>% group_by(CODE_DEPT) %>%
  summarise(numcoiffeurs = n(), population = PTOT[1])



# 2.5) Corréler les effectifs à la population
cor.test(aggrfacs$numfacs,aggrfacs$population)


# -  joindre les resultats au sf departements
deps = left_join(deps,st_drop_geometry(aggrfacs))
deps = left_join(deps,st_drop_geometry(aggrcoiffeurs))


# 2.6) Calculer des indices de concentration

#  - Construction des comptages pour l'ensemble des activites par departement
#  avec une boucle sur les noms de fichier, repeter l'operation precedente d'aggregation et jointure
activityfiles = c(archi="Architecte.gpkg",immo="Courtier_immobilier.gpkg",hotel="Hôtels.gpkg",
                  auberge="Auberge.gpkg",ecole="École_élémentaire.gpkg",lycee="Lycée.gpkg",avocats="Cabinet_avocats.gpkg",
                  maternelle="École_maternelle.gpkg",motel="Motel.gpkg",chambrehote="Chambredhôte.gpkg",
                  primaire="Ecole_primaire.gpkg",notaires="Notaires.gpkg",
                  college = "Collège.gpkg",enssup="Enseignement_Supérieur.gpkg",coiffeur="Salon_de_coiffure.gpkg",
                  comptable= "Comptable.gpkg",geometre="Géomètre.gpkg")

educfiles = c(ecole="École_élémentaire.gpkg",lycee="Lycée.gpkg",
              maternelle="École_maternelle.gpkg",primaire="Ecole_primaire.gpkg",
              college = "Collège.gpkg",enssup="Enseignement_Supérieur.gpkg")

for(educ in names(educfiles)){
  show(educ)
  d <- st_transform(st_read(paste0("data/osmdata/",educfiles[educ])),"EPSG:2154")
  djoin = st_join(d, deps)
  daggr = djoin %>% group_by(CODE_DEPT) %>% summarise(count = n())
  names(daggr)[2]<-c(educ)
  deps = left_join(deps,st_drop_geometry(daggr))
}


#  - specialisation en ens sup parmi education
deps[is.na(deps)]=0
educ_counts = st_drop_geometry(deps[,names(educfiles)])
type_counts = matrix(rep(colSums(educ_counts),nrow(educ_counts)),byrow = T)
dep_counts = matrix(rep(rowSums(educ_counts),ncol(educ_counts)),byrow = F)
specialisations = educ_counts*sum(educ_counts) / (type_counts*dep_counts)
names(specialisations)<-paste0("spec_",names(specialisations))

deps <- cbind(deps,specialisations)

# - Optionnel : idem avec prof liberales
# ...

# - cartographie
library(mapsf)

# spec en ens sup parmi activités education
mf_map(deps,var = "spec_enssup",type="choro")

# nombre de coiffeurs
mf_map(deps,var = "numcoiffeurs",type="choro")



# 2.7) Calculer l'autocorrélation spatiale

# - Optionnel : à la main (produits de matrices)


#  - Moran avec le package spdep, fonction moran.test
library(spdep)

depsnb = spdep::poly2nb(deps)
w = spdep::nb2listw(depsnb)

spdep::moran.test(deps$numfacs, listw = w)

spdep::moran.test(deps$numcoiffeurs, listw = w)

# - indice de geary
spdep::geary.test(deps$numcoiffeur,w)


# - indice de Moran local (LISA)
localmoran_coiff = spdep::localmoran(deps$numcoiffeurs,w)

deps$localmoran_coiff = localmoran_coiff[,c("Ii")]
mf_map(deps, var="localmoran_coiff", type="choro")

deps$categ = attr(localmoran_coiff,"quadr")$mean
mf_map(deps,var="categ",type= "typo",pal=c("blue","pink","lightblue","red"))


