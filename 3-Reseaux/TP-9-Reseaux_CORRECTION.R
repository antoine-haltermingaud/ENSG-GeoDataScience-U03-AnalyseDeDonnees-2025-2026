
##
# TP Reseaux

library(igraph)
library(ggplot2)

#####
## Partie 1
# graphes aleatoires


# 1.1) tester la generation de graphe aléatoire: igraph::erdos.renyi.game()
g = igraph::erdos.renyi.game(n=100,p.or.m = 0.2,type = "gnp")

# meme chose (fonction pas dépréciée)
g = sample_gnp(n=100,p=0.2)

# objet graphe
g

# sommets : vertices
V(g)

# liens : edges
E(g)

# calculer la densite
length(V(g))
length(E(g))
vcount(g)
ecount(g)

densite = ecount(g)/(vcount(g)*(vcount(g)-1)/2)
densite

# attributs des noeuds et liens
V(g)$name = as.character(1:vcount(g))


# definir des poids pour les liens
E(g)$weight = runif(ecount(g),min = 0.5,max=1)


# matrice d'adjacence : application aux chemins d'une longueur donnée
a = as_adjacency_matrix(g,sparse=T)
# nombre de chemins de longueur 2
a%*%a

# nombre de composantes connexes?
igraph::components(g)

# taille de la plus grande composante en fonction de p (étude de percolation)
n=100
probas = seq(0.01,1,by=0.01)
sizes = c()
for(p in probas){
  g = sample_gnp(n=n,p=p)
  compsizes = components(g)$csize
  sizes = append(sizes,max(compsizes)/n)
}
plot(probas,sizes,type='l')


# extraire le sous-graphe correspondant a la plus grosse composante
g = sample_gnp(n=100,p=0.03)
comps = components(g)
largest_comp_ind = which(comps$csize==max(comps$csize))
tokept = (comps$membership==largest_comp_ind)
largestcomp = subgraph(g,tokept)

# diametre du graphe (non pondere car pas d'attribut weight)
diameter(largestcomp)

# diametre pondere
diameter(largestcomp,weights = runif(n=ecount(largestcomp),min=0.5,max=1))

# diametre en fonction taille et proba du graphe aleatoire
logprobas = seq(-5,-1,by=0.5)
ns = seq(100,1000,by=100)
B=100
res = data.frame()
for(n in ns){
  show(n)
    for(p in 10^logprobas){
      for(b in 1:B){
        g = sample_gnp(n=n,p=p)
        comps = components(g)
        largest_comp_ind = which(comps$csize==max(comps$csize))
        tokept = (comps$membership==largest_comp_ind)
        largestcomp = subgraph(g,tokept)
        res = rbind(res,c(n=n,p=p,d=diameter(largestcomp),size=max(comps$csize)/n))
      }
    }
}
colnames(res)<-c("n","p","d","size")

ggplot(res)+geom_smooth(aes(x=p,y=d,group=n,color=n))+scale_x_log10()


# plotter le graphe
g = sample_gnp(n=100,p=0.03)
plot(g,vertex.label=NA,vertex.size=0)

# layouts: algorithme de spatialisation du graphe
# -> tester layout fruchterman reingold : layout_with_fr
coords = igraph::layout_with_fr(g)
V(g)$x = coords[,1]
V(g)$y = coords[,2]
plot(g,vertex.label=NA,vertex.size=0)


# 1.2) Generer et plotter un graphe en grille (lattice): igraph::make_lattice

#  (en theorie : nei = 1.5 pour connexions diagonales (future implémentation dans igraph))
g = igraph::make_lattice(length = 10,dim=2)
coords = layout_on_grid(g)
V(g)$x = coords[,1];V(g)$y = coords[,2]
plot(g,vertex.label=NA,vertex.size=0)

plot_grille<-function(g){
  coords = layout_on_grid(g)
  V(g)$x = coords[,1];V(g)$y = coords[,2]
  plot(g,vertex.label=NA,vertex.size=0)
}

# Supprimer des liens aléatoirement dans le graphe en grille
coords = layout_on_grid(g);V(g)$x = coords[,1];V(g)$y = coords[,2]
gsub = subgraph_from_edges(g,sample(E(g),0.5*ecount(g)))
plot(gsub,vertex.label=NA,vertex.size=0)

#  étudier la taille de la plus grande composante connexe
#  en fonction de la proportion de liens gardés et de la taille du graphe
probas = seq(0.1,0.9,by=0.1)
ns = seq(10,20,by=1)
B=100
res = data.frame()
for(n in ns){
  show(n)
  for(p in probas){
    for(b in 1:B){
      g = igraph::make_lattice(length = n,dim=2)
      coords = layout_on_grid(g)
      V(g)$x = coords[,1];V(g)$y = coords[,2]
      gsub = subgraph_from_edges(g,sample(E(g),p*ecount(g)))
      comps = components(gsub)
      largest_comp_ind = which(comps$csize==max(comps$csize))
      tokept = (comps$membership==largest_comp_ind)
      largestcomp = subgraph(gsub,tokept)
      res = rbind(res,c(n=n,p=p,d=diameter(largestcomp),size=max(comps$csize)/vcount(g)))
    }
  }
}
colnames(res)<-c("n","p","d","size")

ggplot(res)+geom_smooth(aes(x=p,y=size,group=n,color=n))



# 1.3) perturber les coordonnées des noeuds de la grille pour obtenir
#  des plus courts chemins uniques;
# étudier le diametre en fonction des liens supprimes
# algos: shortest_paths()/ distances() : algorithme adapte au cas (voir doc)

g = igraph::make_lattice(length = 100,dim=2)
coords = layout_on_grid(g)
V(g)$x = jitter(coords[,1]);V(g)$y = jitter(coords[,2])
plot(g,vertex.label=NA,vertex.size=0)

gsub = subgraph_from_edges(g,sample(E(g),0.95*ecount(g)))

# ajouter la distance euclidienne comme poids des liens
v_ends = igraph::ends(gsub,1:ecount(gsub))
lengths = apply(v_ends, 1, function(e){
  sqrt( (V(gsub)$x[e[1]] - V(gsub)$x[e[2]])^2 + 
          (V(gsub)$y[e[1]] - V(gsub)$y[e[2]])^2 )
})
E(gsub)$weight = lengths

# tous les plus courts chemins: distances
igraph::distances(gsub,v=V(gsub),to=V(gsub),weights = E(gsub)$weight)


# certains plus courts chemins: shortest_paths
path = shortest_paths(gsub,
                      from = sample.int(vcount(gsub),1),
                      to = sample.int(vcount(gsub),1)
)
plot(gsub, vertex.size=5, vertex.label=NA,
     vertex.color = ifelse(V(gsub)%in%path$vpath[[1]],'green','black')
)

# plus court chemin entre coins dans le reseau en grille (sur la plus grande composante)
comps = components(gsub)
index_of_largest_component = which(comps$csize==max(comps$csize))
vertices_in_largest = comps$membership==index_of_largest_component
subgraph_largest = induced_subgraph(gsub, vertices_in_largest)

first_col = V(subgraph_largest)[V(subgraph_largest)$x < min(V(subgraph_largest)$x + 1)]
from = first_col[first_col$y==max(first_col$y)]

last_col = V(subgraph_largest)[V(subgraph_largest)$x > max(V(subgraph_largest)$x - 1)]
to = last_col[last_col$y==min(last_col$y)]

path = shortest_paths(subgraph_largest,from = from,to = to)$vpath[[1]]

plot(subgraph_largest,vertex.size=5,vertex.label=NA,
     vertex.color = ifelse(V(subgraph_largest)%in%path,'green','black')
)



#####
## Partie 2
# Analyse de reseau social
# Data : co-occurence des personnages de A Song of Ice and Fire
#  https://github.com/mathbeveridge/asoiaf

library(readr)
library(igraph)

# 2.1) charger les donnees
# Data available under a CC-BY-NC-SA Licence at https://github.com/mathbeveridge/asoiaf
nodes <- read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-nodes.csv")
edges <- read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-edges.csv")


# construire le graphe: graph_from_data_frame
g = graph_from_data_frame(d = edges,directed = F,vertices = nodes)

# 2.2) ploter le graph avec un layout adapte
coords = layout_with_fr(g)
V(g)$x=coords[,1];V(g)$y=coords[,2]
plot(g,vertex.label=NA,vertex.size=0)

# pour bien visualiser: gephi, par exemple apres export en gml
# https://gephi.org/
# 
#igraph::write_graph(g,file="",format="gml")

# Alternatives:
# package ggnetwork ~ compatible avec igraph
#  https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html



# 2.3) distribution des degres

deg = degree(g)
deg_pondere = strength(g)

# histogramme
hist(deg,breaks=25)
hist(deg_pondere,breaks=25)

# loi rang-taille : log(degre) ~ log(rang)
plot(sort(log(deg_pondere),decreasing = T),log(1:vcount(g)))

# ajuster des power law avec plus de parametres, ou des distributions log-normale
# package poweRlaw
library(poweRlaw)
wdeg_estimator = poweRlaw::conpl$new(deg_pondere)
est = poweRlaw::estimate_xmin(wdeg_estimator,xmax = max(deg_pondere))
wdeg_estimator$setXmin(est)

wdeg_estimator_lnorm = poweRlaw::conlnorm$new(deg_pondere)
est_lnorm = poweRlaw::estimate_xmin(wdeg_estimator_lnorm,xmax = max(deg_pondere))
wdeg_estimator_lnorm$setXmin(est_lnorm)

plot(wdeg_estimator);lines(wdeg_estimator, col=2, lwd=2);lines(wdeg_estimator_lnorm, col=3, lwd=2)



# 2.4) centralites : closeness, betwenness, eigenvalue
cl = closeness(g,weights = 1/E(g)$weight)
bw = betweenness(g,weights = 1/E(g)$weight)
eig = eigen_centrality(g,weights = 1/E(g)$weight)$vector

V(g)[which(deg_pondere==max(deg_pondere))]
V(g)[which(cl==max(cl))]
V(g)[which(bw==max(bw))]
V(g)[which(eig==max(eig))]

V(g)[which(bw>quantile(bw,0.99))]

# role de la centralité pour expliquer le nombre d'occurences?
cor(cl,deg_pondere)
cor(bw,deg_pondere)
cor(eig,deg_pondere)


# 2.5) detection de communautes : cluster_... -> methode de Louvain cluster_louvain
coms = cluster_louvain(g)
coms

coms = cluster_fast_greedy(g)
coms

# 2.6) plotter avec multiples infos: communaute, centralite, degre
# (export dans le fichier "./graph.png")
coords <- layout_nicely(g)
V(g)$x = coords[,1];V(g)$y = coords[,2]

png('graph.png',width=20,height=20, units='cm',res=300)
plot(
  g,
  vertex.size = 3+log(strength(g))/2,
  vertex.frame.color = NA,
  vertex.color = coms$membership,
  vertex.label.cex = log(1000*cl)/5 #eig/2
)
dev.off()




#########
## Partie 3 : OSM et reseaux de transports

library(osmdata)
library(sf)
library(ggplot2)


# routes principales pour Paris
bb <- osmdata::getbb('paris fr', format_out = 'polygon')
#roads <- osmdata::opq(bbox = bb, timeout = 200) %>%
#  add_osm_features(list('highway'='primary','highway'='secondary','highway'='tertiary')) %>%
#  osmdata_sf()
roads <- osmdata::opq(bbox = bb, timeout = 200) %>%
    add_osm_features(list('highway'='primary')) %>%
    osmdata_sf()

# visualiser
ggplot()+geom_sf(data=roads$osm_lines)

# exporter en shapefile
#st_write(roads$osm_lines,dsn = 'roads.gpkg',layer='roads')


# restaurants pour Paris
restaurants <- osmdata::opq(bbox = bb, timeout = 200) %>%
  add_osm_feature(key='amenity',value='restaurant') %>% osmdata_sf()
ggplot()+geom_sf(data=restaurants$osm_points)+geom_sf(data=restaurants$osm_polygons,color='red')


# exporter les routes en sp (pour utilisation avec des packages non compatibles avec sf)
# DEPRACTED
#  %>% osmdata_sp()


# transformer les donnees brutes en graphe igraph pour calculer des temps de parcours
#  -> fonctions disponible ici : https://github.com/JusteRaimbault/TransportationNetwork (pas encore déployé en package)
source('https://raw.githubusercontent.com/JusteRaimbault/TransportationNetwork/master/NetworkAnalysis/network.R')

# -> les fonctions addTransportationLayer(), addPoints(), addPointsLayer(), addAdministrativeLayer()
#   permettent de construire itérativement un graphe multimodal

# reprojection
r <- roads$osm_lines %>% st_transform(crs="EPSG:2154")

# Ajouter une seule couche de transport pour construire un réseau routier
#  (pour le snapping = aggregation des noeuds, ici les donnees ne sont pas projetees, on aggrege a 100m ~ 0.001)
#g <- addTransportationLayer(link_layer = r, snap = 0.05)
g <- addTransportationLayer(link_layer = roads$osm_lines, snap = 0.001)

plot(g, vertex.size=0, vertex.label=NA)


# plot d'un plus court chemin aleatoire (vitesse constante = 1 -> a adapter a une vitesse reelle)
path = shortest_paths(g,
                      from = sample.int(vcount(g),1),
                      to = sample.int(vcount(g),1),
                      weights = E(g)$length
)
plot(g, vertex.size=2, vertex.label=NA,vertex.frame.color = NA,
     vertex.color = ifelse(V(g)%in%path$vpath[[1]],'red', 'black')
)



# Tester avec le réseau de métro/train (utilisation de l'argument "station")
metro_data <- opq(bbox = bb, timeout = 200) %>%
  add_osm_feature(key='route',value='subway') %>% osmdata_sf()
stations_data <-opq(bbox = st_bbox(metro_data$osm_lines), timeout = 200) %>%
  add_osm_feature(key='station',value='subway') %>% osmdata_sf()

g_metro <- addTransportationLayer(link_layer = metro_data$osm_lines,
                                  stations_layer = stations_data$osm_points,
                                  snap = 0.0005)
plot(g_metro, vertex.size=ifelse(V(g_metro)$station,5,0)
     , vertex.label=NA, vertex.color=ifelse(V(g_metro)$station,'red','black'))




# Ajouter une couche administrative (connecte les centroides des zones au noeud le plus proche)
mairies_data <-opq(bbox = st_bbox(metro_data$osm_lines), timeout = 200) %>%
  add_osm_feature(key='amenity',value='townhall') %>% osmdata_sf()
plot(mairies_data$osm_points$geometry)
g_full <- addAdministrativeLayer(g_metro,
                                 admin_layer = mairies_data$osm_points,
                                 attributes = list(),
                                 empty_graph_heuristic=F
)
plot(g_full, vertex.size=ifelse(V(g_metro)$station,5,0)
     , vertex.label=NA, vertex.color=ifelse(V(g_metro)$station,'red','black'))
# TODO to fix



# Calculer des accessibilités en termes de temps de trajet
distances(g_metro,v=V(g_metro)$station==1,to=V(g_metro)$station==1,weights = E(g_metro)$length)


# Pour aller plus loin :
# - calcul des accessibilités aux aménités
# - utilisation des paramètres du modèle gravitaire
# - scenario : taxe sur l'essence, impact sur l'accessibilité
# - package r5r



