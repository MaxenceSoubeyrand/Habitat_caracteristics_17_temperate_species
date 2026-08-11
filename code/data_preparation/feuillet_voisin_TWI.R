rm(list=ls())

setwd("C:/Users/SOUMAA/OneDrive - BuroVirtuel/Documents/TWI")

library(tidyverse)
library(raster)
library(sf)
library(lidR)

#OUvrir shp qvec les feuillets
path <- "//vulcain/raigeop/depot_dde/Catalogue_du_systeme_DDE/Couches_de_donnees/Donnee_utilitaire/FEUILLET_CARTO_DECOUPAGE_20K_S/FGDB_GEO/FEUILLET_20K"

list.files(path)

###j'essaie avec st. 
feuillet <- sf::st_read("FEUILLET_20K.gdb")

centroids <- st_centroid(feuillet$Shape)

buffer <- st_buffer(centroids, 15000)

voisins <- data.frame(focal=NULL, voisins=NULL)

for(i in 1:length(buffer[,1])){
  print(i)
  feu_voisins <- st_filter(feuillet, buffer[i])
  voisins <- voisins %>% 
    bind_rows(data.frame(focal=feuillet$FCA_NO_FEUIL_CARTO[i], voisins=feu_voisins$FCA_NO_FEUIL_CARTO))
}

voisins <- filter(voisins, focal != voisins)

saveRDS(voisins, "feuillets_voisins.rds")
