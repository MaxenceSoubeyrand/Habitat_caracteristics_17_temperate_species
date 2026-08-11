rm(list=ls())

setwd("~/postdoc/habitat_caracterisation/data_compilation/PET_pente")

library(tidyverse)
library(raster)
library(sf)
library(lidR)
library(stars)

#Root on the MRNF servor
root_lidar = "//smullin/lidar/PUBLIC"


#Read plot for extracting slope

placette <- readRDS("placette_pente.rds") %>% 
  mutate(feu_250k= substr(feuillet, 1,3)) %>% 
  rename(feu_20k=feuillet)


path_twi <- file.path(root_lidar,
          placette$feu_250k, placette$feu_20k,
          paste0("Pentes_", placette$feu_20k, ".tif"))

pla_sp <- sf::st_as_sf(x = placette,                         
                           coords = c("longitude", "latitude"),
                           crs = 4326)


res<- data.frame(ID_PE=NULL, slope=NULL)

#For each feuillets
comp=1

for(i in unique(pla_sp$feu_20k)){ 

  comp <- comp+1
  
  path_feuillet <- file.path(root_lidar,
                             substr(i, 1,3), i,
                             paste0("Pentes_", i, ".tif"))
  
  if(file.exists(path_feuillet)){ 
    print(i)

    
    pla_sub <- pla_sp %>%    
      filter(feu_20k==i)
    
    if(nrow(pla_sub)>=1){
      raster<- raster(path_feuillet)
      
      extract <- extract(raster, pla_sub)
      
      res<- res %>% bind_rows(
        data.frame(ID_PE=pla_sub$ID_PE,
                   slope=extract))
    }


 
  }
}

saveRDS(res, "slope.rds")

#Re run for the NA in order to be sure it was not a connexion problem
a=left_join(pla_sp, res) %>% unique()

id_pe_na <- a$ID_PE[which(is.na(a$slope))]

pla_sp_na <- filter(pla_sp, ID_PE %in% id_pe_na)

res_na <- data.frame(ID_PE=NULL, slope=NULL)

comp_na <- 1

for(i in unique(pla_sp_na$feu_20k)){ 
  print(paste0(comp_na, " sur 777 soit: ", round(comp_na/777*100,2),  " %"))
  comp_na <- comp_na + 1
  
  path_feuillet <- file.path(root_lidar,
                             substr(i, 1,3), i,
                             paste0("Pentes_", i, ".tif"))
  
  if(file.exists(path_feuillet)){ 
    print(i)
    
    
    pla_sub <- pla_sp_na %>%    
      filter(feu_20k==i)
    
    if(nrow(pla_sub)>=1){ 
      raster <- raster(path_feuillet)
      
      extract_pla <- extract(raster, pla_sub)
      
      res_na <- res_na %>% bind_rows(
        data.frame(ID_PE=pla_sub$ID_PE, 
                   slope=extract_pla))
    }
  }
}


res2 <- bind_rows(res, res_na) %>% 
  group_by(ID_PE) %>% 
  summarize(slope=mean(slope, na.rm=T))

saveRDS(res2, "PET_pente_final.rds")
