rm(list=ls())

setwd("~/postdoc/habitat_caracterisation/data_compilation/TWI")

library(tidyverse)
library(raster)
library(sf)
library(lidR)
library(stars)

root_lidar = "//smullin/lidar/PUBLIC"


#Il fqut lancer une fois qu'on a toute les PET PEP POE
#Réorganiser l'extraction pour extraire tout un feuillet à la place de une placette par une placette.
#Coder un truc pour les placettes voisines.

placette <- readRDS("placette_TWI.rds") %>% 
  mutate(feu_250k= substr(feuillet, 1,3)) %>% 
  rename(feu_20k=feuillet)


path_twi <- file.path(root_lidar,
          "Hydrographie/Indice_humidite_topographique/3-Donnees",
          placette$feu_250k, placette$feu_20k,
          paste0("TWI_", placette$feu_20k, ".tif"))

pla_sp <- sf::st_as_sf(x = placette,                         
                           coords = c("longitude", "latitude"),
                           crs = 4326)

#pla_sp_buf <-  st_buffer(pla_sp, 11.28)

#cherche la trichette: setwd("~/postdoc/habitat_caracterisation/data_compilation/TWI")
pla_sp_buf <- readRDS("placette_buffer.rds")


feuillet_voisins <- readRDS("feuillets_voisins.rds") %>% 
  filter(focal %in% placette$feu_20k)

filter(placette, placette$feu_20k=="12M15SO")

res_summary <- data.frame(ID_PE=NULL, mean=NULL, sd=NULL, q05=NULL, median=NULL, q95=NULL)
res <- list()

comp=1

for(i in unique(feuillet_voisins$focal)[2584:2978]){ #Pour tous les feuillets qui existe au Québec (certains ne seront car pas de TWI dans tous les feuillet)
  print(paste0(comp, " sur 2978 soit:", round(comp/2978*100,2),  " %"))
  comp <- comp+1
  
  path_feuillet_focal <- file.path(root_lidar,
                                           "Hydrographie/Indice_humidite_topographique/3-Donnees",
                                            substr(i, 1,3), i,
                                           paste0("TWI_", i, ".tif"))
  
  if(file.exists(path_feuillet_focal)){ #Si le feuillet est présent dans les TWI
    print(i)
    
    #Ok je n'arrive pas à charger les 8 feuillet et les fusionner dans un temps correct. 
    #Alors j'esssaie sans voisins et je vais essayer de déterminer le nombre qui sort de l'extent. 
    
    pla_sp_buf_foc <- pla_sp_buf %>%    
      filter(feu_20k==i)
    
    if(nrow(pla_sp_buf_foc)>=1){ #Au cas où il n'y a pas de placette dans le feuillet. 
      raster_focal <- raster(path_feuillet_focal)
      
      extract_pla_buf <- extract(raster_focal, pla_sp_buf_foc)
      
      res <- c(res, list(extract_pla_buf))
      res_summary <- res_summary %>% bind_rows(
        data.frame(ID_PE=pla_sp_buf_foc$ID_PE,
                   ID_PE=pla_sp_buf_foc$ID_PE,
                   mean=sapply(extract_pla_buf, mean, na.rm=T, simplify = TRUE),
                   sd=sapply(extract_pla_buf, sd, na.rm=T, simplify = TRUE),
                   q05=sapply(extract_pla_buf, quantile, na.rm=T, probs = 0.05, names = FALSE, simplify = TRUE),
                   median=sapply(extract_pla_buf, median, na.rm=T, simplify = TRUE),
                   q95=sapply(extract_pla_buf, quantile, na.rm=T, probs = 0.95, names = FALSE, simplify = TRUE)))
    }


  #   #On doit charger et fusionner tous les rasters adjacents au feuillet focal i.
  #   feuillets_voisins_sub <- filter(feuillet_voisins, focal==i)$voisins
  #   
  #   chemin_feuillet_voisins <- str_subset(string = path_twi,
  #                                         pattern=paste(c(i, feuillets_voisins_sub), collapse = "|")) %>% 
  #     unique() #Parfois il existe des voisins qui ne sont pas dans les TWI. 
  #   #Donc mon code ne peut pas gérer si ça tombe sur un voisins TWI inconnu et extraiera seulement sur la partie connue. 
  #   
  #   chemin_voisins_focal <- NULL
  #   for(j in chemin_feuillet_voisins) { #j="//smullin/lidar/PUBLIC/Hydrographie/Indice_humidite_topographique/3-Donnees/32P/32P08NO/TWI_32P08NO.tif"
  #     if(file.exists(j)){
  #       chemin_voisins_focal <- c(chemin_voisins_focal,j)
  #     }
  #   }
  #   
  #   rasters_sf <- lapply(chemin_voisins_focal, raster)
  #   rasters_sf <- lapply(rasters_sf, st_as_stars)
  #   raster_fu <- do.call(st_as_stars, c(rasters_sf, merge = TRUE))
  #   
  #   
  #   merge(rasters_list[1], rasters_list)
  #   plot(raster(j))
  #   rasters_merged <- do.call(raster::merge, rasters_list)
  #   
  #   
  #   twi_raster <-  raster(path_twi[1])
  #   
  #   
  #   
  #   twi_value <- extract(twi_raster, pla_sp_buf[i,])[[1]]
  #   res <- c(res, list(twi_value))
  #   res_summary <- res_summary %>% bind_rows(
  #     data.frame(ID_PE=pla_sp$ID_PE[i],
  #                mean=mean(twi_value),
  #                sd=sd(twi_value),
  #                q05=quantile(twi_value, probs=0.05, names = FALSE),
  #                median=median(twi_value),
  #                q95=quantile(twi_value, probs=0.95, names = FALSE)))
  #   
  # 
  # }else{
  #   file_not_exist <- c(file_not_exist, path_twi[i])
  #   print("nooo")
  }
}

saveRDS(res_summary, "TWI_final.rds")#2584
saveRDS(res, "TWI_detailed_final.rds")



#on relance sur les NAs pour être sûr que ce n'est pas à cause de bug de connexion
a=left_join(pla_sp_buf, res_summary) %>% unique()

id_pe_na <- a$ID_PE[which(is.na(a$mean))]

pla_sp_na <- filter(pla_sp_buf, ID_PE %in% id_pe_na)

res_na_summary <- data.frame(ID_PE=NULL, mean=NULL, sd=NULL, q05=NULL, median=NULL, q95=NULL)
res_na <- list()

comp_na <- 1

for(i in unique(pla_sp_na$feu_20k)){ # i="32N16NO" #Pour tous les feuillets qui existe au Québec (certains ne seront car pas de TWI dans tous les feuillet)
  print(paste0(comp_na, " sur 849 soit: ", round(comp_na/849*100,2),  " %"))
  comp_na <- comp_na + 1
  
  path_feuillet <- file.path(root_lidar,
                             substr(i, 1,3), i,
                             paste0("TWI_", i, ".tif"))
  
  if(file.exists(path_feuillet)){ #Si le feuillet est présent dans les TWI
    print(i)
    
    
    pla_sub <- pla_sp_na %>%    
      filter(feu_20k==i)
    
    if(nrow(pla_sub)>=1){ #Au cas où il n'y a pas de placette dans le feuillet. 
      raster <- raster(path_feuillet)
      
      extract_pla <- extract(raster, pla_sub)
      
      res_na <- res_na %>% bind_rows(
        data.frame(ID_PE=pla_sub$ID_PE, 
                   slope=extract_pla))
      
      res_na <- c(res_na, list(extract_pla_buf))
      res_na_summary <- res_na_summary %>% bind_rows(
        data.frame(ID_PE=pla_sp_buf_foc$ID_PE,
                   ID_PE=pla_sp_buf_foc$ID_PE,
                   mean=sapply(extract_pla_buf, mean, na.rm=T, simplify = TRUE),
                   sd=sapply(extract_pla_buf, sd, na.rm=T, simplify = TRUE),
                   q05=sapply(extract_pla_buf, quantile, na.rm=T, probs = 0.05, names = FALSE, simplify = TRUE),
                   median=sapply(extract_pla_buf, median, na.rm=T, simplify = TRUE),
                   q95=sapply(extract_pla_buf, quantile, na.rm=T, probs = 0.95, names = FALSE, simplify = TRUE)))
      
      
    }
  }
}


res2 <- bind_rows(res, res_na) %>% 
  group_by(ID_PE) %>% 
  summarize(slope=mean(slope, na.rm=T))

saveRDS(res2, "PET_altitude_final.rds")





