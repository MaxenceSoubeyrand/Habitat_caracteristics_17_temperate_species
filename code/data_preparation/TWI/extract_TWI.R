rm(list=ls())


library(tidyverse)
library(raster)
library(sf)
library(lidR)
library(stars)

#Path on MRNF servor
root_lidar = "//smullin/lidar/PUBLIC"


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

#Buffer because we need to extract several values near the gps loaction to cover 400m²
pla_sp_buf <- readRDS("placette_buffer.rds")


feuillet_voisins <- readRDS("feuillets_voisins.rds") %>% 
  filter(focal %in% placette$feu_20k)

filter(placette, placette$feu_20k=="12M15SO")

res_summary <- data.frame(ID_PE=NULL, mean=NULL, sd=NULL, q05=NULL, median=NULL, q95=NULL)
res <- list()

comp=1

for(i in unique(feuillet_voisins$focal)){ 
  comp <- comp+1
  
  path_feuillet_focal <- file.path(root_lidar,
                                           "Hydrographie/Indice_humidite_topographique/3-Donnees",
                                            substr(i, 1,3), i,
                                           paste0("TWI_", i, ".tif"))
  
  if(file.exists(path_feuillet_focal)){ 
    print(i)
    
    pla_sp_buf_foc <- pla_sp_buf %>%    
      filter(feu_20k==i)
    
    if(nrow(pla_sp_buf_foc)>=1){
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


  
  }
}

saveRDS(res_summary, "TWI_final.rds")#2584
saveRDS(res, "TWI_detailed_final.rds")



#Re run for the NA in order to be sure it was not a connexion problem
a=left_join(pla_sp_buf, res_summary) %>% unique()

id_pe_na <- a$ID_PE[which(is.na(a$mean))]

pla_sp_na <- filter(pla_sp_buf, ID_PE %in% id_pe_na)

res_na_summary <- data.frame(ID_PE=NULL, mean=NULL, sd=NULL, q05=NULL, median=NULL, q95=NULL)
res_na <- list()

comp_na <- 1

for(i in unique(pla_sp_na$feu_20k)){ 
  comp_na <- comp_na + 1
  
  path_feuillet <- file.path(root_lidar,
                             substr(i, 1,3), i,
                             paste0("TWI_", i, ".tif"))
  
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





