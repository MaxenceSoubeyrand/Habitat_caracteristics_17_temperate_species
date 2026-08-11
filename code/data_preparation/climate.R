rm(list=ls())

library(tidyverse)
library(SPEI)

setwd("~/postdoc/habitat_caracterisation/data_compilation")

PEP <-  readRDS(file = "PEP.rds")
PET <-  readRDS(file = "PET.rds")

sum(is.na(PET$elevation))

PE <- bind_rows(PEP, PET)

min(PE$latitude)

PE_clim <- PE %>% 
  ungroup() %>% 
  select(ID1=ID_PE, lat=latitude, long=longitude, el=elevation) %>% 
  unique() %>% 
  mutate(ID2=1:214352)%>% 
  select(ID1, ID2, lat, long, el)

ID_PE_lat_lon <- PE %>% 
  ungroup() %>% 
  select(ID_PE, latitude, longitude)

#Ici je veux un tableau qui dit quelles sont les années on doit récupérer
PE_ex <- PE %>% 
  ungroup() %>% 
  select(ID_PE, NO_MES, Latitude=latitude, Longitude=longitude, inv_year=year) %>% 
  unique() %>% 
  group_by(ID_PE, NO_MES, Latitude, Longitude) %>% 
  complete(inv_year = seq(inv_year - 1, inv_year - 9))
  

# write.csv(PE_clim[1:86910,], "../../data/climate/PE_clim1.csv", row.names=F)
# write.csv(PE_clim[86911:173820,], "../../data/climate/PE_clim2.csv", row.names=F)
# write.csv(PE_clim[173821:260730,], "../../data/climate/PE_clim3.csv", row.names=F)
# write.csv(PE_clim[260731:347640,], "../../data/climate/PE_clim4.csv", row.names=F)


#Exécuter dans climateNA de 1960 à 2021
clim_files <- c("PE_clim1_1960-2021M.csv", "PE_clim2_1960-2021M.csv",
                "PE_clim3_1960-2021M.csv", "PE_clim4_1960-2021M.csv")

clim_list <- list()

for(i in clim_files){ #i=clim_files[1]
  
  clim <- read.csv(paste0("../../data/climate/",i))
  
  clim_test <- filter(clim, ID2%in%1:500)
  
  # on définit les colonnes identifiants
  id_cols <- c("Year","ID1", "ID2", "Latitude", "Longitude", "Elevation")
  
  # on pivote toutes les colonnes restantes (mensuelles) automatiquement
  clim_drought <- clim_test %>%
    pivot_longer(
      cols = -all_of(id_cols),  # tout sauf les colonnes identifiants
      names_to = c("Variable", "Month"),
      names_pattern = "([A-Za-z0-9_]+)(\\d{2})", # Variable = Tave, Tmax, PPT..., Month = 01,02...
      values_to = "Value"
    ) %>%
    mutate(Month = as.integer(Month)) %>%
    pivot_wider(
      names_from = Variable,
      values_from = Value
    ) %>% 
    select(id_cols, Month, Tave, PPT, RH, CMI, DD5_) %>% 
    group_by(ID2) %>% 
    mutate(PET=thornthwaite(Tave, lat=unique(Latitude), verbose=F),
           D=PPT-PET,
           spei_12=spei(D, scale = 12, verbose=F)$fitted) %>%
    # filtrer la saison de croissance
    # Calcul de métriques
    filter(Month >= 5 & Month <= 8) %>%
    group_by(ID2, Year) %>% 
    summarise(
      n_drought_moderate = sum(spei_12 < -1, na.rm = TRUE),
      n_drought_severe = sum(spei_12 < -1.5, na.rm = TRUE),
      .groups = "drop"
    )
  
  clim_temp_vpd <- clim_test %>%
    rowwise() %>% 
    mutate(
      TAVE = mean(c_across(Tave05:Tave08), na.rm = TRUE),   # Temp moyenne saison de croissance
      PPT  = sum(c_across(PPT05:PPT08), na.rm = TRUE),     # Précipitations totales saison
      RH   = mean(c_across(RH05:RH08), na.rm = TRUE)      # Humidité relative moyenne
    ) %>%
    ungroup() %>%
    mutate(
      es  = 0.6108 * exp(17.27 * TAVE / (TAVE + 237.3)),
      ea  = RH / 100 * es,
      VPD = es - ea
    ) %>%
    select(ID1, ID2, Year, Latitude, Longitude, TAVE, PPT, VPD)
  
  clim_drought_temp_vpd <- left_join(clim_drought, clim_temp_vpd)
  
  #on récupère les vrais ID_PE
  clim_drought_temp_vpd_PE <- clim_drought_temp_vpd %>% 
    left_join(PE_clim, by=c("ID2" = "ID2"),
              relationship = "many-to-many") %>% 
    select(ID_PE=ID1.y, Year, Latitude, Longitude, TAVE, PPT, VPD, n_drought_moderate, n_drought_severe)
  
  #On récupère les années d'inventaires
  clim_drought_temp_vpd_PE_inv <- select(ungroup(PE_ex), -Latitude, -Longitude) %>%
    left_join(clim_drought_temp_vpd_PE, c("ID_PE" = "ID_PE",
                       "inv_year" = "Year"), relationship = "many-to-many") %>% 
    arrange(ID_PE) %>% 
    na.omit()

  
  clim_mean <- clim_drought_temp_vpd_PE_inv %>%
    group_by(ID_PE, NO_MES) %>% 
    summarize(TAVE= mean(TAVE),
              PPT=mean(PPT),
              VPD=mean(VPD),
              n_drought_moderate=sum(n_drought_moderate),
              n_drought_severe=sum(n_drought_severe))
  
  clim_list <- c(clim_list, list(clim_mean))
}

clim_PE <- reduce(clim_list, bind_rows)

saveRDS(clim_PE, "climate_PE.rds")

