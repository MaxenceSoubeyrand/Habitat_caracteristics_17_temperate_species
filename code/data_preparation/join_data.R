rm(list=ls())

library(tidyverse)

setwd("~/postdoc/habitat_caracterisation/data_compilation")

PEP <-  readRDS(file = "PEP.rds")
PET <-  readRDS(file = "PET.rds")

PE <- bind_rows(PEP, PET)  
  # group_by(ID_PE, species, year) %>% 
  # filter(if(any(duplicated(year))){str_detect(NO_MES, "[[:alpha:]]", negate=T)}else{TRUE})

clim <- readRDS(file = "climate_PE.rds") %>% 
  group_by(ID_PE,  NO_MES) %>% 
  summarize(TAVE=mean(TAVE),
         PPT=mean(PPT),
         VPD=mean(VPD),
         DD5=mean(DD5)) %>% 
  filter(TAVE!=-9999)


str(PE)
str(clim)

ab <- left_join(PE, clim, by = c("ID_PE", "NO_MES"), relationship = "many-to-many")


#Adding frost day
frost <- readRDS(file = "climate_frost.rds") %>% 
  select(ID_PE, NO_MES, frost=gel_10y) %>% 
  st_drop_geometry()

ab <- left_join(ab, frost, by = c("ID_PE", "NO_MES"), relationship = "many-to-many")

#Adding drougth (SPEI)
spei <- readRDS(file = "climate_spei.rds") %>% 
  select(ID_PE, NO_MES, spei_moderate=spei_moderate_10y, spei_severe=spei_severe_10y) %>% 
  st_drop_geometry()

ab <- left_join(ab, spei, by = c("ID_PE", "NO_MES"), relationship = "many-to-many")


#Adding soil variables
soil <- readRDS("sol/sol_clay_pH_CEC.rds") %>% 
  dplyr::select(ID_PE, clay2=clay, pH2=pH, CEC2=CEC) %>% 
  group_by(ID_PE) %>% 
  summarize(clay2=mean(clay2),
            pH2=mean(pH2),
            CEC2=mean(CEC2))



ab <- left_join(ab, soil, by = c("ID_PE"), relationship = "many-to-many")
#3542 NA ok. 

#Ici il faut rajouter les TWIs
TWI <- readRDS("~/postdoc/habitat_caracterisation/data_compilation/TWI/TWI_final.rds") %>% 
  dplyr::select(ID_PE, TWI=mean) %>% 
  group_by(ID_PE) %>% 
  summarize(TWI=mean(TWI))

ab <- left_join(ab, unique(TWI))

#Ici on rajoute l'altitude
elevation <- readRDS("PET_altitude/PET_altitude_final.rds") 

colnames(elevation)[2] <- "elevation_PET"
ab <- left_join(ab, elevation) %>% 
  ungroup() %>% 
  mutate(elevation=ifelse(is.na(elevation), elevation_PET, elevation)) %>% 
  dplyr::select(-elevation_PET)
#5500 NA, OK pour l'instant



#On rajoute la pente
slope <- readRDS("~/postdoc/habitat_caracterisation/data_compilation/PET_pente/PET_pente_final.rds") %>%
  na.omit() %>% 
  filter(slope<=100)

ab <- left_join(ab, dplyr::select(slope, ID_PE, slope_extract=slope)) %>% 
  mutate(slope_extract=case_when(slope_extract>100 ~ slope,
                                 is.na(slope_extract)~ slope,
                                 .default = slope_extract))

# #Ici il faut enlever les redondances PET/PEP
ab <- ab %>% 
  group_by(ID_PE, species, year) %>% 
  filter(if(any(duplicated(year))){str_detect(NO_MES, "[[:alpha:]]", negate=T)}else{TRUE})



# plan_ex <- unique(dplyr::select(ab, ID_PE, NO_MES, year)) %>% 
#   # filter(ID_PE == "0008500101") %>% 
#   group_by(ID_PE) %>%
#   filter(if(any(duplicated(year))){str_detect(NO_MES, "[[:alpha:]]", negate=T)}else{TRUE})
# 
# #381542 inventaires, il y avait 17901 inventaire dans les PETs qui se sont retrouvé dans les PEPs.
# #Nous on veut garder ces inventaires dans les PEPs. 
# 
# #Maintenant il faut filtrer le jeu de données avec le plan expérimental
# 
# ab <- plan_ex %>% 
#   left_join(ab)

sum(is.na(ab$pH))
sum(is.na(ab$pH2))

sum(is.na(ab$TWI))
sum(is.na(ab$slope_extract)) #mieux que slope donc faudra utiliser slope_extract
sum(is.na(ab$elevation)) #ok

mean(ab$pH_B, na.rm=T)
mean(ab$pH2, na.rm=T)

tail(ab[which(is.na(ab$ST)),]$NO_MES)

#399440

saveRDS(ab, "abundance_PE_clim.rds")
