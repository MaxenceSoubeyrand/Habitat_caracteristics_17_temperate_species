#Opening Quebec governement permanent plots.
#Extract abundance of species
#Extract environmental data

#TWI, elevation, climate and a soil variables are extracted in other data sources

rm(list=ls())

library(RODBC)
library(readxl)
library(tidyverse)

#Placette échantillon permanente
#Extract abundance
#https://www.donneesquebec.ca/recherche/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui/resource/72828f8a-d008-4119-b246-89e6103120cb

con <- odbcConnectAccess2007("full_pathway_to_the_database.mdb")
data_tables <- sqlTables(con ,TABVLE_TYPE=='TABLE')$TABLE_NAME
dendro_PEP <- sqlFetch(con, "DENDRO_ARBRES", as.is=T) 

#Get abundance and Basal Area (ST) by ID_PE, NO_MES and species
dendro_PEP_ab <- dendro_PEP %>% 
  select(ID_PE, NO_MES, ESSENCE, DHP) %>% 
  group_by(ID_PE, ESSENCE, NO_MES) %>% 
  summarize(abundance = n()*10000/400, 
            ST = sum(pi * (DHP/1000/2)^2) * 10000 / 400) %>% 
  group_by(ID_PE, NO_MES) %>% 
  mutate(rel_abundance = abundance/sum(abundance)) %>% 
  rename(species=ESSENCE) %>% 
  na.omit()

#get site and NO_MES where abundance is available
plan_ex <- dendro_PEP_ab %>% 
  ungroup() %>% 
  select(ID_PE, NO_MES) %>% 
  unique()

#Stand age
age <- sqlFetch(con, "PEE_ETAGE_ORI_SOND", as.is=T) %>% 
  select(ID_PE, CL_AGE_ET, NO_MES) %>%
  mutate(CL_AGE_ET = str_replace(CL_AGE_ET, "[0-9]+", "Even"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "VIR", "Old uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "VIN", "Old uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "JIR", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "JIN", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "12030", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "9030", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "12030", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "12070", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "9050", "Young uneven")) %>% 
  full_join(plan_ex) %>% 
  group_by(ID_PE) %>% 
  mutate(age = replace_na(CL_AGE_ET, na.omit(unique(CL_AGE_ET))[1])) %>% 
  select(ID_PE, age, NO_MES) %>% 
  unique()




#Extract GPS position
placette_PEP <- read_excel("~/postdoc/data/PEP/PLACETTE.xlsx") %>% 
  select(LATITUDE, LONGITUDE, ID_PE, DERN_SOND) %>% 
  mutate(ID_PE=as.character(ID_PE),
         year_sond=year(DERN_SOND)) %>% 
  select(ID_PE, latitude=LATITUDE, longitude=LONGITUDE)
  


#Extract Bioclimatic domain
classif <- sqlFetch(con, "CLASSI_ECO_PE", as.is=T) %>% 
  select(ID_PE, bioclimatic_domain=DOM_BIO)



#Year of inventory
year <- sqlFetch(con, "PLACETTE_MES", as.is=T) %>% 
  mutate(year=year(DATE_SOND)) %>% 
  select(ID_PE, NO_MES, year)


#Extract soil variables: PC_ARGILE, pH and CEC
sol_PEP <- sqlFetch(con, "STATION_SOL", as.is=T) %>% 
  group_by(ID_PE) %>% 
  mutate(pH_B=if_else(is.na(PH_HORIZB), mean(PH_HORIZB, na.rm = TRUE), PH_HORIZB),
         pH_humus=if_else(is.na(PH_HUMUS), mean(PH_HUMUS, na.rm = TRUE), PH_HUMUS),
         pc_clay=if_else(is.na(PC_ARGILB), mean(PC_ARGILB, na.rm = TRUE), PC_ARGILB),
         ep_mat_org=if_else(is.na(EPMATORG), mean(EPMATORG, na.rm = TRUE), EPMATORG),
         pc_pierre=if_else(is.na(POURCPIERR), mean(POURCPIERR, na.rm = TRUE), POURCPIERR)) %>% 
  select(ID_PE, NO_MES, pH_B, pH_humus, pc_clay, ep_mat_org, pc_pierre)



#Soil chemistry 
chimie_PEP <- read_excel("~/postdoc/data/PEP/sol_chimie.xlsx") %>% 
  filter(HORIZON=="B") %>% 
  right_join(plan_ex) %>% 
  group_by(ID_PE) %>% 
  mutate(pH=if_else(is.na(pH_eau), mean(pH_eau, na.rm = TRUE), pH_eau),
            CEC=if_else(is.na(CEC), mean(CEC, na.rm = TRUE), CEC)) %>% 
  select(ID_PE, NO_MES, pH, CEC) %>% 
  arrange(ID_PE, NO_MES) %>% 
  group_by(ID_PE, NO_MES) %>% 
  summarize(pH=mean(pH),
            CEC=mean(CEC))

#Drainage
drainage <- sqlFetch(con, "PEE_ORI_SOND", as.is=T) %>% 
  mutate(drainage=CL_DRAI) %>% 
  select(ID_PE, drainage, NO_MES)%>%
  mutate(drainage = case_when(drainage %in% 0 ~ "excessive",
                              drainage %in% c(10:14) ~ "rapid",
                              drainage == 16 ~ "complex",
                              drainage %in% c(20:24) ~ "good",
                              drainage %in% c(30:34) ~ "moderate",
                              drainage %in% c(40:44) ~ "imperfect",
                              drainage %in% c(50:54) ~ "poor",
                              drainage %in% c(60:64) ~ "very_poor"))

#Perturbation (25 to 75% of basal area removed)
#Origin (more than 75% of basal area removed)
perturbation <- sqlFetch(con, "PEE_ORI_SOND", as.is=T) %>% 
  mutate(
      perturbation = case_when(
        PERTURB  %in%
          c("CBA","CBT","CEF","CPT","CRB","CRS","CS","CT","ETR", "RPS")~"logging",
        PERTURB  %in% c("CPR","CDV","CPH") ~ "logging_pr",
        PERTURB  %in% c("CA","CAM","CB","CD","CDL","CE","CEA","CIP","CJ","CJG","CJP","CJT","CP",
            "CPC","CPF","CPI","CPM","CPS","CPX","CTR","DEG","DLD","DRM","EC",
            "ECE","EPC","ESI","PCP") ~ "partial_logging",
        PERTURB  %in% c("BR", "BRP") ~ "burn",
        PERTURB  %in% c("ES") ~ "outbreak",
        PERTURB  %in% c("EL") ~ "partial_outbreak",
        PERTURB  %in% c("CHT","DT") ~ "winfall",
        PERTURB  %in% c("CHP", "VEP", "DP") ~ "partial_winfall",
        PERTURB  %in% c("P", "PLN", "PLR", "PRR", "ENS", "REA") ~ "plantation",
        PERTURB  %in% c("ENR", "RR",  "RRG") ~ "partial_plantation",
        PERTURB  %in% c("FR") ~ "wasteland",
        .default = "no_perturbation"),
      origine = case_when(
        ORIGINE  %in%
          c("CBA","CBT","CEF","CPT","CRB","CRS","CS","CT","ETR", "RPS")~"logging",
        ORIGINE  %in% c("CPR","CDV","CPH") ~ "logging_pr",
        ORIGINE  %in% c("CA","CAM","CB","CD","CDL","CE","CEA","CIP","CJ","CJG","CJP","CJT","CP",
                        "CPC","CPF","CPI","CPM","CPS","CPX","CTR","DEG","DLD","DRM","EC",
                        "ECE","EPC","ESI","PCP") ~ "partial_logging",
        ORIGINE  %in% c("BR", "BRP") ~ "burn",
        ORIGINE  %in% c("ES") ~ "outbreak",
        ORIGINE  %in% c("EL") ~ "partial_outbreak",
        ORIGINE  %in% c("CHT","DT") ~ "winfall",
        ORIGINE  %in% c("CHP", "VEP", "DP") ~ "partial_winfall",
        ORIGINE  %in% c("P", "PLN", "PLR", "PRR", "ENS", "REA") ~ "plantation",
        ORIGINE  %in% c("ENR", "RR",  "RRG") ~ "partial_plantation",
        ORIGINE  %in% c("FR") ~ "wasteland",
      .default = "no_perturbation")) %>%
  select(ID_PE, NO_MES, perturbation, origine_year=AN_ORIGINE, origine, perturbation_year=AN_PERTURB)


#Topographic variables
topo <- sqlFetch(con, "STATION_PE", as.is=T) %>% 
  select(ID_PE, NO_MES, VERSANT, EXPOSITION, PC_PENT, ALTITUDE) %>% 
  group_by(ID_PE) %>% 
  summarize(versant=unique(VERSANT)[1],
            exposition=mean(EXPOSITION, nar.rm=T),
            slope=mean(PC_PENT, nar.rm=T),
            elevation=mean(ALTITUDE)) %>% 
  ungroup() %>% 
  #Linéariser l'exposition pour que 0 soit nord,1 soit sud et est/ouest soit 0.5. 
  mutate(exposition=case_when(exposition<180~exposition/180,
                               exposition>180~(360-exposition)/180))


# Join all data frames
df_PEP_mes  <-  list(dendro_PEP_ab, year, age, perturbation, drainage, chimie_PEP, sol_PEP)
PEP <- df_PEP_mes %>% reduce(full_join, by=c("ID_PE", "NO_MES"))

df_PEP <- list(PEP, placette_PEP, classif, topo)

PEP <- df_PEP %>% reduce(full_join, by='ID_PE') %>% 
  mutate(NO_MES=as.character(NO_MES)) %>% 
  arrange(ID_PE, NO_MES, species) %>% 
  filter(!is.na(ST))

saveRDS(PEP, "PEP.rds")
