#Opening Quebec governement temporary plots.
#Extract presence/absence of species
#Extract environmental data

#Extract for temporary plots 2, 3, 4 and 5. Then, join all data sets

rm(list=ls())

library(RODBC)
library(readxl)
library(tidyverse)

################################################################################
#Placette échantillon temporaire 2
#https://www.donneesquebec.ca/recherche/dataset/placettes-echantillons-temporaires-2e-inventaire-1980-1993/resource/19ce54d1-15fb-4236-8973-d315dd8b3012
con <- odbcConnectAccess2007("full_pathway_to_the_database.mdb")
data_tables <- sqlTables(con ,TABVLE_TYPE=='TABLE')$TABLE_NAME

dendro <- sqlFetch(con, "DENDRO_TIGES", as.is=T) 

#Get abundance and basal area (ST)
dendro_ab <- dendro %>% 
  select(ID_PE, ESSENCE, ST_HA) %>% 
  group_by(ID_PE, ESSENCE) %>% 
  summarize(abundance = n()*10000/400, 
            ST = sum(ST_HA)) %>% 
  group_by(ID_PE) %>% 
  mutate(rel_abundance = abundance/sum(abundance)) %>% 
  rename(species=ESSENCE) %>% 
  na.omit() %>% 
  arrange(ID_PE)

#Stand age
age <- sqlFetch(con, "PEE_2_CONV_3", as.is=T) %>% 
  select(ID_PE, CL_AGE) %>%
  mutate(CL_AGE = str_replace(CL_AGE, "[0-9]+", "Even"),
         CL_AGE = str_replace_all(CL_AGE, "VIR", "Old uneven"),
         CL_AGE = str_replace_all(CL_AGE, "VIN", "Old uneven"),
         CL_AGE = str_replace_all(CL_AGE, "JIN", "Young uneven"),
         CL_AGE = str_replace_all(CL_AGE, "JIR", "Young uneven"),
         CL_AGE = str_replace_all(CL_AGE, "12030", "Young uneven"),
         CL_AGE = str_replace_all(CL_AGE, "9030", "Young uneven"),
         CL_AGE = str_replace_all(CL_AGE, "12030", "Young uneven"),
         CL_AGE = str_replace_all(CL_AGE, "12070", "Young uneven"),
         CL_AGE = str_replace_all(CL_AGE, "9050", "Young uneven")) %>%
  select(age=CL_AGE, ID_PE)

#Extract biocliamtic domain
classif <- sqlFetch(con, "CLASSI_ECO_PE", as.is=T) %>% 
  select(ID_PE, bioclimatic_domain=DOM_BIO)


#Extract GPS localisation
placette <- read_excel("~/postdoc/data/PET/PLACETTE2.xlsx") %>% 
  select(latitude=LATITUDE, longitude=LONGITUDE, FEUILLET, ID_PE, DATE_SOND) %>% 
  mutate(ID_PE=as.character(ID_PE),
         year=year(DATE_SOND)) %>% 
  select(-DATE_SOND)

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
  select(ID_PE, perturbation, origine_year=AN_ORIGINE, origine)

#Extract drainage
drainage <- sqlFetch(con, "PEE_2_CONV_4", as.is=T) %>% 
  mutate(drainage=CL_DRAI) %>% 
  select(ID_PE, drainage)%>%
  mutate(drainage = case_when(drainage %in% 0 ~ "excessive",
                              drainage %in% c(10:14) ~ "rapid",
                              drainage == 16 ~ "complex",
                              drainage %in% c(20:24) ~ "good",
                              drainage %in% c(30:34) ~ "moderate",
                              drainage %in% c(40:44) ~ "imperfect",
                              drainage %in% c(50:54) ~ "poor",
                              drainage %in% c(60:64) ~ "very_poor"))


# Join multiple data frames
df  <-  list(dendro_ab ,placette, classif, perturbation, drainage, age)

PET2 <- df %>% reduce(left_join, by='ID_PE') %>% 
  mutate(NO_MES="PET2") %>% 
  filter(!is.na(latitude)) #remove data where latitude and longitude not known



################################################################################
##Placette échantillon temporaire 3
#https://www.donneesquebec.ca/recherche/dataset/placettes-echantillons-temporaires-3e-inventaire-1992-2003
con <- odbcConnectAccess2007("full_pathway_to_the_database.mdb")
data_tables <- sqlTables(con ,TABVLE_TYPE=='TABLE')$TABLE_NAME

dendro <- sqlFetch(con, "DENDRO_TIGES", as.is=T) 

dendro_ab <- dendro %>% 
  select(ID_PE, ESSENCE, ST_HA) %>% 
  group_by(ID_PE, ESSENCE) %>% 
  summarize(abundance = n()*10000/400, 
            ST = sum(ST_HA)) %>% 
  group_by(ID_PE) %>% 
  mutate(rel_abundance = abundance/sum(abundance)) %>% 
  rename(species=ESSENCE) %>% 
  na.omit() %>% 
  arrange(ID_PE)


age <- sqlFetch(con, "PEE_ORI_SOND", as.is=T) %>% 
  select(ID_PE, CL_AGE) %>%
  mutate(CL_AGE = str_replace(CL_AGE, "[0-9]+", "Even"),
         CL_AGE = str_replace_all(CL_AGE, "VIR", "Old uneven"),
         CL_AGE = str_replace_all(CL_AGE, "VIN", "Old uneven"),
         CL_AGE = str_replace_all(CL_AGE, "JIN", "Young uneven"),
         CL_AGE = str_replace_all(CL_AGE, "JIR", "Young uneven"),
         CL_AGE = str_replace_all(CL_AGE, "12030", "Young uneven"),
         CL_AGE = str_replace_all(CL_AGE, "9030", "Young uneven"),
         CL_AGE = str_replace_all(CL_AGE, "12030", "Young uneven"),
         CL_AGE = str_replace_all(CL_AGE, "12070", "Young uneven"),
         CL_AGE = str_replace_all(CL_AGE, "9050", "Young uneven")) %>%
  select(age=CL_AGE, ID_PE)

table(age$age)

classif <- sqlFetch(con, "CLASSI_ECO_PE", as.is=T) %>% 
  select(ID_PE, bioclimatic_domain=DOM_BIO)

placette <- read_excel("~/postdoc/data/PET/PLACETTE3.xlsx") %>% 
  select(latitude=LATITUDE, longitude=LONGITUDE, FEUILLET, ID_PE, DATE_SOND) %>% 
  mutate(ID_PE=as.character(ID_PE),
         year=year(DATE_SOND)) %>% 
  select(-DATE_SOND)


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
  select(ID_PE, perturbation, origine_year=AN_ORIGINE, origine, perturbation_year=AN_PERTURB)



drainage <- sqlFetch(con, "PEE_ORI_SOND", as.is=T) %>% 
  mutate(drainage=CL_DRAI) %>% 
  select(ID_PE, drainage)%>%
  mutate(drainage = case_when(drainage %in% 0 ~ "excessive",
                              drainage %in% c(10:14) ~ "rapid",
                              drainage == 16 ~ "complex",
                              drainage %in% c(20:24) ~ "good",
                              drainage %in% c(30:34) ~ "moderate",
                              drainage %in% c(40:44) ~ "imperfect",
                              drainage %in% c(50:54) ~ "poor",
                              drainage %in% c(60:64) ~ "very_poor"))

topo <- sqlFetch(con, "STATION_PE", as.is=T) %>% 
  select(ID_PE, VERSANT, PC_PENT, ALTITUDE) %>% 
  group_by(ID_PE) %>% 
  summarize(versant=unique(VERSANT)[1],
            slope=mean(PC_PENT, nar.rm=T),
            elevation=mean(ALTITUDE)) %>% 
  ungroup()


# Join multiple data.frames
df  <-  list(dendro_ab ,placette, classif, perturbation, drainage, topo, age) 

PET3 <- df %>% reduce(full_join, by='ID_PE') %>% 
  mutate(NO_MES="PET3") %>% 
  filter(!is.na(latitude)) #remove data where latitude and longitude not known

################################################################################
#Placette échantillon temporaire 4
#https://www.donneesquebec.ca/recherche/dataset/placettes-echantillons-temporaires-4e-inventaire-2004-a-aujourd-hui

con <- odbcConnectAccess2007("full_pathway_to_the_database.mdb")
data_tables <- sqlTables(con ,TABVLE_TYPE=='TABLE')$TABLE_NAME

dendro <- sqlFetch(con, "DENDRO_ARBRES", as.is=T) 

dendro_ab <- dendro %>% 
  select(ID_PE, ESSENCE, DHP) %>% 
  group_by(ID_PE, ESSENCE) %>% 
  summarize(abundance = n()*10000/400, 
            ST = sum(pi * (DHP/1000/2)^2) * 10000 / 400) %>% 
  group_by(ID_PE) %>% 
  mutate(rel_abundance = abundance/sum(abundance)) %>% 
  rename(species=ESSENCE) %>% 
  na.omit()


age <- sqlFetch(con, "PEE_ETAGE_ORI_SOND", as.is=T) %>% 
  select(ID_PE, CL_AGE_ET) %>%
  mutate(CL_AGE_ET = str_replace(CL_AGE_ET, "[0-9]+", "Even"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "VIR", "Old uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "VIN", "Old uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "JIN", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "JIR", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "12030", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "9030", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "12030", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "12070", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "9050", "Young uneven")) %>%
  select(age=CL_AGE_ET, ID_PE)

classif <- sqlFetch(con, "CLASSI_ECO_PE", as.is=T) %>% 
  select(ID_PE, bioclimatic_domain=DOM_BIO)


#Extract lat lon
placette <- read_excel("~/postdoc/data/PET/PLACETTE4.xlsx") %>% 
  select(latitude=LATITUDE, longitude=LONGITUDE, FEUILLET,ID_PE, DATE_SOND) %>% 
  mutate(ID_PE=as.character(ID_PE),
         year=year(DATE_SOND)) %>% 
  select(-DATE_SOND)

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
  select(ID_PE, perturbation, origine_year=AN_ORIGINE, origine, perturbation_year=AN_PERTURB)

drainage <- sqlFetch(con, "PEE_ORI_SOND", as.is=T) %>% 
  mutate(drainage=CL_DRAI) %>% 
  select(ID_PE, drainage)%>%
  mutate(drainage = case_when(drainage %in% 0 ~ "excessive",
                              drainage %in% c(10:14) ~ "rapid",
                              drainage == 16 ~ "complex",
                              drainage %in% c(20:24) ~ "good",
                              drainage %in% c(30:34) ~ "moderate",
                              drainage %in% c(40:44) ~ "imperfect",
                              drainage %in% c(50:54) ~ "poor",
                              drainage %in% c(60:64) ~ "very_poor"))

topo <- sqlFetch(con, "STATION_PE", as.is=T) %>% 
  select(ID_PE, VERSANT, PC_PENT, ALTITUDE) %>% 
  group_by(ID_PE) %>% 
  summarize(versant=unique(VERSANT)[1],
            slope=mean(PC_PENT, nar.rm=T),
            elevation=mean(ALTITUDE)) %>% 
  ungroup()


# Join multiple data frames
df  <-  list(dendro_ab ,placette, classif, perturbation, drainage, topo, age)
  mutate(NO_MES="PET4")

################################################################################
#Placette échantillon temporaire 5
#https://www.donneesquebec.ca/recherche/dataset/placettes-echantillons-temporaires-du-cinquieme-inventaire/resource/d09ae743-f3ca-4bfc-90d5-65c2614a679e 
con <- odbcConnectAccess2007("full_pathway_to_the_database.mdb")
data_tables <- sqlTables(con ,TABVLE_TYPE=='TABLE')$TABLE_NAME

dendro <- sqlFetch(con, "DENDRO_ARBRES", as.is=T) 

dendro_ab <- dendro %>% 
  select(ID_PE, ESSENCE, DHP) %>% 
  group_by(ID_PE, ESSENCE) %>% 
  summarize(abundance = n()*10000/400, 
            ST = sum(pi * (DHP/1000/2)^2) * 10000 / 400) %>% 
  group_by(ID_PE) %>% 
  mutate(rel_abundance = abundance/sum(abundance)) %>% 
  rename(species=ESSENCE) %>% 
  na.omit()


age <- sqlFetch(con, "PEE_ETAGE_ORI_SOND", as.is=T) %>% 
  select(ID_PE, CL_AGE_ET) %>%
  mutate(CL_AGE_ET = str_replace(CL_AGE_ET, "[0-9]+", "Even"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "VIR", "Old uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "VIN", "Old uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "JIN", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "JIR", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "12030", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "9030", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "12030", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "12070", "Young uneven"),
         CL_AGE_ET = str_replace_all(CL_AGE_ET, "9050", "Young uneven")) %>%
  select(age=CL_AGE_ET, ID_PE)

classif <- sqlFetch(con, "CLASSI_ECO_PE", as.is=T) %>% 
  select(ID_PE, bioclimatic_domain=DOM_BIO)


#Extract lat lon
placette <- read_excel("~/postdoc/data/PET/PLACETTE5.xlsx") %>% 
  select(latitude=LATITUDE, longitude=LONGITUDE, FEUILLET, ID_PE, DATE_SOND) %>% 
  mutate(ID_PE=as.character(ID_PE),
         year=year(DATE_SOND)) %>% 
  select(-DATE_SOND)

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
  select(ID_PE, perturbation, origine_year=AN_ORIGINE, origine, perturbation_year=AN_PERTURB)


drainage <- sqlFetch(con, "PEE_ORI_SOND", as.is=T) %>% 
  mutate(drainage=CL_DRAI) %>% 
  select(ID_PE, drainage)%>%
  mutate(drainage = case_when(drainage %in% 0 ~ "excessive",
                              drainage %in% c(10:14) ~ "rapid",
                              drainage == 16 ~ "complex",
                              drainage %in% c(20:24) ~ "good",
                              drainage %in% c(30:34) ~ "moderate",
                              drainage %in% c(40:44) ~ "imperfect",
                              drainage %in% c(50:54) ~ "poor",
                              drainage %in% c(60:64) ~ "very_poor"))

topo <- sqlFetch(con, "STATION_PE", as.is=T) %>% 
  select(ID_PE, VERSANT, PC_PENT, ALTITUDE) %>% 
  group_by(ID_PE) %>% 
  summarize(versant=unique(VERSANT)[1],
            slope=mean(PC_PENT, nar.rm=T),
            elevation=mean(ALTITUDE)) %>% 
  ungroup()


# Join multiple data frames
df  <-  list(dendro_ab ,placette, classif, perturbation, drainage, topo, age)
PET5 <- df %>% reduce(full_join, by='ID_PE') %>% 
  mutate(NO_MES="PET5")

###############################################################################
#Join all PET inventories
PET <- bind_rows(PET2, PET3, PET4, PET5)%>% 
  filter(!is.na(ST))

#Get GPS localisation to extract elevation (altitude) and slopes (pente) 
#in other scripts where the data is missing
PET_altitude <- PET %>% 
  filter(is.na(elevation)) %>% 
  select(ID_PE, NO_MES, FEUILLET, latitude, longitude)%>% 
  unique()

PET_pente <- PET %>% 
  filter(is.na(slope)) %>% 
  select(ID_PE, NO_MES, FEUILLET, latitude, longitude) %>% 
  unique()


#Save the data to extract elevation and slopes
saveRDS(PET_altitude, "PET_altitude/PET_altitude_NA.rds")
saveRDS(PET_pente, "PET_pente/PET_pente_NA.rds")

#Save the PET data
saveRDS(PET, "PET.rds")
