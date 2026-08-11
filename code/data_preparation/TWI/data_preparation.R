#prepare data to extract TWI

rm(list=ls())

library(tidyverse)
library(readxl)
library(RODBC)

placette_PEP <- read_excel("PLACETTE_PEP.xlsx") %>% 
  select(LATITUDE, LONGITUDE, ID_PE, FEUILLET) %>% 
  mutate(ID_PE=as.character(ID_PE),
         data="PEP") %>% 
  select(ID_PE, latitude=LATITUDE, longitude=LONGITUDE, feuillet=FEUILLET)

placette_PET2 <- read_excel("PLACETTE2.xlsx") %>% 
  select(LATITUDE, LONGITUDE, ID_PE, FEUILLET) %>% 
  mutate(ID_PE=as.character(ID_PE),
         data="PET2") %>% 
  select(ID_PE, latitude=LATITUDE, longitude=LONGITUDE, feuillet=FEUILLET)

placette_PET3 <- read_excel("PLACETTE3.xlsx") %>% 
  select(LATITUDE, LONGITUDE, ID_PE, FEUILLET) %>% 
  mutate(ID_PE=as.character(ID_PE),
         data="PET3") %>% 
  select(ID_PE, latitude=LATITUDE, longitude=LONGITUDE, feuillet=FEUILLET)

placette_PET4 <- read_excel("PLACETTE4.xlsx") %>% 
  select(LATITUDE, LONGITUDE, ID_PE, FEUILLET) %>% 
  mutate(ID_PE=as.character(ID_PE),
         data="PET4") %>% 
  select(ID_PE, latitude=LATITUDE, longitude=LONGITUDE, feuillet=FEUILLET)

placette_PET5 <- read_excel("PLACETTE5.xlsx") %>% 
  select(LATITUDE, LONGITUDE, ID_PE, FEUILLET) %>% 
  mutate(ID_PE=as.character(ID_PE),
         data="PET5") %>% 
  select(ID_PE, latitude=LATITUDE, longitude=LONGITUDE, feuillet=FEUILLET)

placette_POE <- read_excel("PLACETTE_POE.xlsx") %>% 
  select(LATITUDE, LONGITUDE, ID_POE, FEUILLET) %>% 
  mutate(ID_POE=as.character(ID_POE),
         data="POE") %>% 
  select(ID_PE=ID_POE, latitude=LATITUDE, longitude=LONGITUDE, feuillet=FEUILLET)

df_PEP_mes  <-  list(placette_PEP, 
                     placette_PET2, placette_PET3, placette_PET4, placette_PET5, 
                     placette_POE)
placette <- df_PEP_mes %>% reduce(bind_rows) %>% unique()

saveRDS(placette, "placette_TWI.rds")
