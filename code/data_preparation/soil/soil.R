rm(list=ls())

library(tidyverse)
library(raster)
library(sf)

#download data here: https://www.donneesquebec.ca/recherche/dataset/siigsol-100m-carte-des-proprietes-du-sol

clay <- raster("argile_fr_siigsol.tif")
pH <- raster("pH_fr_siigsol.tif")
CEC <- raster("cec_fr_siigsol.tif")

#Read data to retrieve latitude and longitude of all plots selected

placette <- readRDS("plots.rds") %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326)

clay_placette <- extract(clay, placette)
pH_placette <- extract(pH, placette)
CEC_placette <- extract(CEC, placette)

sol <- bind_rows(ID_PE=placette$ID_PE, clay=clay_placette, pH=pH_placette, CEC=CEC_placette)

saveRDS(sol, "sol_clay_pH_CEC.rds")