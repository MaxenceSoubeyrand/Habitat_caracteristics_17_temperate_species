rm(list=ls())

setwd("~/postdoc/habitat_caracterisation/data_compilation/sol")

library(tidyverse)
library(raster)
library(sf)


clay <- raster("~/postdoc/data/sol/argile/argile_fr_siigsol.tif")
pH <- raster("~/postdoc/data/sol/pH/pH_fr_siigsol.tif")
CEC <- raster("~/postdoc/data/sol/CEC/cec_fr_siigsol.tif")

plot(clay)

#Pour chaque placette, extraire l'argile:

placette <- readRDS("~/postdoc/habitat_caracterisation/data_compilation/TWI/placette_TWI.rds") %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326)

clay_placette <- extract(clay, placette)
pH_placette <- extract(pH, placette)
CEC_placette <- extract(CEC, placette)

sol <- bind_rows(ID_PE=placette$ID_PE, clay=clay_placette, pH=pH_placette, CEC=CEC_placette)

saveRDS(sol, "sol_clay_pH_CEC.rds")
