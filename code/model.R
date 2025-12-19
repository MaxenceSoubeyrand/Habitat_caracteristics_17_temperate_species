#Script that execute random forest and compute SHAP values

rm(list=ls())

library(tidyverse)

library(viridis)
library(ggpubr)
theme_set(theme_bw())
library(sf)
library(ggh4x)
#random forest
library(ranger)
library(fastshap)

setwd("~/postdoc/habitat_caracterisation/github")

temp_spe <- c("ERR", "BOJ","ERS", "THO", "EPR", 
              "HEG", "PIB", "PRU", "FRN", "ERP",
              "CHR", "TIL", "FRA", "PIR", "CET",
              "OSV", "ORA")

spe_name <- c("Red maple",
              "Yellow birch",
              "Sugar maple",
              "White cedar",
              "Red spruce",
              "American beech",
              "Eastern white pine",
              "Eastern hemlock",
              "Black ash",
              "Striped maple",
              "Red oak",
              "Basswood",
              "White ash",
              "Red pine",
              "Black cherry",
              "American hophornbeam",
              "American elm")

species <- bind_rows(species=temp_spe, species2=spe_name)

#Bioclimatic domain
#Download here:
#https://www.donneesquebec.ca/recherche/fr/dataset/systeme-hierarchique-de-classification-ecologique-du-territoire
dom_bio <- st_read(dsn = "~/postdoc/data/carte_qc/dom_bio/dom_bio.shp",
                   layer = "dom_bio")
dom_bio_map <- subset(dom_bio, dom_bio$DOM_BIO %in% c("1", "2", "3","4", "5", "6"))

#Opening the data
ab_full <- readRDS("data/abundance_PE_clim.rds") 

colnames(ab_full) <- gsub("origine", "origin", colnames(ab_full))

str(ab_full)

ab_temp_spe <- ab_full %>% 
  filter(species %in% temp_spe) %>% 
  right_join(species) %>% 
  select(-species, species=species2)

#select the environmental variables
ab_abs <- subset(ab_full, !ab_full$ID_PE %in% unique(ab_temp_spe$ID_PE))  %>% 
  ungroup() %>% 
  filter(bioclimatic_domain %in% as.character(1:5)) %>% 
  dplyr::select(ID_PE, NO_MES, latitude, longitude, bioclimatic_domain, abundance,
                TAVE, VPD, frost, spei_moderate, spei_severe,
                origin, age, perturbation, 
                elevation, 
                slope=slope_extract, TWI,
                CEC2, clay2, pH2, species) %>% 
  na.omit()


ab <- ab_temp_spe %>% ungroup() %>% 
  dplyr::select(ID_PE, NO_MES, latitude, longitude, bioclimatic_domain, abundance,
                TAVE, VPD, frost, spei_moderate, spei_severe,
                origin, age, perturbation, 
                elevation, 
                slope=slope_extract, TWI,
                CEC2, clay2, pH2, species) %>% 
  na.omit() %>% 
  bind_rows(ab_abs)

ab_0 <- ab %>% 
  pivot_wider(names_from = species, values_from = abundance, values_fill = 0) %>% 
  select(ID_PE, NO_MES, latitude, longitude, bioclimatic_domain,
         TAVE, VPD, frost, spei_moderate, spei_severe,
         origin, age, perturbation, 
         elevation,
         slope, TWI,
         CEC2, clay2, pH2, species$species2) %>% 
  pivot_longer(cols=species$species2, names_to = "species", values_to = "abundance")

var_exp <- c("TAVE" , "VPD", "spei_severe", "elevation", "slope", "TWI", "CEC", "clay", "pH","origin", "age")
var_rep <- c("abundance")

#Create object where the results will be stored

R2 <- NULL
all_imp <- NULL
obs_shap <- NULL
validation <- NULL

for(i in species$species2){ 
  print(i)
  
  dir.create(paste0("results/", i), showWarnings=F)
  
  ab_sp <- ab_0 %>% 
    filter(species == i) %>% 
    select(-species) %>% 
    rename(CEC=CEC2, pH=pH2, clay=clay2)
  
  bio_clim_select <- ab_sp %>% 
    filter(abundance>0) %>% 
    group_by(bioclimatic_domain) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(n=n/sum(n)) %>% 
    filter(n>0.05) %>% 
    select(bioclimatic_domain)
  
  ab_sp <- ab_sp %>%
    filter(abundance != 0 | (abundance == 0 & bioclimatic_domain %in% bio_clim_select$bioclimatic_domain))
  
  ab_sp_pres <- ab_sp %>% 
    filter(abundance!=0)
  
  ab_sp <- ab_sp %>% 
    filter(abundance==0) %>% 
    slice_sample(n=round(0.2*nrow(ab_sp_pres))) %>% 
    bind_rows(ab_sp_pres)
  
  calibration_indices <- sample(nrow(ab_sp), 0.7 * nrow(ab_sp))
  
  calibration_ID_PE <- ab_sp$ID_PE[calibration_indices]
  
  calibration_data <- ab_sp %>%
    dplyr::slice(calibration_indices)
  
  validation_data <- ab_sp %>%
    dplyr::slice(-calibration_indices)
  
  #######Random forest
  formula <- as.formula(paste0("abundance ~ ", paste0(var_exp, collapse = "+")))
  
  cor(calibration_data$spei_severe, calibration_data$TAVE)
  
  set.seed(1)
  fit <- ranger(formula,
                data=calibration_data,
                importance = 'permutation',
                num.trees = 500)
  
  imp <- data.frame(importance=ranger::importance(fit), variable=names(ranger::importance(fit)), species=i, row.names = NULL)

  
  all_imp <- bind_rows(all_imp, imp)
  
  
  #######Validation
  validation_ranger <- data.frame(ID_PE=validation_data$ID_PE, latitude=validation_data$latitude,
                                  longitude=validation_data$longitude, obs=validation_data[, var_rep, drop=T], 
                                  pred=predict(fit, data=validation_data[, var_exp]),
                                  species=i)
  
  validation <- bind_rows(validation, validation_ranger)
  
  
  
  #######R²
  R2_sp <- 1 - (sum((validation_ranger$obs-validation_ranger$pred)^2)/
                  sum((validation_ranger$obs-mean(validation_ranger$obs))^2))
  
  R2_sp
  
  R2 <- bind_rows(R2, data.frame(species=i, R2=R2_sp))
  
  

  #######SHAP values
  calibration_data_uni <- calibration_data %>%
    filter(abundance>0) %>%
    slice_sample(prop = 1)

  # Prediction wrapper
  pfun <- function(object, newdata) {
    predict(object, data = newdata)$predictions
  }

  # Compute fast (approximate) Shapley values using 100 Monte Carlo repetitions
  library(future)
  plan(multisession, workers = 4)

  shap <- fastshap::explain(fit, X = calibration_data_uni[, var_exp], pred_wrapper = pfun, nsim = 100)

  colnames(shap) <- paste0(colnames(shap), "_shap")

  shap_values_map <- calibration_data_uni %>%
    select(ID_PE, bioclimatic_domain, latitude, longitude, all_of(var_exp), abundance)%>%
    mutate(species=i) %>%
    bind_cols(shap)  %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 crs = 4326)

  obs_shap <- bind_rows(obs_shap, shap_values_map)
  
  #Save the results
 saveRDS(imp, paste0("results_rf/",i,"/res_model/importance.rds"))
 saveRDS(data.frame(species=i, R2=R2_sp), paste0("results_rf/",i,"/res_model/R2.rds"))
 saveRDS(validation_ranger, paste0("results_rf/",i,"/res_model/validation.rds"))
 saveRDS(shap_values_map, paste0("results_rf/",i,"/res_model/shap_values.rds"))
}

saveRDS(all_imp, paste0("results/importance.rds"))
saveRDS(R2, paste0("results/R2.rds"))
saveRDS(validation, paste0("results/validation.rds"))
saveRDS(obs_shap, paste0("results/shap_values.rds"))
