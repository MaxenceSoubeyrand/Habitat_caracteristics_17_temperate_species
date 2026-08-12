rm(list=ls())
gc()

library(tidyverse)
#library(shapr)
library(viridis)
library(ggpubr)
theme_set(theme_bw())
library(sf)
library(ggh4x)
#random forest
library(ranger)
library(fastshap)

temp_spe <- c("ERR", "BOJ","ERS", "THO", "EPR", 
              "HEG", "PIB", "PRU", "FRN", "ERP",
              "CHR", "TIL", "FRA", "PIR", "CET",
              "OSV", "ORA")

spe_name <- c(
  "Acer rubrum",                 # Red maple
  "Betula alleghaniensis",       # Yellow birch
  "Acer saccharum",              # Sugar maple
  "Thuja occidentalis",          # White cedar
  "Picea rubens",                # Red spruce
  "Fagus grandifolia",           # American beech
  "Pinus strobus",               # Eastern white pine
  "Tsuga canadensis",            # Eastern hemlock
  "Fraxinus nigra",              # Black ash
  "Acer pensylvanicum",          # Striped maple
  "Quercus rubra",               # Red oak
  "Tilia americana",             # Basswood
  "Fraxinus americana",          # White ash
  "Pinus resinosa",              # Red pine
  "Prunus serotina",             # Black cherry
  "Ostrya virginiana",           # American hophornbeam
  "Ulmus americana"              # American elm
)

species <- bind_rows(species=temp_spe, species2=spe_name)

#opening domain biocliamtic available here
#https://www.donneesquebec.ca/recherche/fr/dataset/systeme-hierarchique-de-classification-ecologique-du-territoire
dom_bio <- st_read(dsn = "data/dom_bio/dom_bio.shp",
                   layer = "dom_bio")
dom_bio_map <- subset(dom_bio, dom_bio$DOM_BIO %in% c("1", "2", "3","4", "5", "6"))

quebec <- st_read(dsn = "~/postdoc/quebec/quebec.shp",
                  layer = "quebec")

#data available upon request maxence.soubeyrand@uqat.ca
ab_full <- readRDS("data/abundance_PE_clim.rds")

ab_full <- ab_full %>% ungroup() %>% 
  filter(!origine %in% c("plantation", "wasteland", "winfall"),
         !perturbation %in% c("partial_plantation", "burn", "partial_winfall"))  %>% 
  mutate(
    perturbation = case_when(
      perturbation == "partial_logging"  ~ "logging_pr",
      perturbation == "partial_outbreak" ~ "outbreak_pr",
      TRUE ~ perturbation
    )
  )

###Change modality names
colnames(ab_full) <- gsub("origine", "origin", colnames(ab_full))

str(ab_full)

ab_temp_spe <- ab_full %>% 
  filter(species %in% temp_spe) %>% 
  right_join(species) %>% 
  select(-species, species=species2)



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
                elevation, ion2, 
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

#Extract district 
lat_lon <- ab_0 %>% 
  dplyr::select(ID_PE, latitude, longitude) %>% 
  unique() %>% 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326   # WGS84
  )

district <- st_read(dsn = "~/postdoc/data/carte_qc/district/district.shp",
                    layer = "district") %>% 
  dplyr::select(DIS_ECO, DOM_BIO)

district <- st_transform(district, crs = st_crs(lat_lon))
intersection <- data.frame(st_intersects(lat_lon, district)) 

lat_lon_district <- bind_cols(lat_lon, district=intersection[,2])

ab_0 <- ab_0 %>% 
  left_join(lat_lon_district)

var_exp <- c("TAVE" , "VPD", "elevation", "slope", "TWI", "CEC", "clay", "pH","origin", "perturbation", "age")

var_rep <- c("abundance")

for(i in  spe_name){ 

  print(i)
  

  
  ab_sp <- ab_0 %>% 
    filter(species == i) %>% 
    select(-species) %>% 
    rename(CEC=CEC2, pH=pH2, clay=clay2)

  
  #presence by dom_bio
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
  
  #K-fold
  districts <- unique(ab_sp$district)
  
  n_k <- 5
  
  set.seed(123)  
  
  #table district - kfold
  kfold_table <- data.frame(
    district = districts,
    kfold = sample(rep(1:n_k, length.out = length(districts)))
  )
  
  var_exp <- c("TAVE" , "VPD", "elevation", "slope", "TWI", "CEC", "clay", "pH","origin", "perturbation", "age")
  
  validation_k <- NULL
  R2_k <- NULL
  imp_k <- NULL
  obs_shap <- NULL
  
  for(k in 1:n_k){
    print(k)
    district_k <- kfold_table$district[kfold_table$kfold==k]
    
    data_train <- ab_sp %>% 
      filter(!district%in%district_k)
    
    test_train <- ab_sp %>% 
      filter(district%in%district_k)
    
    #######Random forest
    formula <- as.formula(paste0("abundance ~ ", paste0(var_exp, collapse = "+")))
    
    set.seed(1)
    fit <- ranger(formula,
                  data=data_train,
                  importance = 'permutation',
                  num.trees = 500)
    
    #######Importance
    imp <- data.frame(importance=ranger::importance(fit), variable=names(ranger::importance(fit)), species=i, kfold=k, row.names = NULL)

    imp_k <- bind_rows(imp_k, imp)
    
    
    #######Validation
    validation_ranger <- data.frame(ID_PE=test_train$ID_PE, latitude=test_train$latitude,
                                    longitude=test_train$longitude, obs=test_train[, var_rep, drop=T], 
                                    pred=predict(fit, data=test_train[, var_exp])$predictions,
                                    species=i, kfold=k)
    
    validation_k <- bind_rows(validation_k, validation_ranger)
    
    #######R² 
    R2_sp <- 1 - (sum((validation_ranger$obs-validation_ranger$pred)^2)/
                    sum((validation_ranger$obs-mean(validation_ranger$obs))^2))
    
    R2_k <- bind_rows(R2_k, data.frame(species=i, kfold=k, R2=R2_sp))
    
    
    
    #######SHAP values
    data_train_uni <- data_train %>%
      filter(abundance>0) %>%
      slice_sample(prop = 1)

    # Prediction wrapper
    pfun <- function(object, newdata) {
      predict(object, data = newdata)$predictions
    }

    # Compute fast (approximate) Shapley values using 30 Monte Carlo repetitions
    library(future)
    plan(multisession, workers = 4)

    shap <- fastshap::explain(fit, X = data_train_uni[, var_exp], pred_wrapper = pfun, nsim = 30)

    colnames(shap) <- paste0(colnames(shap), "_shap")

    #For maps
    shap_values_map_kfold <- data_train_uni %>%
      select(ID_PE, bioclimatic_domain, latitude, longitude, all_of(var_exp), abundance)%>%
      mutate(species=i) %>%
      bind_cols(shap)  %>%
      sf::st_as_sf(coords = c("longitude", "latitude"),
                   crs = 4326) %>%
      mutate(kfold=k)

    obs_shap <- bind_rows(obs_shap, shap_values_map_kfold)
    print("ok shap")
  }
  
  #Save species by species
 saveRDS(imp_k, paste0("results_rf_kfold/",i,"/res_model/importance.rds"))
 saveRDS(R2_k, paste0("results_rf_kfold/",i,"/res_model/R2.rds"))
 saveRDS(validation_k, paste0("results_rf_kfold/",i,"/res_model/validation.rds"))
 saveRDS(obs_shap, paste0("results_rf_kfold/",i,"/res_model/shap_values.rds"))
}