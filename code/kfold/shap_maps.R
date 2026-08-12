rm(list=ls())

library(tidyverse)
library(viridis)
library(ggpubr)
theme_set(theme_bw())
library(sf)
library(ggh4x)
library(rnaturalearth)
library(rnaturalearthdata)

canada <- ne_countries(scale = "medium", country = "Canada", returnclass = "sf")
US <- ne_countries(scale = "medium", country = "United States of America", returnclass = "sf")

quebec <- st_read(dsn = "~/postdoc/data/quebec.shp",
                  layer = "quebec")

#opening domain biocliamtic available here
#https://www.donneesquebec.ca/recherche/fr/dataset/systeme-hierarchique-de-classification-ecologique-du-territoire
dom_bio <- st_read(dsn = "data/dom_bio/dom_bio.shp",
                   layer = "dom_bio")

#opening ecological disctricts available here
#https://www.donneesquebec.ca/recherche/fr/dataset/systeme-hierarchique-de-classification-ecologique-du-territoire
district <- st_read(dsn = "data/district/district.shp",
                    layer = "district") %>% 
  dplyr::select(DIS_ECO, DOM_BIO)

spe_latin <- c("Acer rubrum",
               "Betula alleghaniensis",
               "Acer saccharum",
               "Thuja occidentalis",
               "Picea rubens",
               "Fagus grandifolia",
               "Pinus strobus",
               "Tsuga canadensis",
               "Fraxinus nigra",
               "Acer pensylvanicum",
               "Quercus rubra",
               "Tilia americana",
               "Fraxinus americana",
               "Pinus resinosa",
               "Prunus serotina",
               "Ostrya virginiana",
               "Ulmus americana")

#Shap values of the baseline model
shap <- readRDS("results/res_model/all_shap_values.rds")

#SHAP values of the spatialized model
shap_fold <- readRDS("~/postdoc/habitat_caracterisation/model/abundance/results_rf_kfold/res_model/all_shap.rds") %>% 
  st_drop_geometry() %>% 
  select(ID_PE, species, kfold, TAVE_shap, VPD_shap, elevation_shap, slope_shap, TWI_shap, 
         CEC_shap, clay_shap, pH_shap, origin_shap, perturbation_shap, age_shap)

library(data.table)

dt <- as.data.table(shap_fold)

mean_shap_fold <- dt[, lapply(.SD, mean, na.rm = TRUE),
                     by = .(ID_PE, species),
                     .SDcols = patterns("shap")] %>% 
  data.frame()






                                       3,3))

obs <- shap %>% 
  dplyr::select(ID_PE, TAVE, VPD, elevation, slope, TWI, CEC, clay, pH) %>% 
  unique()

#Districts
district <- st_transform(district, crs = st_crs(obs))
intersection <- data.frame(st_intersects(obs, district)) 

district$id <- 1:nrow(district)

ID_PE_district <- data.frame(ID_PE=obs$ID_PE, id=intersection[,2]) %>% 
  left_join(district)

mean_shap_fold_d <- mean_shap_fold %>% 
  right_join(ID_PE_district)

dt <- as.data.table(mean_shap_fold_d)

median_shap_fold_district <- dt[, lapply(.SD, median, na.rm = TRUE),
                     by = .(DIS_ECO, species),
                     .SDcols = patterns("shap")] %>% 
  data.frame()


shap_kfold <- pivot_longer(data = median_shap_fold_district, 
             cols = contains("shap"),
             names_to = "variable",
             values_to = "shap_kfold")

#Baseline SHAP values
shap_district <- select(shap, ID_PE, species, TAVE_shap, VPD_shap,
       elevation_shap, slope_shap, TWI_shap, CEC_shap, clay_shap, pH_shap,
       origin_shap, perturbation_shap, age_shap) %>% 
  st_drop_geometry() %>% 
  right_join(ID_PE_district) %>% 
  group_by(species, DIS_ECO) %>% 
  summarize(across(contains("shap"), median)) %>% 
  pivot_longer(cols = contains("shap"),
               names_to = "variable",
               values_to = "shap_model")

shap_model_kfold <- shap_kfold %>% 
  left_join(shap_district) %>% 
  mutate(shap_diff=shap_model-shap_kfold) %>% 
  filter(shap_diff>-100,
         shap_diff<100) %>% 
  mutate(variable=str_remove(variable, "_shap"))

imp <- readRDS("results/baseline/res_model/all_importance.rds") 

imp3 <- imp %>% 
  group_by(species) %>% 
  slice_max(importance, n=3)

shap_model_kfold <- shap_model_kfold %>% 
  left_join(imp3) %>% 
  mutate(col=ifelse(is.na(importance), "No imp", "Imp"))

df_cor <- shap_model_kfold %>%
  group_by(species, variable) %>%
  summarise(r = cor(shap_model, shap_kfold))

df_cor$variable <- factor(df_cor$variable,
                          levels=c("TAVE", "VPD", "elevation", "slope", "TWI", "CEC",
                                   "clay", "pH", "origin", "perturbation", "age"),
                          labels=c("TAVE", "VPD", "elevation.", "slope", "TWI", "CEC",
                                   "clay", "pH", "origin", "perturbation", "age"))


plot_cor <- ggplot(df_cor, aes(x = variable, y = r, color=species)) +
  geom_point(size = 3) +
  coord_flip()+
  labs(y="Correlation of SHAP values between (baseline vs. kfolds)",
       x="Variable", 
       color="Species") +
  theme_bw()

ggsave(
  "figures/plot_cor.png",
  plot = plot_cor,
  width =7,
  height = 5
)

shap_model_kfold_d <- shap_model_kfold %>% 
  left_join(district) %>% 
  st_as_sf()

shap_model_kfold_d$variable <- factor(shap_model_kfold_d$variable,
       levels=c("TAVE", "VPD", "elevation", "slope", "TWI", "CEC",
                "clay", "pH", "origin", "perturbation", "age"),
       labels=c("TAVE", "VPD", "elevation.", "slope", "TWI", "CEC",
                "clay", "pH", "origin", "perturbation", "age"))

shap_model_kfold_d$DOM_BIO <- factor(shap_model_kfold_d$DOM_BIO,
                                      labels=c(c("Hickory -\nsugar maple", "Basswood -\nsugar maple", "Yellow birch -\nsugar maple ",
                                                 "Balsam fir -\nyellow birch", "Balsam fir -\nwhite birch", "Spruce -\nmoss")))


plot_dom_bio <- ggplot(shap_model_kfold_d,
       aes(x = variable, y = shap_diff, color = DOM_BIO)) +
  stat_summary(
    fun.min = \(x) quantile(x, 0.05, na.rm = TRUE),
    fun.max = \(x) quantile(x, 0.95, na.rm = TRUE),
    geom = "linerange",
    linewidth = 1, 
    position = position_dodge(width = 0.6)
  ) +
  labs(y="Difference of SHAP values between baseline and kfold models",
       x=NULL, 
       color="Bioclimatic\ndomain")+
  facet_wrap(~ species, ncol=2) +
  scale_x_discrete(
    guide = guide_axis(n.dodge = 2)
  )+
  theme(
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    legend.key.height = unit(0.8, "cm")
  )




ggsave(
  "figures/autocor_spat_dom_bio.png",
  plot = plot_dom_bio,
  width = 9,
  height = 12
)



#cartes mais c'est trop de cartes à montrer
ggplot() +
  geom_sf(data = canada, color = "black", alpha=0) +
  geom_sf(data = US, color = "black", alpha=0) +
  geom_sf(data=quebec, alpha=0, color = "grey0") +
  geom_sf(data=filter(shap_model_kfold_3, species=="Acer rubrum", variable=="TAVE"), aes(fill = shap_diff, color = shap_diff)) +
  #geom_sf(data=dom_bio_sp, fill=NA, color="red", linewidth=0.5, linetype="solid") +
  # scale_fill_manual(values = bi_palette, drop = FALSE) +
  # scale_color_manual(values = bi_palette, drop = FALSE) +
  facet_grid(species~variable) +
  theme_minimal()+
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )+
  coord_sf(ylim = c(45.1, 49.5),
           xlim = c(-79, -64.5))
  
