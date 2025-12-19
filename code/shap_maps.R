#Figure 4, 5 and 6, map the SHAP values

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

#Bioclimatic domain
#Download here:
#https://www.donneesquebec.ca/recherche/fr/dataset/systeme-hierarchique-de-classification-ecologique-du-territoire
dom_bio <- st_read(dsn = "~/postdoc/data/carte_qc/dom_bio/dom_bio.shp",
                   layer = "dom_bio")

#cological district
#Download here:
#https://www.donneesquebec.ca/recherche/fr/dataset/systeme-hierarchique-de-classification-ecologique-du-territoire
district <- st_read(dsn = "~/postdoc/data/carte_qc/district/district.shp",
                   layer = "district") %>% 
  select(DIS_ECO, DOM_BIO)

shap <- readRDS("results/res_model/all_shap.rds") %>% 
  rename(SPEI=spei_severe, SPEI_shap=spei_severe_shap)

imp <- readRDS("results/res_model/all_importance.rds")
imp$variable[imp$variable == "spei_severe"] <- "SPEI"
#val <- readRDS("~/postdoc/habitat_caracterisation/model/abundance/results_rf/res_model/all_validation.rds")

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

#Table with the bioclimatic domain where the northern population lies.
spe_dom_bio <- data.frame(spe_name=spe_name,
                          limit_dom_bio=c(5,4,4,4,4,
                                          3,4,3,4,4,
                                          3,3,3,4,3,
                                          3,3))

quebec <- st_read(dsn = "data/quebec/quebec.shp",
                  layer = "quebec")

canada <- ne_countries(scale = "medium", country = "Canada", returnclass = "sf")
US <- ne_countries(scale = "medium", country = "United States of America", returnclass = "sf")


obs <- shap %>% 
  select(TAVE, VPD, SPEI, elevation, slope, TWI, CEC, clay, pH) %>% 
  unique()

#Group observations by district
district <- st_transform(district, crs = st_crs(obs))
intersection <- data.frame(st_intersects(obs, district)) 

district$id <- 1:nrow(district)

#Observations
obs_district <- obs %>%
  st_drop_geometry() %>% 
  mutate(row.id=1:nrow(obs)) %>% 
  right_join(intersection) %>% 
  select(-row.id) %>% 
  rename(pixel_id=col.id) %>% 
  group_by(pixel_id) %>% 
  summarise(across(everything(), median))

district <- filter(district, id %in% obs_district$pixel_id)

obs_district <- left_join(district, st_drop_geometry(obs_district),
                          by = c("id" = "pixel_id")) %>% 
  pivot_longer(cols=`TAVE`:`pH`, names_to = "Variable", values_to = "Observation")

shap_district_all <- NULL
id_geometry_all <- NULL
obs_shap_all <- NULL
data_bi_all <- NULL

for (i in spe_name){
  
  #####Shapley Value######
  print(i)
  
  shap_sp <- filter(shap, species==i)
  imp_sp <- filter(imp, species==i)
  
  #Intersect the district and SHAP values
  intersection <- data.frame(st_intersects(shap_sp, district)) 
  
  #Mediane e each district
  shap_sp_district <- shap_sp %>%
    select(contains("shap")) %>% 
    mutate(row.id=1:nrow(shap_sp)) %>% 
    right_join(intersection) %>% 
    select(-row.id) %>% 
    rename(pixel_id=col.id) %>% 
    group_by(pixel_id) %>% 
    summarise(across(TAVE_shap:age_shap, median))
  
  #Keep district with at least one observation
  district_sp <- filter(district, id %in% shap_sp_district$pixel_id)

  shap_district_med <- left_join(district_sp, st_drop_geometry(shap_sp_district),
                                 by = c("id" = "pixel_id")) %>% 
    pivot_longer(cols=TAVE_shap:age_shap, names_to = "Variable", values_to = "Shapley") %>% 
    mutate(`Shapley`=case_when(`Shapley` < -50 ~ -50,
                               `Shapley` > 50 ~ 50,
                               .default = `Shapley`))
  
  
  shap_district_sp <- st_drop_geometry(mutate(shap_district_med, Variable=str_remove_all(Variable, "_shap"))) %>%
    mutate(species=i)
  
  id_geometry_sp <- unique(select(shap_district_med, id)) %>%
    mutate(species=i)
  
  shap_district_all <- bind_rows(shap_district_all, shap_district_sp)
  id_geometry_all <- bind_rows(id_geometry_all, id_geometry_sp)
  
  ######FOR the SI#######
  #Get all the maps 
  plot_med <- list()
  plot_obs <- list()
  p=1

  imp_sp$Variable <- unique(shap_district_med$Variable)

  var_tri_imp <- imp_sp %>%
    arrange(desc(importance), decreasing=T)
  
  dom_bio_sp <- dom_bio %>% 
    filter(DOM_BIO==filter(spe_dom_bio, spe_name==i)$limit_dom_bio)

  for(var in unique(var_tri_imp$Variable)){
    shap_district_var_med <- shap_district_med  %>%
      filter(Variable==var)

    obs_district_var <- obs_district  %>%
      filter(Variable==str_remove(pattern="_shap", string=var))

    plot_med[[p]] <- ggplot()+
      geom_sf(data = canada, color = "black") +
      geom_sf(data = US, color = "black") +
      geom_sf(data=quebec, alpha=0) +
      geom_sf(data=shap_district_var_med,aes(fill=`Shapley`, color=`Shapley`)) +
      geom_sf(data=dom_bio_sp, fill=NA, color="red", linewidth=0.5, linetype="solid") +
      scale_fill_gradient2(
        "SHAP values",
        low = "dodgerblue3",
        high = "gold",
        midpoint = 0
      ) +
      scale_color_gradient2(
        "SHAP values",
        low = "dodgerblue3",
        high = "gold",
        midpoint = 0
      )+
      ggtitle(str_remove(pattern="_shap", string=var))+
      theme(plot.title=element_text(size=70)) +
      theme_minimal()+
      coord_sf(ylim = c(45.1, 49.5),
               xlim = c(-79, -64.5))

    if(var %in% c("origin_shap")){

      plot_obs[[p]] <- ggplot(shap_sp, aes(x=origin, y=origin_shap)) +
        geom_boxplot() +
        ggtitle(str_remove(pattern="_shap", string=var)) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        ylab("SHAP value")
    }
    if(var %in% c("age_shap")){

      plot_obs[[p]] <- ggplot(shap_sp, aes(x=age, y=age_shap)) +
        geom_boxplot() +
        ggtitle(str_remove(pattern="_shap", string=var))+
        ylab("SHAP value")
    }
    if(!var %in% c("origin_shap", "age_shap")){
      plot_obs[[p]] <- ggplot()+
        geom_sf(data = canada, color = "black") +
        geom_sf(data = US, color = "black") +
        geom_sf(data=quebec, alpha=0) +
        geom_sf(data=obs_district_var, aes(fill=Observation, color=Observation)) +
        scale_fill_viridis() +
        scale_color_viridis() +
        ggtitle(str_remove(pattern="_shap", string=var))+
        theme(plot.title=element_text(size=70)) +
        theme_minimal()+
        coord_sf(ylim = c(45.1, 49.5),
                 xlim = c(-79, -64.5))
    }

    p=p+1
  }

  #Plot obs vs shap
  obs_shap <- shap_district_sp %>% left_join(obs_district)
  
  obs_shap_all <- bind_rows(obs_shap_all, select(st_drop_geometry(obs_shap), -geometry))
  
  plot_obs_shap <- ggplot(filter(obs_shap, !Variable %in% c("age", "origin")), aes(x=Observation, y=Shapley))+
    geom_point() +
    facet_wrap(~Variable, scales="free", ncol=3, nrow=3)+
    geom_smooth(method = "gam") +
    labs(title=i, y="SHAP values")+
    theme(plot.title = element_text(hjust = 0.5, size=12))
  
  ggsave(plot=plot_obs_shap, 
         filename=paste0("results/", i,"/plot_obs_shap.png"), 
         width=6, height=6)
  
  #bivariate choropleth maps
  quantiles_global <- obs_district %>%
    st_drop_geometry() %>%
    group_by(Variable) %>%
    summarise(
      q1 = quantile(Observation, 1/3, na.rm = TRUE),
      q2 = quantile(Observation, 2/3, na.rm = TRUE)
    )
  
  data_bi <- obs_shap  %>% 
    left_join(quantiles_global, by = "Variable") %>%
    mutate(
      obs_class = case_when(
        Observation <= q1 ~ "Low_O",
        Observation <= q2 ~ "Med_O",
        TRUE ~ "High_O"
      ),
      shap_class = case_when(
        Shapley <= quantile(Shapley, 1/3, na.rm = TRUE) ~ "Low_S",
        Shapley <= quantile(Shapley, 2/3, na.rm = TRUE) ~ "Med_S",
        TRUE ~ "High_S"
      ),
      bi_class = paste(obs_class, shap_class, sep = "_")
    ) %>% 
    na.omit() %>% 
    st_as_sf()
  
  data_bi_all <- bind_rows(data_bi_all, data_bi)
  
  bi_palette <- c( "Low_O_Low_S" = "#b0b0b0", 
                   "Med_O_Low_S" = "#8da0c4", 
                   "High_O_Low_S" = "#6c83b5", 
                   
                   "Low_O_Med_S" = "#91c28a", 
                   "Med_O_Med_S" = "#70a1a5", 
                   "High_O_Med_S" = "#567994", 
                   
                   "Low_O_High_S" = "#5ca36c", 
                   "Med_O_High_S" = "#4a8c60", 
                   "High_O_High_S" = "#2a5a5b" )
  
  legend_data <- expand.grid(
    obs_class  = c("Low_O", "Med_O", "High_O"),
    shap_class = c("Low_S", "Med_S", "High_S")
  )
  
  legend_data$bi_class <- paste(legend_data$obs_class, legend_data$shap_class, sep = "_")
  
  # matrice 3×3
  bivariate_legend <- ggplot(legend_data, aes(x = shap_class, y = obs_class, fill = bi_class)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = bi_palette) +
    coord_equal() +
    
    scale_x_discrete(labels = c("Negative", "Neutral", "Positive"),
                     guide = guide_axis(n.dodge = 2)) +
    scale_y_discrete(labels = c("Low", "Medium", "High")) +
    labs(x = "SHAP values", y = "Observation") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 8, face = "bold"),
      axis.text = element_text(size = 8),
      panel.grid = element_blank()
    )
  
  order_var <- str_remove(unique(var_tri_imp$Variable), "_shap") %>% 
    setdiff(c("age", "origin"))
  
  data_bi$Variable <- factor(
    data_bi$Variable,
    levels = order_var
    )
  
  map_all <- ggplot() +
    geom_sf(data = canada, color = "black") +
    geom_sf(data = US, color = "black") +
    geom_sf(data=quebec, alpha=0) +
    geom_sf(data=data_bi, aes(fill = bi_class, color = bi_class)) +
    geom_sf(data=dom_bio_sp, fill=NA, color="red", linewidth=0.5, linetype="solid") +
    scale_fill_manual(values = bi_palette, drop = FALSE) +
    scale_color_manual(values = bi_palette, drop = FALSE) +
    facet_wrap(~Variable, ncol = 3, nrow=3) +
    theme_minimal()+
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold")
    )+
    coord_sf(ylim = c(45.1, 49.5),
             xlim = c(-79, -64.5))
  
  library(patchwork)
  
  final_plot <- ggarrange(map_all, bivariate_legend,widths = c(7.5, 1.5))
  
  #Plot origine and age
  p1 <- plot_obs[[which(var_tri_imp$Variable=="origin_shap")]]
  p2 <- plot_med[[which(var_tri_imp$Variable=="origin_shap")]]
  
  p3 <- plot_obs[[which(var_tri_imp$Variable=="age_shap")]]
  p4 <- plot_med[[which(var_tri_imp$Variable=="age_shap")]]
  
  bottom_row <- ggarrange(p1, p2, p3, p4, ncol=2, nrow=2)
  
  final_plot2 <- ggarrange(final_plot, bottom_row, 
                           ncol=1, nrow=2,
                           heights = c(8, 6))
  

  
  obs_shap_dom_bio <- obs_shap %>%
    add_count(DOM_BIO) %>%  
    filter(n >= 20) %>%        
    select(-n) 
  
  obs_shap_dom_bio$DOM_BIO2  <- case_when(
    obs_shap_dom_bio$DOM_BIO  == "1" ~ "H-M",
    obs_shap_dom_bio$DOM_BIO  == "2" ~ "B-SM",
    obs_shap_dom_bio$DOM_BIO  == "3" ~ "YB-SM",
    obs_shap_dom_bio$DOM_BIO  == "4" ~ "BF-YB",
    obs_shap_dom_bio$DOM_BIO  == "5" ~ "BF-WB",
    obs_shap_dom_bio$DOM_BIO  == "6" ~ "B-S")
  obs_shap_dom_bio$DOM_BIO2  <- factor(obs_shap_dom_bio$DOM_BIO2, levels = c("H-M", "B-SM", "YB-SM",
                                                             "BF-YB", "BF-WB", "B-S"))
  
  dom_bio_plot <- ggplot(obs_shap_dom_bio, aes(x=Shapley, y=DOM_BIO2, fill=Variable))+
    geom_boxplot(outlier.size = 0) +
    labs(x ="SHAP value", y = "Bioclimatic domain") 
  
  combined_plot <- ggarrange(final_plot2, dom_bio_plot,
            nrow=1, ncol=2,
            widths=c(9,4))+ 
    bgcolor("white") + 
    border("white")
  
  combined_plot <-annotate_figure(combined_plot, top=i) + 
    bgcolor("white") + 
    border("white")

  ggsave(plot=combined_plot, 
         filename=paste0("results/", i,"/plot_bivariate.png"), 
         width=13, height=9)
}



#Figure  SHAP when species have climate in first importance

sp_TAVE <- data.frame(species=c("American elm", "Black cherry", "Eastern hemlock", 
                      "Eastern white pine" , "Red maple", "Red pine", "Red spruce", 
                      "White ash","Yellow birch"),
                      Variable=c(rep("TAVE", 5), "VPD", "TAVE", "TAVE", "TAVE"))

plot_shap_tave <- list()

for(sp in sp_TAVE$species){
  dom_bio_sp <- dom_bio %>% 
    filter(DOM_BIO==filter(spe_dom_bio, spe_name==sp)$limit_dom_bio)
  
  plot_shap_tave[[sp]] <- ggplot() +
    geom_sf(data = canada, color = "black") +
    geom_sf(data = US, color = "black") +
    geom_sf(data=quebec, alpha=0) +
    geom_sf(data=filter(data_bi_all, species==sp, Variable==filter(sp_TAVE, species==sp)$Variable), aes(fill = bi_class, color = bi_class)) +
    geom_sf(data=dom_bio_sp, fill=NA, color="red", linewidth=0.5, linetype="solid") +
    scale_fill_manual(values = bi_palette, drop = FALSE) +
    scale_color_manual(values = bi_palette, drop = FALSE) +
    theme(plot.title=element_text(size=70)) +
    ggtitle(case_when(sp=="American elm" ~ "American elm - TAVE",
                      sp=="Black cherry" ~ "Black cherry - TAVE",
                      sp=="Eastern hemlock" ~ "Eastern hemlock - TAVE",
                      sp=="Eastern white pine" ~ "Eastern white pine - TAVE",
                      sp=="Red maple" ~ "Red maple - TAVE",
                      sp=="Red pine" ~ "Red pine - VPD",
                      sp=="Red spruce" ~ "Red spruce - TAVE",
                      sp=="White ash" ~ "White ash - TAVE",
                      sp=="Yellow birch" ~ "Yellow birch - TAVE")) +
    theme_minimal()+
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold")
    )+
    coord_sf(ylim = c(45.1, 49.5),
             xlim = c(-79, -64.5))
  
}

# matrice 3×3
bivariate_legend <- ggplot(legend_data, aes(x = shap_class, y = obs_class, fill = bi_class)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = bi_palette) +
  coord_equal() +
  # remplacer les labels
  scale_x_discrete(labels = c("Negative", "Neutral", "Positive"),
                   guide = guide_axis(n.dodge = 2)) +
  scale_y_discrete(labels = c("Low", "Medium", "High")) +
  labs(x = "SHAP values", y = "Climate observation") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  )

blank <- ggplot() + theme_void()

plot_tave <- ggarrange(plot_shap_tave$`American elm`,
                       plot_shap_tave$`Black cherry`,
                       plot_shap_tave$`Eastern hemlock`,
                       plot_shap_tave$`Eastern white pine`,
                       plot_shap_tave$`Red maple`,
                       plot_shap_tave$`Red spruce`,
                       plot_shap_tave$`White ash`,
                       plot_shap_tave$`Yellow birch`,
                       plot_shap_tave$`Red pine`,
                       bivariate_legend,
                       nrow=5, ncol=2) + 
  bgcolor("white") + 
  border("white")

ggsave(plot=plot_tave, filename=paste0("figures/tave.pdf"), 
       width=7, height=10)

ggsave(plot=plot_tave, filename="figures/tave.png", 
       width=7, height=10, dpi =1000, units="in")


#Figure  SHAP when species have elevation in first importance
el_species <- c("American beech", "Black ash", "Red oak")

plot_shap_el <- list()

for(sp in el_species){
  dom_bio_sp <- dom_bio %>% 
    filter(DOM_BIO==filter(spe_dom_bio, spe_name==sp)$limit_dom_bio)
  
  plot_shap_el[[sp]] <- ggplot() +
    geom_sf(data = canada, color = "black") +
    geom_sf(data = US, color = "black") +
    geom_sf(data=quebec, alpha=0) +
    geom_sf(data=filter(data_bi_all, species==sp, Variable=="elevation"), aes(fill = bi_class, color = bi_class)) +
    geom_sf(data=dom_bio_sp, fill=NA, color="red", linewidth=0.5, linetype="solid") +
    scale_fill_manual(values = bi_palette, drop = FALSE) +
    scale_color_manual(values = bi_palette, drop = FALSE) +
    theme(plot.title=element_text(size=70)) +
    ggtitle(paste0(sp, " - Elevation")) +
    theme_minimal()+
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold")
    )+
    coord_sf(ylim = c(45.1, 49.5),
             xlim = c(-79, -64.5))
}

# matrice 3×3
bivariate_legend <- ggplot(legend_data, aes(x = shap_class, y = obs_class, fill = bi_class)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = bi_palette) +
  coord_equal() +
  # remplacer les labels
  scale_x_discrete(labels = c("Negative", "Neutral", "Positive"),
                   guide = guide_axis(n.dodge = 2)) +
  scale_y_discrete(labels = c("Low", "Medium", "High")) +
  labs(x = "SHAP values", y = "Elevation observation") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  )

plot_el <- ggarrange(plot_shap_el$`American beech`,
                     plot_shap_el$`Black ash`,
                     plot_shap_el$`Red oak`,
                     bivariate_legend,
                     nrow=2, ncol=2) + 
  bgcolor("white")+ border("white")

ggsave(plot=plot_el, filename=paste0("figures/elevation.pdf"), 
       width=7, height=4)
ggsave(plot=plot_el, filename="figures/elevation.png", 
       width=7, height=4, dpi =1000, units="in")

#Figure  SHAP when species have soil in first importance
sp_soil <- data.frame(species=c("Sugar maple", "White cedar", "Striped maple", "Basswood", "American hophornbeam"),
                      variable=c("CEC", "clay", "clay", "pH", "pH"))

plot_shap_soil <- list()

for(sp in sp_soil$species){
  data_bi_all_sp <- filter(data_bi_all, species==sp, Variable==filter(sp_soil,species==sp)$variable)
  
  dom_bio_sp <- dom_bio %>% 
    filter(DOM_BIO==filter(spe_dom_bio, spe_name==sp)$limit_dom_bio)
  
  plot_shap_soil[[sp]] <- ggplot() +
    geom_sf(data = canada, color = "black") +
    geom_sf(data = US, color = "black") +
    geom_sf(data=quebec, alpha=0) +
    geom_sf(data=data_bi_all_sp, aes(fill = bi_class, color = bi_class)) +
    geom_sf(data=dom_bio_sp, fill=NA, color="red", linewidth=0.5, linetype="solid") +
    scale_fill_manual(values = bi_palette, drop = FALSE) +
    scale_color_manual(values = bi_palette, drop = FALSE) +
    theme(plot.title=element_text(size=70)) +
    ggtitle(case_when(sp=="Sugar maple" ~ "Sugar maple - CEC",
                      sp=="White cedar" ~ "White cedar - Clay",
                      sp=="Striped maple" ~ "Striped maple - Clay",
                      sp=="Basswood" ~ "Basswood - pH",
                      sp=="American hophornbeam" ~ "American hophornbeam - pH")) +
    theme_minimal()+
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold")
    )+
    coord_sf(ylim = c(45.1, 49.5),
             xlim = c(-79, -64.5))
}

# matrice 3×3
bivariate_legend <- ggplot(legend_data, aes(x = shap_class, y = obs_class, fill = bi_class)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = bi_palette) +
  coord_equal() +
  # remplacer les labels
  scale_x_discrete(labels = c("Negative", "Neutral", "Positive"),
                   guide = guide_axis(n.dodge = 2)) +
  scale_y_discrete(labels = c("Low", "Medium", "High")) +
  labs(x = "SHAP values", y = "Soil observation") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  )

plot_soil <- ggarrange(plot_shap_soil$`White cedar`,
                       plot_shap_soil$`Striped maple`,
                       plot_shap_soil$`Basswood`, 
                       plot_shap_soil$`American hophornbeam`, 
                       plot_shap_soil$`Sugar maple`,
                       bivariate_legend,
                     nrow=3, ncol=2)+ bgcolor("white")+ border("white")

ggsave(plot=plot_soil, filename=paste0("figures/soil.pdf"), 
       width=7, height=6)
ggsave(plot=plot_soil, filename="figures/soil.png", 
       width=7, height=6, dpi =1000, units="in")

#pour la figure résumé des résultats:
saveRDS(plot_shap_soil$`Sugar maple`, "figures/sugar_maple_CEC_shap.rds")

bivariate_legend <- ggplot(legend_data, aes(x = shap_class, y = obs_class, fill = bi_class)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = bi_palette) +
  coord_equal() +
  # remplacer les labels
  scale_x_discrete(labels = c("Negative", "Neutral", "Positive"),
                   guide = guide_axis(n.dodge = 2)) +
  scale_y_discrete(labels = c("Low", "Medium", "High")) +
  labs(x = "SHAP values", y = "CEC observation") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  )

saveRDS(bivariate_legend, "figures/bivariate_legend.rds")



