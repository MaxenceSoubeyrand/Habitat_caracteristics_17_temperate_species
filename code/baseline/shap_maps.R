#Figure 3 and 4

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

#Availbale upon request: maxence.soubeyrand@uqat.ca
shap <- readRDS("results/res_model/all_shap_values.rds") 


imp <- readRDS("results/res_model/all_importance.rds") 

#Table with the bioclimatic domain where the northern population lies.
spe_dom_bio <- data.frame(spe_name=spe_latin,
                          limit_dom_bio=c(5,4,4,4,4,
                                          3,4,3,4,4,
                                          3,3,3,4,3,
                                          3,3))

quebec <- st_read(dsn = "data/quebec/quebec.shp",
                  layer = "quebec")

canada <- ne_countries(scale = "medium", country = "Canada", returnclass = "sf")
US <- ne_countries(scale = "medium", country = "United States of America", returnclass = "sf")

obs <- shap %>% 
  dplyr::select(TAVE, VPD, elevation, slope, TWI, CEC, clay, pH) %>% 
  unique()

district <- st_transform(district, crs = st_crs(obs))
intersection <- data.frame(st_intersects(obs, district)) 

district$id <- 1:nrow(district)

obs_district <- obs %>%
  st_drop_geometry() %>% 
  mutate(row.id=1:nrow(obs)) %>% 
  right_join(intersection) %>% 
  dplyr::select(-row.id) %>% 
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

for (i in spe_latin){
  #####Shapley Value######
  print(i)
  
  shap_sp <- filter(shap, species==i)
  sum(is.na(shap_sp$TAVE_shap))
  imp_sp <- filter(imp, species==i)
  
  intersection <- data.frame(st_intersects(shap_sp, district)) 
  
  #Median of SHAP values
  shap_sp_district <- shap_sp %>%
    dplyr::select(contains("shap")) %>% 
    mutate(row.id=1:nrow(shap_sp)) %>% 
    right_join(intersection) %>% 
    dplyr::select(-row.id) %>% 
    rename(pixel_id=col.id) %>% 
    group_by(pixel_id) %>% 
    summarise(across(TAVE_shap:age_shap, median))
  
  #districts with at least one value
  district_sp <- filter(district, id %in% shap_sp_district$pixel_id)
  
  #Values in the district
  shap_district_med <- left_join(district_sp, st_drop_geometry(shap_sp_district),
                                 by = c("id" = "pixel_id")) %>% 
    pivot_longer(cols=TAVE_shap:age_shap, names_to = "Variable", values_to = "Shapley") %>% 
    mutate(`Shapley`=case_when(`Shapley` < -50 ~ -50,
                               `Shapley` > 50 ~ 50,
                               .default = `Shapley`))
  
  shap_district_sp <- st_drop_geometry(mutate(shap_district_med, Variable=str_remove_all(Variable, "_shap"))) %>%
    mutate(species=i)
  
  id_geometry_sp <- unique(dplyr::select(shap_district_med, id)) %>%
    mutate(species=i)
  
  shap_district_all <- bind_rows(shap_district_all, shap_district_sp)
  id_geometry_all <- bind_rows(id_geometry_all, id_geometry_sp)
  
  ######SI#######
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
        ylab("SHAP value")
    }
    if(var %in% c("perturbation_shap")){

      plot_obs[[p]] <- ggplot(shap_sp, aes(x=perturbation, y=perturbation_shap)) +
        geom_boxplot() +
        ggtitle(str_remove(pattern="_shap", string=var))+
        ylab("SHAP value")
    }
    if(var %in% c("age_shap")){
      
      plot_obs[[p]] <- ggplot(shap_sp, aes(x=age, y=age_shap)) +
        geom_boxplot() +
        ggtitle(str_remove(pattern="_shap", string=var))+
        ylab("SHAP value")
    }
    if(!var %in% c("origin_shap", "perturbation_shap", "age_shap")){
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


  maps_combined <- ggarrange(plot_med[[1]], plot_obs[[1]],
                             plot_med[[2]], plot_obs[[2]],
                             plot_med[[3]], plot_obs[[3]],
                             plot_med[[4]], plot_obs[[4]],
                             plot_med[[5]], plot_obs[[5]],
                             plot_med[[6]], plot_obs[[6]],
                             plot_med[[7]], plot_obs[[7]],
                             plot_med[[8]], plot_obs[[8]],
                             plot_med[[9]], plot_obs[[9]],
                             plot_med[[10]], plot_obs[[10]],
                             plot_med[[11]], plot_obs[[11]],
                             plot_med[[12]], plot_obs[[12]],
                             ncol=2, nrow=12)


  maps_combined <-annotate_figure(maps_combined, top=i) + 
    bgcolor("white")

  ggsave(plot=maps_combined, 
         filename=paste0("results/", i,"/maps_shap_obs.png"),
         width=9, height=12*1.818)

  
  
  #Plot obs vs shap
  obs_shap <- shap_district_sp %>% left_join(obs_district)
  
  obs_shap_all <- bind_rows(obs_shap_all, dplyr::select(st_drop_geometry(obs_shap), -geometry))
  
  plot_obs_shap <- ggplot(filter(obs_shap, !Variable %in% c("age", "origin", "perturbation")), aes(x=Observation, y=Shapley))+
    geom_point() +
    facet_wrap(~Variable, scales="free", ncol=3, nrow=3)+
    geom_smooth(method = "gam") +
    labs(title=bquote(italic(.(i))), y="SHAP values")+
    theme(plot.title = element_text(hjust = 0.5, size=12))
  
  ggsave(plot=plot_obs_shap, 
         filename=paste0("~/postdoc/habitat_caracterisation/model/abundance/results_rf_perturbation/", i,"/plot_obs_shap.png"), 
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
  
  bi_palette <- c( "Low_O_Low_S" = "#e8e8e8", 
                   "Med_O_Low_S" = "#dc8ecd", 
                   "High_O_Low_S" = "#c900a1", 
                   
                   "Low_O_Med_S" = "#96e3e3", 
                   "Med_O_Med_S" = "#968ecd", 
                   "High_O_Med_S" = "#9600a1", 
                   
                   "Low_O_High_S" = "#00d9d9", 
                   "Med_O_High_S" = "#008ecd", 
                   "High_O_High_S" = "#0000a1" )
  
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
    setdiff(c("age", "origin", "perturbation"))
  
  data_bi$Variable <- factor(
    data_bi$Variable,
    levels = order_var
    )
  
  map_all <- ggplot() +
    geom_sf(data = canada, color = "black", alpha=0) +
    geom_sf(data = US, color = "black", alpha=0) +
    geom_sf(data=quebec, alpha=0, color = "grey0") +
    geom_sf(data=data_bi, aes(fill = bi_class, color = bi_class)) +
    geom_sf(data=dom_bio_sp, fill=NA, color="red", linewidth=0.5, linetype="solid") +
    scale_fill_manual(values = bi_palette, drop = FALSE) +
    scale_color_manual(values = bi_palette, drop = FALSE) +
    facet_wrap(~Variable, ncol = 3, nrow=3) +
    theme_minimal()+
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold"),
      panel.grid = element_blank()
    )+
    coord_sf(ylim = c(45.1, 49.5),
             xlim = c(-79, -64.5))
  
  library(patchwork)
  
  final_plot <- ggarrange(map_all, bivariate_legend,widths = c(7.5, 1.5))
  
  #Plot origine, perturbation and age
  p1 <- plot_obs[[which(var_tri_imp$Variable=="origin_shap")]]
  p2 <- plot_med[[which(var_tri_imp$Variable=="origin_shap")]]
  
  p3 <- plot_obs[[which(var_tri_imp$Variable=="age_shap")]]
  p4 <- plot_med[[which(var_tri_imp$Variable=="age_shap")]]
  
  p5 <- plot_obs[[which(var_tri_imp$Variable=="perturbation_shap")]]
  p6 <- plot_med[[which(var_tri_imp$Variable=="perturbation_shap")]]
  
  bottom_row <- ggarrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3)
  
  final_plot2 <- ggarrange(final_plot, bottom_row, 
                           ncol=1, nrow=2,
                           heights = c(8, 8))
  

  
  obs_shap_dom_bio <- obs_shap %>%
    add_count(DOM_BIO) %>%      
    filter(n >= 20) %>%        
    dplyr::select(-n) 
  
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
         width=14, height=10)
}

##################################################################
######SHAP figures with species with TAVE the most important######
##################################################################

sp_TAVE <- data.frame(
  species = c(
    "Ulmus americana",      # American elm
    "Prunus serotina",      # Black cherry
    "Tsuga canadensis",     # Eastern hemlock
    "Pinus strobus",        # Eastern white pine
    "Acer rubrum",          # Red maple
    "Pinus resinosa",       # Red pine
    "Picea rubens",         # Red spruce
    "Fraxinus americana",   # White ash
    "Betula alleghaniensis" # Yellow birch
  ),
  Variable = c(rep("TAVE", 5), "VPD", "TAVE", "TAVE", "TAVE")
)

plot_shap_tave <- list()

for(sp in sp_TAVE$species){
  dom_bio_sp <- dom_bio %>% 
    filter(DOM_BIO==filter(spe_dom_bio, spe_name==sp)$limit_dom_bio)
  
  species_labels <- c(
    "Ulmus americana" = "Ulmus~~americana - TAVE",
    "Prunus serotina" = "Prunus~~serotina - TAVE",
    "Tsuga canadensis" = "Tsuga~~canadensis - TAVE",
    "Pinus strobus" = "Pinus~~strobus - TAVE",
    "Acer rubrum" = "Acer~~rubrum - TAVE",
    "Pinus resinosa" = "Pinus~~resinosa - VPD",
    "Picea rubens" = "Picea~~rubens - TAVE",
    "Fraxinus americana" = "Fraxinus~~americana - TAVE",
    "Betula alleghaniensis" = "Betula~~alleghaniensis - TAVE"
  )
  
  sci_name <- gsub("'", "\\'", strsplit(species_labels[sp], " - ")[[1]][1]) 
  com_name <- gsub("'", "\\'", strsplit(species_labels[sp], " - ")[[1]][2])
  
  title_text <- paste0("italic(", sci_name, ")~-~", com_name)
  
  plot_shap_tave[[sp]] <- ggplot() +
    geom_sf(data=filter(data_bi_all, species==sp, Variable==filter(sp_TAVE, species==sp)$Variable), aes(fill = bi_class, color = bi_class)) +
    geom_sf(data = canada, color = "black", alpha=0) +
    geom_sf(data = US, color = "black", alpha=0) +
    geom_sf(data=quebec, alpha=0, color = "grey0") +
    geom_sf(data=dom_bio_sp, fill=NA, color="red", linewidth=0.5, linetype="solid") +
    scale_fill_manual(values = bi_palette, drop = FALSE) +
    scale_color_manual(values = bi_palette, drop = FALSE)+
    coord_sf(ylim = c(45.1, 49.5),
             xlim = c(-79, -64.5)) +
    annotate(
      "text",
      x = -79 - 0.02 * diff(c(-79, -64.5)),  
      y = 49.5 + 0.03 * diff(c(45.1, 49.5)),  
      label = title_text,
      hjust = 0, vjust = 1,  
      size = 6,              
      fontface = "bold",
      parse=T
    ) +
    theme_minimal()+
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
}

bi_palette <- c( "Low_O_Low_S" = "#e8e8e8", 
                 "Med_O_Low_S" = "#dc8ecd", 
                 "High_O_Low_S" = "#c900a1", 
                 
                 "Low_O_Med_S" = "#96e3e3", 
                 "Med_O_Med_S" = "#968ecd", 
                 "High_O_Med_S" = "#9600a1", 
                 
                 "Low_O_High_S" = "#00d9d9", 
                 "Med_O_High_S" = "#008ecd", 
                 "High_O_High_S" = "#0000a1" )

# matrice 3×3
bivariate_legend <- ggplot(legend_data, aes(x = shap_class, y = obs_class, fill = bi_class)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = bi_palette) +
  coord_equal() +
  scale_x_discrete(labels = c("Negative", "Neutral", "Positive")) +
  scale_y_discrete(labels = c("Low", "Medium", "High")) +
  labs(x = "SHAP values", y = "Climate\nobservation") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    panel.grid = element_blank()

  )

blank <- ggplot() + theme_void()

show_axes <- function(plot, x = TRUE, y = TRUE) {
  p <- plot +
    theme(
      # Axe X
      axis.text.x  = if (x) element_text() else element_blank(),
      axis.ticks.x = if (x) element_line() else element_blank(),

      # Axe Y
      axis.text.y  = if (y) element_text() else element_blank(),
      axis.ticks.y = if (y) element_line() else element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      panel.spacing = unit(0, "pt"),
      axis.title = element_blank()
    )
  
  print(p)
}

library(cowplot)

empty_plot <- ggplot() +
  theme_void() +             
  theme(panel.background = element_rect(fill = "white", color="white")) 


plot_tave <- ggarrange(
  show_axes(plot_shap_tave$`Ulmus americana`, x=F, y=T),
  show_axes(plot_shap_tave$`Prunus serotina`, x=F, y=F),
  show_axes(plot_shap_tave$`Tsuga canadensis`, x=F, y=F),
  show_axes(plot_shap_tave$`Pinus strobus`, x=F, y=T),
  show_axes(plot_shap_tave$`Acer rubrum`, x=F, y=F),
  show_axes(plot_shap_tave$`Picea rubens`, x=F, y=F),
  show_axes(plot_shap_tave$`Fraxinus americana`, x=T, y=T),
  show_axes(plot_shap_tave$`Betula alleghaniensis`, x=T, y=F),
  show_axes(plot_shap_tave$`Pinus resinosa`, x=T, y=F),
  empty_plot, bivariate_legend, empty_plot,
  nrow=4, ncol=3,
  widths=c(1.06, 1, 1),
  heights=c(1, 1, 1.05)
) + 
  bgcolor("white") + 
  border("white")

ggsave(plot=plot_tave, filename="figures/tave.png", 
       width=16.5, height=10, dpi =1000, units="in")


##################################################################
###SHAP figures with species with elevation the most important####
##################################################################
el_species <- c("Fagus grandifolia",   # American beech
                "Quercus rubra") 

plot_shap_el <- list()

for(sp in el_species){
  dom_bio_sp <- dom_bio %>% 
    filter(DOM_BIO==filter(spe_dom_bio, spe_name==sp)$limit_dom_bio)
  
  species_labels <- c(
    "Fagus grandifolia" = "Fagus~~grandifolia - Elevation",
    "Quercus rubra" = "Quercus~~rubra - Elevation")
  
 
  sci_name <- gsub("'", "\\'", strsplit(species_labels[sp], " - ")[[1]][1])  # échappe les apostrophes
  com_name <- gsub("'", "\\'", strsplit(species_labels[sp], " - ")[[1]][2])
  

  title_text <- paste0("italic(", sci_name, ")~-~", com_name)
  
  plot_shap_el[[sp]] <- ggplot() +
    geom_sf(data=filter(data_bi_all, species==sp, Variable=="elevation"), aes(fill = bi_class, color = bi_class)) +
    geom_sf(data = canada, color = "black", alpha=0) +
    geom_sf(data = US, color = "black", alpha=0) +
    geom_sf(data=quebec, alpha=0, color = "grey0") +
    geom_sf(data=filter(data_bi_all, species==sp, Variable=="elevation"), aes(fill = bi_class, color = bi_class)) +
    geom_sf(data=dom_bio_sp, fill=NA, color="red", linewidth=0.5, linetype="solid") +
    scale_fill_manual(values = bi_palette, drop = FALSE) +
    scale_color_manual(values = bi_palette, drop = FALSE) +
    annotate(
      "text",
      x = -79 - 0.02 * diff(c(-79, -64.5)),  
      y = 49.5 + 0.03 * diff(c(45.1, 49.5)),
      label = title_text,
      hjust = 0, vjust = 1,  
      size = 6,              
      fontface = "bold", 
      parse=T
    ) +
    theme_minimal()+
    theme(
      legend.position = "none",
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9, face = "bold"),
      panel.grid = element_blank()
    )+
    coord_sf(ylim = c(45.1, 49.5),
             xlim = c(-79, -64.5))
}

##################################################################
######SHAP figures with species with soil the most important######
##################################################################

sp_soil <- data.frame(
  species = c(
    "Acer saccharum",        # Sugar maple
    "Thuja occidentalis",    # White cedar
    "Acer pensylvanicum",    # Striped maple
    "Fraxinus nigra",
    "Tilia americana",       # Basswood
    "Ostrya virginiana"      # American hophornbeam
  ),
  variable = c("CEC", "clay", "clay", "clay", "pH", "pH")
)

plot_shap_soil <- list()

for(sp in sp_soil$species){ 
  data_bi_all_sp <- filter(data_bi_all, species==sp, Variable==filter(sp_soil,species==sp)$variable)
  
  dom_bio_sp <- dom_bio %>% 
    filter(DOM_BIO==filter(spe_dom_bio, spe_name==sp)$limit_dom_bio)
  
  species_labels <- c(
    "Acer saccharum" = "Acer~~saccharum - CEC",
    "Thuja occidentalis" = "Thuja~~occidentalis - Clay",
    "Acer pensylvanicum" = "Acer~~pensylvanicum - Clay",
    "Fraxinus nigra" = "Fraxinus~~nigra - Clay",
    "Tilia americana" = "Tilia~~americana - pH",
    "Ostrya virginiana" = "Ostrya~~virginiana - pH"
  )
  

  sci_name <- gsub("'", "\\'", strsplit(species_labels[sp], " - ")[[1]][1])  
  com_name <- gsub("'", "\\'", strsplit(species_labels[sp], " - ")[[1]][2])
  
  title_text <- paste0("italic(", sci_name, ")~-~", com_name)
  
  
  plot_shap_soil[[sp]] <- ggplot() +
    geom_sf(data=data_bi_all_sp, aes(fill = bi_class, color = bi_class)) +
    geom_sf(data = canada, color = "black", alpha=0) +
    geom_sf(data = US, color = "black", alpha=0) +
    geom_sf(data=quebec, alpha=0, color = "grey0") +
    geom_sf(data=dom_bio_sp, fill=NA, color="red", linewidth=0.5, linetype="solid") +
    scale_fill_manual(values = bi_palette, drop = FALSE) +
    scale_color_manual(values = bi_palette, drop = FALSE) +
    annotate(
      "text",
      x = -79 - 0.02 * diff(c(-79, -64.5)),  
      y = 49.5 + 0.03 * diff(c(45.1, 49.5)),  
      label = title_text,
      hjust = 0, vjust = 1,   
      size = 6,              
      fontface = "bold",
      parse=T
    ) +
    theme_minimal()+
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold"),
      panel.grid = element_blank()
    )+
    coord_sf(ylim = c(45.1, 49.5),
             xlim = c(-79, -64.5))
}

# matrice 3×3
bivariate_legend <- ggplot(legend_data, aes(x = shap_class, y = obs_class, fill = bi_class)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = bi_palette) +
  coord_equal() +
  scale_x_discrete(labels = c("Negative", "Neutral", "Positive")) +
  scale_y_discrete(labels = c("Low", "Medium", "High")) +
  labs(x = "SHAP values", y = "Observation\nvalues") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    panel.grid = element_blank()
  )

plot_shap_el_soil <- ggarrange(
  show_axes(plot_shap_el$`Fagus grandifolia`, x=F, y=T),
  show_axes(plot_shap_el$`Quercus rubra`, x=F, y=F),
  show_axes(plot_shap_soil$`Thuja occidentalis`, x=F, y=T),
  show_axes(plot_shap_soil$`Acer pensylvanicum`, x=F, y=F),
  show_axes(plot_shap_soil$`Fraxinus nigra`, x=F, y=F),
  show_axes(plot_shap_soil$`Tilia americana`, x=F, y=F), 
  show_axes(plot_shap_soil$`Ostrya virginiana`, x=T, y=T), 
  show_axes(plot_shap_soil$`Acer saccharum`, x=T, y=F),
  bivariate_legend,
  nrow=3, ncol=3
) + 
  bgcolor("white") + 
  border("white")



ggsave(plot=plot_shap_el_soil, filename="figures//el_soil.png", 
       width=16.5, height=7.5, dpi =1000, units="in")

ggsave(plot=plot_shap_el_soil, filename=paste0("figures/el_soil.pdf"), 
       width=16.5, height=7.5)


#For the summary figures (figure 5)
#Availbale upon request: maxence.soubeyrand@uqat.ca
saveRDS(plot_shap_soil$`Acer saccharum`, "figures/sugar_maple_CEC_shap.rds")

bivariate_legend <- ggplot(legend_data, aes(x = shap_class, y = obs_class, fill = bi_class)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = bi_palette) +
  coord_equal() +
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
