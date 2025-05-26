#Script for plotting the SHAP values maps and observations
#Create figures 4, 5, 6 and SI map figures
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

shap <- readRDS("~/postdoc/habitat_caracterisation/model/abundance/results_rf/res_model/all_shap.rds")
imp <- readRDS("~/postdoc/habitat_caracterisation/model/abundance/results_rf/res_model/all_importance.rds")

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



quebec <- st_read(dsn = "data/carte_qc/quebec/quebec.shp",
                  layer = "quebec")

canada <- ne_countries(scale = "medium", country = "Canada", returnclass = "sf")
US <- ne_countries(scale = "medium", country = "United States of America", returnclass = "sf")

obs <- shap %>% 
  select(TAVE, VPD, elevation, slope, TWI, CEC, clay, pH) %>% 
  unique()

#Create a grid to map shap values
grid <- st_make_grid(obs, cellsize = 0.1)


intersection <- data.frame(st_intersects(obs, grid)) 

#put observation in the grid
obs_grid <- obs %>%
  st_drop_geometry() %>% 
  mutate(row.id=1:nrow(obs)) %>% 
  right_join(intersection) %>% 
  select(-row.id) %>% 
  rename(pixel_id=col.id) %>% 
  group_by(pixel_id) %>% 
  summarise(across(everything(), median))

grid <- grid[obs_grid$pixel_id]

obs_grid <- st_sf(geometry = grid, obs_grid) %>% 
  pivot_longer(cols=`TAVE`:`pH`, names_to = "Variable", values_to = "Observation")

shap_grid_all <- NULL
id_geometry_all <- NULL


# For each species and each variable, create a SHAp values and observation maps
for (i in spe_name){ #i="White ash"
  #####Shapley Value######
  
  shap_sp <- filter(shap, species==i)
  imp_sp <- filter(imp, species==i)
  
  #Intersect to determine pixel id for each tabvle lines
  intersection <- data.frame(st_intersects(shap_sp, grid)) 
  
  #Median of SHAP values and observation in each pixel
  shap_sp_grid <- shap_sp %>%
    select(contains("shap")) %>% 
    mutate(row.id=1:nrow(shap_sp)) %>% 
    right_join(intersection) %>% 
    select(-row.id) %>% 
    rename(pixel_id=col.id) %>% 
    group_by(pixel_id) %>% 
    summarise(across(TAVE_shap:age_shap, median))
  
  #Remove pixel without values
  grid_sp <- grid[shap_sp_grid$pixel_id]
  
  #Put values in the grid
  shap_grid_med <- st_sf(geometry = grid_sp, shap_sp_grid) %>% 
    pivot_longer(cols=TAVE_shap:age_shap, names_to = "Variable", values_to = "Shapley") %>% 
    mutate(`Shapley`=case_when(`Shapley` < -50 ~ -50,
                               `Shapley` > 50 ~ 50,
                               .default = `Shapley`))
  
  #Keep for plotting the  figures in the manuscript after the for loop 
  shap_grid_sp <- st_drop_geometry(mutate(shap_grid_med, Variable=str_remove_all(Variable, "_shap"))) %>%
    mutate(species=i)
  
  id_geometry_sp <- unique(select(shap_grid_med, pixel_id)) %>%
    mutate(species=i)

  shap_grid_all <- bind_rows(shap_grid_all, shap_grid_sp)
  id_geometry_all <- bind_rows(id_geometry_all, id_geometry_sp)
  
  ######For the SI#######
  plot_med <- list()
  plot_obs <- list()
  p=1

  imp_sp$Variable <- unique(shap_grid_med$Variable)

  var_tri_imp <- imp_sp %>%
    arrange(desc(importance), decreasing=T)

  for(var in unique(var_tri_imp$Variable)){
    shap_grid_var_med <- shap_grid_med  %>%
      filter(Variable==var)

    obs_grid_var <- obs_grid  %>%
      filter(Variable==str_remove(pattern="_shap", string=var))

    plot_med[[p]] <- ggplot()+
      geom_sf(data = canada, color = "black") +
      geom_sf(data = US, color = "black") +
      geom_sf(data=quebec, alpha=0) +
      geom_sf(data=shap_grid_var_med,aes(fill=`Shapley`, color=`Shapley`)) +
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
        geom_sf(data=obs_grid_var, aes(fill=Observation, color=Observation)) +
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

  #median SHAP values
  maps_med <- ggarrange(plot_med[[1]], plot_med[[2]], plot_med[[3]],
                        plot_med[[4]], plot_med[[5]], plot_med[[6]],
                        plot_med[[7]], plot_med[[8]], plot_med[[9]],
                        plot_med[[10]], ncol=3, nrow=4)
  maps_med <-annotate_figure(maps_med, top=i)+ bgcolor("white")

  ggsave(plot=maps_med,
         filename=paste0("results/", i,"/shap_maps_med.png"),
         width=12, height=8)

  #median observation
  maps_med_obs <- ggarrange(plot_obs[[1]], plot_obs[[2]], plot_obs[[3]],
                            plot_obs[[4]], plot_obs[[5]], plot_obs[[6]],
                            plot_obs[[7]], plot_obs[[8]], plot_obs[[9]],
                            plot_obs[[10]], ncol=3, nrow=4)
  maps_med_obs <-annotate_figure(maps_med_obs, top=i)+ bgcolor("white")

  ggsave(plot=maps_med_obs,
         filename=paste0("results/", i,"/maps_med_obs.png"),
         width=12, height=8)


  #Both combined

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
                             ncol=2, nrow=10)


  maps_combined <-annotate_figure(maps_combined, top=i)+ bgcolor("white")

  ggsave(plot=maps_combined, filename=paste0("results/", i,"/maps_combined.png"), width=9, height=8*2.5)

  
  
  maps_combined_2_col <- ggarrange(plot_med[[1]], plot_obs[[1]], plot_med[[6]], plot_obs[[6]],
                                   plot_med[[2]], plot_obs[[2]], plot_med[[7]], plot_obs[[7]],
                                   plot_med[[3]], plot_obs[[3]], plot_med[[8]], plot_obs[[8]],
                                   plot_med[[4]], plot_obs[[4]], plot_med[[9]], plot_obs[[9]],
                                   plot_med[[5]], plot_obs[[5]], plot_med[[10]], plot_obs[[10]],
                                   ncol=4, nrow=5) 
  
  maps_combined_2_col <-annotate_figure(maps_combined_2_col, top=text_grob(i, face = "bold", size = 16))+ 
    bgcolor("white") + border("white")
  
  
  ggsave(plot=maps_combined_2_col, 
         filename=paste0("results/", i,"/SI.png"),
         width=9*2, height=8*2.5/2, 
         dpi=1000, units="in")
  
  ggsave(plot=maps_combined_2_col, 
         filename=paste0("results/", i,"/SI.pdf"),
         width=9*2, height=8*2.5/2)
  

}

#Figure of SHAP values with the species with TAVE ranked first
shap_TAVE <- left_join(shap_grid_all, id_geometry_all) %>% 
  filter(Variable=="TAVE",
         species%in%c("American elm", "Black cherry", "Eastern hemlock", 
                      "Eastern white pine" , "Red maple", "Red spruce", 
                      "White ash","Yellow birch")) %>%  
  st_as_sf(sf_column_name = "geometry")

shap_TAVE %>% filter(species=="White ash")

shap_grid_all %>% filter(species=="White ash",
                         Variable=="TAVE") 
plot_shap_tave <- list()

for(sp in unique(shap_TAVE$species)){
  plot_shap_tave[[sp]] <- ggplot() + 
    geom_sf(data = canada, color = "black") +
    geom_sf(data = US, color = "black") +
    geom_sf(data=quebec, alpha=0) +
    geom_sf(data=filter(shap_TAVE, species==sp), aes(fill=Shapley), color=alpha("grey",0)) +
    scale_fill_gradient2(
      "SHAP\nvalues",
      low = "dodgerblue3", 
      high = "gold",
      midpoint = 0
    ) +
    theme(plot.title=element_text(size=70)) +
    ggtitle(sp)+
    theme_minimal()+
    coord_sf(ylim = c(45.1, 49.5),
             xlim = c(-79, -64.5))
  
}


plot_obs_tave <- ggplot() + 
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data=quebec, alpha=0) + 
  geom_sf(data=filter(obs_grid, Variable=="TAVE"), aes(fill=Observation), color=alpha("grey",0)) +
  scale_fill_viridis("TAVE") +
  theme(plot.title=element_text(size=70)) +
  ggtitle("Observed TAVE (in °C)")+
  theme_minimal()+
  coord_sf(ylim = c(45.1, 49.5),
           xlim = c(-79, -64.5))

plot_tave <- ggarrange(plot_shap_tave$`American elm`,
                       plot_shap_tave$`Black cherry`,
                       plot_shap_tave$`Eastern hemlock`,
                       plot_shap_tave$`Eastern white pine`,
                       plot_shap_tave$`Red maple`,
                       plot_shap_tave$`Red spruce`,
                       plot_shap_tave$`White ash`,
                       plot_shap_tave$`Yellow birch`,
                       plot_obs_tave,
                       nrow=5, ncol=2,
                       align = "hv") + 
  bgcolor("white") + 
  border("white")

ggsave(plot=plot_tave, filename=paste0("figures/tave.pdf"), 
       width=9, height=10)

ggsave(plot=plot_tave, filename="figures/tave.png", 
       width=9, height=10, dpi =1000, units="in")


#Figure of SHAP values with the species with elevation ranked first
shap_el <- left_join(shap_grid_all, id_geometry_all) %>% 
  filter(Variable=="elevation",
         species%in%c("American beech", "Black ash", "Red oak",
                      "Red pine")) %>%  
  st_as_sf(sf_column_name = "geometry")

plot_shap_el <- list()

for(sp in unique(shap_el$species)){
  plot_shap_el[[sp]] <- ggplot() + 
    geom_sf(data = canada, color = "black") +
    geom_sf(data = US, color = "black") +
    geom_sf(data=quebec, alpha=0) + 
    geom_sf(data=filter(shap_el, species==sp), aes(fill=Shapley), color=alpha("grey",0)) +
    scale_fill_gradient2(
      "SHAP\nvalues",
      low = "dodgerblue3", 
      high = "gold",
      midpoint = 0
    ) +
    geom_sf(data=quebec, alpha=0) +
    theme(plot.title=element_text(size=70)) +
    ggtitle(sp)+
    theme_minimal() +
    coord_sf(ylim = c(45.1, 49.5),
             xlim = c(-79, -64.5))
}

plot_obs_el <- ggplot()  + 
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data=quebec, alpha=0) + 
  geom_sf(data=filter(obs_grid, Variable=="elevation"), aes(fill=Observation), color=alpha("grey",0)) +
  scale_fill_viridis("Elevation") +
  geom_sf(data=quebec, alpha=0) +
  theme(plot.title=element_text(size=70)) +
  ggtitle("Observed elevation (in m)")+
  theme_minimal()+
  coord_sf(ylim = c(45.1, 49.5),
           xlim = c(-79, -64.5)) 

plot_el <- ggarrange(plot_shap_el$`American beech`,
                     plot_shap_el$`Black ash`,
                     plot_shap_el$`Red oak`,
                     plot_shap_el$`Red pine`, 
                     plot_obs_el, 
                     nrow=3, ncol=2,
                     align = "hv")+ bgcolor("white")+ border("white")

ggsave(plot=plot_el, filename=paste0("figures/el.pdf"), 
       width=9, height=5.5)
ggsave(plot=plot_el, filename="figures/el.png", 
       width=9, height=5.5, dpi =1000, units="in")

#Figure of SHAP values with the species with soil variables ranked first
shap_soil <- left_join(shap_grid_all, id_geometry_all) %>%
  filter((species == "Sugar maple" & Variable == "CEC") |
           (species == "White cedar" & Variable == "clay") |
           (species == "Striped maple" & Variable == "clay") |
           (species == "Basswood" & Variable == "pH")|
           (species == "American hophornbeam" & Variable == "pH")) %>%  
  st_as_sf(sf_column_name = "geometry")

plot_shap_soil <- list()
plot_obs_soil <- list()

for(sp in unique(shap_soil$species)){
  plot_shap_soil[[sp]] <- ggplot() + 
    geom_sf(data = canada, color = "black") +
    geom_sf(data = US, color = "black") +
    geom_sf(data=quebec, alpha=0) + 
    geom_sf(data=filter(shap_soil, species==sp), aes(fill=Shapley), color=alpha("grey",0)) +
    scale_fill_gradient2(
      "SHAP\nvalues",
      low = "dodgerblue3", 
      high = "gold",
      midpoint = 0
    ) +
    geom_sf(data=quebec, alpha=0) +
    theme(plot.title=element_text(size=70)) +
    ggtitle(case_when(sp=="Sugar maple" ~ "Sugar maple - CEC",
                      sp=="White cedar" ~ "White cedar - Clay",
                      sp=="Striped maple" ~ "Striped maple - Clay",
                      sp=="Basswood" ~ "Basswood - pH",
                      sp=="American hophornbeam" ~ "American hophornbeam - pH"))+
    theme_minimal()+
    coord_sf(ylim = c(45.1, 49.5),
             xlim = c(-79, -64.5)) 
}



plot_obs_soil_CEC <- ggplot() + 
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data=quebec, alpha=0) + 
  geom_sf(data=filter(obs_grid, Variable=="CEC"), aes(fill=Observation), color=alpha("grey",0)) +
  scale_fill_viridis("CEC") +
  geom_sf(data=quebec, alpha=0) +
  theme(plot.title=element_text(size=70)) +
  ggtitle(expression("Observed CEC (cmol·kg"^-1*")"))+
  theme_minimal()+
  coord_sf(ylim = c(45.1, 49.5),
           xlim = c(-79, -64.5)) 

plot_obs_soil_clay <- ggplot() + 
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data=quebec, alpha=0) + 
  geom_sf(data=filter(obs_grid, Variable=="clay"), aes(fill=Observation), color=alpha("grey",0)) +
  scale_fill_viridis("Clay") +
  geom_sf(data=quebec, alpha=0) +
  theme(plot.title=element_text(size=70)) +
  ggtitle('Observed clay %')+
  theme_minimal()+
  coord_sf(ylim = c(45.1, 49.5),
           xlim = c(-79, -64.5))



plot_obs_soil_pH <- ggplot() + 
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data=quebec, alpha=0) + 
  geom_sf(data=filter(obs_grid, Variable=="pH"), aes(fill=Observation), color=alpha("grey",0)) +
  scale_fill_viridis("pH") +
  geom_sf(data=quebec, alpha=0) +
  theme(plot.title=element_text(size=70)) +
  ggtitle(expression("Observed pH"))+
  theme_minimal()+
  coord_sf(ylim = c(45.1, 49.5),
           xlim = c(-79, -64.5))

blank <- ggplot() + theme_void()

clay_plots <- ggarrange(
  ggarrange(plot_shap_soil$`Striped maple`, plot_shap_soil$`White cedar`, blank, ncol = 1,
            heights = c(1, 1, 0)),
  ggarrange(blank, plot_obs_soil_clay, blank, ncol = 1,
            heights = c(0.5, 1, 0.5)),
  ncol = 2
)

CEC_plots <- ggarrange(plot_shap_soil$`Sugar maple`,
                       plot_obs_soil_CEC, nrow=1)

pH_plots <- ggarrange(
  ggarrange(plot_shap_soil$`American hophornbeam`, plot_shap_soil$`Basswood`, blank, ncol = 1,
            heights = c(1, 1, 0)),
  ggarrange(blank, plot_obs_soil_pH, blank, ncol = 1,
            heights = c(0.5, 1, 0.5)),
  ncol = 2
)

plot_soil <- ggarrange(CEC_plots,
                       pH_plots,
                       clay_plots,
                       nrow=3, ncol=1,
                       heights=c(0.2,0.4,0.4)) + bgcolor("white")+ border("white")


ggsave(plot=plot_soil, filename=paste0("figures/soil.pdf"), 
       width=9, height=9.5)
ggsave(plot=plot_soil, filename="figures/soil.png", 
       width=9, height=9.5, dpi =1000, units="in")

#Save plot for the summary figure (figure 7)
plot_shap_soil$`Sugar maple`

saveRDS(plot_shap_soil$`Sugar maple`, "figures/sugar_maple_CEC_shap.rds")



