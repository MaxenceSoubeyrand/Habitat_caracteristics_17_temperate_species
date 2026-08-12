#Figure 2

rm(list=ls())

library(tidyverse)
library(viridis)
library(ggpubr)
theme_set(theme_bw())
library(sf)
library(ggh4x)

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
  

imp <- readRDS("results/res_model/all_importance.rds") 

R2 <- readRDS("results/res_model/all_R2.rds")

imp <- imp %>% 
  group_by(species) %>% 
  mutate(importance=importance/max(importance)) %>% 
  select(-species)



imp$species <- factor(imp$species, levels = spe_latin, 
                          labels = spe_latin)

imp$variable <- factor(imp$variable  , levels = unique(imp$variable), 
                           labels = unique(imp$variable))

imp$R2 <- ""

group <- data.frame(variable=c("TAVE", "VPD", "elevation", "slope", "TWI", 
                               "CEC", "clay", "pH",  "origin", "perturbation", "age", "R²"),
                    group=c("Climate", "Climate",
                            "Topography", "Topography", "Topography", 
                            "Soil", "Soil", "Soil", 
                            "Stand dynamics", "Stand dynamics", "Stand dynamics", ""))



R2_imp <- R2 %>% 
  mutate(R2=as.character(round(R2,2))) %>% 
  bind_cols(variable="R²", importance=0.5) %>% 
  bind_rows(imp) %>% 
  left_join(group)

R2_imp$group <- factor(R2_imp$group  , levels = c("", "Climate","Topography", "Soil", "Stand dynamics"))

R2_imp$variable <- factor(R2_imp$variable  , levels = c("R²", "TAVE", "VPD", "elevation", "slope",
                          "TWI", "CEC", "clay", "pH", "origin", "perturbation", "age"), 
                       labels = c("R²", "TAVE", "VPD", "Elevation", "Slope",
                                  "TWI", "CEC", "Clay", "pH", "Origin", "Perturb", "Age"))

all_imp_plot <- ggplot(R2_imp, aes(x=interaction(variable, group), y=species, fill=importance)) +
  geom_tile() +
  scale_fill_gradient2(
    "Relative\nimportance",
    low = "dodgerblue3", 
    high = "gold",
    midpoint = 0.5,
    guide = guide_colorbar(
      barwidth = 15,  
      barheight = 1.5 
    )
  ) +
  geom_text(aes(label = R2), color = "black", size = 6) +
  scale_y_discrete(limits=rev)+
  xlab("Variable") + 
  ylab("Species") + 
  theme(strip.text.x = element_text(size = 12),
        axis.text.y = element_text(face = "italic"),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.title = element_text(margin = margin(r=25, b = 0), size = 16),
        legend.text = element_text(size=14),
        legend.position = "bottom") +
  scale_x_discrete(guide = "axis_nested")

imp_variable <- R2_imp %>% 
  filter(variable != "R²") %>% 
  group_by(group, variable) %>% 
  summarise(
    n_high = sum(importance > 0.5, na.rm = TRUE),
    n_low  = sum(importance <= 0.5, na.rm = TRUE),
    .groups = "drop"
  )

imp_long <- imp_variable %>% 
  tidyr::pivot_longer(cols = c(n_high, n_low),
                      names_to = "class",
                      values_to = "count") %>% 
  mutate(
    count_signed = ifelse(class == "n_low", -count, count),
    class = recode(class,
                   n_high = "Relative importance > 0.5",
                   n_low  = "Relative importance  ≤ 0.5")
  )

importance_class <- ggplot(imp_long, aes(y = variable, x = count_signed, fill = class)) +
  geom_col() +
  labs(y = "Variable",
       x = "# species",
       fill = NULL) +
  scale_fill_manual(values = c(
    "Relative importance > 0.5" = "#d73027",
    "Relative importance  ≤ 0.5" = "#4575b4"
  ), guide = guide_legend(ncol = 1)) +
  theme_minimal()+
  theme(strip.text.x = element_text(size = 12),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size=14),
        legend.position = "bottom")


all_imp_plot_class <- ggarrange(all_imp_plot, importance_class, nrow=1, ncol=2, 
          widths = c(0.7,0.3), 
          labels=c("A", "B"),
          font.label = list(size = 18, color = "black", face = "bold", family = NULL))+
  theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave(plot=all_imp_plot_class, 
       filename="figures/figure_importance.png", 
       width=16, height=8, dpi =1000, units="in")