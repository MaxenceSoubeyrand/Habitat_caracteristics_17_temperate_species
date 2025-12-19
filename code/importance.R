#Figure 3 showing R² and importance

rm(list=ls())

library(tidyverse)
library(viridis)
library(ggpubr)
theme_set(theme_bw())
library(sf)
library(ggh4x)

imp <- readRDS("results/res_model/all_importance.rds")
R2 <- readRDS("results/res_model/all_R2.rds")

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

imp <- imp %>% 
  group_by(species) %>% 
  mutate(importance=importance/max(importance))



imp$species <- factor(imp$species, levels = spe_name, 
                          labels = spe_name)

imp$variable <- factor(imp$variable  , levels = unique(imp$variable), 
                           labels = unique(imp$variable))

imp$R2 <- ""

group <- data.frame(variable=c("TAVE", "VPD", "spei_severe", "elevation", "slope", "TWI", 
                               "CEC", "clay", "pH",  "origin", "age", "R²"),
                    group=c("Climate", "Climate", "Climate",
                            "Topography", "Topography", "Topography", 
                            "Soil", "Soil", "Soil", 
                            "Stand dynamics", "Stand dynamics", ""))



R2_imp <- R2 %>% 
  mutate(R2=as.character(round(R2,2))) %>% 
  bind_cols(variable="R²", importance=0.5) %>% 
  bind_rows(imp) %>% 
  left_join(group)

R2_imp$variable[R2_imp$variable == "spei_severe"] <- "SPEI"

R2_imp$group <- factor(R2_imp$group  , levels = c("", "Climate","Topography", "Soil", "Stand dynamics"))

R2_imp$variable <- factor(R2_imp$variable  , levels = c("R²", "TAVE", "VPD", "SPEI", "elevation", "slope",
                          "TWI", "CEC", "clay", "pH", "origin", "age"), 
                       labels = c("R²", "TAVE", "VPD", "SPEI", "elevation", "slope",
                                  "TWI", "CEC", "clay", "pH", "origin", "age"))

all_imp_plot <- ggplot(R2_imp, aes(x=interaction(variable, group), y=species, fill=importance)) +
  geom_tile() +
  scale_fill_gradient2(
    "Relative\nimportance",
    low = "dodgerblue3", 
    high = "gold",
    midpoint = 0.5
  ) +
  geom_text(aes(label = R2), color = "black", size = 6) +
  scale_y_discrete(limits=rev)+
  xlab("Variable") + 
  ylab("Species") + 
  theme(strip.text.x = element_text(size = 12),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size=12)) +
  scale_x_discrete(guide = "axis_nested")

ggsave(plot=all_imp_plot, 
       filename="figures/figure_importance.png", 
       width=12, height=7, dpi =1000, units="in")

ggsave(plot=all_imp_plot, 
       filename="figures/figure_importance.pdf", 
       width=12, height=7)

