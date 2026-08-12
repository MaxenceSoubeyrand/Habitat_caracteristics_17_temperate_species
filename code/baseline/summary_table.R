#Figure 5

rm(list=ls())

library(ggplot2)
library(reshape2)
library(dplyr)
library(ggh4x)
library(ggtext)

#1: increase northward abundance, -1 decrease. 
df <- data.frame(
  Species = c("Acer rubrum",
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
               "Ulmus americana"),
  pH = c(1, 1, 0, 0, 0, 0, "west", 1, 0, 0, 0, -1, 0, 0, 0, 0, 0),
  TAVE = c(-1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1),
  Age = c(0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Clay = c(0, "west", 0, 0, 0, 1, 1, -1, 1, -1, 0, 1, -1, 0, -1, 0, 0),
  CEC = c("west", 0, "east", "east", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0),
  Elevation = c(0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0),
  VPD = c(0, "east", 0, 0, "east", 0, -1, 0, "west", "east", 0, 0, 0, 0, 0, 0, 0),
  TWI = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Slope = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  Origin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  Perturb = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  SPEI = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

group <- data.frame(variable=c("TAVE", "VPD", "Elevation", "Slope", "TWI", 
                               "CEC", "Clay", "pH",  "Origin", "Perturb", "Age"),
                    group=c("Climate", "Climate",
                            "Topography", "Topography", "Topography", 
                            "Soil", "Soil", "Soil", 
                            "Stand dynamics", "Stand dynamics", "Stand dynamics"))

df_melt <- melt(df, id.vars = "Species") %>% 
  left_join(group)
  
df_melt$group <- factor(df_melt$group  , levels = c("Climate","Topography", "Soil", "Stand dynamics"))

df_melt$variable <- factor(df_melt$variable  , levels = c("TAVE", "VPD", "Elevation", "Slope",
                                                    "TWI", "CEC", "Clay", "pH", "Origin", "Perturb", "Age"), 
                      labels = c("TAVE", "VPD", "Elevation", "Slope",
                                 "TWI", "CEC", "Clay", "pH", "Origin", "Perturb", "Age"))

df_melt$value <- factor(df_melt$value, levels = c(-1, 0, 1, "east", "west"), labels = c(
  "**negatively**", 
  "no correlation",
  "**positively**",
  "**negatively** in West, **positively** in East",
  "**positively** in West, **negatively** in East"
))

df_melt$group <- factor(df_melt$group  , levels = c("Climate","Topography", "Soil", "Stand dynamics"))

df_melt <- df_melt %>%
  mutate(
    label = case_when(
      value == "**positively**" ~ "+",
      value == "**negatively**" ~ "-",
      value == "**negatively** in West, **positively** in East" ~ "-/+",
      value == "**positively** in West, **negatively** in East" ~ "+/-",
      TRUE ~ ""
    )
  )

plot <- ggplot(df_melt, aes(x = interaction(variable, group), y = Species, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("**negatively**" = "dodgerblue3", 
               "no correlation" = "white",
               "**positively**" = "gold",
               "**negatively** in West, **positively** in East" = "purple",
               "**positively** in West, **negatively** in East" = "grey40"),
    breaks = c("**negatively**",
               "**positively**",
               "**negatively** in West, **positively** in East",
               "**positively** in West, **negatively** in East")
  ) +
  geom_text(aes(label = label), size = 8, color = "black") +
  labs(x = "Variable",
       y = "Species",
       fill = NULL) +
  scale_y_discrete(limits=rev) +
  theme(legend.position = "right", 
        axis.text.y = element_text(face = "italic"),
        legend.title.position = "top",
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.title = element_text(size = 16, hjust = 0),
        legend.text = element_markdown(size=14)) +               
  guides(
    fill = guide_legend(
      title = "Northern populations abundance\ncovaries with variable contribution",
      title.position = "top",
      title.hjust = 0 
    )
  )+
  scale_x_discrete(guide = "axis_nested")

plot

#Availbale upon request: maxence.soubeyrand@uqat.ca
ers_shap <- readRDS("figures/sugar_maple_CEC_shap.rds") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size=12))

#Availbale upon request: maxence.soubeyrand@uqat.ca
bivariate_legend <- readRDS("figures/bivariate_legend.rds")

blank <- ggplot() + theme_void()

ers_shap_legend <- ggarrange(blank, ggarrange(ers_shap, bivariate_legend, 
                                              ncol=2, nrow=1,
                                              widths=c(3,1)), blank, 
                             ncol=3, widths=c(1,5,1))

plot_ers <- ggarrange(plot, ers_shap_legend, ncol=1, heights=c(0.7, 0.3), 
                      labels=c("A", "B"), 
                      label.x = c(0,0.1), label.y = c(1,1),
                      font.label = list(size = 16)) +
  bgcolor("white") + border("white")



ggsave(plot=plot_ers, filename="figures/summary.png",
       width=14, height=10, dpi =1000, units="in")

ggsave(plot=plot_ers, filename="figures/summary.pdf",
       width=14, height=10)
