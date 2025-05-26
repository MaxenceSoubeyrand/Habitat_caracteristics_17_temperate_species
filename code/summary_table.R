#Script to plot the summary figure (Figure 7)
rm(list=ls())

library(ggplot2)
library(reshape2)
library(dplyr)
library(ggh4x)
library(ggtext)


# Create data frame
#1: increase: abundance at north, -1: decrease, west: increase at north west, east: increase at north east
df <- data.frame(
  Species = c("Red maple", "Yellow birch", "Sugar maple", "White cedar", "Red spruce", 
              "American beech", "Eastern white pine", "Eastern hemlock", "Black ash", 
              "Striped maple", "Red oak", "Basswood", "White ash", "Red pine",
              "Black cherry", "American hophornbeam", "American elm"),
  pH = c(1, 1, 0, 0, 0, 0, "west", 1, 0, 0, 0, -1, 0, 0, 0, 0, 0),
  TAVE = c(-1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1),
  Age = c(0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Clay = c(0, "west", 0, 0, 0, 1, 1, -1, 1, -1, 0, 1, -1, 0, -1, 0, 0),
  CEC = c("west", 0, "east", "east", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0),
  Elevation = c(0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0),
  VPD = c(0, "east", 0, 0, "east", 0, -1, 0, "west", "east", 0, 0, 0, 0, 0, 0, 0),
  TWI = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Slope = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  Origin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
)

group <- data.frame(variable=c("TAVE", "VPD", "Elevation", "Slope", "TWI", 
                               "CEC", "Clay", "pH",  "Origin", "Age"),
                    group=c("Climate", "Climate", 
                            "Topography", "Topography", "Topography", 
                            "Soil", "Soil", "Soil", 
                            "Stand dynamic", "Stand dynamic"))

# Transform the data frame for ggplot
df_melt <- melt(df, id.vars = "Species") %>% 
  left_join(group)
  

# Replace "east" and "west" 
df_melt$value <- factor(df_melt$value, levels = c(-1, 0, 1, "east", "west"), labels = c(
  "Abundance covaries **negatively**", 
  "No correlation",
  "Abundance covaries **positively**",
  "**Negative** in West, **positive** in East",
  "**Positive** in West, **negative** in East"
))

df_melt$group <- factor(df_melt$group  , levels = c("Climate","Topography", "Soil", "Stand dynamic"))

df_melt <- df_melt %>%
  mutate(
    label = case_when(
      value == "Abundance covaries **positively**" ~ "+",
      value == "Abundance covaries **negatively**" ~ "-",
      value == "**Negative** in West, **positive** in East" ~ "-/+",
      value == "**Positive** in West, **negative** in East" ~ "+/-",
      TRUE ~ ""
    )
  )

plot <- ggplot(df_melt, aes(x = interaction(variable, group), y = Species, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("Abundance covaries **negatively**" = "dodgerblue3", 
               "No correlation" = "white",
               "Abundance covaries **positively**" = "gold",
               "**Negative** in West, **positive** in East" = "purple",
               "**Positive** in West, **negative** in East" = "grey40"),
    breaks = c("Abundance covaries **negatively**",
               "Abundance covaries **positively**",
               "**Negative** in West, **positive** in East",
               "**Positive** in West, **negative** in East")
  ) +
  geom_text(aes(label = label), size = 8, color = "black") +
  labs(x = "Variable",
       y = "Species",
       fill = NULL) +
  scale_y_discrete(limits=rev) +
  theme(legend.position = "right", 
        legend.title.position = "top",
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.title = element_text(size = 16),
        legend.text = element_markdown(size=14)) +            
  guides(fill = guide_legend(title = "For northern populations", 
                             ncol = 1, override.aes = list(color = NA)))+
  scale_x_discrete(guide = "axis_nested")  

#Add +, -, -/+ and +/- in legends with inkscape. 

plot

#as example sugar maple
ers_shap <- readRDS("~/postdoc/habitat_caracterisation/article/figures/experiment/sugar_maple_CEC_shap.rds") + 
  geom_hline(yintercept = 46.5, linewidth=1.5, linetype='dashed', col="red") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size=12))

plot_ers <- ggarrange(plot, ers_shap, ncol=1, heights=c(0.7, 0.3), 
                      labels=c("A", "B"), 
                      label.x = c(0,0.2), label.y = c(1,1),
                      font.label = list(size = 16)) +
  bgcolor("white") + border("white")

plot_ers

ggsave(plot=plot_ers, filename="figures/summary.png",
       width=13, height=10, dpi =1000, units="in")

ggsave(plot=plot_ers, filename="figures/summary.pdf",
       width=13, height=10)

