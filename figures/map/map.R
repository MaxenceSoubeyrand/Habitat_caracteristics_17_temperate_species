#Script that plot the map (Figure 1)
rm(list=ls())

setwd("~/postdoc/habitat_caracterisation/article/figures/map")

library(tidyverse)
library(ggthemes)
library(mapview)
library(viridis)
library(sf)
library(maps)
library(ggspatial)
library(ggnewscale)
library(cowplot)
library(ggpubr)
library(grid)
library(ggplotify)

library(RODBC)

#Data
temp_spe <- c("BOJ", "ERR", "ERS", "HEG", "PRU", 
              "CHR", "PIB", "PIR", "ERP", "EPR",
              "TIL", "FRN", "THO", "FRA", "OSV",
              "ORA","CET")

#Bioclimatic domain
dom_bio <- st_read(dsn = "~/postdoc/data/carte_qc/dom_bio/dom_bio.shp",
                   layer = "dom_bio")
dom_bio_map <- subset(dom_bio, dom_bio$DOM_BIO %in% c("1", "2", "3","4", "5", "6"))

ab_PE <- readRDS("~/postdoc/habitat_caracterisation/data_compilation/abundance_PE_clim.rds") %>% 
  filter(species %in% temp_spe) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326) %>% 
  mutate(Site="PEP/PET") %>% 
  select(ID_PE) %>% 
  unique()

lim_bio <- st_bbox(ab_PE)

canada <- sf::st_as_sf(map('world', regions="canada", plot = FALSE, fill = TRUE))
world <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))

main <- ggplot(ab_PE)+ 
  geom_sf(data = canada) +
  geom_sf(data = world) +
  geom_sf(data=dom_bio_map, aes(fill=DOM_BIO)) +
  geom_sf(size=0.02, alpha=0.1) +
  scale_fill_manual(
                    values = c("#FB65FF", "#C2BF84", "#FFCD5A", "#81ED66", "#63CAE9", "#B3CCFF"),
                     breaks = c("1", "2", "3","4", "5", "6"),
                     labels = c("Hickory -\nmaple", "Basswood -\nsugar maple", "Yellow birch -\nsugar maple ","Balsam fir -\nyellow birch", "Balsam fir -\nwhite birch", "Spruce -\nmoss")) +
  labs(fill="Bioclimatic domain") +
  coord_sf(xlim = c(lim_bio[1], lim_bio[3]), ylim = c(lim_bio[2]-0.1, lim_bio[4]+0.6), expand=T) +
  theme(panel.background = element_rect(fill = "dodgerblue3"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.position="bottom",
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16)
  ) + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.15, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.35, height = unit(0.1, "cm")) 


countries <- data.frame(lat=c(51.66781288717857, 38.99658150626441),
                        long=c(-90.42096831020812, -88.50786675658247),
                        country=c("Canada", "USA"))

emprise <- ggplot(data=world) +
  geom_sf() +
  geom_text(data=countries, aes(x=long, y=lat, label=country), size=2.5)+
  coord_sf(xlim = c(-99, -51), ylim = c(30, 62), expand = FALSE) +
  theme_void() +
  annotate("rect", xmin = lim_bio[1], xmax = lim_bio[3], ymin = lim_bio[2]-0.1, ymax = lim_bio[4]+0.6,
           alpha = .6, fill = "grey50", color="black", linewidth=0.5) +
  theme(
    panel.background = element_rect(fill = "dodgerblue3"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                size = 1))

map <-
  ggdraw() +
  draw_plot(main) +
  draw_plot(emprise, x=0.83, y=0.71, width=.15, height=.15)


png(filename="figures/map/map.png", 
    unit="in", height=7, width=7, res=1000)
map
dev.off()

pdf(file="figures/map/map.pdf")
map
dev.off()