#Fig S1, S2 and S3 validation plots and Moran tests

rm(list=ls())

library(tidyverse)
library(viridis)
library(ggpubr)
theme_set(theme_bw())

val <- readRDS("~/postdoc/habitat_caracterisation/model/abundance/results_rf/res_model/all_validation.rds")

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



quantiles <- NULL
val_qq <- NULL
moran_all <- NULL

for (i in spe_name){ #i="Red maple"
  print(i)
  
  #####Validation######
  val_sp <- filter(val, species==i)
  
  #Quatiles
  quantiles_sp <- data.frame(species=i,
    quantile_obs=quantile(val_sp$obs, probs = seq(0.01, 0.99, by = 0.01)),
    quantile_pred=quantile(val_sp$prediction, probs = seq(0.01, 0.99, by = 0.01)))
  
  quantiles <- bind_rows(quantiles, quantiles_sp)
  
  
  #Validation Moran test
  val_sp$resid <- val_sp$obs - val_sp$pred
  
  val_sp_sf <- st_as_sf(
    val_sp,
    coords = c("longitude", "latitude"),
    crs = 4326  # WGS84
  )
  
  val_sp_sf_utm <- st_transform(val_sp_sf, 3857)
  
  
  
  library(sf)
  library(spdep)
  
  coords <- st_coordinates(val_sp_sf_utm)
  
  #Regional
  nb_50 <- dnearneigh(coords, 0, 50000)   # 50 km
  lw_50 <- nb2listw(nb_50, style="W", zero.policy = TRUE)
  moran_50 <- moran.test(val_sp$resid, lw_50, zero.policy = TRUE)
  
  #Local
  nb_10 <- dnearneigh(coords, 0, 10000)   # 50 km
  lw_10 <- nb2listw(nb_10, style="W", zero.policy = TRUE)
  moran_10 <- moran.test(val_sp$resid, lw_10, zero.policy = TRUE)
  
  moran_all <- bind_rows(moran_all,
                         data.frame(species=i,
                                    `Moran I 10 km`=moran_10$estimate[1],
                                    `p-value 10 km`=moran_10$p.value[1],
                                    `Moran I 50 km`=moran_50$estimate[1],
                                    `p-value 50 km`=moran_50$p.value[1],
                                    row.names =NULL))
  
}

#Moran plots
moran_long <- moran_all %>%
  pivot_longer(
    cols = c(Moran.I.10.km, Moran.I.50.km),
    names_to = "Distance",
    values_to = "Moran_I"
  ) %>%
  # Add a column for significance
  mutate(
    Distance = recode(Distance,
                      "Moran.I.10.km" = "10 km (local)",
                      "Moran.I.50.km" = "50 km (regional)"),
    Signif = case_when(
      Distance == "10 km (local)" & p.value.10.km < 0.05 ~ "< 0.05",
      Distance == "10 km (local)" & p.value.10.km >= 0.05 ~ "≥ 0.05",
      Distance == "50 km (regional)" & p.value.50.km < 0.05 ~ "< 0.05",
      Distance == "50 km (regional)" & p.value.50.km >= 0.05 ~ "≥ 0.05"
    )
  )

moran_long$species <- factor(moran_long$species, 
                             levels = sort(unique(moran_long$species)))  # ordre alphabétique
moran_long$species <- fct_rev(moran_long$species)              # inverse l’ordre

moran_plot <- ggplot(moran_long, aes(x = Moran_I, y = species,
                       color = Distance, shape = Signif)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c("< 0.05" = 16, "≥ 0.05" = 1)) +
  labs(
    x = "Moran's I",
    y = "Species",
    color = "Distance",
    shape = "P-value"
  ) +
  theme_bw(base_size = 14) + 
  theme(strip.text.x = element_text(size = 12),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.title = element_text(size = 14),
        panel.background = element_rect(fill = "white"),  # fond blanc avec bord noir
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(size=12),
        legend.position = "bottom") +
  theme(legend.box = "horizontal") +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  )

ggsave(plot=moran_plot, filename=paste0("figures/moran_plot.pdf"), 
       width=8, height=8, bg = "white")

ggsave(plot=moran_plot, filename="figures/moran_plot.png", 
       width=8, height=8, dpi =1000, units="in", bg = "white")


#Predictions vs observations
abline_df <- val %>%
  group_by(species) %>%
  summarise(xmin = min(obs, prediction),
            xmax = max(obs, prediction)) %>%
  mutate(ymin = xmin, ymax = xmax)

val_plot <- ggplot(val, aes(x = obs, y = prediction)) +
  geom_point() +
  facet_wrap(~species, ncol=3, scale="free") +
  geom_segment(data = abline_df,
               aes(x = xmin, xend = xmax, y = ymin, yend = ymax),
               color = "red", linetype = "dashed") +
  labs(x = "Observed abundance", y = "Predicted abundance") +
  theme(strip.text.x = element_text(size = 12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16))

ggsave(plot=val_plot, filename=paste0("figures/validation_pred_obs.pdf"), 
       width=8, height=10)

ggsave(plot=val_plot, filename="figures/validation_pred_obs.png", 
       width=8, height=10, dpi =1000, units="in")

#Quantiles quantiles plot
abline_df <- quantiles %>%
  group_by(species) %>%
  summarise(xmin = min(quantile_obs),
            xmax = max(quantile_obs)) %>%
  mutate(ymin = xmin, ymax = xmax)

# Plot
quantiles_plot <- ggplot(quantiles, aes(x = quantile_obs, y = quantile_pred)) +
  geom_point() +
  facet_wrap(~species, ncol=3, scale="free") +
  geom_segment(data = abline_df,
               aes(x = xmin, xend = xmax, y = ymin, yend = ymax),
               color = "red", linetype = "dashed") +
  labs(x = "Quantiles of observed abundance",
       y = "Quantiles of predicted abundance") +
  theme(strip.text.x = element_text(size = 12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16))

ggsave(plot=quantiles_plot, filename=paste0("figures/validation_quant_quant.pdf"), 
       width=8, height=10)

ggsave(plot=quantiles_plot, filename="figures/validation_quant_quant.png", 
       width=8, height=10, dpi =1000, units="in")


