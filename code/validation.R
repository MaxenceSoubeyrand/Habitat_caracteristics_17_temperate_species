#Create validaotion plots in SI
rm(list=ls())

library(tidyverse)
library(viridis)
library(ggpubr)
theme_set(theme_bw())


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

for (i in spe_name){
  #####Validation######
  val_sp <- filter(val, species==i)
  
  #Quantile quantile plots
  quantiles_sp <- data.frame(species=i,
    quantile_obs=quantile(val_sp$obs, probs = seq(0.01, 0.99, by = 0.01)),
    quantile_pred=quantile(val_sp$prediction, probs = seq(0.01, 0.99, by = 0.01)))
  
  quantiles <- bind_rows(quantiles, quantiles_sp)
}

val_plot <- ggplot(val, aes(x=obs, prediction)) +
  geom_point() +
  facet_wrap(~species, ncol=3,
             scale="free") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(x = "Observed abundance", y = "Predicted abundance") + 
  theme(strip.text.x = element_text(size = 12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16))

ggsave(plot=val_plot, filename=paste0("figures/validation_pred_obs.pdf"), 
       width=8, height=10)

ggsave(plot=val_plot, filename="figures/validation_pred_obs.png", 
       width=8, height=10, dpi =1000, units="in")


quantiles_plot <- ggplot(quantiles, aes(x = quantile_obs,
                y = quantile_pred)) +
  geom_point() +
  facet_wrap(~species, ncol=3,
             scale="free") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  
  labs(x = "Quantiles of observed abundance", y = "Quantiles of predicted abundance") + 
  theme(strip.text.x = element_text(size = 12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16))

ggsave(plot=quantiles_plot, filename=paste0("figures/validation_quant_quant.pdf"), 
       width=8, height=10)

ggsave(plot=quantiles_plot, filename="figures/validation_quant_quant.png", 
       width=8, height=10, dpi =1000, units="in")


