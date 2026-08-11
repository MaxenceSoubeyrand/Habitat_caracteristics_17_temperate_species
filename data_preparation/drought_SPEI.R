rm(list=ls())

library(terra)
library(stringr)
library(dplyr)

path <- "~/postdoc/data/climate/data_era5/monthly_spei_quebec/"



files <- list.files("~/postdoc/data/climate/data_era5/monthly_spei_quebec/", pattern = "SPEI1.*\\.nc$", full.names = TRUE)

# extraire année et mois
dates <- str_extract(basename(files), "\\d{6}")   # ex: 194001
years <- as.integer(substr(dates, 1, 4))
months <- as.integer(substr(dates, 5, 6))

file_info <- tibble(file = files, year = years, month = months)


calc_spei_count <- function(files_year, threshold) {
  
  # lire tous les fichiers de l'année
  r_stack <- rast(files_year)
  
  # filtrer valeurs aberrantes
  r_stack[r_stack < -10] <- NA
  r_stack[r_stack > 10]  <- NA
  
  # compter mois sous le seuil par pixel
  r_count <- app(r_stack, function(x) sum(x < threshold, na.rm = TRUE))
  
  return(r_count)
}

years_unique <- sort(unique(file_info$year))

# listes pour les deux seuils
stack_moderate <- list()
stack_severe   <- list()

for(y in years_unique) {
  print(y)
  
  files_y <- file_info %>% filter(year == y) %>% pull(file)
  
  stack_moderate[[as.character(y)]] <- calc_spei_count(files_y, -1.5)
  stack_severe[[as.character(y)]]   <- calc_spei_count(files_y, -2)
}

r_moderate <- rast(stack_moderate)
r_severe   <- rast(stack_severe)

names(r_moderate) <- names(r_severe) <- as.character(years_unique)

plot(r_moderate[[1:3]])  # première années
plot(r_severe[[1:3]])

#On récupère les données pour extraire. 
PEP <-  readRDS(file = "~/postdoc/habitat_caracterisation/data_compilation/PEP.rds")
PET <-  readRDS(file = "~/postdoc/habitat_caracterisation/data_compilation/PET.rds")

PE <- bind_rows(PEP, PET)  

#Coordinate and inv_year
ID_PE <- unique(select(PE, ID_PE, NO_MES, year, latitude, longitude))

ID_PE_sf <- ID_PE %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

get_spei_10y <- function(year, xy, raster_stack) {
  
  # années à extraire
  yrs <- as.character((year - 10):year)
  yrs <- yrs[yrs %in% names(raster_stack)]
  if(length(yrs) == 0) return(NA_real_)
  
  # extraire les valeurs pour ce point
  vals <- extract(raster_stack[[yrs]], xy)[1,]   # première ligne = point
  
  # somme en ignorant les NA
  sum(vals, na.rm = TRUE)
}

coords <- st_coordinates(ID_PE_sf)
# appliquer la fonction
ID_PE_sf$spei_moderate_10y <- pmap_dbl(
  list(
    year = ID_PE_sf$year,
    x = st_coordinates(ID_PE_sf)[,1],
    y = st_coordinates(ID_PE_sf)[,2]
  ),
  function(year, x, y) get_spei_10y(year, matrix(c(x,y), ncol=2), r_moderate)
)

ID_PE_sf$spei_severe_10y <- pmap_dbl(
  list(
    year = ID_PE_sf$year,
    x = st_coordinates(ID_PE_sf)[,1],
    y = st_coordinates(ID_PE_sf)[,2]
  ),
  function(year, x, y) get_spei_10y(year, matrix(c(x,y), ncol=2), r_severe)
)

saveRDS(ID_PE_sf, "~/postdoc/habitat_caracterisation/data_compilation/climate_spei.rds")
