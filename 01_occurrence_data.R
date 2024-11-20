# INFO ------------------------------------------------------------------------#
#                                                                              #
# Script name: 01 [ENM] - Download and Spatialization of Occurrences           #
#                                                                              #
# Author:      Marcela A. Barros                                               #
# Copyright    Copyright 2024 BARROS, M. A.                                    #
# Email:       marcela.barros@unesp.br                                         #
#                                                                              #
# Date:        2024-20-11                                                      #
#                                                                              #
# Description: In this script, you will perform the first step of the          #
# modeling process: downloading occurrence records of the species from the     #
# Ocean Biodiversity Information System (OBIS) and Global Biodiversity         #
# Information Facility (GBIF) databases, processing them, and verifying their  #
# alignment with the study area (i.e., the Atlantic Ocean). Subsequently, the  #
# occurrences in the final dataset will be spatialized to reduce sampling bias.#
#                                                                              #
#------------------------------------------------------------------------------#

# ============================================================================ #
# 0. Packages
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 00. Load packages
# ---------------------------------------------------------------------------- #
library(spocc)
library(tidyverse)


#==============================================================================#
# 1. Import and Data Processing
# =============================================================================#

# ---------------------------------------------------------------------------- #
# 01. Species Data: Downloading from GBIF and OBIS
# ---------------------------------------------------------------------------- #
sp <- occ(
  query = "Specie",
  from = c("gbif", "obis"),
  limit = 50000,            # This limit was established based on the most recorded species in Ceriantharia
  has_coords = TRUE
)

## Separate datasets
gbif <- sp$gbif$data$Specie
obis <- sp$obis$data$Specie

## Create and export complete dataset 
dataset <- rbind(gbif, obis) %>%
  select(longitude, latitude)

write.csv(dataset, "path/specie_complete.csv", row.names = FALSE)

## Clear objects
rm(list = setdiff(ls(), "dataset"))


# ---------------------------------------------------------------------------- #
# 02. Processing occurrence data
# ---------------------------------------------------------------------------- #
## Remove duplicates and NA data ---------- 
dataset <- dataset %>%
  distinct() %>%
  drop_na()

## Remove data outside the study area ----------
study_area <- sf::st_read("path/iho.shp") # Atlantic Ocean shapefile

coords <- sf::st_as_sf( # Create an sf object
  dataset,
  coords = c("longitude", "latitude"),
  crs = "WGS84"
)

coords_check <- sf::st_intersects(coords, study_area) # Check coordinates with the study area
coords_area <- data.frame(dataset,
  coords_check = sapply(
    coords_check,
    function(x) ifelse(length(x) == 0, "OUT", "IN")
  )
) %>%
  filter(coords_check == "IN") %>% # Filter occurrences within the Atlantic
  select(longitude, latitude)

dataset <- coords_area

## Check occurrences in the study area ---------- 
ggplot() +
  geom_sf(data = study_area, fill = "lightgrey") +
  geom_point(
    data = dataset,
    aes(longitude, latitude),
    color = "red",
    size = 1
  ) +
  coord_sf() +
  theme_minimal()

## Remove occurrences in the same raster cell ---------- 
env <- terra::rast(
  list.files(
    path = "path/all_layers/",
    pattern = ".tif",
    full.names = TRUE
  )
)

occ_cells <- raster::extract(env[[1]], dataset, cellnumbers = TRUE)
occ_cell_duplic <- duplicated(occ_cells[, 1])
dataset <- dataset[!occ_cell_duplic, ]

## Clear objects ----------
rm(list = setdiff(ls(), "dataset"))

## Spatialize occurrences ----------
dataset <- dataset %>%
  mutate(species = "species") # Create the "species" column

sp_thin <- spThin::thin(
  loc.data = dataset,
  lat.col = "latitude",
  long.col = "longitude",
  spec.col = "species",
  thin.par = 1, # Distance in km
  reps = 100,
  locs.thinned.list.return = TRUE,
  write.files = FALSE,
  write.log.file = FALSE
)

## Select a spatialized dataset ----------
sp_thin <- sp_thin[[1]] %>%
  rename(longitude = Longitude, latitude = Latitude)

## Export spatialized dataset ----------
write.csv(sp_thin, "path/specie_thin.csv", row.names = FALSE)

## Clean the environment ----------
rm(list = ls())
