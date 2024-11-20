# INFO ------------------------------------------------------------------------#
#                                                                              #
# Script name: 02 [ENM] - Crop and mask environmental layers                   #
#                                                                              #
# Author:      Marcela A. Barros                                               #
# Copyright    Copyright 2024 BARROS, M. A.                                    #
# Email:       marcela.barros@unesp.br                                         #
#                                                                              #
# Date:        2024-20-11                                                      #
#                                                                              #
# Description: In this script, you will load the variables already downloaded  #
# from the Bio-ORACLE database (https://www.bio-oracle.org/index.php) and      #
# preprocess them for use in the models. A crop and mask will be applied to    #
# the study area, and in our case, we will separate it into the South and      #
# North Atlantic. However, feel free to adapt it to your specific case.        #
#                                                                              #
#------------------------------------------------------------------------------#

# ============================================================================ #
# 0. Packages
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 00. Load packages
# ---------------------------------------------------------------------------- #
library(terra)
library(tidyverse)

# ---------------------------------------------------------------------------- #
# 01. Load data
# ---------------------------------------------------------------------------- #

## Environmental data ----------
env <- rast(
  list.files(
    path = "path_vars/all_layers/",
    pattern = ".tif",
    full.names = TRUE
  )
)

## Study area shapefile ----------
atl_ocean <- sf::st_read("path_shapefile/iho.shp")

# ---------------------------------------------------------------------------- #
# 02. Create environmental clipping and masking
# ---------------------------------------------------------------------------- #

## Definir códigos para filtragem do shapefile -----
north_code <- c(
  "25", "27", "15A", "20", "19", "18",
  "26", "23", "24", "22", "21A", "21",
  "4", "3", "34"
)
south_code <- c("32", "33")

## Filter the shapefile ----------
north_atl <- atl_ocean %>%
  filter(id %in% north_code)

south_atl <- atl_ocean %>%
  filter(id %in% south_code)

## Clip and mask the data ----------
north_env <- crop(env, north_atl)
north_env <- mask(env, north_atl)

south_env <- crop(env, south_atl)
south_env <- mask(env, south_atl)

## Export data ----------
writeRaster(
  north_env,
  filename = paste0("path_vars/north_atl/", names(north_env), ".tif")
)

writeRaster(
  south_env,
  filename = paste0("path_vars/south_atl/", names(south_env), ".tif")
)

## Clear objects ----------
rm(list = ls())
