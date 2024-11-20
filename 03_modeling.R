# INFO ------------------------------------------------------------------------#
#                                                                              #
# Script name: 03 [ENM] - Ecological Niche Modeling                            #
#                                                                              #
# Author:      Marcela A. Barros                                               #
# Copyright    Copyright 2024 BARROS, M. A.                                    #
# Email:       marcela.barros@unesp.br                                         #
#                                                                              #
# Date:        2024-20-11                                                      #
#                                                                              #
# Description: This script is for the modeling process. After loading the      #
# occurrences and environmental variables, the models will be calibrated,      #
# trained, and projected, followed by the creation of the final ensemble.      #
#                                                                              #
#------------------------------------------------------------------------------#

# ============================================================================ #
# 0. Packages
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# 00. Load packages
# ---------------------------------------------------------------------------- #
library(sdm)
library(tidyverse)

# ---------------------------------------------------------------------------- #
# 01. Load data
# ---------------------------------------------------------------------------- #

## Occurrences ----------
occ <- read.csv("path/specie_thin.csv")


## Environmental data ----------
north_env <- rast(
  list.files(
    path = "vars/north_atl/",
    pattern = ".tif",
    full.names = TRUE
  )
)

south_env <- rast(
  list.files(
    path = "vars/south_atl/",
    pattern = ".tif",
    full.names = TRUE
  )
)


# ---------------------------------------------------------------------------- #
# 02. Select variables for the models
# ---------------------------------------------------------------------------- #

## Extract environmental data ----------
occ_data <- terra::extract(north_env, occ)
occ_data <- select(occ_data, -ID)

## Multicollinearity analysis ----------
pearson_corr <- usdm::vifcor(
  occ_data,
  th = 0.8,
  method = "pearson"
)

pearson_corr # Summary

sp_env <- usdm::exclude(north_env, pearson_corr)
names(sp_env)

## Export selected variables ----------
writeRaster(
  sp_env,
  filename = paste0("02_vars/specie/", names(sp_env), ".tif")
)

## Clear objects ----------
rm(list = setdiff(ls(), c("occ", "north_env", "sp_env")))


# ---------------------------------------------------------------------------- #
# 03. Train and project models
# ---------------------------------------------------------------------------- #

## Create instructions for models ----------
occ <- occ %>% mutate(specie = 1, .before = longitude) # cria coluna de presença

model_instr <- sdmData(
  formula = specie ~ . + coords(longitude + latitude),
  train = occ,
  predictors = sp_env,
  bg = list(
    method = "gRandom",
    n = 10000
  )
)

## Train Species Model ----------
maxent_model <- sdm(
  formula = specie ~ .,
  data = model_instr,
  methods = "Maxent",
  replication = "cv",
  cv.folds = 5,
  n = 10,
  test.percent = 30,
  parallelSettings = list(
    ncore = 3, # Define the number of processor cores to use for modeling
    method = "parallel"
  )
)

### Export Model -----
write.sdm(maxent_model, "path/models/specie_maxent")

## Create Projections ----------
maxent_proj <- predict(
  maxent_model,
  newdata = sp_env,
  parallelSetting = list(
    ncore = 3
  )
)

### Export Projections ----------
writeRaster(
  maxent_proj,
  filename = "path/projs/specie_maxent_proj.tif"
)

## Create Ensemble ----------
maxent_ens <- ensemble(
  maxent_model,
  newdata = maxent_proj,
  setting = list(
    method = "weighted",
    stat = "TSS",
    opt = 2 # Combine individual model outputs into an ensemble using the option to maximize specificity and sensitivity.
  ),
  parallelSetting = list(
    ncore = 3
  )
)

### Export Ensemble ----------
writeRaster(
  maxent_ens,
  filename = "path/projs/specie_maxent_ens.tif"
)
