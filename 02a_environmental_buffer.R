# INFO ------------------------------------------------------------------------#
#                                                                              #
# Script name: 02a [ENM] - Environmental buffer                                #
#                                                                              #
# Author:      Marcela A. Barros                                               #
# Copyright    Copyright 2024 BARROS, M. A.                                    #
# Email:       marcela.barros@unesp.br                                         #
#                                                                              #
# Date:        2024-20-11                                                      #
#                                                                              #
# Description: This script complements Script 02. To minimize spatial          #
# correlation (Aiello-Lammens et al., 2015), occurrences were restricted       #
# within a 500 km buffer surrounding all points and spaced at least 10 km      #
# part. Following this process, ten species met the minimum threshold and      #
# were selected for model training.                                            #
#                                                                              #
#------------------------------------------------------------------------------#


occ_north_sf <- sf::st_as_sf(occ_north, coords = c("longitude","latitude"), crs = raster::crs(sp_env_north))
occ_south_sf <- sf::st_as_sf(occ_south, coords = c("longitude","latitude"), crs = raster::crs(sp_env_south))

eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

occ_north_sf <- sf::st_transform(occ_north_sf, crs = eckertIV)
occ_south_sf <- sf::st_transform(occ_south_sf, crs = eckertIV)

occ_north_buff <- sf::st_buffer(occ_north_sf, dist = 500000) %>% 
  sf::st_union() %>% 
  sf::st_sf() %>%
  sf::st_transform(crs = raster::crs(sp_env_north))

occ_south_buff <- sf::st_buffer(occ_south_sf, dist = 500000) %>% 
  sf::st_union() %>% 
  sf::st_sf() %>%
  sf::st_transform(crs = raster::crs(sp_env_south))

sp_env_north_bg <- raster::crop(sp_env_north, occ_north_buff)
sp_env_north_bg <- raster::mask(sp_env_north_bg, occ_north_buff)

sp_env_south_bg <- raster::crop(sp_env_south, occ_south_buff)
sp_env_south_bg <- raster::mask(sp_env_south_bg, occ_south_buff)

plot(sp_env_north_bg[[1]])
plot(sp_env_south_bg[[1]])
