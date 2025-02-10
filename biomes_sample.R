## Biomes

library(tidyverse)
library(tictoc)
library(stars)
library(sf)
library(terra)
library(tidyterra)

# biomes <- read_sf("~/Documents/Projects/DATA/terr-ecoregions-TNC/tnc_terr_ecoregions.shp")
# biomes

load("data/deltas_combined.Rda")

## 100 pixels per biome
biome_sample <- out |>
    group_by(WWF_MHTNUM) |>
    slice_sample(n = 100)

save(biome_sample, file = "data/biome_sample.Rda")
