# True positives
# Data from Hammond et al 2022: https://doi.org/10.1038/s41467-022-29289-2
# Forest die-off events

library(tidyverse)
library(sf)

pts <- read_csv("data/GTM_full_database_download_20241213-084339.csv") |>
    janitor::clean_names()

pts <- pts |>
    st_as_sf(coords = c("long", "lat"), crs = 4326)

pts <- pts |>
    st_transform("ESRI:54008") # project to sinusoidal

