library(tidyverse)
library(fs)
library(tictoc)

# When working from back up disk:
# "/Volumes/Data backups/DATA/LP_DAAC_Data_Pool/daac_data_download_r/MOD13A2-061"
load("data/tiles.Rda")

fl <- tiles |> filter(grid == "h10v08") |> slice(1) |> pull(file)

sd <- sf::gdal_subdatasets(fl)
# 1: NDVI, 2: EVI
# 3: VI quality
# 12: pixel reliability (-1 = NA, 0 = good, 1 = use with caution, 2 = snow/ice, 3 = clouds)

x <- terra::rast(fl)
lobstr::obj_size(x) # 9.66MB


range(x$`"1 km 16 days NDVI"` )
range(x$`"1 km 16 days VI Quality"`)

y <- stars::read_stars(sd[[3]])
z <- terra::rast(sd[[1]])

x$`"1 km 16 days NDVI"`

# A simple multiplication can mask the low values, then this can be exported to GeoTif
terra::plot(
    x$`"1 km 16 days NDVI"` * (x$`"1 km 16 days pixel reliability"` == 0)
    )

# J260501: I can extract both layers, multiply and then mask out the low quality values

terra::plot(z)
plot(y)

range(z)
range(as.matrix(z), na.rm = TRUE)

#### Files to read: only if working with external back up disk ####
fls <- dir_ls("~/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/MYD13A2-061/") |>
    str_subset(pattern = "\\.hdf$") # aqua
fls2 <- dir_ls("/Volumes/Data backups/DATA/LP_DAAC_Data_Pool/daac_data_download_r/MOD13A2-061/") |>
    str_subset(pattern = "\\.hdf$") # terra

dat <- tibble(file = c(fls, fls2))
dat <- dat |>
    mutate(grid = str_sub(file, start = 96L, end = 101L),
           date = str_sub(file, 88L, 94L) |> as.Date("%Y%j"),
           dataset = str_sub(file, 79L, 85L),
           year = str_sub(file, 88L, 91L), jour = str_sub(file, 92L, 94L)) |>
    select(dataset, grid, date, year, jour, file)

dat |> skimr::skim()
dat
