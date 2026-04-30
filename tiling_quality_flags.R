library(tidyverse)
library(fs)
library(tictoc)

# When working from back up disk:
# "/Volumes/Data backups/DATA/LP_DAAC_Data_Pool/daac_data_download_r/MOD13A2-061"


#### Files to read ####
fls <- dir_ls("/Volumes/Data backups/DATA/LP_DAAC_Data_Pool/daac_data_download_r/MYD13A2-061/") |>
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
