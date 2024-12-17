library(tidyverse)
library(fs)
library(tictoc)

#### Files to read ####
fls <- dir_ls("~/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/MYD13A2-061/") |>
    str_subset(pattern = "\\.hdf$") # aqua
fls2 <- dir_ls("~/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/MOD13A2-061/") |>
    str_subset(pattern = "\\.hdf$") # terra

dat <- tibble(file = c(fls, fls2))
dat <- dat |>
    mutate(grid = str_sub(file, start = 110L, end = 115L),
           date = str_sub(file, 102L, 108L) |> as.Date("%Y%j"),
           dataset = str_sub(file, 93L, 99L),
           year = str_sub(file, 102L, 105L), jour = str_sub(file, 106L, 108L)) |>
    select(dataset, grid, date, year, jour, file)

dat |> skimr::skim()
# 1037 unique dates
# 294 unique tiles
# 25 years
# 46 day of the year where observation is made = years of 46 weeks of 8 days each approx
# 2 datasets
#dat |> pull(jour) |> unique() |> as.numeric() |> sort()

## find implicit missing values
# the calendar is made with the same spects as the file system so we can detect
# if there are time points with complete files missing.
calendar <- expand.grid(
    year = dat |> pull(year) |> unique(),
    jour = dat |> pull(jour) |> unique()) |>
    as_tibble() |>
    mutate(y_j = paste0(year,jour) ) |>
    mutate(date = as.Date(y_j, "%Y%j")) |>
    arrange((date)) |>
    rownames_to_column("tid")

# note that calendar has 1150 rows, while dat has 1037 data points, so there are
# implicit missing values
dat |> right_join(calendar) |>
    arrange(date) |>
    filter(is.na(file)) |>
    print(n = 113)
## I'm combining two datasets terra and aqua, each with biweekly observations.
## But they did not started at the same time, all dates before 2002-06-18 have
## only one dataset available, so only biweekly analysis can be done back to 2000.
## All tiles miss info for dates after Feb 2024 when I downloaded the data.

#nas <-
dat |> right_join(calendar) |>
    arrange(date) |>
    filter(date < "2024-02-18", date > "2002-06-25") |> #filter(y_j == '2011137')
    filter(is.na(file)) |>
    print()  # All tiles have 13 dates missing
    #pull(y_j)

# there are 20 tiles (in the Arctic) where there are less obs (765, 947-49): do first the complete ones.
dat |> group_by(grid) |>
    summarize(n = n()) |>
    arrange(n) |>
    print(n=294)

## data dictionary for a weekly timeseries analysis:
tiles <- dat |> right_join(calendar) |>
    arrange(date) |>
    filter(date < "2024-02-18", date > "2002-06-25") |> #pull(tid) |> as.numeric() |> min()
    mutate(tid = as.numeric(tid) - 114) # resetting the time to start at 1.

## adjust the time in the calendar as well
calendar <- calendar |>
    filter(date < "2024-02-18", date > "2002-06-25") |> # pull(tid) |> as.numeric() |> min()
    mutate(tid = as.numeric(tid) - 114)

## did I missed the download?: NO
# load("~/Documents/Projects/sandbox/NASA_NDVI-data_MOLA_MYD13A2-061.Rda") # Aqua
# load("~/Documents/Projects/sandbox/NASA_NDVI-data_MOLT_MOD13A2-061.Rda") # Terra
# df_files <- df_files |>
#     unnest(raw_files) |>
#     mutate(file = str_sub(raw_files, start = 2L, end = 46)) |> # MYD13A2
#     select(-raw_files) |>
#     mutate(destination = "~/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/MYD13A2-061/") |>
#     mutate(path = paste0(path, file),
#            destination = paste0(destination, file))
#
# df_files |>
#     select(file, destination, path) |>
#     mutate(grid = str_sub(file, start = 18L, end = 23L),
#            date = str_sub(file, 10L, 17L) |> as.Date("%Y%j"),
#            dataset = str_sub(file, 1L, 7L),
#            y_j = str_sub(file, 10L, 16L)) |>
#     select(grid:y_j, file:path) |>
#     filter(y_j %in% nas)
# Files are missing altogether in both Terra and Aqua

#### gdal_transform to tifs ####
load("data/tiles.Rda")
tiles <- tiles |>
    mutate(
        orig_file = str_remove(
            file, "/Users/juanrocha/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/") |>
            str_remove("MYD13A2-061/|MOD13A2-061/") |>
            str_remove("\\.hdf"),
        destination = paste0(orig_file, ".tif")
    ) |>
    mutate(cmd = glue::glue(
        "gdal_translate ",
        'HDF4_EOS:EOS_GRID:"', "{file}" , '":MODIS_Grid_16DAY_1km_VI:"1 km 16 days NDVI"', # file name
        " ", # space
        "data/tiles_tif/", "{destination}"
    ))

# save(tiles, file = "data/tiles_gdal_translate.Rda") # file for gdal_translate
# save(tiles, calendar, file = "data/tiles.Rda") # file for tiling later

#### run this on the Terminal of RStudio ####
tic()
walk(.x = t_missing$cmd, .f = system)
toc() #takes over night for all


#### check result ####
tifs <- dir_ls("data/tiles_tif/") |> str_remove("data/tiles_tif/")

t_missing <- tiles |>
    filter(!destination %in% tifs) |>
    filter(!is.na(file)) |> # 69 files failed
    arrange(grid) #|>
    #print(n=69) #select(destination, cmd)

# save and investigate later why they failed. Some of these files are corrupt, zero byte
# but not all of them.
#save(tiles, t_missing, file = "data/tiles_gdal_translate.Rda")
load("data/tiles_gdal_translate.Rda")

t_missing
#cat(df_files |> filter(files %in% prblm) |> pull(path), sep = "\n", file = "errors2_MYD13A2.txt")
