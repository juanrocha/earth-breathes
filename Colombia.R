library(tidyverse)
library(fs)
library(tictoc)


# time series
library(fpp3)
library(furrr)
library(tsibble)
library(fable)
library(imputeTS)
library(slider)
library(fractaldim)

## avoid messages in parallel:
# Suppress summarise info
options(dplyr.summarise.inform = FALSE, dplyr.join.inform = FALSE)

coords <- read_table(file = "~/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/sn_bound_10deg.txt", skip = 5, col_types = "iidddd")
crds <- coords |>
    filter(ih == 10, iv == 8)


## Colombian files
#### Files to read ####
fls <- dir_ls("~/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/MYD13A2-061/") |>
    str_subset(pattern = "\\.hdf$")
fls2 <- dir_ls("~/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/MOD13A2-061/") |>
    str_subset(pattern = "\\.hdf$")

fls <- tibble(
    files = c(fls, fls2)
)

tic()
fls <- fls |>
    mutate(grid = str_sub(files, start = 110L, end = 115L))
toc() #0.3s

tic()
fls |>
    group_by(grid) |>
    summarize(n = n()) |> skimr::skim()
toc() # 294 tiles, 552 temporal observations (biweeks)


fls |>
    filter(grid == "h10v08") |>
    pull(files) |>
    file.size() |>
    sum() / 1024^3 # 18Gb Terra and Aqua combined for one grid

fls <- fls |> filter(grid == "h10v08") # 1037 files for Colombia

fls <- fls |>
    mutate(
        orig_file = str_remove(
            files, "/Users/juanrocha/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/") |>
            str_remove("MYD13A2-061/|MOD13A2-061/") |>
            str_remove("\\.hdf"),
        destination = paste0(orig_file, ".tif")
    )




#### Does work: selecting the desired subdataset
sf::gdal_utils("info", glue::glue('HDF4_EOS:EOS_GRID:"', fls$files[1] , '":MODIS_Grid_16DAY_1km_VI:"1 km 16 days NDVI"') )
## but this does not work
sf::gdal_utils(
    "translate",
    source = glue::glue('HDF4_EOS:EOS_GRID:"', fls$files[1] , '":MODIS_Grid_16DAY_1km_VI:"1 km 16 days NDVI"')  ,
    destination = paste0("data/Colombia/", fls$destination[1])
)

#### work with GDAL directly ####
fls <- fls |>
    mutate(cmd = glue::glue(
        "gdal_translate ",
        'HDF4_EOS:EOS_GRID:"', "{files}" , '":MODIS_Grid_16DAY_1km_VI:"1 km 16 days NDVI"', # file name
        " ", # space
        "data/Colombia/", "{destination}"
    ))

save(fls, file = "data/tiles_colombia.Rda")

#### run this on the Terminal of RStudio ####
tic()
walk(.x = fls$cmd, .f = system)
toc() #94s, 3GB

#### read back in R ####
# library(terra)
# ## test with one
tic()
ndvi <- dir_ls("data/tiles_tif/") |>
    str_subset(pattern = "h10v08")
toc() # 1.4s
# dat <- rast(ndvi[1])
# dat
# # for some stupid reason the tiffs are created with +4 zeroes
# plot(dat/10^4)
# range(dat)
#
# ndvi[1]
## one does not need to read all files, not weekly obs throughout
dat <- tibble(
    fls = ndvi) |>
    mutate(
        date0 = str_sub(ndvi, 25L, 31L)) |>
    mutate(date = as.Date(date0, "%Y%j")) |>
    mutate(jour = str_sub(date0, 5L, -1L))

dat |> pull(jour) |> unique()
# you need to secure files are read on the right time order, so you need a df with
# file names and time stamps.
dat <- dat |> arrange(date)

calendar <- expand.grid(
    year = c(2002:2024),
    week = dat |> pull(jour) |> unique()) |>
    as_tibble() |>
    mutate(y_j = paste0(year,week) ) |>
    mutate(date = as.Date(y_j, "%Y%j")) |>
    filter(date > "2002-06-25", date < "2024-02-14") |>  # obs on files
    arrange((date)) |>
    rownames_to_column("tid") # fake time index that is not periodic but preserve order

incomp_dates <- dat$date
save(calendar, incomp_dates, file = "data/calendar.Rda")

#### stars ####
library(stars)
# this is a cube on stars, but does not behave nicely
# x <- read_stars(dat$fls, along = "time", quiet = TRUE, proxy = TRUE) # proxy object
# dim(x)

# this is multiple tiff objs in stars
x <- map(
    dat |> filter(date > "2002-06-25") |> pull(fls),
    read_stars, proxy = TRUE)

# remove missing values on the fly
tic()
df_dat <- map(
    x,
    function(x) {
        z <- as.data.frame(x) |>
            pivot_longer(cols = ends_with("tif"), names_to = "file", values_to = "ndvi") |>
            filter(!is.na(ndvi))
        return(z)
    } ) |>
    bind_rows() |>
    as_tibble()
toc() # 73s
# lobstr::obj_size(df_dat) # 36.46 GB
# warning: ignoring unrecognized unit: NDVI

tic()
df_dat <- df_dat |>
    mutate(ndvi = ndvi / 10^8) # correct units
toc() # 2.1s

tic()
df_dat <- df_dat |>
    left_join(
        dat |>
            mutate(file = as.character(fls) |> str_remove("data/Colombia/")) |>
            select(-fls, -jour, -date0)
        )
toc() # 34s

df_dat <- select(df_dat, -file)

df_dat

## create ids
tic()
df_dat <- df_dat |>
    group_by(x,y)
toc() #49s

id <- group_indices(df_dat)
keys <- group_keys(df_dat) # id is the nrow

tic()
df_dat <- df_dat |>
    add_column(id = id)
toc() # 51s

# safe to remove xy coords
df_dat <- df_dat |> ungroup() |> select(-x,-y)
lobstr::obj_size(df_dat) # 21Gb

# diagnose missing values
dat |>
    # keep only weekly obs
    filter(date > "2002-06-25") |>
    right_join(calendar) |> #filter(is.na(fls))
    nrow()
## all pixels miss 12 data points in time


tic()
nobs <- df_dat |>
    filter(date > "2002-06-25") |> # remove dates for which there is only biweekly obs
    group_by(id) |>
    summarize(n=n()) |>
    mutate(p = n/996) # from previous call
toc() # 23s

nobs <- nobs |> filter(p < 0.9) # remove pixels with less than 90% of obs, ~10.2k pixels

tic()
df_dat <- df_dat |>
    anti_join(nobs)
toc() #42s

## explore one pixel
pxl <- df_dat |> filter(id == 10)

tic()
pxl |>
    # group_by(yr) |> summarize(n=n()) # 46 weeks years = 8 day weeks
    right_join(calendar |> select(date, tid)) |> # this recover the missing values, complete the time series
    mutate(ndvi = na_interpolation(ndvi, "spline"),
           id = na_interpolation(id, "linear")) |>
    mutate(tid = as.numeric(tid)) |>
    mutate(kndvi = tanh(ndvi^2)) |>
    as_tsibble(index = "tid") |>
    # Multiple seasonality with STL
    model(STL(kndvi ~ season(period = 23) + season(45) )) |>
    # Fourier
    #model(ARIMA(GPP ~ trend + fourier(period = 46, K = 10) + fourier(11.5, K = 5))) |>
    components()
toc() # 0.2s per pixel

## make it into a function
decompose <- function(x){
     x <- x |>
        # this recover the missing values, complete the time series
        right_join(calendar |> select(date, tid)) |>
        # interpolate missing values
        mutate(ndvi = na_interpolation(ndvi, "spline"),
               id = na_interpolation(id, "linear")) |>
        mutate(tid = as.numeric(tid)) |>
        # calculate kernel ndvi
        mutate(kndvi = tanh(ndvi^2)) |>
         select(-ndvi, -date) # can be recovered later
     res <- x |>
        as_tsibble(index = "tid") |>
        model(STL(kndvi ~ season(period = 23) + season(45) )) |>
        components()

    x$remainder <- res$remainder
    return(x)
}

safe_decomp <- safely(decompose)
tic()
pxl |> safe_decomp()
toc()

tic()
df_dat <- df_dat |>
    split(~id)
toc() #133.192 s

length(df_dat) * 0.1 / 60 / 60 # 30hrs sequential

plan(multisession, workers = 12)

out <- list()

tic()
out <- future_map(df_dat, safe_decomp, .progress = TRUE)
toc() # 23774.257 | 6hrs, 1 tile, >1M pixels, 983 time steps

out <- transpose(out)

is_ok <- map_lgl(out$error, is.null)
all(is_ok) # TRUE: all are okay, no errors

lobstr::obj_size(out)
tic()
out <- out$result |>
    bind_rows()
toc() #25s

# save(out, calendar, keys, file = "data/detrended_kndvi_Colombia.Rda")
# rm(calendar, keys, pxl, df_dat)
# gc()

tic()
out <- out |> split(~id)
toc() # 376s

#### calculate ews ####
tic()
load("data/detrended_kndvi_Colombia.Rda")
toc() # 66s
# set up the rolloing window
window <- 92 # 2yrs
## test later if you need uniroot test and first-diff
early_warnings <- function(x){ # x is the dataset, y is the ts variable to calculate ews
    x <- x |>
        mutate(
            ews_std = slider::slide_dbl(
                .x = remainder, .f = sd, na.rm = TRUE, .before = window, .after = 0, .complete = TRUE),
            ews_ac1 = slider::slide_dbl(
                .x = remainder, # turned back to pearson, kendall does not work better
                .f = function(x) cor(x,lag(x,1), use = "pairwise.complete.obs", "pearson"),
                .before = window, .after = 0, .complete = TRUE),
            ews_kur = slider::slide_dbl(
                .x = remainder, .f = moments::kurtosis, na.rm = TRUE,
                .before = window, .after = 0, .complete = TRUE),
            ews_skw = slider::slide_dbl(
                .x = remainder, .f = function(x) abs(moments::skewness(x, na.rm = TRUE)),
                .before = window, .after = 0, .complete = TRUE) ,
            # calculating fractal dimension here doubles the time but it's done.
            ews_fd = slider::slide_dbl(
                .x = remainder,
                .f = function(x) {
                    z <- fd.estimate(x, window.size = window, method = "madogram")$fd
                    return(as.vector(z))} , .before = window, .after = 0, .complete = TRUE)
        )
    return(x)
}

safe_ews <- safely(early_warnings)
## test on one pxl:
tic()
early_warnings(out[[1]])
toc() # 0.285 s

ews <- list()

tic()
ews <- future_map(out, safe_ews, .progress = TRUE)
toc() # 9305.982 sec elapsed per tile, 2.5hrs

ews <- transpose(ews)
is_ok <- map_lgl(ews$error, is.null)
all(is_ok) # all good no errors!

ews$result[[1]]

ews <- ews$result # remove error reports

lobstr::obj_size(ews) # 80Gb

# tic()
# save(ews, keys, calendar, file = "data/ews_colombia.Rda", compress = "gzip")
# toc() # 4495.874 | 1.2hrs

tic()
load("data/ews_colombia.Rda")
toc() #192.748 sec elapsed!, 3.2mins


#fls <- dir_ls("data/tmp") # fails
fls <- list.files(path = "data/tmp/")

ews [[1]]

extract_delta <- function(p) {
    load(paste0("data/tmp/", p))
    d <- bind_rows (
        # maximum
        x |>
            select(id, tid, starts_with("ews")) |>
            pivot_longer(starts_with("ews"), names_to = "ews", values_to = "value") |>
            as_tibble() |>
            group_by(ews) |>
            mutate(is_max = value == max(value, na.rm = TRUE)) |>
            filter(is_max == TRUE) |> select(-is_max) |>
            mutate(type = "max") |>
            #add_count() |> ## detecting cases where max returns more than one hit
            ## when that happens, filter selects the first instance in time. It's mainly FD
            filter(tid == min(tid)) ,
        # minimum
        x |>
            select(id, tid, starts_with("ews")) |>
            pivot_longer(starts_with("ews"), names_to = "ews", values_to = "value") |>
            as_tibble() |>
            group_by(ews) |>
            mutate(is_min = value == min(value, na.rm = TRUE)) |>
            filter(is_min == TRUE) |> select(-is_min) |>
            mutate(type = "min") |>
            # selects first instance when diff == 0 many consecutive time steps
            filter(tid == min(tid))
    ) |> arrange(ews, tid) |>
        group_by(ews, id) |>
        summarize(delta = diff(value), .groups = "drop") |>
        pivot_wider(names_from = ews, values_from = delta)
    return(d)
}

tic()
fls[[1]] |> extract_delta()
toc() # 0.3 per pxl.

deltas <- list()

tic()
deltas <- future_map(fls, extract_delta, .progress = TRUE)
toc() # 8172.635 | 2.2hrs

tic()
deltas <- bind_rows(deltas)
toc() # 0.3s

save(deltas, file="data/deltas_Colombia.Rda")

load("data/detr")

deltas |>
    left_join(keys |> rownames_to_column("id") |> mutate(id = as.numeric(id))) |>
    ggplot(aes(x,y)) +
    geom_tile(aes(fill = ews_skw)) +
    scale_fill_viridis_c()

## impractical to save to a single file, rather save a file per latitude or per pixel
## lat info is lost in this step, use id
## I did the individual file and multiple Rda option. The ind file takes 1.2hrs,
## multiple files take longer than that and I ran out of memory.
## library(nanoparquet)
# tic()
# future_walk(
#     ews, # test first with head() for timing
#     function(x){
#         # Rda: 0.02s, 6 pxls, 48kb each
#         save(x, file = paste0("data/tmp/pxl_", as.character(x[1,1]), ".Rda"))
#         # CSV: 0.2s, 6pxls, 123kb each, 10times the time, >2x large files
#         #write_csv(x, file = paste0("data/tmp/pxl_", as.character(x[1,1]), ".csv"))
#         ## parquet: 0.02, 6pxls, 54Kb each
#         #write_parquet(x, file = paste0("data/tmp/pxl_", as.character(x[1,1]), ".parquet"))
#     })
# toc() # mistake, made on sequential

## Saving and loading one big Rda file is faster, but implies that the object with
## the data needs to be loaded on memory. If the object is >70GB, such as the ews object
## that means the parallelization wont work because one needs to copy that object.
## Saving in multiple little files is a hazzle, takes a long time (overnight), it is
## over 1M pixels = >1M small files. Read and write gets slower, but the parallelization
## works wonderfully: EWS in 2.5hrs and deltas in a similar mark per tile.
## Perhaps go back to the idea of proxy file?

# oopts <- options(future.globals.maxSize = 4.5 * 1e9)  ## 4.5 GB
# on.exit(options(oopts))

#### gdalcubes: not working ####
library(gdalcubes)
gdalcubes_options(parallel = TRUE)
gdalcubes_gdalformats() |> str_subset("Tif") # includes HDF4
collection_formats()

tic()
x <- create_image_collection(
    files = dat |> filter(date > "2002-06-25", date < "2024-02-14") |> pull(fls), # obs on files,
    date_time = dat |>  filter(date > "2002-06-25", date < "2024-02-14") |> pull(date) # obs on files
    )
toc() #4.2s

tic()
v <- cube_view(
    extent = x, srs = "ESRI:54008",
    # this specs follows the info on the MODIS website
    #nx = 1200, ny = 1200, # high res cube
    dx = 10000, dy = 10000, # low res cube
    nt = length( dat |>  filter(date > "2002-06-25", date < "2024-02-14") |> pull(date) )
    #extent = list(left = crds$lon_min, right = crds$lon_max, bottom = crds$lat_min, top = crds$lat_max)
    #srs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", # sinusoidal projection
)
toc() #0.003s
v
dc <- raster_cube(x, v)
dc

## see gdalcubes_selection
## subset cubes: object["band", "time", "x", "y"]
dc["band1", c("2000-02-18T00:00:00","2024-02-10T00:07:20"), c(200), c(200)] |> plot() # not working
dc["band1", "2002-06-26T00:00:00", , ] |> plot(key.pos = 1, col = viridis::viridis, nbreaks = 7) # working

## missing values
tic()
dc |>
    reduce_time("count(band1)") |>
    plot(key.pos = 1)
toc() #123s in parallel

## pre-processing
tic()
tst <- dc |>
    apply_pixel(names = "kndvi", FUN = function(x){
        tanh((x["band1"] / 10000)^2)
    })



tst_missing <- tst |>
    apply_pixel(name = "missing", FUN = function(x){
        is.na(x["knvdi"])
    })

tst_filled <- tst |>
    select_bands("kndvi") |>
    fill_time(method = "linear") |>
    write_ncdf("result.nc")

## this does not work
tst_stl <- tst_filled |>
    reduce_time(names = c("reminder"), FUN = function(x){
        kndvi <- x["kndvi"]
        if (all(is.na(kndvi))) {
            return(c(NA))
        }
        library(dplyr)
        library(tsbile)
        library(fpp3)
        tryCatch({
            d <- tibble(kndvi = kndvi) |>
                rownames_to_column("tid") |>
                mutate(tid = as.numeric(tid))
            r <- d |>
                as_tsibble(index = "tid") |>
                model(STL(kndvi ~ season(23) + season(45) )) |>
                components()
            return(c(r$reminder))
        }, error = function(x){
            return(c(NA))
        })
    })

tst_stl

tst_stl["reminder", "2002-06-26T00:00:00", , ] |> plot(key.pos = 1, col = viridis::viridis, nbreaks = 7) # working


triCatch({
    d <- tibble(kndvi = kndvi, nas = nas) |>
        rownames_to_column("tid") |>
        mutate(tid = as.numeric(tid))
    r <- d |>
        as_tsibble(index = "tid") |>
        model(STL(kndvi ~ season(23) + season(45) )) |>
        components()
    return(r$reminder)
}, error = function(x){
    return(c(NA))
})
