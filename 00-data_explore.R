library(tidyverse)
library(fs)
library(tictoc)

## spatial with terra or stars
library(terra)
library(stars)

## For gdalcubes
library(gdalcubes)
gdalcubes_options(parallel = 12)
gdalcubes_options(debug = TRUE)

#### Files to read ####
fls <- dir_ls("~/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/MYD13A2-061/") |>
    str_subset(pattern = "\\.hdf$")

# fls <- tibble(
#     files = fls
# )

dat <- expand.grid(
    file = fls,
    subdataset = c("1 km 16 days NDVI")
) |> as_tibble()

tic()
dat <- dat |>
    mutate(grid = str_sub(file, start = 110L, end = 115L),
           date = str_sub(file, 102L, 108L) |> as.Date("%Y%j"))
toc() #0.3s


dat <- dat |>
    mutate(gdal_datasets = paste0('HDF4_EOS:EOS_GRID:\"', file, '\":MODIS_Grid_16DAY_1km_VI:\"', subdataset, "\"")) |>
    mutate(file = as.character(file), subdataset = as.character(subdataset))


tic()
fls |>
    group_by(grid) |>
    summarize(n = n()) |> skimr::skim()
toc() # 294 tiles, 552 temporal observations (biweeks)


fls |>
    filter(grid == "h10v08") |>
    pull(files) |>
    file.size() |>
    sum() / 1024^3 # 9.5Gb

x <- fls |> filter(grid == "h10v08") |> pull(files)

## corner coords ##

coords <- read_table(file = "~/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/sn_bound_10deg.txt", skip = 5, col_types = "iidddd")
crds <- coords |>
    filter(ih == 10, iv == 8)

longitude <- seq(from = crds$lon_min, to = crds$lon_max, length.out = 1200)
latitude <- seq(from = crds$lat_min, to = crds$lat_max, length.out = 1200)

#### stars ####

## It does not work with stars: it seems not good with hdf4 files, and I don't
## see how to twick the gdal options
## see: https://github.com/r-spatial/stars/issues/306
sd <- gdal_subdatasets(dat$file[100])
# fail with one file, with or without curvilinear
x <- read_stars(sd[[1]], curvilinear = list(x = longitude, y = latitude))
# fails with proxy object, with or without long name format (pointing to subdataset)
y <- read_stars(
    .x = dat |> filter(grid == "h10v08") |> slice(100) |> pull(gdal_datasets),
    sub = 1, driver = "HDF4", #curvilinear = list(x = longitude, y = latitude),
    quite = TRUE, proxy = FALSE
)

#### terra ####
# package luna offers tools to download Modis data ()
# Terra works with rast for one file, but not optimal: it brings file to memory
library(raster)
r <- brick(fls |> filter(grid == "h10v08") |> slice(10) |> pull(files))
plot(r[[1]]) # fails

x <- terra::rast(
    x = dat |> filter(grid == "h10v08") |> slice(10) |> pull(file) ,
    drivers = "HDF4", lyrs = 1, raw = TRUE,# nrows = 1200, ncols = 1200,
    #xmin = crds$lon_min, xmax = crds$lon_max, ymin = crds$lat_min, ymax = crds$lat_max,
    #crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs",
    #keeptime = TRUE, keepunits = TRUE
    )
plot(x)

# produce the same GDAL ERROR:
#band 1: IReadBlock failed at X offset 0, Y offset 0: GDreadfield() failed for block. (GDAL error 1)
s <- sds(fls |> filter(grid == "h10v08") |> slice(10) |> pull(files))

s[1]
crs(x) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"# |> cat() # coordinate reference system
dim(x) # 1200 x 1200 pxls, 12 layers
nlyr(x)
ncell(x)
res(x) # resolution
x |> names()

x
# I think the problem is that rast creates a pointer to the data and the coordinate system
# is not correct, the corner coordinates do not match with Colombia
# see: https://gis.stackexchange.com/questions/199059/understanding-gdal-error-1-too-many-points-failed-to-transform
describe(fls |> filter(grid == "h10v08") |> slice(10) |> pull(files))

x[10,10]

plot(s[1]) # fails

#### hdf5 ####
# I will need to convert all files to HDF5 or NetCDF: https://pjbartlein.github.io/REarthSysSci/hdf5_intro.html
# not ideal
library(rhdf5)
h5ls(file = fls |> filter(grid == "h10v08") |> slice(10) |> pull(files))

#### gdalcubles: ####
## following: https://gdalcubes.github.io/source/tutorials/vignettes/gc01_MODIS.html
gdalcubes_gdalformats() |> str_subset("HDF") # includes HDF4
collection_formats()
# Colombia h10v08: https://modis-land.gsfc.nasa.gov/MODLAND_grid.html
# Brazil Amazon h11v09
tic()
x <- create_image_collection(
    files = dat |> filter(grid == "h10v08") |> pull(file),
    format = "MxD13A2")
toc() #98s
x

gdalcubes_gdalversion()
Sys.getenv()
# creating a cube view, pixel size is 1000m, see: https://lpdaac.usgs.gov/products/myd13a2v061/
# sinusoidal projection from: https://epsg.io/54008
tic()
v <- cube_view(
    extent = x,
    # this specs follows the info on the MODIS website
    #nx = 1200, ny = 1200, nt = 485, srs = "ESRI:54008", # same sinusoidal but in short code
    # lower res
    dx = 500, dy = 500, dt = "P16D",
    aggregation = "median", resampling = "average",
    #srs = "ESRI:54008", # same sinusoidal but in short code: "ESRI:54008"
    srs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", # sinusoidal projection
    # this specs the tutorial of gdalcubes
    # srs = "EPSG:3857", # WGS84 / pseudo mercator projection
    # dx = 5000, dy = 5000, dt = "P16D"
    )
toc() #0.003s
v
dc <- raster_cube(x, v)
dc


## see gdalcubes_selection
## subset cubes: object["band", "time", "x", "y"]
dc["NDVI", c("2002-07-04","2024-02-22") , c(200), c(200)] |> plot() ## All zeroes, why?
## errors are: GDAL CE_Failure (1): GDreadfield() failed for block.
dc["NDVI", c("2024-02-22") , , ] |> plot(col=heat.colors, key.pos=1)
## same errors, then assumes all values are nans

## the following wont work neither until GDAL read is fixed
# tic()
# resp <- dc |>
#     select_bands(c("NDVI")) |>
#     fill_time(method = "near") |>
#     reduce_time("max(NDVI)")
# toc()
#
# resp
#
# tic()
# resp |> plot(key.pos = 1)
# toc() # 215s
#
# x |> class()
#
# tic()
# dc |>
#     select_bands("NDVI") |>
#     reduce_time("median(NDVI)") |>
#     plot(col=heat.colors, key.pos=1)
# toc()


#### On Tiffs ####
tic()
ndvi <- dir_ls("data/tiles_tif/") |>
    str_subset(pattern = "h10v08")
toc() # 1.4s

dat <- tibble(
    fls = ndvi) |>
    mutate(
        date0 = str_sub(ndvi, 25L, 31L)) |>
    mutate(date = as.Date(date0, "%Y%j")) |>
    mutate(jour = str_sub(date0, 5L, -1L)) |>
    filter(date > "2002-06-25", date < "2024-02-14")

dat

tic(); x <- create_image_collection(
    files = dat$fls, date_time = dat$date, band_names = "ndvi"
); toc()
x

tic()
v <- cube_view(
    extent = x,
    dx = 3000, dy = 3000, nt = nrow(dat), # larger pixels, lower resolution (pixels of 3k)
    #nx = 1200, ny = 1200, nt = nrow(dat), # original res at 1km / pxl
    srs = "ESRI:54008", # same sinusoidal but in short code
)
toc() #0.003s
v

dc <- raster_cube(x,v)
dc

dc["ndvi", c("2002-06-26T00:00:00","2024-02-10T00:07:20") , c(200), c(200)] |> plot() ## All zeroes, why?
## errors are: GDAL CE_Failure (1): GDreadfield() failed for block.
dc["ndvi", c("2002-06-26T00:00:00") , , ] |> plot(key.pos = 1, col = viridis::viridis)

nans <- apply_pixel(dc, FUN=function(x) { is.na(x["ndvi",]) }, names = "nans")

kndvi <- apply_pixel(dc, "tanh((ndvi/10000)^2)", "kndvi")
    #fill_time(method = "near") will do imputation myself

join_bands(list(kndvi, nans))

tic()
kndvi["kndvi", c("2002-06-26T00:00:00") , , ] |> plot(key.pos = 1, col = viridis::viridis)
toc() #1.1s on full res | 108.231s when filling missing values with "near"

# unstable behaviour: it adds one time step at the end despite using the same time units
# as the object kndvi. changing the date at the end does not work. Fill an issue on github, stop using it for now
one_pxl <- kndvi["kndvi", c("2002-06-26T00:00:00","2024-02-09T00:00:00") , c(200), c(200)]
one_pxl |> as_tibble() |> tail() #its adding an NaN at the end!

tic()
ews <- kndvi |>
    reduce_time(
        names = c( "ews_ac1", "ews_fd", "ews_kur", "ews_skw", "ews_std"), FUN = function(x){
            kndvi <- x["kndvi",]
            #kndvi <- kndvi[1:983] # fix the appropriate length
            if (all(is.na(kndvi))) {
                return(rep(NA, 5))
            }
            library(dplyr)
            # time series
            library(fpp3)
            library(tsibble)
            library(fable)
            library(imputeTS)
            library(slider)
            library(fractaldim)
            load('data/calendar.Rda')
                # Interpolate implicit and explict missing values
                d <- tibble(kndvi = kndvi, date = incomp_dates) |>
                    right_join(calendar) |>
                    mutate(nans = is.na(kndvi)) |>
                    mutate(kndvi = na_interpolation(kndvi, option = "spline"))
                # Perfomr STL decomposition and extract the remainder time series
                r <- as_tsibble(d, index = "tid") |>
                    model(STL(kndvi ~ season(23) + season(45) )) |>
                    components()
                d$remainder <- r$remainder
                ## Set the window for early warning signals = 3 years of 46 weeks/yr
                window = 46*3 # years of 46 weeks
                d <- d |>
                    mutate(
                        # fractal dim cannot have NAs, hence computed first
                        ews_fd = slider::slide_dbl(
                            .x = remainder,
                            .f = function(x) {
                                z <- fd.estimate(x, method = "madogram")$fd
                                return(as.vector(z))} , .before = window, .after = 0, .complete = TRUE)) |>
                    # recover missing values
                    mutate(remainder = case_when(nans == TRUE ~ NA, .default = remainder)) |>
                    mutate(
                        ews_std = slider::slide_dbl(
                            .x = remainder, .f = sd, na.rm = TRUE,
                            .before = window, .after = 0, .complete = TRUE),
                        ews_ac1 = slider::slide_dbl(
                            .x = remainder, # turned back to pearson, kendall does not work better
                            .f = function(x) cor(x,lag(x,1), use = "pairwise.complete.obs", "pearson"),
                            .before = window, .after = 0, .complete = TRUE),
                        ews_kur = slider::slide_dbl(
                            .x = remainder, .f = moments::kurtosis, na.rm = TRUE,
                            .before = window, .after = 0, .complete = TRUE),
                        ews_skw = slider::slide_dbl(
                            .x = remainder, .f = function(x) abs(moments::skewness(x, na.rm = TRUE)),
                            .before = window, .after = 0, .complete = TRUE)
                    )
                # Compute delta per pixel
                delta <- bind_rows (
                    # maximum
                    d |>
                        select(tid, starts_with("ews")) |>
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
                    d |>
                        select(tid, starts_with("ews")) |>
                        pivot_longer(starts_with("ews"), names_to = "ews", values_to = "value") |>
                        as_tibble() |>
                        group_by(ews) |>
                        mutate(is_min = value == min(value, na.rm = TRUE)) |>
                        filter(is_min == TRUE) |> select(-is_min) |>
                        mutate(type = "min") |>
                        # selects first instance when diff == 0 many consecutive time steps
                        filter(tid == min(tid))
                ) |> arrange(ews, tid) |>
                    group_by(ews) |>
                    summarize(delta = diff(value), .groups = "drop") |>
                    pivot_wider(names_from = ews, values_from = delta)

                return(unlist(delta))
        })
toc()

ews

tic()
system.time(ews |> write_ncdf("test_ews.nc", overwrite = TRUE))
toc() # 15s per pixel with one worker.

# If an image is 1200*1200 it has max 1.44 M pxls to compute. If the work is divided
# in 12 cores / workers, each core computes 120k pxls, at 15s/pxl = 500hrs = 21 days
# of computing time, assuming no errors (for one tile!)

tic()
ews |> write_ncdf("test_ews.nc", overwrite = TRUE)
toc() # 3848.025 | takes 1hrs!!
### one pxl

tb <- kndvi["kndvi", 1:983 , c(500), c(500)] |> as_tibble()


tb <- tb |>
    add_column(date = incomp_dates) |>
    right_join(calendar) |>
    mutate(nans = is.na(kndvi)) |>
    mutate(kndvi = na_interpolation(kndvi, option = "spline"))
out <- tb |>
    as_tsibble(index = "tid") |>
    model(STL(kndvi ~ season(23) + season(45) )) |>
    components()
tb$remainder <- out$remainder


tb <- tb |>
    mutate(
        # fractal dim cannot have NAs
        ews_fd = slider::slide_dbl(
            .x = remainder,
            .f = function(x) {
                z <- fd.estimate(x, method = "madogram")$fd
                return(as.vector(z))} , .before = window, .after = 0, .complete = TRUE)) |>
    mutate(remainder = case_when(nans == TRUE ~ NA, .default = remainder)) |>
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
            .before = window, .after = 0, .complete = TRUE)
    )
tb
d <- bind_rows (
    # maximum
    tb |>
        select(tid, starts_with("ews")) |>
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
    tb |>
        select(tid, starts_with("ews")) |>
        pivot_longer(starts_with("ews"), names_to = "ews", values_to = "value") |>
        as_tibble() |>
        group_by(ews) |>
        mutate(is_min = value == min(value, na.rm = TRUE)) |>
        filter(is_min == TRUE) |> select(-is_min) |>
        mutate(type = "min") |>
        # selects first instance when diff == 0 many consecutive time steps
        filter(tid == min(tid))
) |> arrange(ews, tid) |>
    group_by(ews) |>
    summarize(delta = diff(value), .groups = "drop") |>
    pivot_wider(names_from = ews, values_from = delta)
d
tb |> pivot_longer(cols = starts_with("ews"), names_to = "ews", values_to = "value") |>
    ggplot(aes(date, value)) + geom_line() + facet_wrap(~ews, scales = "free")


tb |>  pull(kndvi) |> fd.estimate(window.size=window) -> fd
