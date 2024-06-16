library(tidyverse)
library(fs)
library(tictoc)

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
ndvi <- dir_ls("data/Colombia/")
# dat <- rast(ndvi[1])
# dat
# plot(dat)

dat <- tibble(
    fls = ndvi) |>
    mutate(
        date = str_sub(ndvi, 24L, 30L)) |>
    mutate(date = as.Date(date, "%Y%j"))
dat <- dat |> arrange(date)
#### stars ####
library(stars)
x <- read_stars(
    dat$fls, quiet = TRUE, proxy = TRUE) # proxy object
x


# you need to secure files are read on the right time order, so you need a df with
# file names and time stamps.

#### gdalcubes: not working ####
library(gdalcubes)
gdalcubes_gdalformats() |> str_subset("Tif") # includes HDF4
collection_formats()

tic()
x <- create_image_collection(
    files = dat$fls, date_time = dat$date)
toc() #98s

tic()
v <- cube_view(
    extent = x,
    # this specs follows the info on the MODIS website
    nx = 1200, ny = 1200, nt = length(dat$fls),
    srs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", # sinusoidal projection
)
toc() #0.003s
v
dc <- raster_cube(x, v)
dc

## see gdalcubes_selection
## subset cubes: object["band", "time", "x", "y"]
dc["band1", c("2000-02-18T00:00:00","2024-02-10T00:07:20"), c(200), c(200)] |> plot() # not working
dc["band1", "2000-02-18T00:00:00", , ] |> plot() # working


