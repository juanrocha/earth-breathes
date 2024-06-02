library(tidyverse)
library(fs)
library(tictoc)
library(terra)
library(stars)
library(gdalcubes)
gdalcubes_options(parallel = 12)


#### Files to read ####
fls <- dir_ls("~/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/MYD13A2-061/") |>
    str_subset(pattern = "\\.hdf$")

fls <- tibble(
    files = fls
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
    sum() / 1024^3 # 9.5Gb

x <- fls |> filter(grid == "h10v08") |> pull(files)

#### stars ####

## It does not work with stars: it seems not good with hdf4 files, and I don't
## see how to twick the gdal options
y <- stars::read_stars(
    .x = fls |> filter(grid == "h10v08") |> slice(10) |> pull(files),
    sub = 1, driver = "HDF4",
    quite = TRUE, proxy = TRUE
)

#### terra ####
# package luna offers tools to download Modis data ()
# Terra works with rast for one file, but not optimal: it brings file to memory
library(raster)
r <- brick(fls |> filter(grid == "h10v08") |> slice(10) |> pull(files))
plot(r[[1]]) # fails

x <- terra::rast(
    x = fls |> filter(grid == "h10v08") |> slice(10) |> pull(files),
    drivers = "HDF4", lyrs = 1)

crs(x) |> cat() # coordinate reference system
dim(x) # 1200 x 1200 pxls, 12 layers
nlyr(x)
ncell(x)
res(x) # resolution
x |> names()

x


x[10,10]

plot(x) # fails

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
    files = fls |> filter(grid == "h11v09") |> pull(files),
    format = "MxD13A2")
toc() #98s

gdalcubes_gdalversion()

Sys.getenv()

# creating a cube view, pixel size is 1000m, see: https://lpdaac.usgs.gov/products/myd13a2v061/
# sinusoidal projection from: https://epsg.io/54008
tic()
v <- cube_view(
    extent = x,
    # this specs follows the info on the MODIS website
    nx = 1200, ny = 1200, nt = 552,
    srs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", # sinusoidal projection
    # this specs the tutorial of gdalcubes
    # srs = "EPSG:3857", # WGS84 / pseudo mercator projection
    # dx = 5000, dy = 5000, dt = "P16D"
    aggregation = "mean", resampling = "bilinear"
    )
toc() #0.003s
v
dc <- raster_cube(x, v)
dc


## see gdalcubes_selection
## subset cubes: object["band", "time", "x", "y"]
dc["NIR", c("2000-02-18T00:00:00","2024-02-02T00:07:11") , c(200), c(200)] |> plot() ## All zeroes, why?


tic()
resp <- dc |>
    select_bands(c("NDVI")) |>
    reduce_time("max(NDVI)")
toc()

resp

tic()
resp |> plot(key.pos = 1)
toc() # 9.4s

x |> class()
