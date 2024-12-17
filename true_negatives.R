# A dataset with true negatives based on LULCC - Copernicus
#
library(tidyverse)
#library(tidync)
library(terra)
library(tictoc)
library(fs)

fls <- dir_ls(path = "~/Documents/Projects/DATA/LULCC/", recurse = TRUE) |>
    str_subset(pattern = "\\.nc$")


dat <- terra::rast(fls[31]) |>
    terra::subset("change_count")

## importing with tidync
##
#dat <- tidync::tidync(fls[31])
# tic()
# dat <- dat |>
#     activate(change_count)
# toc()
# arr <- dat |> hyper_array()
# df0 <- arr$change_count |> as_tibble()
# rm(arr, dat)
# gc()

plot(dat)
# LULCC is at 300m resolution, but the res here is in degrees
res(dat) # 0.0027777777778 ~ 300m.
d1k <- aggregate(dat, fact = 3.6, fun = "sum")

plot(d1k)
rm(dat)


# reproject to sinusoidal to make compatible with MODIS
load("data/deltas_combined.Rda")
out |> class()

tic()
out <- out |>
    rast(type = "xyz", crs = "ESRI:54008")
toc() # 35s

tic()
d1k <- project(d1k, out)
toc() # 142s

## remove the zeroes that are on the ocean
tic()
d1k <- mask(d1k, mask = out$ews_ac1)
toc() #10s

save(d1k, file = "data/true_negatives_1k.Rda")
