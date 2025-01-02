# A dataset with true negatives based on LULCC - Copernicus
#
library(tidyverse)
#library(tidync)
library(terra)
library(tictoc)
library(fs)

#### true negatives ####

fls <- dir_ls(path = "~/Documents/Projects/DATA/LULCC/", recurse = TRUE) |>
    str_subset(pattern = "\\.nc$")


dat <- terra::rast(fls[31]) |>
    terra::subset("change_count")

#plot(dat)

# LULCC is at 300m resolution, but the res here is in degrees
res(dat) # 0.0027777777778 ~ 300m.
tic()
d1k <- aggregate(dat, fact = 3.6, fun = "sum")
toc() #75s

# plot(d1k)
rm(dat)


# reproject to sinusoidal to make compatible with MODIS
load("data/deltas_combined.Rda")
out |> class()

tic()
out <- out |>
    rast(type = "xyz", crs = "ESRI:54008")
toc() # 44s

tic()
d1k <- project(d1k, out, method = "near")
toc() # 142s

## remove the zeroes that are on the ocean
tic()
d1k <- mask(d1k, mask = out$ews_ac1)
toc() #10s

writeRaster(d1k, filename = "data/true_negatives_1k.tif",
            progress = TRUE, overwrite = TRUE)

#### extract points ####

# d1k <- rast("data/true_negatives_1k.tif")

plot(d1k)

dat <- extract(d1k, pts)
pts <- pts |> left_join(dat, by = c("ref_id" = "ID"))

pts |>
    mutate(detected = change_count > 0)

world |>
    sf_project(to = "ESRI:54008")
    ggplot() +
    geom_sf() +
    scale_color_viridis_c()


pts |>
    filter(change_count > 0)

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#### left overs ####
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
