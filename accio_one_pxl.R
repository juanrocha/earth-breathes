# Goal: given the coordinates of a pixel retrieve the time series.

library(tidyverse)
library(tictoc)
library(stars)
library(sf)

## Import points of interest and load the dataset with the grids
pts <- read_csv("data/GTM_full_database_download_20241213-084339.csv") |>
    janitor::clean_names() |>
    select(-species, -ref_id, -doi)

pts <- pts |>
    st_as_sf(coords = c("long", "lat"), crs = 4326)

# working on sinusoidal projection
pts <- pts |>
    st_transform("ESRI:54008")

# grd <- read_table(
#     file = "~/Documents/Projects/DATA/LP_DAAC_Data_Pool/daac_data_download_r/sn_bound_10deg.txt",
#     skip = 6)

load("data/corners.Rda")

pts <- pts |>
    st_coordinates() |>
    cbind(pts) |>
    rename(lon = X, lat = Y)

check_up <- function(lon, lat ,y){
    filter(y, xmin < lon & lon < xmax & ymin < lat & lat < ymax) |>
        pull(grid)
}

# test
check_up(lon = pts$lon[3], lat = pts$lat[3], y = bb)

pts <- pts |>
    rowwise() |>
    mutate(grid = check_up(lon, lat, bb)) |>
    ungroup()

pts <- pts |>
    group_by(grid) |>
    split(~grid)

pts[[1]]
names(pts)[1]

## read one grid, using code from ews_tile.R
load("data/tiles.Rda")

tic("read files")
x <- map(
    tiles |>
        filter(grid == names(pts)[10]) |>
        mutate(destination = paste0("data/tiles_tif/", destination)) |>
        pull(destination),
    stars::read_stars, proxy = TRUE, .progress = TRUE)
toc() # 2.9s | map is faster

st_crs(x[[1]])
st_crs(pts[[1]])

tic()
x <- map(
    .x = x,
    function(x){
        st_extract(st_set_crs(x, "ESRI:54008"), pts[[10]]) |>
            rename(ndvi = ends_with("tif")) |>
            mutate(ndvi = ndvi / 10^8)  # correct units
    }
)
toc() # 27.224 sec elapsed for 290 points

# add the time id
x <- map2(x, seq_along(x), function(x,y) x |> add_column(tid = y))

x <- bind_rows(x)

x <- x |>
    group_by(geometry) |>
    mutate(id = cur_group_id()) |>
    ungroup()

x |> filter(id == 3) |> ggplot(aes(ndvi)) + geom_density() + geom_vline(aes(xintercept =  median(ndvi)))
    left_join(calendar) |>
    ggplot(aes(date, ndvi)) + geom_line()

## next steps: replicate the extraction of one pixel for all pixels. But do it once
# you have a concrete set of true positives and true negatives.
# It would also be nice to do the same for a random sample of pixels per biome,
# to see if one can detect differences between forest / savanna or other RS that
# are in fact change in LU.


#### left overs ####
## Before I was working with the lon lat bbox given by NASA in 10 deg; but it didn't work:
# for some strange reason some points appear in more than one tile. Not sure if this
# is an error from NASA, the tiles should not have overlap. However, I hope it gets solved
# with coordinates in sinusoidal
pts |> add_column("tile_id" = out) |>
    unnest(tile_id)

# annother option is to reconstruct the bbox for all tiles manually to be on the safe side
### I solved the bbox problem by extracting the bbox from the files using corners.R
###
## remove empty grids
# grd <- grd |>
#     mutate(across(starts_with("l"), as.numeric)) |>
#     mutate(all_neg = lon_min < -998 & lon_max < -998 &lat_min < -98 & lat_max < -98 ) |>
#     filter(!all_neg)
#
# grd <- grd |>
#     # remove last row, it's a note from the data provider
#     slice(-n())
#
# grd <- grd |>
#     unite("tile_id", ih, iv, sep= "v") |>
#     mutate(tile_id = str_c("h", tile_id))
