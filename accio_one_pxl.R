# Goal: given the coordinates of a pixel retrieve the time series.

library(tidyverse)
library(tictoc)
library(stars)
library(sf)
library(future)
library(furrr)

## Import points of interest and load the dataset with the grids
pts <- read_csv("data/GTM_full_database_download_20241213-084339.csv") |>
    janitor::clean_names() |>
    select(-species, -ref_id, -doi)

pts <- pts |>
    st_as_sf(coords = c("long", "lat"), crs = 4326)

## Data curated by Nielja:
pts <- arrow::read_feather("data/25_01_20_hammond_all.feather") |> #skimr::skim()
    filter(!is.na(lon), !is.na(lat)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

# working on sinusoidal projection
pts <- pts |>
    st_transform("ESRI:54008")

# biome sample pxls
pts <- biome_sample |>
    st_as_sf(coords = c("x", "y"), crs = "ESRI:54008")

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
            # recover the date
            mutate(y_j = str_sub(names(x), 10L, 16L)) |>
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

rm(x) # dont' want to confuse myself later

## next steps: replicate the extraction of one pixel for all pixels. But do it once
# you have a concrete set of true positives and true negatives.
# It would also be nice to do the same for a random sample of pixels per biome,
# to see if one can detect differences between forest / savanna or other RS that
# are in fact change in LU.
out <- list()

tic()
for (i in seq_along(names(pts))){
    tic("read files")
    x <- map(
        tiles |>
            filter(grid == names(pts)[i]) |>
            mutate(destination = paste0("data/tiles_tif/", destination)) |>
            pull(destination),
        stars::read_stars, proxy = TRUE, .progress = TRUE)
    toc() # 2.9s | map is faster
    tic("extract points")
    x <- map(
        .x = x,
        function(x){
            st_extract(st_set_crs(x, "ESRI:54008"), pts[[i]]) |>
                rename(ndvi = ends_with("tif")) |>
                # recover the date
                mutate(y_j = str_sub(names(x), 10L, 16L)) |>
                mutate(ndvi = ndvi / 10^8)  # correct units
        },
        .progress = TRUE
    )
    toc()
    # tic("add time id") # time id is recovered with the calendar in ews
    # x <- map2(x, seq_along(x), function(x,y) x |> add_column(tid = y))
    # toc()

    x <- bind_rows(x)
    out[[i]] <- x
}
toc() #2070.042 sec elapsed | 35m
names(pts)

pts[[1]] |> nrow()
out[[1]] |>
    group_by(geometry) |>
    mutate(id = cur_group_id()) |>
    ungroup() |>
    pull(id) |>
    unique() |> length()

## Some points get lost because they are within the same pixel. To test it check:
pts[[1]] |>
    group_by(geometry) |>
    mutate(id = cur_group_id()) |>
    ungroup() |>
    pull(id) |>
    unique() |> length()

# meaning there are actually 422 unique points
# ask Nielja why:

pts |> bind_rows() |>
    filter(is.na(distance_to_paired)) |>  # 1254 points has NA on distance_to_paired
    select(lon, lat, distance_to_paired, true_pos_neg) |>
    mutate(class = case_when(true_pos_neg == "true_pos" ~ TRUE, .default = FALSE)) |>
    pull(class) |> all() # all missing distance are true positives.

## combine points into one datase:
tic()
out <- out |> bind_rows()
toc() # 1.2s

out |>
    group_by(geometry) |>
    mutate(id = cur_group_id()) |>
    ungroup() |>
    pull(id) |>
    unique() |> length() # 2043 points

tic()
out <- out |>
    group_by(geometry) |>
    mutate(id = cur_group_id()) |>
    ungroup() |>
    split(~id)
toc() # 9.4s

#### early warning signals ####
## load EWS functions
source("tools.R")
plan(multisession)
window <- 92 # 2yrs

deltas <- list()

tic("calculating ews")
deltas <- future_map(out, .f = safe_delta, .progress = TRUE)
toc() # 12504.892 | 3.5hrs!
# 2000+pxls ~ 12.9s

## True pos and true neg analysis

deltas <- transpose(deltas)
is_ok <- map(deltas$error, is.null)
all(unlist(is_ok))

dlt_df <- bind_rows(deltas$result)

keys <- map(out, function(x) select(x, geometry, id) |> unique()) |> bind_rows()

pts <- bind_rows(pts)
pts |>
    st_join(keys) |>
    left_join(dlt_df) |>
    ggplot(aes(abs(ews_ac1), true_pos_neg)) +
    geom_violin()

## Visually there is no differences between true positives and true negatives.
## test for changes on the time series at the time where the die-off is expected to happen
## test again not on the residuals but in the residuals + long term trend.
## residuals are zero mean unit variance so it might have too little info

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

pts
out[[199]]

x |>
    mutate(y_j = parse_date_time(y_j, 'Yj')) |>
    pivot_longer(cols = kndvi:season_adjust, names_to = "comps", values_to = "values") |>
    ggplot(aes(y_j, values)) +
    geom_line() +
    geom_vline(xintercept = parse_date_time("2012001", "Yj"), color= "red") +
    facet_wrap(~comps, ncol =1, scales = "free_y")

x |>  pull(kndvi) |> var()
x |> pull(remainder) |> mean()
x |> mutate(ts = trend + remainder) |>
    mutate(y_j = parse_date_time(y_j, 'Yj')) |>
    ggplot(aes(y_j, ts)) +
    geom_line()

out[[199]] |>
    ggplot(aes(ndvi)) + geom_density()

m_pelt <- x |>
    mutate(test = remainder + season_23 + season_adjust) |>
    pull(test) |>
    cpt.mean(penalty = "BIC", method = "PELT", Q = 3)

cpts(m_pelt)

# 250207: work only on pixels where the dieoff event occurred on the second half of the time series
# test different rolling windows
# for pixels with occurrence on the first half of the ts, run ews backwards

keys

after_2009 <- pts |>
    filter(year_disturbance > 2009) |>
    pull(geometry)

dlt_df |>
    right_join(
        keys |> filter(geometry %in% after_2009)
    ) |>
    right_join(pts) |> #select(id, year_disturbance)
    ggplot(aes(ews_fd, true_pos_neg)) +
    geom_violin()


out[[1]]


### Save to further explore later
# out: list with ndvi data per pixel
# pts: df with points by Nielja
# keys: id of point coordinates
# dlt_df: df with ews results

save(out, pts, keys, dlt_df, file = "data/dieoffs.Rda")

## Points for sample biomes:
save(out, pts, keys, file = "data/biome_data.Rda")
