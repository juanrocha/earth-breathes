library(tidyverse)
library(terra)
library(tidyterra)
library(tictoc)
library(fs)

## No need to read this again, only here for archiving
# fls <- dir_ls("results/")
#
# tic()
# is_empty <- map_lgl(fls, function(x){
#     load(x)
#     length(deltas) > 0
# })
# toc() # 654s in sequential, FALSE is empty, true is not empty
#
# fls[is_empty][27]
#
# load_tiles <- function(file){
#     load(file)
#     deltas <- transpose(deltas)
#     is_ok <- map_lgl(deltas$error, is.null)
#     which(!is_ok) |> length() # 0 errors
#     dat <- deltas$result |>
#         bind_rows()
#     dat <- dat |>
#         left_join(
#             keys |>
#                 rownames_to_column("id") |>
#                 mutate(id=as.numeric(id)) ) |>
#         select(x,y,starts_with("ews"))
#     rm(deltas, keys)
#     return(dat)
# }
#
# # test
# tic()
# out <- map(.x = fls[is_empty], load_tiles)
# toc() # 1564.453 sec elapsed, 26mins
#
### try one variable at the time to save memory
# tic()
# out <- out |>
#     bind_rows()
# toc() #1.6 sec elapsed
# tic()
# save(out, file = "data/deltas_combined.Rda")
# toc() #169.167 sec elapsed

load("data/deltas_combined.Rda")

## create an empty raster, and work on sinusoidal coordinates
tic()
out <- rast(
    out, #|> select(x,y),
    type = "xyz", crs="ESRI:54008")
toc() # 26s

## Terrestrial ecosystems data:
terr_eco <- vect("~/Documents/Projects/DATA/terr-ecoregions-TNC/tnc_terr_ecoregions.shp")
terr_eco <- project(terr_eco, "ESRI:54008") # project to sinusoidal

## raterize biomes
tic()
terr_eco <- rasterize(terr_eco, out, field = "WWF_MHTNUM")
toc() #5.5s

add(out) <- terr_eco

## bring back to df
tic()
out <- out |>
    as.data.frame(xy = TRUE, na.rm = TRUE) |>
    as_tibble()
toc() # 82.157 sec elapsed


tic()
df_stats <- out |>
    rename(biome = WWF_MHTNUM) |>
    pivot_longer(cols = starts_with("ews_"), names_to = "stat", values_to = "value") |>
    group_by(biome, stat) |>
    summarize(p05 = quantile(value, 0.05), p95 = quantile(value, 0.95),
              p10 = quantile(value, 0.1), p90 = quantile(value, 0.9))
toc() # 45s

save(df_stats, file = "data/quantiles.Rda")


#### Plots ####
# If needed re-load the out object, currently data frame, convert in spatRaster
# then plot with terra.


## re-project in WGS84
tic()
out <- project(out, "epsg:4326")
toc() # 1711.365 sec elapsed, 28min


# plot with terra
tic()
plot(out, "ews_std")
toc() # 5.7s, 2.1s if re-projected

# plot with ggplot and tidyterra:
# Error: vector memory limit of 128.0 Gb reached, see mem.maxVSize()
# tic()
# out |>
#     ggplot() +
#     geom_spatraster(aes(fill = ews_ac1))
# toc()

lobstr::obj_size(out) # 9.54MB
terra::ncell(out)*terra::nlyr(out) # 29B data points, not good with tidyterra


#### Detect corrupted files ####
tic()
is_wrong <- map_lgl(fls, function(x){
    load(x)
    deltas <- transpose(deltas)
    is_ok <-  map_lgl(deltas$error, is.null)
    all(is_ok)
})
toc() # 826s; TRUE is okay, FALSE is wrong

is_empty <- !is_empty # correct the logic
is_wrong <- !is_wrong

fls[is_wrong] # remember, FALSE is the correct answer

file_move(fls[is_wrong], "left-overs/")
