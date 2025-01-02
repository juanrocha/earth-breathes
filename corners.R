# get the bbox of each MODIS tile in its native coord system (sinusoidal)

library(tidyverse)
library(tictoc)
library(stars)

load("data/tiles.Rda")

# only need one date
tiles <- tiles |>
    filter(date == "2024-02-10")

check_bbox <- function(x){
    # x is the file
    rst <- stars::read_stars(
        paste0("data/tiles_tif/", x),
        proxy = TRUE, .progress = TRUE)
    bb <- rst |>
        st_as_sf() |>
        st_bbox()
    return(bb)
}

safe_bbox <- safely(check_bbox)

#test
tic()
check_bbox(tiles$destination[3])
toc() # 2s


bbox <- list()

tic()
bbox <- map(tiles$destination, safe_bbox, .progress = TRUE)
toc() #740.256 sec elapsed

bbox <- transpose(bbox)
is_ok <- map(bbox$error, is.null)
all(is_ok) # TRUE

bbox$result[[1]] |> head()

bb <- bbox$result |>
    map(.f = function(x){
        print(x) |> as_tibble() |>
            add_column(names = c("xmin", "ymin", "xmax", "ymax")) |>
            pivot_wider(names_from = names, values_from = value)
    }) |> bind_rows()

bb <- bb |> add_column(grid = tiles$grid)

save(bb, file = "data/corners.Rda")
