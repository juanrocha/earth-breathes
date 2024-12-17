library(tidyverse)
library(fs)
library(tictoc)
library(future)
library(furrr)
#setwd("~/Documents/Projects/Resilience/earth-breathes")
# 16 cores, 124GB RAM
# load tiles
load("data/tiles.Rda")
#lobstr::obj_size(tiles) ## 228Mb
# memory should not be a problem. The only large obj is df_dat which is chunked.
#oopts <- options(future.globals.maxSize = 7.0 * 1e9) # 7GB per core ~ 124Gb / 16 cores
#on.exit(options(oopts))
#i = 3

#grd <- tiles |> pull(grid) |> unique()
# tile 53 is Colombia, use that one for testing since it's already done

dn <- dir_ls("results/") |> str_remove_all("results/deltas_|\\.Rda")

grd <- tiles |> group_by(grid) |>
    summarize(n = n()) |>
    filter(!is.na(grid), !grid %in% dn) |>
    arrange(desc(n)) |>
    #print(n=295) |>
    pull(grid)

# there are 21 tiles with less than 980obs / 983 expected. Process them first
# the others are from arctic regions, probably less obs due to dark winter

# tiles |>
#     filter(grid == grd[53]) |>
#     mutate(destination = paste0("data/tiles_tif/", destination)) |>
#     select(destination)

# set up the rolling window
window <- 92 # 2yrs

tic()
for (i in seq_along(grd)) {
    tic()
    source("tools.R", local = TRUE) # load functions, local = TRUE

    load("data/tiles.Rda")
    ## read files
    tic("read files")
    x <- map(
        tiles |>
            filter(grid == grd[i]) |>
            mutate(destination = paste0("data/tiles_tif/", destination)) |>
            pull(destination),
        stars::read_stars, proxy = TRUE, .progress = TRUE)
    toc() # 2.9s | map is faster

    # remove missing values on the fly
    tic("remove NAs")
    df_dat <- map(
        x,
        function(x) {
            z <- base::as.data.frame(x) |>
                pivot_longer(cols = ends_with("tif"), names_to = "file", values_to = "ndvi") |>
                mutate(y_j = str_sub(file, 10L, 16L)) |> ## this is very slow
                mutate(ndvi = ndvi / 10^8) |> # correct units
                filter(!is.na(ndvi))
            return(z)
        }, .progress = TRUE ) |>
        bind_rows() |> as_tibble()
    toc() # 136s with map, <3min

    ## remove file and create pixel ids
    tic("create pxl ids")
    df_dat <- df_dat |>
        select(-file) |>
        group_by(x,y)
    toc() # 51s

    tic("extract keys")
    keys <- group_keys(df_dat) # id is the nrow
    toc() #0.08s

    tic("add IDs")
    df_dat <- df_dat |>
        add_column(id = group_indices(df_dat))
    toc() # 61s

    # safe to remove xy coords
    df_dat <- df_dat |> ungroup() |> select(-x,-y)
    #lobstr::obj_size(df_dat) # 21Gb

    ### Remove low quality pixels, with less than 90% complete obs
    obs <- tiles |> filter(grid == grd[i]) |> nrow()
    tic("Identify low quality pxls")
    nans <- df_dat |>
        group_by(id) |>
        summarize(n = n() / obs)
    toc() # 16s

    tic("remove low quality pxls")
    df_dat <- df_dat |>
        anti_join(nans |> filter(n < 0.9), by = join_by("id"))
    toc() # 48s

    # remove unnecesary objects
    tic("cleaning")
    rm(tiles, x, nans, obs)
    gc()
    toc() #53s
    #### test one pxl ####
    # pxl <- df_dat |> filter(id == 777)
    # pxl
    # source("tools.R")
    # tic()
    # pxl |>
    #     decompose() |>
    #     early_warnings() |> #ggplot(aes(tid,ews_ac1)) + geom_line()
    #     extract_delta()
    # toc() #0.25s
    # tic()
    # safe_delta(pxl)
    # toc() #0.2

    ## On sequential, that would be 0.22 s per pxl * 1.1M pxls = 2.8 days of computation
    ## or ~5hrs on parallel with 12 cores.
    ## A full tile 1200^2 pxls will take 6.3hrs on 14 cores

    #### Run on one tile ####
    tic("splitting list")
    df_dat <- df_dat |> split(~id)
    toc() # 139s | <3min
    plan(multisession)
    opts <- furrr_options(
        globals = list(
            calendar = calendar, keys = keys, decompose = decompose,
            early_warnings = early_warnings, extract_delta = extract_delta,
            fast_ews = fast_ews, safe_delta = safe_delta)
        # chunk_size = round(length(df_dat) / 16)
        )

    deltas <- list()

    tic("calculating ews")
    deltas <- future_map(df_dat, .f = safe_delta, .progress = TRUE, .options = opts)
    toc() # 12504.892 | 3.5hrs!

    plan(sequential)
    tic("remove larg objs")
    rm(df_dat) # perhaps removing largest obj on memory avoids crashes
    toc()
    ## save result and investigate problems later
    save(deltas, keys, file = paste0("results/deltas_", grd[i], ".Rda") )
    ## clean up
    rm(deltas, keys)
    gc()
    cat("Step ", i, ": Tile ", grd[i], " took ")
    toc() # log = TRUE, quiet = FALSE
    }
toc()

#3625.301 | 1hr for 1:10 tiles, but most of them on the sea

plan(sequential) # close the workers
#tic.clearlog()

## Total timing: depends on the amount of pixels to process, which differs from tile to tile
## 5min reading and set up
## 3min re-arranging as list for parallel


#### shorter time series ####
# some tiles have less points in time, specially in the Arctic.
df_dat
