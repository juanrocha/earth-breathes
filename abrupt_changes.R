library(tidyverse)
library(strucchange)
library(trend)
library(changepoint)
library(stars)
library(tictoc)


## Apply break point analysis to Colombia
load("data/tiles.Rda")

## Using the approzch from ews_tile that was computationally ok
# i = 53 is Colombia

## read files
tic("read files")
x <- map(
    tiles |>
        filter(grid == "h10v08") |>
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
obs <- tiles |> filter(grid == "h10v08") |> nrow()
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

tic("splitting list")
df_dat <- df_dat |> split(~id)
toc() # 139s | <3min

### Experimentation with structural change

tst <- df_dat[[1]]

tst <- tst |>
    mutate(y_j = parse_date_time(y_j, "yj"),
           ndvi = tanh(ndvi^2))
tst |>
    ggplot(aes(y_j, ndvi)) +
    geom_line()

tst <- tst |>
    rownames_to_column("tid") |> mutate(tid = as.numeric(tid)) |>
    tsibble::as_tsibble(index = "tid") |>
    fabletools::model(feasts::STL(ndvi ~ season(46))) |>
    fabletools::components() |>
    mutate(lag1 = lag(ndvi))

tst |> filter(tid >950) |>
    ggplot(aes(tid, remainder)) + geom_line()
## change in slope: raw ndvi
sens.slope(ts(tst$ndvi))
cor.test(tst$ndvi, tst$tid, method = "kendall")
## change point
pettitt.test(tst$ndvi) # means
## change point that last at least a year (46 weeks of 8 days)
## I don't trust it still... it gives cpts when it shouldnt', never returns NA as
## the help file says when there is no cpt (e.g. when pettitt fails)
## Help says if cpt == length() then is no cpt detected.
cptm <- cpt.mean(tst$ndvi, method = "PELT", minseglen = 46) # AMOC forces only one break point
cptv <- cpt.var(tst$ndvi, method = "PELT", minseglen = 46)
cptmv <- cpt.meanvar(tst$ndvi, method = "PELT", minseglen = 46)
slot(cptmv, 'cpts') == length(tst$ndvi)

cpr <- cpt.mean(runif(1000))
slot(cpr,"cpts")
## structural change:
qlr <- Fstats(ndvi ~ lag1, data = tst)
bps <- breakpoints(qlr)
sct <- sctest(qlr, type = "supF")
bps
#### Left overs ####

### This approach does not work (a stack of rasters). While it does not produce
### error, later when extracting values from pixels it returns NAs. Now, if one
### transform the object from stars to sf the values get recovered, but that
### transformation is time consuming per pixel.

dat <- stars::read_stars(
    .x = paste0(
        "data/tiles_tif/",
        tiles |> filter(grid == "h10v08") |>
            arrange(date)  |>  pull(destination)
        ),
    proxy = TRUE
)

dat <- dat |> merge() |>
    st_set_dimensions(3, values = tiles |> filter(grid =="h10v08") |> pull(date)) |>
    st_set_dimensions(names = c("x", "y", "time")) |>
    st_set_crs("ESRI:54008")

# no need to bring up to memory
# df_dat <- as.data.frame(dat)
# df_dat |> as_tibble()



tic() # returns ts by matrix position on row,cols
dat[,500,500,] |>
    st_as_sf() |>
    st_drop_geometry() |>
    pivot_longer(cols = 1:last_col(), names_to = "date", values_to = "ndvi")
toc() #13.8 s

# if copying coords from internet, order needs to be switched
pnt <- st_sfc(st_point(c( -72.25, 2.02)), crs = 4326) |>
    st_transform(crs = "ESRI:54008")

tic() # using coords for Colombia
x <- st_extract(x = dat , at = matrix(c(-8020026, 68762.51), nrow = 1)) #matrix(c(500,500), nrow = 1)
toc() #14s this works

# tic() # using coords for Colombia
# x <- st_extract(x = dat , at = pnt) #matrix(c(500,500), nrow = 1)
# toc() #14s this however returns only NAs

## tests
df <- tibble(
    date = tiles |> filter(grid =="h10v08") |> pull(date),
    ndvi =  as.vector(x)
)

df <- df |>
    mutate(ndvi = ndvi / 10^8) |>  # correct units
    mutate(ndvi = tanh(ndvi^2)) |>  # correct to kNDVI
    rownames_to_column('tid') |> mutate(tid = as.numeric(tid))

# extract slope, pval, pettitt, struct change and chpts
abrupt_change <- function(x){
    # x is the time series vector
    l <- length(x)
    z <- tibble(x = x) |> rownames_to_column("tid") |>
        mutate(tid = as.numeric(tid)) |>
        mutate(lag_x = lag(x))
    ## change in slope: raw ndvi
    ss <- sens.slope(ts(z$x))
    kt <- cor.test(z$x, z$tid, method = "kendall")

    ## change point:
    pt <- pettitt.test(z$x) # means

    ## structural change:
    qlr <- Fstats(x ~ lag_x, data = z)
    bps <- breakpoints(qlr)
    sct <- sctest(qlr, type = "supF")
    bps

    return(z)
}

df |> ggplot(aes(tid, ndvi)) + geom_line() +
    geom_vline(
        xintercept = cpt.meanvar(df$ndvi) |> slot("cpts"),
        color = "red")
cpt.meanvar(df$ndvi) |> slot("cpts")

abrupt_change(df$ndvi)
