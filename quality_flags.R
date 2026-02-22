library(tidyverse)
library(stars)
library(RNetCDF)
load("data/tiles.Rda")
load("data/tiles_gdal_translate.Rda")
tiles

# Problem: How to filter the data quality flags from the original dataset? I transformed
# everything to tif so it can be easily read and processed. But if there is quality flags
# for example clouds, it enters the analysis and should be excluded and interpolated.
# The problem is that low quality pixels can increase variance or autocorrelation
# with many days in a row with cloud coverage (common in the tropics). I really need
# to solve the issue


tst <- stars::read_stars(
    .x = tiles |> filter(grid == "h10v08") |>
            arrange(date)  |>  pull(file) |> head(),
    proxy = TRUE
)

tl <- tiles$file[1] |> tidync::tidync()
tl


print.nc(open.nc(tiles$file[1]))
