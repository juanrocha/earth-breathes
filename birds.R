library(tidyverse)
library(curl)
library(glue)
library(tictoc)
library(furrr)
library(fs)

# Using fasterize# Using eBird API to get raw data
res <- new_handle() |>
    handle_setheaders("X-eBirdApiToken" = keyring::key_get('eBird')) |>
    curl_fetch_memory(url = "https://api.ebird.org/v2/ref/region/list/country/world" )

countries <- res$content |>
    rawToChar() |> jsonlite::prettify() |> jsonlite::fromJSON() |>
    as_tibble()

countries |> filter(name == "Colombia") #CO

# request all birds of Colombia:
h <- new_handle() |>
    handle_setheaders("X-eBirdApiToken" = keyring::key_get('eBird'))

res <- curl_fetch_memory("https://api.ebird.org/v2/data/obs/CO/recent?back=30", handle = h)

birds <-  res$content |>
    rawToChar() |> jsonlite::fromJSON() |>
    as_tibble()

birds |> pull(obsDt) |> range()

birds |>
    ggplot(aes(lng,lat)) +
    geom_point()

## for a particular day
feb <- curl_fetch_memory("https://api.ebird.org/v2/data/obs/CO/historic/2025/2/7", h)

feb$content |> rawToChar() |> jsonlite::fromJSON() |>
    as_tibble()

## for a month
calendar <- tibble(date = as.Date("2020-02-01") + 0:30) |>
    mutate(year = year(date), month = month(date), day = day(date)) |>
    mutate(url =  glue::glue("https://api.ebird.org/v2/data/obs/CO/historic/{year}/{month}/{day}"))

feb <- map(
    .x = calendar$url,
    .f = function(x){
        res <- curl_fetch_memory(x, h)
        res$content |> rawToChar() |> jsonlite::fromJSON() |>
            as_tibble()
    }, .progress = TRUE)

feb <- bind_rows(feb)
feb |>
    ggplot(aes(lng,lat)) +
    geom_point()

## EWS were calculated from 2002-2024
# as.Date("2002-01-01") - as.Date("2024-12-31") # 8400 days
calendar <- tibble(date = as.Date("2002-01-01") + 0:8400) |>
    mutate(year = year(date), month = month(date), day = day(date)) |>
    mutate(url =  glue::glue("https://api.ebird.org/v2/data/obs/CO/historic/{year}/{month}/{day}")) |>
    mutate(url = as.character(url))

calendar <- calendar |> split(~year+month, sep = "_")

#plan(multisession)

tic()
walk2(
    .x = calendar, .y = names(calendar),
    .f = function(x,y){
        tic(paste0("File ", y, " took:"))
        # download the birds data for a month, one query per day
        birds <- map(
            .x = x$url,
            .f = function(x){
                res <- curl_fetch_memory(x, h)
                birds <- res$content |> rawToChar() |> jsonlite::fromJSON() |>
                    as_tibble()
            }, .progress = TRUE)
        # collecte results in data frame and save to csv (better for reloading)
        birds <- bind_rows(birds)
        write_csv(birds, file = paste0("data/ebird_COL/birds_", y, ".csv"))
        toc()
    }, .progress = TRUE
)
toc() # 20006.403 sec elapsed | 5.5hrs on sequential


ls()

#plan(sequential)

## check results
fls <- dir_info("data/ebird_COL/")
tic()
birds <- map(fls$path, read_csv)
toc() # 21.584 sec elapsed

birds <- bind_rows(birds) # 324MB, 2.9M obs
birds |> pull(sciName) |> unique() |> length() # 1965 spp
skimr::skim(birds)


birds |>
    ggplot(aes(lng,lat)) +
    geom_point(size = 1, alpha = 0.25)

save(birds, file = "data/ebird_COL/birds_2002-2024.Rda")
