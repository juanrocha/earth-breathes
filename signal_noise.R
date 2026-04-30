# signal to noise

library(tidyverse)
library(tictoc)
library(stars)
library(sf)
library(terra)
library(tidyterra)
library(changepoint)

#### biome data ####
load("data/biome_data.Rda")
load("data/calendar.Rda")
source("tools.R")


out[[1]]
keys
pts

tic()
out <- bind_rows(out)
toc() # 4s

tic()
out <- out |> 
    st_join(pts |> select(geometry, WWF_MHTNUM))
toc()

## extract the biome names
# biomes <- read_sf("~/Documents/Projects/DATA/terr-ecoregions-TNC/tnc_terr_ecoregions.shp")
# biomes
biomes <- biomes |> 
    as.data.frame() |> 
    select(WWF_MHTNUM, WWF_MHTNAM) |> 
    unique()

out |> 
    rename(biome = WWF_MHTNUM) |>
   # filter(biome == 1) |> 
    mutate(y_j = parse_date_time(y_j, "Yj"),
           kndvi = tanh(ndvi^2)) |> 
    ggplot(aes(kndvi, as_factor(biome))) +
    geom_boxplot(aes(fill = as_factor(biome)), alpha = 0.5) 

out |> 
    as.data.frame() |> 
    group_by(WWF_MHTNUM) |> 
    summarize(mean = mean(ndvi), variance = var(ndvi)) |> 
    left_join(biomes) |> 
    ggplot(aes(mean, variance)) +
    geom_point(aes(color = WWF_MHTNAM))

out |> 
    rename(biome = WWF_MHTNUM) |>
    filter(biome == 6) |> 
    mutate(y_j = parse_date_time(y_j, "Yj"),
           kndvi = tanh(ndvi^2)) |> 
    ggplot(aes(y_j, ndvi)) +
    geom_line(aes(color = as_factor(biome)), alpha = 0.1) +
    geom_smooth()

## Try the decomposition and see if it is easier to distinguish means from variance
## with the different components. The idea is to reduce var and max mean diff.
pxl <- out |> 
    filter(id == 1) |> 
    decompose_long()

pxl |> 
    mutate(y_j = parse_date_time(y_j, "Yj")) |> 
    #pivot_longer(cols = kndvi:season_adjust, names_to = "component", values_to = "value") |> 
    ggplot(aes(y_j, remainder+trend)) +
    geom_line() +
    facet_wrap(~component, ncol = 1, scales = "free_y")

tic()
out <- out |> 
    split(~id)
toc() # 4s

tic()
out <- map(out, decompose_long, .progress = TRUE)
toc() # 259s

out[[1]]

tic()
out <- bind_rows(out)
toc() # 4.2s

out |> 
    filter(WWF_MHTNUM != 99, WWF_MHTNUM!=98, !is.na(WWF_MHTNUM)) |> 
    ggplot(aes(trend + remainder)) +
    geom_boxplot(aes(fill = as_factor(WWF_MHTNUM))) 

#### true and false positives ####
load("data/dieoffs.Rda")
load("data/calendar.Rda")
source("tools.R")
window <-  92

pts |> filter(true_pos_neg == "true_neg", year_disturbance > 2011, paired_id == 831) 
which(pts$paired_id == 831)[2]
tp <- out[[199]] # true positive
tn <- out[[448]] # true negative pair

x <- bind_rows(tp, tn)
tp |> 
    mutate(ndvi = slider::slide_dbl
           (ndvi, mean, .before = 4, .after=4, .complete = TRUE)) |> 
    decompose_long() |> 
    mutate(test = trend + remainder + season_23) |>
    # pull(test) |>
    # cpt.meanvar(penalty = "BIC", method = "BinSeg", Q = 5) |>
    # cpts()
    #early_warnings()
    mutate(y_j = parse_date_time(y_j, "Yj")) |> 
    ggplot(aes(y_j, test)) + 
    geom_line() +
    annotate("point", x = parse_date_time(tp$y_j[c(416,596,682,749,964)], 'Yj'), y = rep(0,5), size = 3 ) +
    geom_vline(xintercept = parse_date_time(c("2012001", "2013001"), 'Yj'), color = "red")

## the pairing is not necessarily working, it is matching very different pixel / biomes.
rm(x)
tp