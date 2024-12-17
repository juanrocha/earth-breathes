library(tidyverse)
library(fs)
library(tictoc)

load("data/deltas_Colombia.Rda")
load("data/tiles.Rda")
lobstr::obj_size(deltas) # 52MB
deltas |>
    bind_rows() |>
    left_join(
        keys |> rownames_to_column('id') |>
            mutate(id = as.numeric(id))) |>
    pivot_longer(cols = starts_with("ews"), names_to = "ews", values_to = "value") |>
    ggplot(aes(value)) +
    geom_density() +
    facet_wrap(~ews, scales = "free")

deltas |>
    bind_rows() |>
    left_join(
        keys |> rownames_to_column('id') |>
            mutate(id = as.numeric(id))) |>
    ggplot(aes(x,y)) +
    geom_tile(aes(fill = ews_ac1)) +
    scale_fill_viridis_c()


load("results/deltas_h10v08.Rda")
deltas |>
    bind_rows() |>
    ggplot(aes(ews_ac1)) +
    geom_density()
