# Colombian case study for the IAvH
library(tidyverse)
library(terra)
library(tidyterra)
library(tictoc)
library(fs)

load("data/deltas_combined.Rda")
load("data/quantiles.Rda")

# map of Colombia
col <- geodata::gadm(country = "COL", level = 0, path = "data/", version = "latest")
# project Colombia in sinusoidal:
col <- project(col, "ESRI:54008")

tic()
out <- out |>
    rast(type = "xyz", crs = "ESRI:54008")
toc() # 35s

rst <- crop(out, col, mask = TRUE)
lobstr::obj_size(rst) # 9.53MB

## free memory:
rm(out)

rst <- rst |>
    as.data.frame(xy = TRUE, na.rm = TRUE) |>
    as_tibble()

tic()
ggsave(
    filename = "Colombia_ews.png", device = "png", path = "figures/", dpi = 500,
    width = 7, height = 10, units = "cm", bg = "white", plot = rst |>
    pivot_longer(cols = starts_with("ews"), names_to = "stat", values_to = "value") |>
    rename(biome = WWF_MHTNUM) |>
    left_join(df_stats) |>
    mutate(detected = (value > p90 | value < p10)) |>
    group_by(x,y) |>
    summarize(ews = sum(detected)) |>
    mutate(ews = case_when(ews == 0 ~ NA, .default = ews)) |>
    ggplot() +
    geom_tile(aes(x,y, fill = ews)) +
    # geom_tile(aes(fill = value, alpha = detected)) +
    geom_sf(data = col, fill = NA, color = "white", width = 0.1) +
    scale_fill_viridis_c("Número de alertas tempranas", option = "A",
        guide = guide_colorbar(direction = "horizontal"), na.value = alpha("gray75", 0.5)) +
    # facet_wrap(~stat) +
    lims(x = c(-8797316,NA), y = c(NA, 1370000)) +
    labs(title = "Resiliencia en Colombia",
         subtitle = "Indicadores de resiliencia basados en NDVI a 1Km de resolución",
         caption = "Datos: MODIS, NASA, Autor: Juan C. Rocha | juan.rocha@su.se", x = NULL, y = NULL) +
    theme_void(base_size = 6) +
    theme(legend.position = c(0.35, 0.07), legend.title.position = "top",
          legend.key.height = unit(2, "mm"), legend.key.width = unit(8,'mm'))
)
toc()

df_stats

rst |>





# plot with terra
tic()
plot(out, "ews_ac1")
toc() #0.3s
# with tidyterra:

tic()
ggplot() +
    geom_spatraster(data = rst, aes(fill = ews_std)) +
    geom_sf(data = col, fill = NA, color = "white") +
    scale_fill_gradient2() +
    theme_void()
toc() # 3.1s

