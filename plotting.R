library(tidyverse)
library(terra)
library(fs)
library(tictoc)


fls <- dir_info("results/")

fls |> arrange(desc(modification_time))

load(fls$path[34])

tic()
out <- map_df(
    fls$path,
    function(x){
        load(x)
        deltas <- transpose(deltas)
        is_ok <-  map_lgl(deltas$error, is.null)
        res <- tibble(
            tile = x,
            ok = sum(is_ok),
            errors = sum(!is_ok)
        )
        return(res)
    }, .progress = TRUE
)
toc() #611s


deltas <- transpose(deltas)
is_ok <-  map_lgl(deltas$error, is.null)

all(is_ok) # all good no errors!

which(!is_ok) |> length() # 408 errors


deltas <- deltas$result |>
    bind_rows()

tic()
deltas |>
    left_join(keys |> rownames_to_column("id") |> mutate(id = as.numeric(id))) |>
    ggplot(aes(x,y)) +
    geom_tile(aes(fill = ews_fd)) +
    scale_fill_viridis_c()
toc()

