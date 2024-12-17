# globals: calendar,window,

## functions for ews
## STL decomposition:
decompose <- carrier::crate(function(x){
    x <- x |>
        # this recover the missing values, complete the time series
        dplyr::right_join(!!calendar |> dplyr::select(y_j, tid), by = dplyr::join_by("y_j")) |>
        dplyr::mutate(nans = is.na(ndvi)) |>
        # interpolate missing values
        dplyr::mutate(ndvi = imputeTS::na_interpolation(ndvi, "spline"),
               id = imputeTS::na_interpolation(id, "linear")) |>
        dplyr::mutate(tid = as.numeric(tid)) |>
        # calculate kernel ndvi
        dplyr::mutate(kndvi = tanh(ndvi^2)) |>
        dplyr::select(-ndvi, -y_j) # can be recovered later
    res <- x |>
        tsibble::as_tsibble(index = "tid") |>
        fabletools::model(feasts::STL(kndvi ~ season(23) + season(45) )) |>
        fabletools::components()

    x$remainder <- res$remainder
    return(x)
})

#safe_decomp <- safely(decompose)


## test later if you need uniroot test and first-diff
early_warnings <- carrier::crate(function(x){ # x is the dataset, y is the ts variable to calculate ews
    x <- x |>
        dplyr::mutate(
        #fractal dim cannot have NAs, hence computed first
        ews_fd = slider::slide_dbl(
            .x = remainder,
            .f = function(x) {
                z <- fractaldim::fd.estimate(x, method = "madogram")
                return(as.vector(z$fd))} ,
            .before = !!window, .after = 0, .complete = TRUE)) |>
        # recover missing values
        dplyr::mutate(remainder = dplyr::case_when(nans == TRUE ~ NA, .default = remainder)) |>
        dplyr::mutate(
            ews_std = slider::slide_dbl(
                .x = remainder, .f = stats::sd, na.rm = TRUE,
                .before = !!window, .after = 0, .complete = TRUE),
            ews_ac1 = slider::slide_dbl(
                .x = remainder, # turned back to pearson, kendall does not work better
                .f = function(x) stats::cor(x, dplyr::lag(x,1), use = "pairwise.complete.obs", "pearson"),
                .before = !!window, .after = 0, .complete = TRUE),
            ews_kur = slider::slide_dbl(
                .x = remainder, .f = moments::kurtosis, na.rm = TRUE,
                .before = !!window, .after = 0, .complete = TRUE),
            ews_skw = slider::slide_dbl(
                .x = remainder, .f = function(x) abs(moments::skewness(x, na.rm = TRUE)),
                .before = !!window, .after = 0, .complete = TRUE)
        )
    return(x)
})

#safe_ews <- safely(early_warnings)

## deltas
extract_delta <- carrier::crate(function(x) {
    d <- dplyr::bind_rows (
        # maximum
        x |>
            dplyr::select(id, tid, tidyselect::starts_with("ews")) |>
            tidyr::pivot_longer(tidyselect::starts_with("ews"), names_to = "ews", values_to = "value") |>
            tibble::as_tibble() |>
            dplyr::group_by(ews) |>
            dplyr::mutate(is_max = value == max(value, na.rm = TRUE)) |>
            dplyr::filter(is_max == TRUE) |> dplyr::select(-is_max) |>
            dplyr::mutate(type = "max") |>
            #add_count() |> ## detecting cases where max returns more than one hit
            ## when that happens, filter selects the first instance in time. It's mainly FD
            dplyr::filter(tid == min(tid)) ,
        # minimum
        x |>
            dplyr::select(id, tid, tidyselect::starts_with("ews")) |>
            tidyr::pivot_longer(tidyselect::starts_with("ews"), names_to = "ews", values_to = "value") |>
            tibble::as_tibble() |>
            dplyr::group_by(ews) |>
            dplyr::mutate(is_min = value == min(value, na.rm = TRUE)) |>
            dplyr::filter(is_min == TRUE) |> dplyr::select(-is_min) |>
            dplyr::mutate(type = "min") |>
            # selects first instance when diff == 0 many consecutive time steps
            dplyr::filter(tid == min(tid))
    ) |> dplyr::arrange(ews, tid) |>
        dplyr::group_by(ews, id) |>
        dplyr::summarize(delta = diff(value), .groups = "drop") |>
        tidyr::pivot_wider(names_from = ews, values_from = delta)
    return(d)
})

## combine them
fast_ews <- function(x){
    x |>
        decompose() |> #0.5s
        early_warnings() |> #0.75s
        extract_delta()
}

safe_delta <- purrr::safely(fast_ews)
