library(dplyr)
library(lubridate)
library(tsfeatures)
library(slider)
library(purrr)
source("./src/helpers.R")

df <- readRDS("./data/interim/data_merged.RDS")$df
df_list <- df |> group_split(id)
outpath <- "./data/features/"

for (df in df_list) {
  
  id <- df$id[1]
  outfile <- file.path(outpath, paste0(id, "_features.csv"))
  
  if (file.exists(outfile)) {
    print(paste0(basename(outfile), " exists, skipping..."))
    next
  } else {
    print(paste0("Processing ", id, "..."))  
  }
  
  # parameters
  epoch_min <- 0.5
  win_mins <- c(5, 10, 20, 60) # rolling windows in minutes
  win_n <- as.integer(win_mins / epoch_min)
  
  df$date_time <- as.POSIXct(df$date_time, tz = "America/Denver")
  activity <- df$vector_sum
  
  w_short <- as.integer(20 / epoch_min)
  w_long  <- as.integer(60 / epoch_min)
  
  windows_short <- create_sliding_windows(
    activity,
    window_size = w_short,
    complete = TRUE
  )
  
  windows_long <- create_sliding_windows(
    activity,
    window_size = w_long,
    complete = TRUE
  )
  
  features_short <- map_dfr(windows_short, compute_ts_features) |> rename_with(~paste0(.x, "_20min"))
  features_long <- map_dfr(windows_long, compute_ts_features) |> rename_with(~paste0(.x, "_60min"))
  
  # activity lags
  lags_min <- c(5, 10, 15, 20)
  lags_epochs <- as.integer(lags_min / epoch_min)
  for (j in seq_along(lags_min)) {
    df[[paste0("lag_", lags_min[j], "min")]] <- dplyr::lag(df$vector_sum, lags_epochs[j])
  }
  
  # Make sure temp is numeric
  temp <- as.numeric(df$mean_temp)
  
  # First derivative of temperature (per minute)
  dtemp <- c(NA_real_, diff(temp)) / epoch_min
  
  # ---- ROLLING TEMP FEATURES ----
  for (k in seq_along(win_n)) {
    w <- win_n[k]
    label <- paste0(win_mins[k], "min")
    
    df[[paste0("temp_mean_", label)]] <- slider::slide_dbl(
      temp, mean, .before = w - 1, .complete = TRUE, na.rm = TRUE
    )
    
    df[[paste0("temp_sd_", label)]] <- slider::slide_dbl(
      temp, sd, .before = w - 1, .complete = TRUE, na.rm = TRUE
    )
    
    df[[paste0("temp_slope_", label)]] <- slider::slide_dbl(
      temp, slope_per_min, epoch_min = epoch_min, .before = w - 1, .complete = TRUE
    )
    
    df[[paste0("temp_drop_", label)]] <- slider::slide_dbl(
      temp, drop_end_minus_start, .before = w - 1, .complete = TRUE
    )
    
    df[[paste0("temp_range_", label)]] <- slider::slide_dbl(
      temp, range_safe, .before = w - 1, .complete = TRUE
    )
    
    # Mean absolute derivative within window (how "jittery" temp is)
    df[[paste0("temp_deriv_meanabs_", label)]] <- slider::slide_dbl(
      dtemp, function(x) mean(abs(x), na.rm = TRUE),
      .before = w - 1, .complete = TRUE
    )
    
    # Cross-signal: correlation between temp and activity within window
    df[[paste0("corr_temp_activity_", label)]] <- slider::slide_dbl(
      seq_along(temp),
      function(idx) {
        i0 <- max(1, idx - (w - 1))
        corr_safe(temp[i0:idx], activity[i0:idx])
      },
      .complete = TRUE
    )
    
    # A very useful indicator feature: low activity + cooling trend
    df[[paste0("lowact_cooling_", label)]] <-
      as.integer(
        df[[paste0("temp_slope_", label)]] < -0.01 &
          # cooling faster than 0.01 C/min
          slider::slide_dbl(activity, function(x)
            mean(x, na.rm = TRUE), .before = w - 1, .complete = TRUE) < 5  # low activity
      )
  }
  
  burn_in <- max(w_long, win_n) - 1
  
  pad_to_n <- function(mat, n) {
    m <- nrow(mat)
    if (m == n) return(mat)
    dplyr::bind_rows(
      as.data.frame(matrix(NA_real_, n - m, ncol(mat))) |> setNames(names(mat)),
      mat
    )
  }
  
  features_short_p <- pad_to_n(features_short, nrow(df))
  features_long_p  <- pad_to_n(features_long,  nrow(df))
  
  df_features <- dplyr::bind_cols(df, features_short_p, features_long_p) |>
    dplyr::slice((burn_in + 1):dplyr::n())
  
  write.csv(df_features, outfile, row.names = FALSE)
  print(paste(id, "saved"))
}