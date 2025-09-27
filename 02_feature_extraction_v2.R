library(dplyr)
library(lubridate)
library(tsfeatures)
library(slider)
library(xgboost)
library(caret)
library(purrr)
source("./helpers.R")

df <- read_geneactiv_csv_raw("./data/validation/raw/AS-off-wrist-testing__100334_2025-01-14 15-23-08_60s.csv")

df$id <- 1:nrow(df)
df$date_time <- as.POSIXct(df$date_time, tz = "UTC")
activity <- df$vector_sum

win_short <- 20
win_long <- 60

compute_features <- function(x) {
  # Check for insufficient data or constant values
  if (length(x) < 3 || var(x, na.rm = TRUE) == 0) {
    # Return a data frame with NA values for all features
    return(data.frame(
      entropy = NA_real_,
      max_level_shift = NA_real_,
      max_var_shift = NA_real_,
      max_kl_shift = NA_real_,
      hurst = NA_real_,
      std1st_der = NA_real_,
      heterogeneity = NA_real_
    ))
  }
  
  tryCatch({
    tsfeatures(
      x,
      features = c(
        "entropy",
        "max_level_shift",
        "max_var_shift",
        "max_kl_shift",
        "hurst",
        "std1st_der",
        "heterogeneity"
      )
    )
  }, error = function(e) {
    # Return NA values if tsfeatures fails
    data.frame(
      entropy = NA_real_,
      max_level_shift = NA_real_,
      max_var_shift = NA_real_,
      max_kl_shift = NA_real_,
      hurst = NA_real_,
      std1st_der = NA_real_,
      heterogeneity = NA_real_
    )
  })
}

# Create sliding windows manually
create_sliding_windows <- function(x, dates, window_size, complete = FALSE) {
  n <- length(x)
  windows <- list()
  
  for (i in seq_len(n)) {
    start_idx <- max(1, i - window_size + 1)
    
    if (complete && (i - window_size + 1) < 1) {
      next  # Skip incomplete windows
    }
    
    windows[[i]] <- x[start_idx:i]
  }
  
  # Remove NULL elements for complete = TRUE case
  windows[!sapply(windows, is.null)]
}

windows_short <- create_sliding_windows(
  activity, 
  df$date_time, 
  win_short, 
  complete = TRUE
)

windows_long <- create_sliding_windows(
  activity, 
  df$date_time, 
  win_long, 
  complete = TRUE
)

features_short <- map_dfr(windows_short, compute_features) |> rename_with(~paste0(.x, "_20min"))
features_long <- map_dfr(windows_long, compute_features) |> rename_with(~paste0(.x, "_60min"))

# activity lags
lags <- c(5, 10, 15, 20) 
for (lag_val in lags) {
  df[[paste0("lag_", lag_val, "min")]] <- dplyr::lag(df$vector_sum, lag_val)
}

# Temporal features
df <- df |> 
  mutate(
    minute = minute(date_time),
    hour = hour(date_time),
    wday = wday(date_time, week_start = 1),
    sin_hour = sin(2 * pi * hour / 24),
    cos_hour = cos(2 * pi * hour / 24),
    sin_wday = sin(2 * pi * wday / 7),
    cos_wday = cos(2 * pi * wday / 7)
  )

df_features_all <- bind_cols(df[60:nrow(df), ], features_short[41:nrow(features_short), ], features_long)

