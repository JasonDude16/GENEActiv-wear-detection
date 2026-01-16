library(dplyr)
library(lubridate)
library(tsfeatures)
library(slider)
library(purrr)
source("./helpers.R")

df <- readRDS("./data/validation/interim/data_merged.RDS")

# parameters
epoch_min <- 1
win_mins <- c(5, 10, 20, 60) # rolling windows in minutes
win_n <- as.integer(win_mins / epoch_min)

df_features_all <- df |> 
  group_split(id) |> 
  map_dfr(function(df) {
    
    df$index <- 1:nrow(df)
    df$date_time <- as.POSIXct(df$date_time, tz = "America/Denver")
    activity <- df$vector_sum
    
    windows_short <- create_sliding_windows(
      activity, 
      window_size = 20, 
      complete = TRUE
    )
    
    windows_long <- create_sliding_windows(
      activity, 
      window_size = 60, 
      complete = TRUE
    )
    
    features_short <- map_dfr(windows_short, compute_ts_features) |> rename_with(~paste0(.x, "_20min"))
    features_long <- map_dfr(windows_long, compute_ts_features) |> rename_with(~paste0(.x, "_60min"))
    
    # activity lags
    lags <- c(5, 10, 15, 20) 
    for (lag_val in lags) {
      df[[paste0("lag_", lag_val, "min")]] <- dplyr::lag(df$vector_sum, lag_val)
    }
    
    # Make sure temp is numeric
    temp <- as.numeric(df$mean_temp)
    
    # First derivative of temperature (per minute)
    dtemp <- c(NA_real_, diff(temp))
    
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
        temp, slope_per_min, .before = w - 1, .complete = TRUE
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
      # You can tune these thresholds later; this is a good starting point.
      df[[paste0("lowact_cooling_", label)]] <-
        as.integer(
          df[[paste0("temp_slope_", label)]] < -0.01 &  # cooling faster than 0.01 C/min
            slider::slide_dbl(activity, function(x) mean(x, na.rm = TRUE),
                              .before = w - 1, .complete = TRUE) < 5  # low activity
        )
    }
    
    df_features_all <- bind_cols(df[60:nrow(df), ], features_short[41:nrow(features_short), ], features_long)
    
    return(df_features_all)
  })

write.csv(df_features_all, "data/features/actigraphy_features.csv", row.names = FALSE)
