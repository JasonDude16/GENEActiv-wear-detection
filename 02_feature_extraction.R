library(dplyr)
library(lubridate)
library(tsfeatures)
library(slider)
library(xgboost)
library(caret)
library(purrr)
source("./helpers.R")

df <- readRDS("./data/validation/interim/data_merged.RDS")
df$merged <- df$df_merged |> filter(id %in% c("AS", "CD"))

df_features_all <- df$df_merged |> 
  group_split(id) |> 
  map(function(df) {
    
    df$index <- 1:nrow(df)
    df$date_time <- as.POSIXct(df$date_time, tz = "UTC")
    activity <- df$vector_sum
    
    windows_short <- create_sliding_windows(
      activity, 
      df$date_time, 
      20, 
      complete = TRUE
    )
    
    windows_long <- create_sliding_windows(
      activity, 
      df$date_time, 
      60, 
      complete = TRUE
    )
    
    features_short <- map_dfr(windows_short, compute_ts_features) |> rename_with(~paste0(.x, "_20min"))
    features_long <- map_dfr(windows_long, compute_ts_features) |> rename_with(~paste0(.x, "_60min"))
    
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
    
    return(df_features_all)
  })

write.csv(df_features_all, "data/features/actigraphy_features.csv", row.names = FALSE)
