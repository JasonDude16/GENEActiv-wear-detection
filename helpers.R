read_geneactiv_csv_raw <- function(file) {
  
  col_names <- c(
    "date_time",
    "mean_x_axis",
    "mean_y_axis",
    "mean_z_axis",
    "mean_lux",
    "button_press_time_sum",
    "mean_temp",
    "vector_sum",
    "sd_x_axis",
    "sd_y_axis",
    "sd_z_axis",
    "peak_lux"
  )
  
  df <- read.csv(
    file,
    skip = 100,
    header = FALSE,
    col.names = col_names
  )
  df$id <- stringr::str_c(strsplit(basename(file), "")[[1]][1:2], collapse = "")
  
  df$date_time <- as.POSIXct(df$date_time, tz = "America/Denver")
  
  return(df)
}

read_csv_event_markers <- function(file) {
  df <- read.csv(file, col.names = c("on_wrist_start", "on_wrist_end"))
  
  df <- df |> 
    mutate(
      on_wrist_start = format(mdy_hm(on_wrist_start), "%Y-%m-%d %H:%M:%S:000"),
      on_wrist_end = format(mdy_hm(on_wrist_end), "%Y-%m-%d %H:%M:%S:000")
    )
  
  id_tmp <- strsplit(stringr::str_remove(basename(file), ".csv"), "")[[1]]
  df$id <- stringr::str_c(id_tmp[(length(id_tmp)-1):length(id_tmp)], collapse = "")
  
  return(df)
}

merge_events <- function(df_raw, df_events) {
  
  df_raw <- df_raw |> mutate(date_time = parse_ms(date_time)) 
  
  df_events <- df_events |>
    mutate(
      start = parse_ms(on_wrist_start),
      end   = parse_ms(on_wrist_end)
    ) 
  
  df_merged <- fuzzy_left_join(
    df_raw,
    df_events,
    by = c(
      "id" = "id",
      "date_time" = "start",
      "date_time" = "end"
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) |> 
    mutate(date_time = as.POSIXct(date_time, tz = "America/Denver"))
  
  valid_indices <- which(!is.na(df_merged$start))
  first_valid <- valid_indices[1]
  last_valid <- valid_indices[length(valid_indices)]
  df_sub <- df_merged[first_valid:last_valid, ]
  
  df_sub <- df_sub |> 
    mutate(worn = !is.na(on_wrist_start)) |>
    select(-on_wrist_start, -on_wrist_end, -start, -end) |>
    distinct() |> 
    select(-id.y) |> 
    rename(id = id.x)
  
  on_off_segments <- df_sub |>
    arrange(id, date_time) |>
    group_by(id) |>
    mutate(run = cumsum(worn != dplyr::lag(worn, default = first(worn)))) |>
    group_by(id, worn, run) |>
    summarise(
      xmin = min(date_time),
      xmax = max(date_time) + seconds(5),
      .groups = "drop"
    )  
  
  return(list("df_merged" = df_sub, "on_off_segments" = on_off_segments))
}

parse_ms <- function(x, tz = "America/Denver") {
  ymd_hms(sub(":(\\d{3})$", ".\\1", x), tz = tz)
}

compute_ts_features <- function(x) {
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

collapse_by_index <- function(df, hz = 40, epoch_sec = 60) {
  n_per_epoch <- hz * epoch_sec
  
  # Convert time to POSIXct if needed
  df$time <- as.POSIXct(df$time, tz = "America/Denver")
  
  # Epoch ID based on row index
  df$epoch <- floor((seq_len(nrow(df)) - 1) / n_per_epoch)
  
  # Get epoch timestamps: first timestamp in each chunk
  epoch_times <- aggregate(time ~ epoch, data = df, FUN = function(x) x[1])
  
  # Compute means
  agg <- aggregate(
    cbind(x, y, z, nonwear_ggir) ~ epoch,
    data = df,
    FUN = mean
  )
  
  # Merge in epoch times
  res <- merge(epoch_times, agg, by = "epoch")
  names(res) <- c("epoch_id", "date_time", "mean_x_axis", "mean_y_axis", "mean_z_axis", "nonwear_ggir")
  
  return(res[order(res$epoch_id), ][-1])
}

expand_NW_to_samples <- function(data_nrows, NWav, sf, windowsizes) {
  
  MediumEpochSize <- windowsizes[2] * sf
  NMediumEpochs <- length(NWav)  
  nw_sample <- rep(NA_integer_, data_nrows)
  
  for (h in seq_len(NMediumEpochs)) {
    from_idx <- (h - 1) * MediumEpochSize + 1
    to_idx   <- h * MediumEpochSize
    to_idx <- min(to_idx, data_nrows)
    nw_sample[from_idx:to_idx] <- NWav[h]
  }
  
  # any trailing samples remain NA
  return(nw_sample)
}