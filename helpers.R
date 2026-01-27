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
  df$id <- stringr::str_c(strsplit(basename(file), "-")[[1]][1], collapse = "")
  df$date_time <- parse_ms(as.POSIXct(df$date_time, tz = "America/Denver"))
  
  return(df)
}

read_csv_event_markers <- function(file) {
  df <- read.csv(file, col.names = c("on_wrist_start", "on_wrist_end", "run"))
  
  df <- df |> 
    mutate(
      on_wrist_start = parse_ms(format(mdy_hm(on_wrist_start), "%Y-%m-%d %H:%M:%S:000")),
      on_wrist_end = parse_ms(format(mdy_hm(on_wrist_end), "%Y-%m-%d %H:%M:%S:000"))
    )
  
  id_tmp <- strsplit(stringr::str_remove(basename(file), ".csv"), "")[[1]]
  df$id <- stringr::str_c(id_tmp[(length(id_tmp)-1):length(id_tmp)], collapse = "")
  
  return(df)
}

adjust_log_time <- function(raw_time, log_time) {
  raw_sec <- second(raw_time)[1]
  if (raw_sec > 30) {
    time_adj <- parse_ms(log_time) - (60 - seconds(raw_sec))  
  } else {
    time_adj <- parse_ms(log_time) + seconds(raw_sec)
  }
  return(time_adj)  
}

labels_to_long_format <- function(df_events) {
  label_is_worn <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(label_is_worn) <- c("date_time", "label_is_worn")
  for (i in seq_along(1:nrow(df_events))) {
    wear <- seq(df_events$on_wrist_start[i], df_events$on_wrist_end[i], 30)
    df_wear <- data.frame("date_time" = wear, label_is_worn = "wear")
    label_is_worn <- rbind(label_is_worn, df_wear)
    if (i < nrow(df_events)) {
      non_wear <- seq(df_events$on_wrist_end[i] + 30, df_events$on_wrist_start[i+1] - 30, 30)    
      df_non_wear <- data.frame("date_time" = non_wear, label_is_worn = "non-wear")
      label_is_worn <- rbind(label_is_worn, df_non_wear)
    }
  } 
  return(label_is_worn)
}

merge_events <- function(df_raw, df_ggir, df_events = NULL) {
  
  diffs <- second(df_raw$date_time[1]) - seq(0, 59, 5)
  offset <- diffs[which.min(abs(diffs))]
  df_ggir$date_time <- df_ggir$date_time + seconds(offset)
  
  
  if (!is.null(df_events)) {
    df_events <- df_events |> 
      mutate(
        on_wrist_start = adjust_log_time(df_raw$date_time, df_events$on_wrist_start),
        on_wrist_end = adjust_log_time(df_raw$date_time, df_events$on_wrist_end)
      )

    res_labels_long <- labels_to_long_format(df_events) 
    df_merged <- reduce(list(df_raw, df_ggir, res_labels_long), left_join)
    df_merged$label_is_worn[is.na(df_merged$label_is_worn)] <- "non-wear"
    df_merged$is_validation <- TRUE
    df_merged$run <- df_events$run[1]
    
  } else {
    df_merged <- reduce(list(df_raw, df_ggir), left_join)
    df_merged$is_validation <- FALSE
    
  }
  
  return(list("df_merged" = df_merged, "df_events" = df_events))
  
}

parse_ms <- function(x, tz = "America/Denver") {
  ymd_hms(sub(":(\\d{3})$", ".\\1", x), tz = tz)
}

# -------------------------------------------------------------------------------------------------------------------------------------

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

# Helper: slope (degC per minute) from a vector
slope_per_min <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || var(x) == 0) return(NA_real_)
  t <- seq_along(x) - 1
  # slope from linear regression x ~ t
  return(coef(lm(x ~ t))[2])
}

# Helper: drop from start to end (end - start)
drop_end_minus_start <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  return(x[length(x)] - x[1])
}

# Helper: range
range_safe <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  return(max(x) - min(x))
}

# Helper: corr
corr_safe <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  if (sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  return(cor(x[ok], y[ok]))
}

# -------------------------------------------------------------------------------------------------------------------------------------

plot_labels_over_time <- function(df, var, var_label, source_cols, levels, date_time_col, gap_padding = 0.2, activity_height = 1.2) {
  
  stopifnot(length(levels) == 2)  
  
  df <- df |>  
    mutate(
      day = lubridate::day(.data[[date_time_col]]), 
      time = hms::as_hms(.data[[date_time_col]])
    )
  
  df_long <- df |> 
    select(all_of(c(source_cols, date_time_col)), day, time) |> 
    pivot_longer(
      cols = -c(date_time_col, day, time), 
      names_to = "source", 
      values_to = "wear"
    )
  
  df_long <- df_long |>
    mutate(
      source = factor(source, levels = source_cols),
      lane = as.integer(source)
    )
  
  K <- length(source_cols)
  rng <- range(df[[var]], na.rm = TRUE)
  
  df <- df |>
    mutate(
      mean_scaled = (.data[[var]] - rng[1]) / diff(rng),  
      y_line = -gap_padding - mean_scaled * activity_height
    )
  
  y_breaks <- c(-gap_padding - activity_height / 2, seq_len(K))
  y_labels <- c(var_label, source_cols)
  
  
  scale_clrs <- c("firebrick", "steelblue")
  names(scale_clrs) <- levels
  
  ggplot() +
    geom_tile(
      data = df_long,
      aes(x = time, y = lane, fill = wear),
      height = 0.9,
      show.legend = F
    ) +
    geom_line(
      data = df,
      aes(x = time, y = y_line),
      linewidth = 0.5
    ) +
    scale_fill_manual(
      values = scale_clrs,
      name = "State"
    ) +
    scale_y_continuous(
      limits = c(-gap_padding - activity_height - 0.1, K + 0.5),
      breaks = y_breaks,
      labels = y_labels
    ) +
    labs(x = "Time", y = NULL) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 11),
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    ) +
    facet_wrap(~day, ncol = 1, labeller = labeller(day = function(x) paste("Day", as.integer(x))))
}