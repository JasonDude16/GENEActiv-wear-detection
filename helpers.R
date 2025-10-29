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

parse_ms <- function(x, tz = "UTC") {
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