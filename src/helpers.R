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
create_sliding_windows <- function(x, window_size, complete = FALSE) {
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
slope_per_min <- function(x, epoch_min) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || var(x) == 0) return(NA_real_)
  t <- seq_along(x) * epoch_min
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

plot_labels_over_time <- function(
    df,
    vars,              
    source_cols,
    levels,
    date_time_col,
    var_labels = NULL,      
    gap_padding = 0.2,
    var_height = 1.2
) {
  
  stopifnot(length(levels) == 2)
  stopifnot(is.character(vars), length(vars) >= 1)

  df <- df |>
    dplyr::mutate(
      month = as.character(lubridate::month(as.POSIXct(.data[[date_time_col]]))),
      month = ifelse(nchar(month) == 1, paste0("0", month), month),
      day = as.character(lubridate::day(as.POSIXct(.data[[date_time_col]]))),
      day = ifelse(nchar(day) == 1, paste0("0", day), day),
      month_day = paste0(month, "-", day),
      time = hms::as_hms(as.POSIXct(.data[[date_time_col]]))
    ) |> 
    dplyr::select(-month, -day)
  
  # Long for wear tiles
  df_long <- df |>
    dplyr::select(dplyr::all_of(c(source_cols, date_time_col)), month_day, time) |>
    tidyr::pivot_longer(
      cols = -c(date_time_col, month_day, time),
      names_to = "source",
      values_to = "wear"
    ) |>
    dplyr::mutate(
      source = factor(source, levels = source_cols),
      lane   = as.integer(source)
    )
  
  K <- length(source_cols)
  M <- length(vars)
  
  # Build long df for the variable lines (each var scaled to 0..1 within itself)
  df_vars_long <- df |>
    dplyr::select(dplyr::all_of(c(date_time_col, "month_day", "time", vars))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to = "var",
      values_to = "value"
    ) |>
    dplyr::group_by(var) |>
    dplyr::mutate(
      rng_min = min(value, na.rm = TRUE),
      rng_max = max(value, na.rm = TRUE),
      denom   = (rng_max - rng_min),
      scaled  = dplyr::if_else(is.finite(denom) & denom > 0, (value - rng_min) / denom, 0.5)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      var = factor(var, levels = vars),
      var_i = as.integer(var),
      # base for each var band:
      # var 1 base = -gap_padding
      # var 2 base = -gap_padding - (var_height + gap_padding)
      # ...
      base = -gap_padding - (var_i - 1) * (var_height + gap_padding),
      y_line = base - scaled * var_height
    )
  
  # y-axis breaks/labels:
  # Put each var label centered in its band, then wear lanes 1..K
  y_breaks_vars  <- (-gap_padding - (seq_len(M) - 1) * (var_height + gap_padding)) - var_height / 2
  y_breaks_wear  <- seq_len(K)
  y_breaks <- c(y_breaks_vars, y_breaks_wear)
  
  if (is.null(var_labels)) {
    y_labels <- c(vars, source_cols)  
  } else {
    y_labels <- c(var_labels, source_cols) 
  }
  
  # plot limits: bottom should include last var band plus a little padding
  y_min <- (-gap_padding - (M - 1) * (var_height + gap_padding)) - var_height - 0.1
  y_max <- K + 0.5
  
  scale_clrs <- c("black", "darkgrey")
  names(scale_clrs) <- levels
  
  ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = df_long,
      ggplot2::aes(x = time, y = lane, fill = wear),
      height = 0.9,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      data = df_vars_long,
      ggplot2::aes(x = time, y = y_line, group = interaction(month_day, var)),
      linewidth = 0.5
    ) +
    ggplot2::scale_fill_manual(values = scale_clrs, name = "State") +
    ggplot2::scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = y_breaks,
      labels = y_labels
    ) +
    ggplot2::labs(x = "Time", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 11),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::facet_wrap(
      ~month_day,
      ncol = 1
    )
}

plot_confusion_matrix <- function(
    df,
    ref_col,                 
    class_cols,              
    title = NULL,
    x_lab = "Prediction",
    y_lab = "Reference",
    positive_label = "Wear",
    negative_label = "Non-wear",
    pos_value = "wear",
    neg_value = "non-wear",
    drop_na = TRUE
) {
  
  # --- helper: map to 0/1 if needed ---
  to01 <- function(x) {
    if (is.numeric(x) || is.integer(x) || is.logical(x)) {
      as.integer(x)
    } else {
      dplyr::case_when(
        tolower(as.character(x)) == tolower(pos_value) ~ 1L,
        tolower(as.character(x)) == tolower(neg_value) ~ 0L,
        TRUE ~ NA_integer_
      )
    }
  }
  
  df2 <- df |>
    dplyr::mutate(
      ref01 = to01(.data[[ref_col]])
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(class_cols),
      names_to = "classifier",
      values_to = "pred_raw"
    ) |>
    dplyr::mutate(
      pred01 = to01(pred_raw)
    )
  
  if (drop_na) {
    df2 <- df2 |>
      dplyr::filter(!is.na(ref01), !is.na(pred01))
  }
  
  # ---- 1) Confusion matrix counts ----
  cm <- df2 |>
    dplyr::group_by(classifier) |>
    dplyr::summarise(
      TP = sum(ref01 == 1 & pred01 == 1, na.rm = TRUE),
      TN = sum(ref01 == 0 & pred01 == 0, na.rm = TRUE),
      FP = sum(ref01 == 0 & pred01 == 1, na.rm = TRUE),
      FN = sum(ref01 == 1 & pred01 == 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(N = TP + TN + FP + FN)
  
  # ---- 2) Metrics ----
  metrics <- cm |>
    dplyr::mutate(
      Accuracy = dplyr::if_else(N > 0, (TP + TN) / N, NA_real_),
      Sensitivity = dplyr::if_else(TP + FN > 0, TP / (TP + FN), NA_real_),
      Specificity = dplyr::if_else(TN + FP > 0, TN / (TN + FP), NA_real_)
    ) |>
    dplyr::select(classifier, Accuracy, Sensitivity, Specificity, N)
  
  # ---- 3) Long format for plotting ----
  cm_long <- cm |>
    tidyr::pivot_longer(TP:FN, names_to = "cell", values_to = "count") |>
    dplyr::mutate(
      pct = dplyr::if_else(N > 0, count / N, NA_real_),
      Reference  = dplyr::if_else(cell %in% c("TP", "FN"), positive_label, negative_label),
      Prediction = dplyr::if_else(cell %in% c("TP", "FP"), positive_label, negative_label),
      cell_type = dplyr::case_when(
        cell %in% c("TP", "TN") ~ "Correct",
        cell == "FP"            ~ "False Positive",
        cell == "FN"            ~ "False Negative",
        TRUE ~ NA_character_
      ),
      label = sprintf("%s\n(n=%d)", scales::percent(pct, accuracy = 0.1), count)
    )
  
  # ---- 4) Plot ----
  p <- ggplot2::ggplot(cm_long, ggplot2::aes(x = Prediction, y = Reference)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = cell_type),
      color = "white",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      size = 4.2,
      lineheight = 0.95
    ) +
    ggplot2::facet_wrap(~ classifier) +
    ggplot2::scale_fill_manual(
      values = c(
        "Correct"        = "lightgreen",
        "False Positive" = "salmon",
        "False Negative" = "salmon"
      ),
      name = NULL
    ) +
    ggplot2::labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  list(
    plot = p,
    cm = cm,
    metrics = metrics,
    cm_long = cm_long
  )
}

plot_metrics_bars <- function(
    metrics_df,
    facet = TRUE,
    percent = TRUE,
    title = NULL,
    base_size = 14,
    palette = "Dark2",
    label_size = 4
) {
  stopifnot(is.data.frame(metrics_df))
  stopifnot("N" %in% names(metrics_df))
  stopifnot("classifier" %in% names(metrics_df))
  
  df_long <- metrics_df |>
    tidyr::pivot_longer(
      cols = -c(classifier, N),
      names_to = "Metric",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      Value_plot = if (percent) Value * 100 else Value,
      label = if (percent) {
        sprintf("%.1f%%", Value_plot)
      } else {
        sprintf("%.2f", Value_plot)
      },
      Metric = factor(Metric, levels = unique(Metric))
    )
  
  p <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(
      x = Metric,
      y = Value_plot,
      fill = if (!facet) classifier else NULL
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = if (facet) "stack" else ggplot2::position_dodge(width = 0.8),
      width = 0.7,
      show.legend = !facet
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = label,
        group = if (!facet) classifier else NULL
      ),
      position = if (facet) {
        ggplot2::position_stack(vjust = 1.05)
      } else {
        ggplot2::position_dodge(width = 0.8)
      },
      vjust = -0.2,
      size = label_size,
      show.legend = FALSE
    ) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      legend.position = "bottom"
    )
  
  # ---- Facet or combined ----
  if (facet) {
    p <- p + ggplot2::facet_wrap(~ classifier)
  } else {
    p <- p +
      ggplot2::scale_fill_brewer(palette = "Dark2", name = "Classifier")
  }
  
  # ---- Labels ----
  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title)
  }
  
  if (percent) {
    p <- p + ggplot2::labs(y = "Value (%)")
  }
  
  p
}