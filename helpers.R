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