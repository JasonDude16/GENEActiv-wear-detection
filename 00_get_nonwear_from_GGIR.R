library(dplyr)
library(lubridate)
source("helpers.R")

bin_files <- list.files("./data/raw/", full.names = TRUE, pattern = ".bin")

df_ggir_nonwear <- purrr::map_dfr(bin_files[1], function(f) {
  print(paste("Reading", basename(f)))
  raw <- GGIRread::readGENEActiv(f)  
  df <- raw$data.out
  
  browser()
  # default args 
  for (version in c("2013", "2023")) {
    res <- GGIR::detect_nonwear_clipping(
      df[c("x", "y", "z")],
      windowsizes = c(5, 900, 3600),
      sf = 40,
      nonwear_approach = version
    )
    
    nw_upsampled <- expand_NW_to_samples(
      data_nrows = nrow(df),
      windowsizes = c(5, 900, 3600),
      NWav = res$NWav,
      sf = 40
    )
    
    df[[paste0("nonwear_ggir_", version)]] <- as.integer(nw_upsampled >= 2)
  }

  # Convert time to POSIXct 
  df$time <- as.POSIXct(df$time, tz = "America/Denver")
  
  # Epoch ID based on row index
  df$epoch <- floor((seq_len(nrow(df)) - 1) / (40 * 60)) # hz * epoch seconds
  
  # Get epoch timestamps: first timestamp in each chunk
  epoch_times <- aggregate(time ~ epoch, data = df, FUN = function(x) x[1])
  
  # Compute means
  agg <- aggregate(
    cbind(x, y, z, nonwear_ggir_2013, nonwear_ggir_2023) ~ epoch,
    data = df,
    FUN = mean
  )
  
  # Merge in epoch times
  res <- merge(epoch_times, agg, by = "epoch")
  names(res) <- c(
    "epoch_id",
    "date_time",
    "mean_x_axis",
    "mean_y_axis",
    "mean_z_axis",
    "nonwear_ggir_2013",
    "nonwear_ggir_2023"
  )
  
  df_60s <- res[order(res$epoch_id), ][-1]
  df_60s$id <- stringr::str_c(strsplit(basename(f), "")[[1]][1:2], collapse = "")
  
  print(paste(basename(f), "done"))
  return(df_60s)
})

saveRDS(df_ggir_nonwear, file = "./data/validation/interim/data_ggir_nonwear.RDS")