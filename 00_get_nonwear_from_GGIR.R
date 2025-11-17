library(dplyr)
library(lubridate)
source("helpers.R")

bin_files <- list.files("./data/raw/", full.names = TRUE, pattern = ".bin")

df_ggir_nonwear <- purrr::map_dfr(bin_files, function(f) {
  raw <- GGIRread::readGENEActiv(f)  
  
  res <- GGIR::detect_nonwear_clipping(
    raw$data.out[c("x", "y", "z")],
    sf = 40,
    windowsizes = c(5, 900, 3600),
    nonwear_approach = "2023"
  )
  
  nw_upsampled <- expand_NW_to_samples(
    data_nrows = nrow(raw$data.out),
    NWav = res$NWav,
    sf = 40,
    windowsizes = c(5, 900, 3600)
  )
  
  raw$data.out$nonwear_ggir <- as.integer(nw_upsampled >= 2)
  df_60s <- collapse_by_index(raw$data.out, hz = 40, epoch_sec = 60)
  df_60s$id <- stringr::str_c(strsplit(basename(f), "")[[1]][1:2], collapse = "")
  
  return(df_60s)
})

saveRDS(df_ggir_nonwear, file = "./data/validation/interim/data_ggir_nonwear.RDS")