library(dplyr)
library(lubridate)

srate <- 40
window_sizes <- c(5, 600, 1800)

raw <- GGIRread::readGENEActiv("./data/raw/JD-ow-test__100336_2025-11-10 14-59-44.bin")
n_epochs <- nrow(raw$data.out) / (60*40)
remainder <- n_epochs - round(n_epochs)
raw$data.out <- raw$data.out[1:(nrow(raw$data.out) - ((remainder * 60 * 40))-1), ]

res <- GGIR::detect_nonwear_clipping(
  raw$data.out[c("x", "y", "z")],
  sf = srate,
  windowsizes = window_sizes,
  nonwear_approach = "2023"
)

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

nw_upsampled <- expand_NW_to_samples(
  data_nrows = nrow(raw$data.out),
  NWav = res$NWav ,
  sf = srate,
  windowsizes = window_sizes
)

raw$data.out$nonwear_ggir <- as.integer(nw_upsampled >= 2)

down_1min <- raw$data.out |>
  mutate(time = as.POSIXct(time, origin = "1970-01-01", tz = "UTC")) |>
  mutate(minute = floor_date(time, "1 minute")) |>
  group_by(minute) |>
  summarise(
    x_mean = mean(x, na.rm = TRUE),
    y_mean = mean(y, na.rm = TRUE),
    z_mean = mean(z, na.rm = TRUE),
    light_mean = mean(light, na.rm = TRUE),
    temp_mean = mean(temperature, na.rm = TRUE),
    nonwear_frac = mean(nonwear_ggir, na.rm = TRUE),
    .groups = "drop"
  )

down_1min <- down_1min |> mutate(nonwear_flag = as.integer(nonwear_frac > 0.5))
