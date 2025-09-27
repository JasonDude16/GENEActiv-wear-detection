library(tsfeatures)
source("./helpers.R")

df_raw <- read_geneactiv_csv_raw("./data/validation/raw/AS-off-wrist-testing__100334_2025-01-14 15-23-08_60s.csv")

# input a list of all the series you want to look at [meaning the columns of the data]
ts_list <- list(
  df_raw$"mean_x_axis",
  df_raw$"mean_y_axis",
  df_raw$"mean_z_axis",
  df_raw$"mean_lux",
  df_raw$"mean_temp"
)

# choose was dynamical features you want to extract for each time series data
features <- tsfeatures(ts_list, features = c("entropy", "acf_features", "lumpiness"))
print(features)


  
