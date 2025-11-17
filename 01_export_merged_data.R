library(tidyverse)
library(fuzzyjoin)
library(lubridate)
source("helpers.R")

data_ggir_nonwear <- readRDS("./data/validation/interim/data_ggir_nonwear.RDS")
data_ggir_nonwear <- data_ggir_nonwear |> select(-mean_x_axis, -mean_y_axis, -mean_z_axis)

files <- list.files("./data/validation/raw/", full.names = T, pattern = ".csv")
df_raw <- map(files, read_geneactiv_csv_raw)

files <- list.files("./data/validation/event_markers/", full.names = T, pattern = ".csv")
df_on_off <- map(files, read_csv_event_markers)

df_list <- map2(df_raw, df_on_off, merge_events)
df_list <- list(
  "df_merged" = bind_rows(map(df_list, ~.x$df_merged)),
  "on_off_segments" = bind_rows(map(df_list, ~.x$on_off_segments))
)

df_all <- inner_join(df_list$df_merged, data_ggir_nonwear, by = c("date_time", "id"))
saveRDS(df_all, file = "./data/validation/interim/data_merged.RDS")