library(tidyverse)
library(fuzzyjoin)
library(lubridate)
source("helpers.R")

files <- list.files("./data/validation/raw/", full.names = T, pattern = ".csv")
df_raw <- map(files, read_geneactiv_csv_raw)

files <- list.files("./data/validation/event_markers/", full.names = T, pattern = ".csv")
df_on_off <- map(files, read_csv_event_markers)

df_list <- map2(df_raw, df_on_off, merge_events)
df_list <- list(
  "df_merged" = bind_rows(map(df_list, ~.x$df_merged)),
  "on_off_segments" = bind_rows(map(df_list, ~.x$on_off_segments))
)

saveRDS(df_list, file = "./data/validation/interim/data_merged.RDS")
