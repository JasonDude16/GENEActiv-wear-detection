library(tidyverse)
library(lubridate)
source("helpers.R")

data_ggir_nonwear <- read.csv("./data/interim/data_ggir_nonwear.csv")
data_ggir_nonwear <- data_ggir_nonwear |> select(-anglez, -ENMO) |> mutate(date_time = parse_ms(date_time))
data_ggir_nonwear <- split(data_ggir_nonwear, f = data_ggir_nonwear$id)

# get validation csvs 
files <- list.files("./data/validation/raw/csv/", full.names = T, pattern = ".csv")
df_raw_validation <- map(files, read_geneactiv_csv_raw)
ids <- reduce(map(files, ~stringr::str_c(strsplit(basename(.x), "-")[[1]][1], collapse = "")), c)
names(df_raw_validation) <- ids

# get regular (non-validation) csvs 
files <- list.files("./data/raw/csv/", full.names = T, pattern = ".csv")
df_raw_non_validation <- map(files, read_geneactiv_csv_raw)
ids <- reduce(map(files, ~stringr::str_c(strsplit(basename(.x), "-")[[1]][1], collapse = "")), c)
names(df_raw_non_validation) <- ids

df_raw <- c(df_raw_validation, df_raw_non_validation)

files <- list.files("./data/validation/events/", full.names = T, pattern = ".csv")
df_events <- map(files, read_csv_event_markers)

ids <- reduce(map(files, function(file) {
  id_tmp <- strsplit(stringr::str_remove(basename(file), ".csv"), "")[[1]]
  id <- stringr::str_c(id_tmp[(length(id_tmp)-1):length(id_tmp)], collapse = "")
  return(id)
}), c)
names(df_events) <- ids

ids <- unique(Reduce(c, lapply(list(df_raw, df_events, data_ggir_nonwear), names)))
df_merged_lists <- setNames(
  lapply(
    ids,
    \(id) list(
      "df_raw" = df_raw[[id]],
      "df_ggir" = data_ggir_nonwear[[id]],
      "df_events" = df_events[[id]]
    )
  ),   ids
)

df_merged <- map(df_merged_lists, function(x) {
  merge_events(x$df_raw, x$df_ggir, x$df_events)
})

df_list <- list(
  "df" = map_dfr(df_merged, ~.x$df_merged),
  "on_off_segments" = map_dfr(df_merged, ~.x$df_events)
)

saveRDS(df_list, file = "./data/interim/data_merged.RDS")