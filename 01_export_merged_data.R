library(tidyverse)
library(lubridate)
source("helpers.R")

data_ggir_nonwear <- read.csv("./data/validation/interim/data_ggir_nonwear.csv")
df_labels <- data_ggir_nonwear |> select(-anglez, -ENMO) |> mutate(date_time = parse_ms(date_time))
df_labels <- split(df_labels, f = df_labels$id)

files <- list.files("./data/validation/raw/", full.names = T, pattern = ".csv")
df_raw <- map(files, read_geneactiv_csv_raw)
ids <- reduce(map(files, ~stringr::str_c(strsplit(basename(.x), "")[[1]][1:2], collapse = "")), c)
names(df_raw) <- ids

files <- list.files("./data/validation/event_markers/", full.names = T, pattern = ".csv")
df_events <- map(files, read_csv_event_markers)

ids <- reduce(map(files, function(file) {
  id_tmp <- strsplit(stringr::str_remove(basename(file), ".csv"), "")[[1]]
  id <- stringr::str_c(id_tmp[(length(id_tmp)-1):length(id_tmp)], collapse = "")
  return(id)
}), c)
names(df_events) <- ids

ids <- Reduce(intersect, lapply(list(df_raw, df_events, df_labels), names))
df_merged_lists <- setNames(
  lapply(ids, \(id) list("df_raw" = df_raw[[id]], "df_events" = df_events[[id]], "df_labels" = df_labels[[id]])),
  ids
)

df_merged <- map(df_merged_lists, function(x) {
  merge_events(x$df_raw, x$df_events, x$df_labels)
})

df_list <- list(
  "df" = map_dfr(df_merged, ~.x$df_merged),
  "on_off_segments" = map_dfr(df_merged, ~.x$df_events)
)

saveRDS(df_list, file = "./data/validation/interim/data_merged.RDS")