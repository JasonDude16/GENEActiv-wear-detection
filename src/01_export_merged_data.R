library(tidyverse)
library(lubridate)
source("src/helpers.R")

data_ggir_nonwear <- read.csv("./data/interim/data_ggir_nonwear.csv")
data_ggir_nonwear <- data_ggir_nonwear |> select(-anglez, -ENMO) |> mutate(date_time = parse_ms(date_time))
data_ggir_nonwear <- split(data_ggir_nonwear, f = data_ggir_nonwear$id)

# get validation csvs 
files <- list.files("./data/validation2/", recursive = TRUE, full.names = T, pattern = ".csv")
df_raw <- map(files, read_geneactiv_csv_raw)
ids <- reduce(map(files, ~get_id_from_root(.x)), c)
names(df_raw) <- ids

files <- list.files("./data/validation2/", recursive = TRUE, full.names = TRUE, pattern = "On-Offwrist")
df_events <- map(files, read_event_markers)
names(df_events) <- stringr::str_extract(basename(files), "WATCH[0-9]{2}")

# TODO: dont drop missing values; figure out what happened
df_events <- map(df_events, ~.x |> drop_na())

ids <- unique(Reduce(c, lapply(list(df_raw, df_events, data_ggir_nonwear), names)))
df_merged_lists <- setNames(
  lapply(
    ids,
    \(id) list(
      "df_raw" = df_raw[[id]],
      "df_ggir" = data_ggir_nonwear[[id]],
      "df_events" = df_events[[id]]
    )
  ), ids
)

df_merged <- map(df_merged_lists, function(x) {
  merge_events(x$df_raw, x$df_ggir, x$df_events)
})

df_list <- list(
  "df" = map_dfr(df_merged, ~.x$df_merged),
  "on_off_segments" = map_dfr(df_merged, ~.x$df_events)
)

saveRDS(df_list, file = "./data/interim/data_merged.RDS")