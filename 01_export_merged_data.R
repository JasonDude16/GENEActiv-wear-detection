library(tidyverse)
library(fuzzyjoin)
library(lubridate)
source("helpers.R")

files <- list.files("./data/validation/raw/", full.names = T, pattern = ".csv")
df_raw <- map_dfr(files, read_geneactiv_csv_raw)

files <- list.files("./data/validation/event_markers/", full.names = T, pattern = ".csv")
df_on_off <- map_dfr(files, read_csv_event_markers)

df_raw <- df_raw %>% mutate(date_time = parse_ms(date_time)) 

df_on_off <- df_on_off %>%
  mutate(
    start = parse_ms(on_wrist_start),
    end   = parse_ms(on_wrist_end)
  ) 

df_merged <- fuzzy_left_join(
  df_raw,
  df_on_off,
  by = c(
    "id" = "id",
    "date_time" = "start",
    "date_time" = "end"
  ),
  match_fun = list(`==`, `>=`, `<=`)
) %>%
  mutate(worn = !is.na(on_wrist_start)) %>%
  select(-on_wrist_start, -on_wrist_end, -start, -end) %>%
  distinct() |> 
  select(-id.y) |> 
  rename(id = id.x)

on_off_segments <- df_merged %>%
  arrange(id, date_time) %>%
  group_by(id) %>%
  mutate(run = cumsum(worn != dplyr::lag(worn, default = first(worn)))) %>%
  group_by(id, worn, run) %>%
  summarise(
    xmin = min(date_time),
    xmax = max(date_time) + seconds(5),
    .groups = "drop"
  )

df_list <- list("df_merged" = df_merged, "on_off_segments" = on_off_segments)
saveRDS(df_list, file = "./data/validation/interim/data_merged.RDS")