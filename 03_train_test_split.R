library(dplyr)
library(tidymodels)

df_features <- read.csv("data/features/actigraphy_features.csv")

# subset to validation data
df_features_valid <- df_features |> filter(is_validation)

df_features_sub <- df_features_sub |> 
  mutate(
    label_is_worn = case_when(
      label_is_worn == "wear" ~ 1, label_is_worn == "non-wear" ~ 0),
    ggir_is_worn = case_when(
      ggir_is_worn == "wear" ~ 1, ggir_is_worn == "non-wear" ~ 0)
  )

split_obj <- group_initial_split(
  df_features_sub,
  group = id,
  prop = 0.7
)

df_train <- training(split_obj)
df_test <- testing(split_obj)

df_train |> 
  group_by(label_is_worn) |> 
  summarise(count = n()) |> 
  ungroup() |> 
  mutate(prop = count / sum(count, na.rm = T))

df_test |> 
  group_by(label_is_worn) |> 
  summarise(count = n()) |> 
  ungroup() |> 
  mutate(prop = count / sum(count))

write.csv(df_train, "./data/features/df_train.csv", row.names = FALSE)
write.csv(df_test, "./data/features/df_test.csv", row.names = FALSE)