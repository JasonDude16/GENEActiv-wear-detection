library(dplyr)
library(tidymodels)

df_features <- map_dfr(list.files("./data/features/", full.names = TRUE), read.csv)

df_features <- df_features |> 
  mutate(
    label_is_worn = as.factor(case_when(
      label_is_worn == "wear" ~ 1, label_is_worn == "non-wear" ~ 0
    )),
    ggir_is_worn = as.factor(case_when(
      ggir_is_worn == "wear" ~ 1, 
      ggir_is_worn == "non-wear" ~ 0
    ))
  )

# subset to validation and non-validation
df_features_validation <- df_features |> filter(is_validation)
df_features_non_validation <- df_features |> filter(!is_validation)
df_features_non_validation$train_test <- NA

set.seed(1)
split_obj <- group_initial_split(
  df_features_validation,
  group = id,
  prop = 0.7
)

df_train <- training(split_obj)
df_train$train_test <- "train"

df_test <- testing(split_obj)
df_test$train_test <- "test"

# train and test class proportions
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

df_all <- rbind(df_train, df_test, df_features_non_validation)

# re-order for clarity (all non-features listed first in dataset)
df_all <- df_all |> 
  select(
    date_time,
    id,
    label_is_worn,
    ggir_is_worn,
    train_test,
    is_validation,
    run,
    button_press_time_sum,
    everything()
)

saveRDS(df_all, "./data/modeling/df_modeling.RDS")