library(dplyr)
library(tidymodels)

df_train <- read.csv("data/features/df_train.csv")
df_train <- df_train |> mutate(label_is_worn = as.factor(label_is_worn))

non_predictors <- c(
  "date_time",
  "id",
  "index",
  "button_press_time_sum",
  "ggir_is_worn",
  "is_validation",
  "run"
)

rec <- recipe(label_is_worn ~ ., data = df_train) |>
  update_role(any_of(non_predictors), new_role = "id") |>
  step_rm(any_of(non_predictors)) |>  
  step_filter_missing(threshold = 0.2) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_zv(all_predictors()) |>
  step_normalize(all_predictors())

# XGBoost specifications
xgb_spec <- boost_tree(
  trees = 300,
  learn_rate = 0.08,
  tree_depth = 8,
  min_n = 5,
  sample_size = 0.8,  
  mtry = 0.6,          
  loss_reduction = 1.0
) %>%
  set_engine("xgboost", counts = FALSE) |>
  set_mode("classification")

wf <- workflow() |> 
  add_model(xgb_spec) |>
  add_recipe(rec)

xgb_fit <- fit(wf, data = df_train)
saveRDS(xgb_fit, "./models/xgb_fit.RDS")