library(dplyr)
library(tidymodels)

df_modeling <- readRDS("./data/modeling/df_modeling.RDS")
df_train <- df_modeling |> filter(train_test == "train")

non_predictors <- c(
  "date_time",
  "button_press_time_sum",
  "id",
  "ggir_is_worn",
  "is_validation",
  "run",
  "train_test"
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
  trees = 800,
  learn_rate = 0.03,
  tree_depth = 5,
  min_n = 30,
  sample_size = 0.7,  
  mtry = 0.5,          
  loss_reduction = 5
) %>%
  set_engine("xgboost", counts = FALSE) |>
  set_mode("classification")

wf <- workflow() |> 
  add_model(xgb_spec) |>
  add_recipe(rec)

xgb_fit <- fit(wf, data = df_train)
saveRDS(xgb_fit, "./models/xgb.RDS")