library(dplyr)
library(tidymodels)

df_modeling <- readRDS("./data/modeling/df_modeling.RDS")
df_train <- df_modeling |> filter(train_test == "train")

rec <- recipe(
  label_is_worn ~ 
    temp_mean_5min +
    temp_mean_10min +
    temp_mean_20min + 
    temp_mean_60min +
    ggir_is_worn,
  data = df_train
) |>
  step_unknown(ggir_is_worn) |> 
  step_dummy(ggir_is_worn) |> 
  step_normalize(all_predictors())

# XGBoost specifications
logreg_spec <- logistic_reg(
  mode = "classification", 
  engine = "glm"
)

wf <- workflow() |> 
  add_model(logreg_spec) |>
  add_recipe(rec)

logreg_fit <- fit(wf, data = df_train)
saveRDS(logreg_fit, "./models/logreg_fit.RDS")
