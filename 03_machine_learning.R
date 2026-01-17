library(dplyr)
library(tidymodels)

df_features <- read.csv("data/features/actigraphy_features.csv")
df_features <- df_features |> mutate(worn = as.factor(case_when(worn == TRUE ~ 1, worn == FALSE ~ 0)))
split_obj <- group_initial_split(
  df_features,
  group = id,
  prop = 0.7
)
train_df  <- training(split_obj)
test_df   <- testing(split_obj)

train_df |> 
  group_by(worn) |> 
  summarise(count = n()) |> 
  ungroup() |> 
  mutate(prop = count / sum(count))

test_df |> 
  group_by(worn) |> 
  summarise(count = n()) |> 
  ungroup() |> 
  mutate(prop = count / sum(count))

non_predictors <- c("date_time", "id", "index", "button_press_time_sum")

rec <- recipe(worn ~ ., data = train_df) |>
  update_role(any_of(non_predictors), new_role = "id") |>
  step_rm(any_of(non_predictors)) |>                    
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

xgb_fit <- fit(wf, data = train_df)

# Paper used threshold 0.55 and 3-epoch majority smoothing 
test_probs <- predict(xgb_fit, new_data = test_df, type = "prob")$.pred_1

threshold <- 0.55
test_pred <- ifelse(test_probs >= threshold, 1L, 0L)

smooth_labels <- function(labels, window_size = 3) {
  n <- length(labels); out <- labels; half <- floor(window_size/2)
  for (i in seq_len(n)) {
    s <- max(1, i - half); e <- min(n, i + half)
    w <- labels[s:e]
    if (sum(w == 1L) > sum(w == 0L)) out[i] <- 1L
    else if (sum(w == 1L) < sum(w == 0L)) out[i] <- 0L
  }
  out
}

test_out <- test_df |>
  mutate(.prob = test_probs) |>
  arrange(id, date_time) |>
  group_by(id) |>
  mutate(
    .pred_raw = as.integer(.prob >= threshold),
    .pred_smooth = smooth_labels(.pred_raw, window_size = 3)
  ) |>
  ungroup() |>
  mutate(
    .pred_raw = factor(.pred_raw, levels = c(0,1)),
    .pred_smooth = factor(.pred_smooth, levels = c(0,1))
  )