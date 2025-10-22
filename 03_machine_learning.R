library(tidymodels)

df_features <- read.csv("data/features/actigraphy_features.csv")
df_features <- df_features |> mutate(worn = as.factor(case_when(worn == TRUE ~ 1, worn == FALSE ~ 0)))
split_obj <- initial_split(df_features, prop = 0.7, strata = worn)
train_df  <- training(split_obj)
test_df   <- testing(split_obj)

non_predictors <- c("date_time")

rec <- recipe(worn ~ ., data = train_df) %>%
  update_role(any_of(non_predictors), new_role = "id") %>%  # keep but not as predictors
  step_rm(any_of(non_predictors)) %>%                       # remove from modeling matrix
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
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
  set_engine("xgboost", counts = FALSE) %>%
  set_mode("classification")

wf <- workflow() %>%
  add_model(xgb_spec) %>%
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
test_pred_smooth <- smooth_labels(test_pred, window_size = 3)

# Evaluate (yardstick)
test_out <- test_df %>%
  mutate(
    .prob = test_probs,
    .pred_raw = factor(test_pred, levels = c(0,1)),
    .pred_smooth = factor(test_pred_smooth, levels = c(0,1))
  )

metrics_raw   <- metric_set(accuracy, sensitivity, specificity, bal_accuracy, f_meas, precision, recall)
raw_stats     <- metrics_raw(test_out, truth = worn, estimate = .pred_raw)
smooth_stats  <- metrics_raw(test_out, truth = worn, estimate = .pred_smooth)
auc_val       <- roc_auc(test_out, truth = worn, .prob, event_level = "second")

print(raw_stats)
print(smooth_stats)
print(auc_val)

# Save
final_results <- test_df %>%
  dplyr::select(any_of("date_time"), worn) %>%
  mutate(
    pred_prob = test_probs,
    pred_label = as.integer(.pred_raw) - 1L,            
    pred_label_smoothed = as.integer(.pred_smooth) - 1L
  )
write.csv(final_results, "actigraphy_predictions_tidymodels.csv", row.names = FALSE)
