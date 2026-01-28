library(tidymodels)
library(dplyr)
library(purrr)

fit_eval_once <- function(df, train_ids, test_ids, threshold = 0.55) {
  
  train_df <- df |> filter(id %in% train_ids)
  test_df  <- df |> filter(id %in% test_ids)
  
  rec <- recipe(worn ~ ., data = train_df) |>
    update_role(c(date_time, id, index), new_role = "id") |>
    step_rm(c(date_time, id, index)) |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
    step_zv(all_predictors()) |>
    step_normalize(all_predictors())
  
  xgb_spec <- boost_tree(
    trees = 300,
    learn_rate = 0.08,
    tree_depth = 8,
    min_n = 5,
    sample_size = 0.8,
    mtry = 0.6,
    loss_reduction = 1
  ) |>
    set_engine("xgboost", counts = FALSE) |>
    set_mode("classification")
  
  wf <- workflow() |> add_recipe(rec) |> add_model(xgb_spec)
  fit <- fit(wf, data = train_df)
  
  probs <- predict(fit, test_df, type = "prob")$.pred_1
  
  test_out <- test_df |>
    mutate(
      .prob = probs,
      .pred = factor(as.integer(probs >= threshold), levels = c(0,1))
    )
  
  tibble(
    bal_acc = bal_accuracy(
      test_out,
      truth = worn,
      estimate = .pred,
      event_level = "second"
    )$.estimate,
    auc = roc_auc(test_out, truth = worn, .prob, event_level = "second")$.estimate
  )
}

ids <- unique(df_features$id)
boot_results <- map_dfr(seq_len(50), function(x) {
  
  sampled_ids <- sample(ids, size = length(ids), replace = TRUE)
  train_ids <- unique(sampled_ids)[1:floor(0.7 * length(ids))]
  test_ids  <- setdiff(ids, train_ids)
  
  if (length(test_ids) < 1) return(NULL)
  
  fit_eval_once(df_features, unique(train_df$id), unique(test_df$id)) |>
    mutate(iter = x)
})

boot_results |>
  summarise(
    bal_acc_mean = mean(bal_acc),
    bal_acc_sd   = sd(bal_acc),
    bal_acc_lo   = quantile(bal_acc, 0.025),
    bal_acc_hi   = quantile(bal_acc, 0.975),
    auc_mean     = mean(auc),
    auc_sd       = sd(auc)
  )