library(dplyr)
library(purrr)
library(tidymodels)

# helper for fitting and evaluating model once 
fit_eval_once_oob <- function(df, boot_ids, threshold = 0.55) {
  
  w_tbl <- tibble(id = boot_ids) |> count(id, name = "w")
  inbag_ids <- w_tbl$id
  oob_ids   <- setdiff(unique(df$id), inbag_ids)
  if (length(oob_ids) < 1) return(NULL)
  
  train_df <- df |>
    filter(id %in% inbag_ids) |>
    left_join(w_tbl, by = "id") |>
    mutate(w = hardhat::frequency_weights(w))
  
  test_df <- df |> filter(id %in% oob_ids)
  
  non_predictors <- c(
    "date_time",
    "button_press_time_sum",
    "id",
    "ggir_is_worn",
    "is_validation",
    "run",
    "train_test"
  )
  
  rec <- recipe(label_is_worn ~ ., data = train_df) |>
    update_role(any_of(non_predictors), new_role = "id") |>
    step_rm(any_of(non_predictors)) |>
    step_filter_missing(threshold = 0.2) |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
    step_zv(all_predictors()) |>
    step_normalize(all_predictors())
  
  xgb_spec <- boost_tree(
    trees = 800,
    learn_rate = 0.03,
    tree_depth = 5,
    min_n = 30,
    sample_size = 0.7,
    mtry = 0.5,
    loss_reduction = 5
  ) |>
    set_engine("xgboost", counts = FALSE) |>
    set_mode("classification")
  
  wf <- workflow() |>
    add_model(xgb_spec) |>
    add_recipe(rec) |>
    add_case_weights(w)
  
  xgb_fit <- fit(wf, data = train_df)
  
  # grab the event prob (yardstick event_level = "second")
  p <- predict(xgb_fit, test_df, type = "prob")
  event_class <- levels(test_df$label_is_worn)[2]
  probs <- p[[paste0(".pred_", event_class)]]
  
  lvl <- levels(test_df$label_is_worn)
  test_out <- test_df |>
    mutate(
      .prob = probs,
      .pred = factor(ifelse(.prob >= threshold, lvl[2], lvl[1]), levels = lvl)
    )
  
  # metrics/summary
  tibble(
    bal_acc = bal_accuracy(test_out, truth = label_is_worn, estimate = .pred, event_level = "second")$.estimate,
    auc = roc_auc(test_out, truth = label_is_worn, .prob, event_level = "second")$.estimate,
    n_inbag_ids = length(inbag_ids),
    n_oob_ids = length(oob_ids)
  )
}

# -------------------------------------------------------------------------------------------------------------------------------------

# get list of ids for sampling
ids <- unique(df_modeling$id)

# model N times with randomly sampled ids and save results
set.seed(1)
boot_results <- map_dfr(seq_len(50), function(i) {
  tictoc::tic()
  print(i)
  boot_ids <- sample(ids, size = length(ids), replace = TRUE) 
  res <- fit_eval_once_oob(df_modeling, boot_ids, threshold = 0.55)
  if (is.null(res)) return(NULL)
  tictoc::toc()
  return(mutate(res, iter = i))
})

boot_summary <- boot_results |>
  summarise(
    bal_acc_mean = mean(bal_acc),
    bal_acc_sd   = sd(bal_acc),
    bal_acc_lo   = quantile(bal_acc, 0.025),
    bal_acc_hi   = quantile(bal_acc, 0.975),
    auc_mean     = mean(auc),
    auc_sd       = sd(auc)
  ) 
print(boot_summary)

# balanced accuracy distribution
hist(
  boot_results$bal_acc,
  breaks = 15,
  main = "Boostrapped results",
  xlab = "Balanced accuracy"
)
abline(v = median(boot_results$bal_acc), lty = 2, lwd = 2)
abline(v = boot_summary$bal_acc_mean + 2*(boot_summary$bal_acc_sd), lty = 2, lwd = 1, col = "red")
abline(v = boot_summary$bal_acc_mean - 2*(boot_summary$bal_acc_sd), lty = 2, lwd = 1, col = "red")

# AUC distribution
hist(
  boot_results$auc,
  breaks = 15,
  main = "Boostrapped results",
  xlab = "AUC"
)
abline(v = boot_summary$auc_mean, lty = 2, lwd = 2)
abline(v = min(boot_summary$auc_mean + 2*(boot_summary$auc_sd), 1), lty = 2, lwd = 1, col = "red")
abline(v = boot_summary$auc_mean - 2*(boot_summary$auc_sd), lty = 2, lwd = 1, col = "red")