set.seed(42)
train_idx <- createDataPartition(features_all$label, p = 0.7, list = FALSE)
train_data <- features_all[train_idx, ]
test_data  <- features_all[-train_idx, ]

# Prepare matrices
X_train <- as.matrix(train_data %>% select(-date_time, -label))
y_train <- train_data$label
X_test  <- as.matrix(test_data %>% select(-date_time, -label))
y_test  <- test_data$label

# ---------------------------
# 7. Train XGBoost
# ---------------------------
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test, label = y_test)

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, eval = dtest),
  verbose = 1
)

# ---------------------------
# 8. Predictions + threshold
# ---------------------------
y_pred_probs <- predict(model, X_test)
threshold <- 0.55
pred_labels <- ifelse(y_pred_probs >= threshold, 1, 0)

# ---------------------------
# 9. Temporal smoothing
# ---------------------------
smooth_labels <- function(labels, window_size = 3) {
  n <- length(labels)
  smoothed <- labels
  half_win <- floor(window_size / 2)
  
  for (i in seq_len(n)) {
    start <- max(1, i - half_win)
    end <- min(n, i + half_win)
    window <- labels[start:end]
    if (sum(window == 1) > sum(window == 0)) {
      smoothed[i] <- 1
    } else if (sum(window == 1) < sum(window == 0)) {
      smoothed[i] <- 0
    } # ties = keep original
  }
  return(smoothed)
}

pred_labels_smoothed <- smooth_labels(pred_labels, window_size = 3)

# ---------------------------
# 10. Save results
# ---------------------------
results <- test_data %>%
  mutate(pred_prob = y_pred_probs,
         pred_label = pred_labels,
         pred_label_smoothed = pred_labels_smoothed)

write.csv(results, "actigraphy_predictions_pipeline.csv", row.names = FALSE)

cat("Pipeline complete. Features + predictions saved to actigraphy_predictions_pipeline.csv\n")