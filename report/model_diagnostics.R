library(tidyverse)

metrics_raw   <- metric_set(accuracy, sensitivity, specificity, bal_accuracy, f_meas, precision, recall)
raw_stats     <- metrics_raw(test_out, truth = worn, estimate = .pred_raw)
smooth_stats  <- metrics_raw(test_out, truth = worn, estimate = .pred_smooth)
auc_val       <- roc_auc(test_out, truth = worn, .prob, event_level = "second")

print(raw_stats)
print(smooth_stats)
print(auc_val)

# ROC
roc_df <- roc_curve(test_out, truth = worn, .prob, event_level = "second")
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(linewidth = 1) +
  geom_abline(linetype = "dashed") +
  coord_equal() +
  labs(title = "ROC curve (test set)",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()


cm <- conf_mat(test_out, truth = worn, estimate = .pred_smooth)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion matrix")


ggplot(test_out, aes(x = .prob, fill = worn)) +
  geom_density(alpha = 0.4) +
  labs(title = "Score distributions by true class",
       x = "Predicted probability for class 1 (wear)",
       y = "Density") +
  theme_minimal()


# Per-epoch correctness after smoothing
err_df <- test_out %>%
  mutate(correct = as.integer(.pred_smooth == worn),
         hour = lubridate::hour(date_time),
         wday = lubridate::wday(date_time, label = TRUE, week_start = 1))

# Accuracy by hour of day
acc_hour <- err_df %>% group_by(hour) %>% summarise(acc = mean(correct), n = n(), .groups = "drop")

ggplot(acc_hour, aes(hour, acc)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Accuracy by hour of day (test)",
       x = "Hour of day",
       y = "Accuracy") +
  theme_minimal()

# Accuracy by weekday
acc_wday <- err_df %>% group_by(wday) %>% summarise(acc = mean(correct), n = n(), .groups = "drop")

ggplot(acc_wday, aes(wday, acc, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Accuracy by weekday (test)",
       x = "Weekday",
       y = "Accuracy") +
  theme_minimal()


by_pid <- test_out %>%
  group_by(id) %>%
  summarise(
    acc = mean(.pred_smooth == worn),
    bal_acc = yardstick::bal_accuracy_vec(worn, .pred_smooth),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(acc)

ggplot(by_pid, aes(reorder(id, acc), acc)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Accuracy by participant (test)",
       x = "Participant",
       y = "Accuracy") +
  theme_minimal()


# pick a participant to inspect
pid <- by_pid$id[1]  # worst performer (or set explicitly)

viz_df <- test_out %>%
  filter(id == pid) %>%
  select(index, date_time, worn, .pred_smooth, .prob) %>%
  mutate(
    truth = if_else(worn == "1", "wear", "nonwear"),
    pred  = if_else(.pred_smooth == "1", "wear", "nonwear"),
    correct = truth == pred
  )

# Ribbon for prediction + points for mismatches
ggplot(viz_df, aes(x = index)) +
  geom_ribbon(aes(ymin = 0, ymax = 1, fill = pred), alpha = 0.25) +
  geom_point(aes(y = as.numeric(worn) - 1), size = 0.7, alpha = 0.8) +
  scale_fill_manual(values = c("wear" = "#4CAF50", "nonwear" = "#FF7043")) +
  scale_y_continuous(NULL, breaks = c(0,1), labels = c("nonwear","wear")) +
  labs(title = paste("Epoch timeline —", pid),
       x = "Time",
       y = NULL,
       fill = "Predicted") +
  theme_minimal()


epoch_len_sec <- 30
bin_size_epochs <- 20  # ~10 minutes

episode_perf <- test_out %>%
  arrange(id, date_time) %>%
  mutate(
    truth_bin = if_else(worn == "1", 1L, 0L),
    pred_bin  = if_else(.pred_smooth == "1", 1L, 0L),
    new_block = truth_bin != dplyr::lag(truth_bin, default = first(truth_bin)),
    block_id  = cumsum(new_block)
  ) %>%
  group_by(id, block_id, truth_bin) %>%
  summarise(
    len_epochs = n(),
    pred_majority = as.integer(mean(pred_bin) >= 0.5),
    correct_majority = as.integer(pred_majority == unique(truth_bin)),
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  filter(truth_bin == 0L) %>%  # nonwear episodes
  mutate(
    len_min = len_epochs * epoch_len_sec / 60,
    bin = cut(len_epochs,
              breaks = seq(0, max(len_epochs) + bin_size_epochs, by = bin_size_epochs),
              include.lowest = TRUE, right = FALSE)
  ) %>%
  group_by(bin) %>%
  summarise(
    mean_acc = mean(correct_majority),
    median_acc = median(correct_majority),
    n_episodes = n(),
    .groups = "drop"
  )

ggplot(episode_perf, aes(x = bin, y = mean_acc, group = 1)) +
  geom_line() +
  geom_point(aes(size = n_episodes)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Episode-level accuracy vs. nonwear episode length (test)",
       x = "True nonwear episode length (epochs, ~10 min bins)",
       y = "Mean majority accuracy",
       size = "Episodes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# mark misclassifications
err_points <- test_out %>%
  mutate(
    is_error = .pred_smooth != worn,
    hour = lubridate::hour(date_time)
  )

# Error rate by hour
err_by_hour <- err_points %>%
  group_by(hour) %>%
  summarise(err_rate = mean(is_error), n = n(), .groups = "drop")

ggplot(err_by_hour, aes(hour, err_rate)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Error rate by hour of day (test)",
       x = "Hour of day",
       y = "Error rate") +
  theme_minimal()

# Probability vs. truth with misclassifications highlighted
ggplot(test_out, aes(x = .prob, y = as.numeric(worn) - 1)) +
  geom_jitter(aes(color = .pred_smooth != worn), height = 0.05, alpha = 0.4) +
  geom_vline(xintercept = 0.55, linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "grey50", "TRUE" = "#E64B35"), name = "Misclassified") +
  scale_y_continuous(breaks = c(0,1), labels = c("nonwear","wear")) +
  labs(title = "Predicted probability vs. true class",
       x = "Predicted probability (wear)",
       y = "True class") +
  theme_minimal()


# extract fitted xgboost model from workflow
xgb_fit_obj <- workflows::extract_fit_parsnip(xgb_fit)$fit

vip::vip(xgb_fit_obj, num_features = 20) +
  labs(title = "Global feature importance (XGBoost)")


df_line <- test_out |>
  filter(id == unique(test_out$id)[1]) |>
  slice(1:2000) |>
  mutate(index = row_number())

# Collapse consecutive runs of the same worn state
bands <- df_line |>
  transmute(index, worn_num = as.integer(as.character(worn)), mean_temp) |>
  mutate(block = cumsum(worn_num != dplyr::lag(worn_num, default = first(worn_num)))) |>
  group_by(block) |>
  summarise(
    worn_num = first(worn_num),
    xmin = min(index) - 0.5,
    xmax = max(index) + 0.5,
    .groups = "drop"
  ) |>
  mutate(
    ymin = min(df_line$mean_temp, na.rm = TRUE),
    ymax = max(df_line$mean_temp, na.rm = TRUE)
  )

ggplot() +
  # one rectangle per contiguous worn/non-worn run
  geom_rect(
    data = bands,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = factor(worn_num)),
    alpha = 0.22
  ) +
  # the temperature trace on top
  geom_line(
    data = df_line,
    aes(index, mean_temp),
    linewidth = 0.8,
    color = "black"
  ) +
  scale_fill_manual(
    name = "Worn state",
    values = c("0" = "#E74C3C", "1" = "#27AE60"),
    labels = c("0" = "Non-worn", "1" = "Worn")
  ) +
  labs(
    title = "Mean temperature with worn / non-worn background bands",
    x = "Epoch index",
    y = "Mean temperature"
  ) +
  theme_classic() +
  theme(legend.position = "top")



df_line <- test_out |>
  filter(id == unique(test_out$id)[1]) |>
  arrange(date_time) |>
  slice(1:2000) |>
  mutate(
    index    = row_number(),
    pred_bin = as.integer(as.character(.pred_smooth)),  # 0/1
    true_bin = as.integer(as.character(worn))           # 0/1
  )

# y-range for full-height tiles
y_min <- min(df_line$mean_temp, na.rm = TRUE)
y_max <- max(df_line$mean_temp, na.rm = TRUE)
y_mid <- (y_min + y_max) / 2

# keep only epochs that are FP or FN
errors <- df_line |>
  mutate(
    err_case = case_when(
      pred_bin == 1 & true_bin == 0 ~ "FP",
      pred_bin == 0 & true_bin == 1 ~ "FN",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(err_case))

ggplot() +
  # one non-overlapping tile per error epoch, spanning full y-range
  geom_tile(
    data = errors,
    aes(x = index, y = y_mid, fill = err_case),
    width = 1, height = y_max - y_min, alpha = 0.28
  ) +
  # temperature trace on top
  geom_line(
    data = df_line,
    aes(x = index, y = mean_temp),
    linewidth = 0.9, color = "black"
  ) +
  scale_fill_manual(
    values = c(FP = "#F39C12", FN = "#E74C3C"),
    name   = "Errors",
    labels = c(FP = "False Positive", FN = "False Negative")
  ) +
  labs(
    title = "Per-epoch False Positives / False Negatives (no collapsing)",
    x = "Epoch index", y = "Mean temperature"
  ) +
  theme_classic() +
  theme(legend.position = "top")

