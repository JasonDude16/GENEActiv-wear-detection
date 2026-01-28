library(tidyverse)
library(scales)
library(yardstick)
source("helpers.R")

df_pred <- readRDS("./data/modeling/df_predictions.RDS")

subj_id <- "GZ"
df_id <- df_pred |> filter(id == subj_id, !is.na(ggir_is_worn))

p_labs_time <- df_id |> 
  filter(lubridate::day(date_time) < 12) |> 
  mutate(
    XGBoost = as.factor(ML_Pred),
    GGIR = as.factor(ggir_is_worn),
    Truth = as.factor(label_is_worn)
  ) |> 
  plot_labels_over_time(
    vars = c("mean_x_axis", "mean_temp"), 
    var_label = c("Activity", "Temperature"), 
    source_cols = c("Truth", "GGIR", "XGBoost"),
    levels = c(0, 1),
    date_time_col =  "date_time",
    var_height = 2
  ) + 
  ggtitle(paste0("Participant:", subj_id))

res <- df_id |> 
  mutate(
    GGIR = ggir_is_worn,
    XGBoost = ML_Pred
  ) |> 
  plot_confusion_matrix(
    ref_col = "label_is_worn",
    class_cols = c("GGIR", "XGBoost"),
    x_lab = "Predicted",
    y_lab = "Truth",
    pos_value = 1,
    neg_value = 0
  )

p_metrics <- plot_metrics_bars(res$metrics, facet = FALSE)

layout <- matrix(c(1, 1, 2, 1, 1, 3, 1, 1, 3), nrow = 3, byrow = TRUE)
combined_plot <- gridExtra::grid.arrange(p_labs_time, res$plot, p_metrics, layout_matrix = layout)
ggsave(paste0("./plots/", subj_id, "_plot.png"), plot = combined_plot, width = 18, height = 11, units = "in")

# -------------------------------------------------------------------------------------------------------------------------------------

df_pred |> 
  filter(id == "24SEC05", !is.na(ggir_is_worn)) |> 
  mutate(
    XGBoost = as.factor(ML_Pred),
    GGIR = as.factor(ggir_is_worn),
  ) |> 
  plot_labels_over_time(
    vars = c("mean_x_axis", "mean_temp"), 
    var_label = c("Activity", "Temperature"), 
    source_cols = c("GGIR", "XGBoost"),
    levels = c(0, 1),
    date_time_col =  "date_time",
    var_height = 2
  ) + 
  ggtitle(paste0("Participant: 24SEC05"))
