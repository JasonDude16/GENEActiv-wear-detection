library(tidyverse)
library(scales)

data_merged <- readRDS("./data/interim/data_merged.RDS")

df_sub <- data_merged$df |> 
  filter(id == "SL", !is.na(ggir_is_worn))

p_labs_time <- df_sub |> 
  mutate(GGIR = ggir_is_worn, Truth = label_is_worn) |> 
  plot_labels_over_time(
    var = "mean_x_axis", 
    var_label = "Activity", 
    source_cols = c("Truth", "GGIR"),
    levels = c("non-wear", "wear"),
    date_time_col =  "date_time",
    activity_height = 2
  ) + 
  ggtitle("Participant: JD")

ref_col <- "label_is_worn"                
class_cols <- "ggir_is_worn"

cm <- df_sub %>%
  mutate(
    label_is_worn = ifelse(label_is_worn == "wear", 1, 0),
    ggir_is_worn = ifelse(ggir_is_worn == "wear", 1, 0)
  ) |>
  pivot_longer(all_of(class_cols),
               names_to = "classifier",
               values_to = "pred") %>%
  filter(!is.na(.data[[ref_col]]), !is.na(pred)) %>%
  group_by(classifier) %>%
  summarise(
    TP = sum(.data[[ref_col]] == 1 & pred == 1),
    TN = sum(.data[[ref_col]] == 0 & pred == 0),
    FP = sum(.data[[ref_col]] == 0 & pred == 1),
    FN = sum(.data[[ref_col]] == 1 & pred == 0),
    .groups = "drop"
  ) |> 
  mutate(N = TP + TN + FP + FN)

# ---- 2) Metrics ----
metrics <- cm %>%
  mutate(
    Accuracy    = (TP + TN) / N,
    Sensitivity = ifelse(TP + FN > 0, TP / (TP + FN), NA_real_),
    Specificity = ifelse(TN + FP > 0, TN / (TN + FP), NA_real_)
  ) %>%
  select(classifier, Accuracy, Sensitivity, Specificity, N)

# ---- 3) Long format with percentages + cell types ----
cm_long <- cm %>%
  pivot_longer(TP:FN, names_to = "cell", values_to = "count") %>%
  mutate(
    pct = count / N,
    Reference  = ifelse(cell %in% c("TP", "FN"), "Wear", "Non-wear"),
    Prediction = ifelse(cell %in% c("TP", "FP"), "Wear", "Non-wear"),
    cell_type = case_when(
      cell %in% c("TP", "TN") ~ "Correct",
      cell == "FP"            ~ "False Positive",
      cell == "FN"            ~ "False Negative",
      TRUE ~ NA_character_
    ),
    # label shows percent + count
    label = sprintf("%s\n(n=%d)", percent(pct, accuracy = 0.1), count)
  )

# ---- 4) Plot: % heatmap, FP/FN colored differently, metrics annotated ----
p_cm <- ggplot(cm_long, aes(x = Prediction, y = Reference)) +
  geom_tile(aes(fill = cell_type), color = "white", linewidth = 0.7, show.legend = F) +
  geom_text(aes(label = label), size = 4.2, lineheight = 0.95) +
  facet_wrap(~ classifier) +
  scale_fill_manual(
    values = c(
      "Correct"         = "grey80",
      "False Positive"  = "orange",
      "False Negative"  = "firebrick"
    ),
    name = NULL
  ) +
  geom_label(
    data = metrics,
    aes(
      x = 1.5, y = 1.5,
      label = sprintf(
        "Acc: %s\nSens: %s\nSpec: %s\nN: %d",
        percent(Accuracy, Accuracy = 0.1),
        percent(Sensitivity, Accuracy = 0.1),
        percent(Specificity, Accuracy = 0.1),
        N
      )
    ),
    inherit.aes = FALSE,
    size = 3.6,
    label.size = 0.25,
    label.r = unit(0.15, "lines"),
    fill = "white",
    alpha = 0.95
  ) +
  labs(
    title = "Ground Truth vs GGIR",
    x = "GGIR",
    y = "Truth"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom"
  )

p_metrics <- metrics |> 
  select(-classifier) |> 
  pivot_longer(-N, names_to = "Metric", values_to = "Value") |> 
  mutate(Value = Value * 100) |> 
  ggplot(aes(Metric, Value)) +
  geom_bar(stat = "identity", show.legend = F) +
  theme_classic() +
  theme(text = element_text(size = 14), axis.title.x = element_blank())

layout <- matrix(c(1, 1, 2, 1, 1, 3), nrow = 2, byrow = TRUE)
gridExtra::grid.arrange(p_labs_time, p_cm, p_metrics, layout_matrix = layout)
