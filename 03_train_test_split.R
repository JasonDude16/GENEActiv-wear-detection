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