library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(caret)
library(xgboost)
library(tibble)

wvs_safe <- readRDS("data/processed/wvs_safe.rds")

# ------------------------------------------------------------
# A. Exploratory approach:
# Try predicting a person's literal country from their answers
#
# This is not the final approach I chose, but I kept it because
# it shows the earlier modeling path and helps tell the project story.
# ------------------------------------------------------------

evaluate_top_n <- function(n, importance_tbl, train_df, test_df, country_levels) {

  # Grab the top N most important questions from the XGBoost model
  top_vars <- importance_tbl %>%
    slice(1:n) %>%
    pull(Feature)

  # Build train/test matrices using only those top questions
  x_train_n <- train_df %>%
    select(all_of(top_vars)) %>%
    as.matrix()

  x_test_n <- test_df %>%
    select(all_of(top_vars)) %>%
    as.matrix()

  # XGBoost expects class labels as integers starting at 0
  y_train <- as.integer(train_df$b_country) - 1
  y_test  <- as.integer(test_df$b_country) - 1

  dtrain_n <- xgb.DMatrix(data = x_train_n, label = y_train)
  dtest_n  <- xgb.DMatrix(data = x_test_n, label = y_test)

  # Fairly standard multiclass XGBoost settings
  params <- list(
    objective = "multi:softprob",
    num_class = length(country_levels),
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    eval_metric = "mlogloss"
  )

  # Train a model using only the top N questions
  model_n <- xgb.train(
    params = params,
    data = dtrain_n,
    nrounds = 150,
    verbose = 0
  )

  # Predict class probabilities for each country
  pred <- predict(model_n, dtest_n)

  pred_mat <- matrix(
    pred,
    nrow = nrow(test_df),
    ncol = length(country_levels),
    byrow = TRUE
  )

  # Top-1 accuracy = was the most likely country the correct one?
  pred_top1 <- max.col(pred_mat) - 1
  top1_acc <- mean(pred_top1 == y_test)

  # Top-3 accuracy = was the true country anywhere in the top 3 guesses?
  top3_acc <- mean(sapply(seq_len(nrow(pred_mat)), function(i) {
    y_test[i] %in% (order(pred_mat[i, ], decreasing = TRUE)[1:3] - 1)
  }))

  tibble(
    n_questions = n,
    top1_acc = top1_acc,
    top3_acc = top3_acc
  )
}

# Remove survey weights for this modeling exercise
# and make country a factor for multiclass classification
wvs_xgb <- wvs_safe %>%
  select(-w_weight) %>%
  mutate(b_country = as.factor(b_country))

country_levels <- levels(wvs_xgb$b_country)

# Create a train/test split while preserving country balance
set.seed(123)
train_idx <- createDataPartition(wvs_xgb$b_country, p = 0.8, list = FALSE)
train_df <- wvs_xgb[train_idx, ]
test_df  <- wvs_xgb[-train_idx, ]

# Full feature matrices for the initial model
x_train <- train_df %>% select(-b_country) %>% as.matrix()
x_test  <- test_df %>% select(-b_country) %>% as.matrix()
y_train <- as.integer(train_df$b_country) - 1
y_test  <- as.integer(test_df$b_country) - 1

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest  <- xgb.DMatrix(data = x_test, label = y_test)

params <- list(
  objective = "multi:softprob",
  num_class = length(country_levels),
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "mlogloss"
)

# Train a full model on all available question features
xgb_full <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 150,
  verbose = 0
)

# Use the trained model to estimate which questions carry the most predictive signal
importance <- xgb.importance(
  feature_names = colnames(x_train),
  model = xgb_full
)

# See how performance changes when we keep only the top 10, 15, or 20 questions
exploratory_xgb_results <- bind_rows(
  evaluate_top_n(10, importance, train_df, test_df, country_levels),
  evaluate_top_n(15, importance, train_df, test_df, country_levels),
  evaluate_top_n(20, importance, train_df, test_df, country_levels)
)

write_csv(importance, "data/processed/exploratory_xgb_importance.csv")
write_csv(exploratory_xgb_results, "data/processed/exploratory_xgb_results.csv")

# ------------------------------------------------------------
# B. Final approach:
# Build country-level profiles and compare users to those profiles
#
# Instead of predicting a person's literal country directly,
# this approach asks:
# "Which countries have average answer patterns most similar
# to this user?"
#
# That ended up being more aligned with the product idea.
# ------------------------------------------------------------

# Start with all question variables, excluding country and weight columns
question_vars <- names(wvs_safe) %>%
  setdiff(c("b_country", "w_weight"))

# For each question, measure how useful it is for separating countries.
# The basic idea:
# - between_var = how much country averages differ from each other
# - within_var  = how much variation there is inside countries
# - signal_ratio = bigger means the question does more work in
#   distinguishing countries
question_signal <- map_dfr(question_vars, function(v) {
  overall_mean <- mean(wvs_safe[[v]], na.rm = TRUE)

  by_country <- wvs_safe %>%
    group_by(b_country) %>%
    summarise(
      country_mean = mean(.data[[v]], na.rm = TRUE),
      country_n = n(),
      .groups = "drop"
    )

  between_var <- weighted.mean(
    (by_country$country_mean - overall_mean)^2,
    w = by_country$country_n,
    na.rm = TRUE
  )

  within_var <- wvs_safe %>%
    group_by(b_country) %>%
    summarise(v = var(.data[[v]], na.rm = TRUE), n = n(), .groups = "drop") %>%
    summarise(weighted = weighted.mean(v, w = n, na.rm = TRUE)) %>%
    pull(weighted)

  tibble(
    question = v,
    between_var = between_var,
    within_var = within_var,
    signal_ratio = between_var / within_var
  )
}) %>%
  arrange(desc(signal_ratio))

# Build one average response profile per country using the top N questions
build_country_profiles <- function(n, question_signal, wvs_safe) {
  top_q <- question_signal %>%
    slice(1:n) %>%
    pull(question)

  wvs_safe %>%
    group_by(b_country) %>%
    summarise(
      across(all_of(top_q), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

# Rank countries by how close their average profile is to a user's answers
# Smaller distance means the country profile is more similar to the user
rank_countries <- function(user_vec, profiles) {
  q_names <- names(user_vec)

  profiles %>%
    rowwise() %>%
    mutate(
      dist = sqrt(sum((c_across(all_of(q_names)) - user_vec)^2))
    ) %>%
    ungroup() %>%
    arrange(dist)
}

# Evaluate how well a given set of top questions works.
# For a random sample of respondents, we check where their actual country
# lands in the ranking generated from profile similarity.
evaluate_questions <- function(n, question_signal, wvs_safe, sample_n = 5000) {
  top_q <- question_signal %>%
    slice(1:n) %>%
    pull(question)

  profiles <- build_country_profiles(n, question_signal, wvs_safe)

  set.seed(123)
  test_sample <- wvs_safe %>%
    select(b_country, all_of(top_q)) %>%
    slice_sample(n = sample_n)

  ranks <- map_dbl(seq_len(nrow(test_sample)), function(i) {
    user <- test_sample[i, ]

    # Convert this person's answers into a named numeric vector
    user_vec <- as.numeric(user %>% select(all_of(top_q)))
    names(user_vec) <- top_q

    # Rank all countries from most similar to least similar
    scores <- rank_countries(user_vec, profiles)

    # Find where the person's true country appears in that ranking
    match(user$b_country, scores$b_country)
  })

  tibble(
    questions = n,
    mean_rank = mean(ranks),
    top3 = mean(ranks <= 3),
    top5 = mean(ranks <= 5)
  )
}

# Compare a few candidate feature set sizes
profile_eval_results <- bind_rows(
  evaluate_questions(10, question_signal, wvs_safe),
  evaluate_questions(15, question_signal, wvs_safe),
  evaluate_questions(20, question_signal, wvs_safe)
)

# Final choice: keep the top 15 questions
final_questions <- question_signal %>%
  slice(1:15)

# Save outputs for later use in the app / downstream scripts
write_csv(question_signal, "data/processed/question_signal.csv")
write_csv(profile_eval_results, "data/processed/profile_eval_results.csv")
write_csv(final_questions, "data/processed/final_questions.csv")

message("Saved question selection outputs to data/processed/")
