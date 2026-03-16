# Install the packages used in this project.
# This only needs to be run once on a new machine.
install.packages(c(
  "plumber",
  "jsonlite",
  "dplyr",
  "readr",
  "tibble",
  "tidyr",
  "httr2"
))

library(dplyr)
library(tidyr)
library(readr)
library(janitor)

# Point R to the project root so the relative file paths below work.
setwd("C:/data exercises/world values")


# ------------------------------------------------------------------------------
# clean_wvs()
#
# Reads the raw WVS CSV export and prepares a cleaner modeling dataset.
#
# The WVS data uses a bunch of negative codes (-1, -2, etc.) to represent
# missing or inapplicable answers. Those need to be converted to NA so they
# don't get treated as real values later.
#
# I also keep only the country code, survey weights, and the question columns.
# ------------------------------------------------------------------------------

clean_wvs <- function(path) {

  # These are the special WVS codes that represent missing data.
  missing_codes <- c(-1, -2, -3, -4, -5, -9, -99, -999, -9999)

  # Read the raw CSV and normalize the column names.
  # clean_names() just makes everything lowercase snake_case.
  raw <- read_csv(path, show_col_types = FALSE) %>%
    clean_names()

  # For modeling I only need:
  #   - the country identifier
  #   - the survey weight
  #   - all question variables (q1, q2, q3, ...)
  model_df <- raw %>%
    select(
      b_country,
      w_weight,
      matches("^q")
    )

  # Occasionally duplicated columns show up in WVS exports.
  # This just drops duplicates defensively.
  model_df <- model_df[, !duplicated(names(model_df))]

  model_df %>%
    mutate(
      # Replace WVS missing codes with NA across all numeric columns
      across(
        where(is.numeric),
        ~ replace(., . %in% missing_codes, NA_real_)
      ),

      # Treat country as a categorical variable
      b_country = as.factor(b_country)
    )
}


# ------------------------------------------------------------------------------
# build_safe_dataset()
#
# This creates the "safe" modeling dataset used for question selection
# and later country profile comparisons.
#
# Main steps:
#   1. Keep only simple survey variables (q1, q2, q3...)
#   2. Drop questions with too much missing data
#   3. Fill remaining missing values with the median
#
# The idea is to end up with a dataset that is reasonably clean and usable
# without overengineering the preprocessing.
# ------------------------------------------------------------------------------

build_safe_dataset <- function(df, missing_thresh = 0.35) {

  # Identify columns that look like plain survey questions (q1, q2, ...)
  q_vars <- names(df)[grepl("^q[0-9]+$", names(df))]

  # Extract the numeric question IDs so we can filter by range
  q_nums <- as.numeric(sub("q", "", q_vars))

  # Only keep earlier questions.
  # Later survey questions are often conditional or country-specific.
  q_vars <- q_vars[q_nums <= 222]

  df2 <- df %>%
    select(b_country, w_weight, all_of(q_vars))

  # Calculate the fraction of missing values for each column
  missing_tbl <- df2 %>%
    summarise(across(everything(), ~ mean(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "var", values_to = "pct_missing")

  # Keep variables that aren't too sparse
  keep_vars <- missing_tbl %>%
    filter(var %in% c("b_country", "w_weight") | pct_missing <= missing_thresh) %>%
    pull(var)

  df3 <- df2 %>%
    select(all_of(keep_vars))

  # Simple median imputation for remaining missing values.
  # It's quick, stable, and good enough for this application.
  safe_median <- function(x) {
    med <- median(x, na.rm = TRUE)

    if (is.na(med)) return(x)

    ifelse(is.na(x), med, x)
  }

  df3 %>%
    mutate(
      across(
        -any_of(c("b_country", "w_weight")),
        safe_median
      )
    )
}


# Build the cleaned modeling datasets
wvs_model <- clean_wvs("data/raw/WVS_csv.csv")
wvs_safe  <- build_safe_dataset(wvs_model)

# Save them so later scripts don't need to repeat the cleaning work
saveRDS(wvs_model, "data/processed/wvs_model.rds")
saveRDS(wvs_safe,  "data/processed/wvs_safe.rds")

message("Saved: data/processed/wvs_model.rds")
message("Saved: data/processed/wvs_safe.rds")
