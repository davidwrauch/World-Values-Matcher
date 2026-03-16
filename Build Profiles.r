library(dplyr)
library(readr)
library(tibble)

# Load the cleaned World Values Survey dataset
# This contains individual responses and a column identifying each person's country
wvs_safe <- readRDS("data/processed/wvs_safe.rds")

# Load the list of questions we decided to keep for the model
# These are the variables that will be used to compare people across countries
final_questions <- read_csv("data/processed/final_questions.csv", show_col_types = FALSE)

# Extract just the question names into a simple vector
# This makes it easier to select these columns later
question_list <- final_questions$question


# ------------------------------------------------------------
# Build country "profiles"
# ------------------------------------------------------------
# For each country, we calculate the average response to each
# question in the survey. This produces a typical value profile
# for that country across all selected questions.
#
# The result is one row per country and one column per question.
# ------------------------------------------------------------

country_profiles <- wvs_safe %>%
  group_by(b_country) %>%
  summarise(
    across(all_of(question_list), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )


# ------------------------------------------------------------
# Function: score_user
# ------------------------------------------------------------
# Given a set of answers from a user, this function finds the
# countries whose average responses are most similar.
#
# The similarity is calculated using Euclidean distance across
# all selected questions.
#
# Smaller distance = more similar values profile
# ------------------------------------------------------------

score_user <- function(user_answers, profiles) {

  # Get the names of the questions the user answered
  q_names <- names(user_answers)

  # Ensure the answers are numeric
  user_answers <- as.numeric(user_answers)
  names(user_answers) <- q_names

  # For each country profile, compute distance from the user
  profiles %>%
    rowwise() %>%
    mutate(
      distance = sqrt(sum((c_across(all_of(q_names)) - user_answers)^2))
    ) %>%
    ungroup() %>%

    # Sort countries from most similar to least similar
    arrange(distance) %>%

    # Ensure country name is a character (helps with display)
    mutate(b_country = as.character(b_country)) %>%

    # Keep only the relevant output columns
    select(b_country, distance) %>%

    # Return the 3 closest countries
    slice(1:3)
}


# ------------------------------------------------------------
# Simple test example
# ------------------------------------------------------------
# Here we simulate a user answering a subset of questions
# and see which countries their answers most resemble
# ------------------------------------------------------------

test_answers <- c(
  q6 = 2,
  q164 = 5,
  q169 = 3,
  q186 = 4,
  q182 = 2,
  q172 = 3,
  q170 = 4,
  q25 = 2,
  q22 = 1,
  q165 = 3,
  q148 = 2,
  q33 = 3,
  q193 = 4,
  q209 = 2,
  q38 = 3
)

# Run the scoring function
print(score_user(test_answers, country_profiles))


# ------------------------------------------------------------
# Save artifacts for later use
# ------------------------------------------------------------
# These files allow the web app (or other scripts) to load the
# questions and country profiles quickly without recomputing them
# ------------------------------------------------------------

saveRDS(question_list, "data/processed/question_list.rds")
saveRDS(country_profiles, "data/processed/country_profiles.rds")

message("Saved: question_list.rds and country_profiles.rds")
