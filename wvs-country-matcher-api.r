library(plumber)
library(jsonlite)
library(dplyr)
library(readr)
library(tibble)
library(tidyr)
library(httr2)

# ------------------------------------------------------------
# Basic CORS support so the API can be called from a browser
# front end running on a different origin.
#
# This allows:
# - GET / POST / OPTIONS requests
# - JSON requests from the client
# ------------------------------------------------------------

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  
  # Browsers often send a quick OPTIONS request first to check
  # whether cross-origin requests are allowed. We answer that here
  # and stop before hitting the rest of the API.
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  plumber::forward()
}

# ------------------------------------------------------------
# Load the precomputed files the API depends on
#
# These are artifacts created earlier in the project so the API
# does not have to rebuild everything on startup.
# ------------------------------------------------------------

country_profiles <- readRDS("../data/processed/country_profiles.rds")
question_list <- readRDS("../data/processed/question_list.rds")

question_meta <- read_csv(
  "../data/processed/question_meta.csv",
  show_col_types = FALSE
)

question_explanations <- read_csv(
  "../data/processed/question_explanations.csv",
  show_col_types = FALSE
)

country_lookup <- read_csv(
  "../data/processed/country_lookup.csv",
  col_types = cols(
    b_country = col_character(),
    country_name = col_character()
  )
)

profile_eval_results <- read_csv(
  "../data/processed/profile_eval_results.csv",
  show_col_types = FALSE
)

# Load the full cleaned survey data just to calculate overall
# average answers for each question. These global means are used
# later to help interpret where a user's answers stand out most.
global_means <- readRDS("../data/processed/wvs_safe.rds") %>%
  summarise(across(-b_country, mean, na.rm = TRUE)) %>%
  pivot_longer(
    everything(),
    names_to = "question_id",
    values_to = "global_mean"
  )

# Make sure country ids are stored as character strings so joins
# work cleanly with the lookup table later on
country_profiles <- country_profiles %>%
  mutate(b_country = as.character(b_country))

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

# Compare a user's answers to each country's average profile.
# The result is the 3 closest countries by Euclidean distance.
#
# Smaller distance = more similar answer pattern
score_user <- function(user_answers, profiles) {
  q_names <- names(user_answers)

  user_answers <- as.numeric(user_answers)
  names(user_answers) <- q_names

  profiles %>%
    rowwise() %>%
    mutate(
      distance = sqrt(sum((c_across(all_of(q_names)) - user_answers)^2))
    ) %>%
    ungroup() %>%
    arrange(distance) %>%
    select(b_country, distance) %>%
    slice(1:3)
}

# Identify which answers stand out most compared with the overall
# average across the survey. This gives the user a quick sense of
# which questions most shaped their result.
interpret_answers <- function(user_answers) {
  user_df <- tibble(
    question_id = names(user_answers),
    answer_value = as.numeric(user_answers)
  )

  user_df %>%
    left_join(global_means, by = "question_id") %>%
    mutate(
      difference = answer_value - global_mean
    ) %>%
    left_join(question_meta, by = "question_id") %>%
    arrange(desc(abs(difference))) %>%
    slice(1:3) %>%
    select(question_text, theme, answer_value, global_mean, difference)
}

# Thin wrapper around the Anthropic Messages API.
# This is used for two things:
# 1. answering general questions about the project
# 2. generating a friendly explanation of a user's result
call_claude <- function(system_prompt, messages, max_tokens = 500) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  model <- Sys.getenv("CLAUDE_MODEL")

  if (api_key == "") {
    stop("ANTHROPIC_API_KEY is not set.")
  }

  if (model == "") {
    stop("CLAUDE_MODEL is not set.")
  }

  resp <- request("https://api.anthropic.com/v1/messages") %>%
    req_headers(
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) %>%
    req_body_json(list(
      model = model,
      max_tokens = max_tokens,
      system = system_prompt,
      messages = messages
    )) %>%
    req_perform()

  body <- resp_body_json(resp, simplifyVector = FALSE)

  # Anthropic returns content in blocks, so we pull out the text
  # from each block and stitch them together into one string
  text_blocks <- vapply(
    body$content,
    function(x) if (!is.null(x$text)) x$text else "",
    FUN.VALUE = character(1)
  )

  paste(text_blocks[nzchar(text_blocks)], collapse = "\n")
}

# System prompt for general questions about the app itself
# This keeps the assistant from drifting into claims the model
# is not actually making
build_background_system_prompt <- function() {
  paste(
    "You are a helpful explainer for a World Values Survey country-matching project.",
    "Only describe the project as comparing a user's answer pattern to country-level average response profiles.",
    "Do not claim the model predicts literal nationality.",
    "Use plain language.",
    "If asked why 15 questions were chosen, explain that 15 was the best practical tradeoff because moving from 10 to 15 helped noticeably, while moving from 15 to 20 helped only a little.",
    "If asked what kinds of questions matter most, use the provided metadata and themes.",
    "If the user asks about immigration, say this app does not provide immigration advice yet."
  )
}

# System prompt for personalized result writeups
# The goal here is to sound useful and grounded without
# over-interpreting the person's psychology
build_result_system_prompt <- function() {
  paste(
    "You are writing a friendly personalized explanation for a World Values Survey matching tool.",
    "Be warm, concise, and grounded in the provided data.",
    "Do not overclaim personality traits.",
    "Describe the result as similarity to country-level average answer patterns.",
    "Mention 2 or 3 major themes that shaped the result.",
    "Do not mention raw variable IDs like q6 unless the user asks."
  )
}

# ------------------------------------------------------------
# API endpoints
# ------------------------------------------------------------

# Simple project summary endpoint so a front end can show a quick
# explanation of what this tool does

#* About the project
#* @get /about
function() {
  list(
    project = "World Values Survey country matcher",
    description = paste(
      "This tool uses 15 questions from the World Values Survey",
      "to compare a user's answers with country-level response patterns."
    ),
    output = "Returns the 3 countries with the most similar value profiles."
  )
}

# Return the 15 selected questions in the intended display order

#* Return the 15 selected questions
#* @get /questions
function() {
  question_meta %>%
    filter(question_id %in% question_list) %>%
    mutate(order = match(question_id, question_list)) %>%
    arrange(order) %>%
    select(order, question_id, question_text, response_type, choices, theme)
}

# Return metadata and explanation text for one specific question.
# Useful if the front end wants hover text, detail views, or a
# "tell me more about this question" feature.

#* Information about one question
#* @param id Question ID (example: q6)
#* @get /question-info
function(id = "") {
  out <- question_meta %>%
    filter(question_id == id) %>%
    left_join(question_explanations, by = c("question_id", "theme"))

  if (nrow(out) == 0) {
    return(list(error = paste("Question not found:", id)))
  }

  out
}

# Main scoring endpoint.
# Expects a JSON body with an `answers` object containing responses
# to all 15 required questions.

#* Score user answers
#* @post /score
#* @parser json
#* @serializer json
function(req, res) {
  body <- req$body

  if (is.null(body$answers)) {
    res$status <- 400
    return(list(error = "Request must include 'answers'"))
  }

  user_answers <- unlist(body$answers)
  missing_questions <- setdiff(question_list, names(user_answers))

  # Require all selected questions so the score is based on the
  # exact same feature set every time
  if (length(missing_questions) > 0) {
    res$status <- 400
    return(list(
      error = "Missing required answers",
      missing_questions = missing_questions
    ))
  }

  # Reorder answers to match the saved question order
  user_answers <- user_answers[question_list]

  results <- score_user(user_answers, country_profiles) %>%
    left_join(country_lookup, by = "b_country") %>%
    select(country_name, distance)

  interpretation <- interpret_answers(user_answers)

  list(
    top_matches = results,
    interpretation = interpretation
  )
}

# Chat endpoint with two modes:
#
# 1. background
#    For general "what is this app doing?" questions
#
# 2. result
#    For generating a short explanation of one user's matches
#
# Both modes package structured project data into a prompt and
# ask Claude to turn that into more natural language.

#* Chat with Claude about the project or the user's result
#* @post /chat
#* @parser json
#* @serializer json
function(req, res) {
  body <- req$body

  mode <- if (!is.null(body$mode)) body$mode else "background"

  if (mode == "background") {
    user_message <- body$message

    if (is.null(user_message) || !nzchar(user_message)) {
      res$status <- 400
      return(list(error = "For background mode, include a non-empty 'message'."))
    }

    top_questions <- question_meta %>%
      filter(question_id %in% question_list) %>%
      mutate(order = match(question_id, question_list)) %>%
      arrange(order) %>%
      select(order, question_id, question_text, theme)

    # Build a compact text summary Claude can use as factual context
    # before answering the user's question
    context_text <- paste(
      "Project facts:",
      "- This app compares a user's answers to country-level average profiles.",
      "- It uses 15 selected questions.",
      "- Evaluation summary:",
      paste(capture.output(print(profile_eval_results)), collapse = "\n"),
      "- Selected questions:",
      paste(capture.output(print(top_questions)), collapse = "\n"),
      sep = "\n"
    )

    reply <- tryCatch(
      {
        call_claude(
          system_prompt = build_background_system_prompt(),
          messages = list(
            list(
              role = "user",
              content = paste(context_text, "\n\nUser question:\n", user_message)
            )
          ),
          max_tokens = 500
        )
      },
      error = function(e) {
        paste("Claude error:", conditionMessage(e))
      }
    )

    return(list(reply = reply))
  }

  if (mode == "result") {
    if (is.null(body$answers)) {
      res$status <- 400
      return(list(error = "For result mode, include 'answers'."))
    }

    user_answers <- unlist(body$answers)
    missing_questions <- setdiff(question_list, names(user_answers))

    if (length(missing_questions) > 0) {
      res$status <- 400
      return(list(
        error = "Missing required answers",
        missing_questions = missing_questions
      ))
    }

    user_answers <- user_answers[question_list]

    top_matches <- score_user(user_answers, country_profiles) %>%
      left_join(country_lookup, by = "b_country") %>%
      select(country_name, distance)

    interpretation <- interpret_answers(user_answers)

    # Pull in both metadata and plain-English explanations so Claude
    # has enough context to write a result summary that sounds helpful
    answered <- tibble(
      question_id = names(user_answers),
      answer_value = as.numeric(user_answers)
    ) %>%
      left_join(question_meta, by = "question_id") %>%
      left_join(question_explanations, by = c("question_id", "theme"))

    result_context <- paste(
      "Top matches:",
      paste(capture.output(print(top_matches)), collapse = "\n"),
      "\nInterpretation:",
      paste(capture.output(print(interpretation)), collapse = "\n"),
      "\nAnswered questions:",
      paste(capture.output(
        print(answered %>% select(question_text, theme, answer_value, friendly_explanation))
      ), collapse = "\n"),
      sep = "\n"
    )

    reply <- tryCatch(
      {
        call_claude(
          system_prompt = build_result_system_prompt(),
          messages = list(
            list(
              role = "user",
              content = paste(
                result_context,
                "\n\nWrite a friendly explanation of this user's result in 2 short paragraphs."
              )
            )
          ),
          max_tokens = 500
        )
      },
      error = function(e) {
        paste("Claude error:", conditionMessage(e))
      }
    )

    return(list(
      top_matches = top_matches,
      interpretation = interpretation,
      reply = reply
    ))
  }

  res$status <- 400
  list(error = "mode must be 'background' or 'result'")
}
