# World Values Matcher

This project compares a user's social values to country-level patterns from the World Values Survey.
See live app here: https://davidwrauch.wixsite.com/work

Users answer 15 questions about religion, sexuality, gender roles, civic behavior, and family values. Their answers are then compared to average responses from different countries to see which national value profiles they most closely resemble. THe user can then interact with a Claude-enabled chat feature to better understand their results and the project.

The goal was to see whether a small number of well-chosen questions could capture meaningful differences between countries.

---

## Why I Built This

The World Values Survey contains hundreds of questions across dozens of countries. It’s an incredible dataset, but it’s hard to get an intuitive feel for how values differ between societies.

I wanted to see whether it was possible to build a simple interactive tool that lets someone compare their own responses with national cultural patterns.

The result is a small web app that turns a large cross-national dataset into something interactive and interpretable.

Also, the current global political moment seems to be pushing people toward voting with their feet, and some are choosing to move to countries that more closely align with their values. This tool may assist in that or start a dialogue.

---

## Data

The project uses the **World Values Survey Wave 7 cross-national dataset**.
https://www.worldvaluessurvey.org/

The dataset includes responses from many countries on topics like:

- religion
- social norms
- gender roles
- family obligations
- political participation
- social trust

---

## Feature Selection

The full dataset includes hundreds of potential variables.

During development I trained exploratory machine learning models (XGBoost) to identify which survey variables best distinguish countries, and used those results to guide the final question feature selection.

To keep the user experience simple, I experimented with models using **10, 15, and 20 questions**.  

The goal was to find the smallest number of questions that still separated countries reasonably well.

| Questions | Top-3 Country Accuracy |
|-----------|-----------------------|
| 10 | 26% |
| 15 | 32% |
| 20 | 34% |

15 questions ended up being the best balance between predictive power and survey length.

---

## Model

For each country, I compute the **average response** to the selected questions.  
This creates a value profile vector for every country.

When a user completes the survey, their answers are compared to each country profile using **Euclidean distance**.

The three countries with the smallest distance are returned as the closest matches.

Distance represents how different the overall answer pattern is from the country average.

---

## API

The backend is built using **Plumber (R)**.

Main endpoints:

GET /questions  
Returns the 15 questions.

POST /score  
Calculates similarity between a user's answers and country profiles.

POST /chat  
Uses an LLM to explain which value dimensions most influenced the result.

---

## Interpretation Layer

The raw results are numeric distances, which aren’t very intuitive.

To make them easier to understand, the app uses a Claude LLM integration that generates short explanations about which themes (religion, sexuality, civic behavior, etc.) most influenced the match.

---

## Deployment

The API is containerized with **Docker** and deployed on **Fly.io**.

Example endpoint:

https://world-values.fly.dev/questions

---

## Stack

R  
tidyverse  
Caret
XGBoost
Plumber  
Docker  
Fly.io  
Anthropic Claude API
