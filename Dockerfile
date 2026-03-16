FROM rocker/tidyverse:4.3.1

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled plumber httr2

RUN R -e "library(plumber); library(jsonlite); library(dplyr); library(readr); library(tibble); library(tidyr); library(httr2)"

WORKDIR /app

COPY api ./api
COPY data ./data

EXPOSE 8000

CMD ["Rscript", "api/run_api.R"]
