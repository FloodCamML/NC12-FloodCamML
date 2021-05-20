library(tidyverse)
library(googledrive)
library(googlesheets4)

# source("C:/Users/Adam Gold/Desktop/google_keys.R")
data <- tibble(
  "date" = NA_Date_,
  "location" = NA_character_,
  "filename" = NA_character_,
  "supervised_score" = NA_real_,
  "supervised_class" = NA_character_,
  "customer_response" = NA_character_
)

