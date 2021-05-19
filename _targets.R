library(targets)
library(tarchetypes)
library(future)
library(tidyverse)

# Options
options(tidyverse.quiet = TRUE)

# Strategy to run multisession future plan (background R sessions on current machine)
plan(multisession)

# Load functions
source('R/utils-pipe.R')
source('R/extract.R')
#source('R/clean.R')
source('R/behavioral_data_preprocessing.R')

# Packages
tar_option_set(
  packages = c(
    "here",
    "config",
    "knitr",
    "kableExtra",
    "tibble",
    "R.matlab",
    "stringr"
  )
)

# Config
config <- config::get()

# Create a dataframe of participants' meta information using raw behavioral data response file names
participants <- tibble(
  id = list.files(here::here(config$path$data, "raw", "behavioral", "responses")) %>%
    purrr::map(~ stringr::str_extract(.x, "CSN\\d{3}"))
)

mapped_extraction <- tar_map(
  #unlist = FALSE, # Return a nested list from tar_map()
  values = participants,
  # Set raw behavioral file targets
  # TODO: change the `str_subset(...` to a function!
  tar_target(
    raw_behavioral_file,
    str_subset(list.files(here::here(config$path$data, "raw", "behavioral", "responses"), full.names = TRUE), id),
    format = "file"
  ),
  # Set raw eyetracker file targets
  # TODO: change the `str_subset(...` to a function!
  tar_target(
    raw_eyetracker_file,
    str_subset(list.files(here::here(config$path$data, "raw", "eyetracker", "responses"), full.names = TRUE), id),
    format = "file"
  ),
  # Extract behavitoral data from raw MAT and output dataframe
  # Side effect: Writes dataframe as CSV to `data/extracted`
  tar_target(
    extracted_behavioral_data,
    extract_behavioral_data(raw_behavioral_file)
  )
)

combined_behavioral <- tar_combine(
  extracted_behavioral_data_combined,
  mapped_extraction$extracted_behavioral_data,
  command = dplyr::bind_rows(!!!.x)
)

list(
  mapped_extraction,
  combined_behavioral,
  tar_render(
    behavioral_data_preprocessing_notebook,
    "notebooks/behavioral_data_preprocessing.Rmd"
  )
)
