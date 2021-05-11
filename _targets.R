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
source('R/clean.R')
source('R/behavioral_data_preprocessing.R')

# Packages
tar_option_set(
  packages = c(
    "here",
    "config",
    "knitr",
    "kableExtra",
    "tibble",
    "R.matlab"
  )
)

# Config
config <- config::get()

# Create a dataframe of participants' meta information using raw behavioral data response file names
participants <- tibble(
  id = list.files(here::here(config$path$data, "raw", "behavioral", "responses")) %>%
    purrr::map(~ stringr::str_extract(.x, "CSN\\d{3}"))
)

list(
  # Per participant mapping of targets using static branching for better manifest output
  tar_map(
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

    # Extract behavioral CSV files and output path to them
    tar_target(
      extracted_behavioral_csv_file,
      extract_behavioral_data(raw_behavioral_file),
      format = "file"
    )
  ),
  tar_render(
    behavioral_data_preprocessing_notebook,
    "notebooks/behavioral_data_preprocessing.Rmd"
  )
)
