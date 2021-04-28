library("targets")
library("tarchetypes")
library("future")
library("future.callr")
library("R.matlab")

# Options
options(tidyverse.quiet = TRUE)

# Strategy to run multisession future plan (background R sessions on current machine)
plan(multisession)

# Load functions
source('R/extract.R')

# Packages
tar_option_set(
  packages = c(
    "here",
    "config",
    "knitr",
    "kableExtra",
    "tidyverse",
    "R.matlab"
  )
)

# Config
config <- config::get()

# Configure pipeline
list (
  # Get list of full file paths to raw eyetracker data per participant
  tar_files_input(
    raw_eyetracker_mat,
    list.files(here::here(config$path$data, "raw", "eyetracker", "responses"), full.names = TRUE),
    format = "file"
  ),
  # Get list of full file paths to raw behavioral data per participant
  tar_files_input(
    raw_behavioral_mat,
    list.files(here::here(config$path$data, "raw", "behavioral", "responses"), full.names = TRUE),
    format = "file"
  ),
  # Extract and store behavioral data as a CSV. Returns path to CSV.
  tar_target(
    extracted_behavioral_csv_path,
    extract_behavioral_data(raw_behavioral_mat),
    pattern = map(raw_behavioral_mat),
    format = "file"
  )
  #tar_target(
    #extracted_eyetracker_data,
    #extract_eyetracker_data(raw_eyetracker_mat_files),
    #pattern = map(raw_eyetracker_mat_files)
  #),
  #tar_target(
    #extracted_recordings_data,
    #extract_recordings_data(extracted_eyetracker_data),
    #pattern = map(extracted_eyetracker_data)
  #),
  #tar_target(
    #extracted_fevent_data,
    #extract_fevent_data(extracted_eyetracker_data),
    #pattern = map(extracted_eyetracker_data)
  #)
)
