library("targets")
library("tarchetypes")
library("future")
library("future.callr")
library("R.matlab")

# Options
options(tidyverse.quiet = TRUE)

# Strategy to run multisession future plan (background R sessions on current machine)
plan(multisession)

source('R/functions.R')

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

# Begin attempt at mat file extraction direct from R for targets pipeline
# Potential extractor method for v5 MAT files: https://stackoverflow.com/questions/58418852/how-to-import-matlab-table-cell-structure-to-r-dataframe-while-conserving-pr
extract_eyetracker_data <- function(mat_file) {
  mat_data <- readMat(mat_file)
  struct <- mat_data$Edf2Mat
  struct_data <- struct[,,1]
  return(struct_data)
}

extract_behavioral_data <- function(mat_file) {
  mat_data <- readMat(mat_file)
  struct <- mat_data$subjdata
  struct_data <- struct[,,1]
  return(struct_data)
}

extract_recordings_data <- function(struct_data) {
  recordings <- struct_data$RECORDINGS
  recordings_fields <- dimnames(recordings)[[1]]
  recordings_temp_df <- as.data.frame(t(recordings[,,]))
  recordings_df <- as_tibble(lapply(recordings_temp_df, unlist))
  return(recordings_df)
}

extract_fevent_data <- function(struct_data) {
  event <- struct_data$FEVENT
  event_fields <- dimnames(event)[[1]]
  event_temp_df <- as.data.frame(t(event[,,]))
  event_df <- as_tibble(lapply(purrr::map_depth(e_df, 2, ~ifelse(is.null(.x), NA, .x)), unlist, use.names = FALSE))
  return(event_df)
}

extract_fsample_data <- function(struct_data) {
  sample <- struct_data$FSAMPLE
  sample_fields <- dimnames(sample)[[1]]
  sample_temp_df <- as.data.frame(t(sample[,,]))
  sample_df <- as.data.frame(t(sample[,,]))
  return(sample_df)
}

# Configure pipeline
list (
  # Using `tar_files_input` from the {tarchetypes} package for input files that
  # are unlikely to change (i.e., raw data files).
  # See: https://docs.ropensci.org/tarchetypes/reference/tar_files_input.html#details
  tar_files_input(
    raw_eyetracker_mat_files,
    list.files(here::here(config$path$data, "raw", "eyetracker", "responses"), full.names = TRUE),
    format = "file"
  ),
  tar_files_input(
    raw_behavioral_mat_files,
    list.files(here::here(config$path$data, "raw", "behavioral", "responses"), full.names = TRUE),
    format = "file"
  ),
  tar_target(
    extracted_eyetracker_data,
    extract_eyetracker_data(raw_eyetracker_mat_files),
    pattern = map(raw_eyetracker_mat_files)
  ),
  tar_target(
    extracted_behavioral_data,
    extract_behavioral_data(raw_behavioral_mat_files),
    pattern = map(raw_behavioral_mat_files)
  ),
  tar_target(
    extracted_recordings_data,
    extract_recordings_data(extracted_eyetracker_data),
    pattern = map(extracted_eyetracker_data)
  ),
  tar_target(
    extracted_fevent_data,
    extract_fevent_data(extracted_eyetracker_data),
    pattern = map(extracted_eyetracker_data)
  )
)
