library(targets)
library(tarchetypes)
library(future)
library(R.matlab)

# Options
options(tidyverse.quiet = TRUE)

future::plan(future::multisession)

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
extract_edf_data <- function(mat_file) {
  mat_data <- readMat(mat_file)
  struct <- mat_data$Edf2Mat
  struct_data <- struct[,,1]

  recordings <- struct_data$RECORDINGS
  recordings_fields <- dimnames(recordings)[[1]]
  recordings_temp_df <- as.data.frame(t(recordings[,,]))
  recordings_df <- as_tibble(lapply(recordings_temp_df, unlist))

  event <- struct_data$FEVENT
  event_fields <- dimnames(event)[[1]]
  event_temp_df <- as.data.frame(t(event[,,]))
  event_df <- as_tibble(lapply(purrr::map_depth(e_df, 2, ~ifelse(is.null(.x), NA, .x)), unlist, use.names = FALSE))

  #sample <- struct_data$FSAMPLE
  #sample_fields <- dimnames(sample)[[1]]
  #sample_df <- as.data.frame(t(sample[,,]))
}

extract_behavioral_data <- function(mat_file) {
  mat_data <- readMat(mat_file)
  struct <- mat_data$subjdata
  struct_data <- struct[,,1]
  # return a df
}

# Configure pipeline
list (
  tar_files(
    raw_eyetracker_mat_files,
    list.files(here::here(config$path$data, "raw", "eyetracker", "responses"), full.names = TRUE)
  ),
  tar_files(
    raw_behavioral_mat_files,
    list.files(here::here(config$path$data, "raw", "behavioral", "responses"), full.names = TRUE)
  ),
  tar_target(
    extracted_behavioral_data,
    extract_behavioral_data(raw_behavioral_mat_files),
    pattern = map(raw_behavioral_mat_files)
  )
)
