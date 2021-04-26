library(targets)
library(tarchetypes)

# Options
options(tidyverse.quiet = TRUE)

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
extract_edf_mat <- function(mat_file) {
  mat_data <- readMat(mat_file)
  struct <- mat_data$Edf2Mat
  struct_data <- struct[,,1]

  recordings <- struct_data$RECORDINGS
  recordings_fields <- dimnames(recordings)[[1]]
  recordings_df <- as.data.frame(t(struct[,,]))

  event <- struct_data$FEVENT
  event_fields <- dimnames(event)[[1]]
  event_df <- as.data.frame(t(struct[,,]))

  sample <- struct_data$FSAMPLE
  sample_fields <- dimnames(sample)[[1]]
  sample_df <- as.data.frame(t(struct[,,]))
}

# Configure pipeline
list (
  tar_files(
    eyetracker_mat_files,
    list.files(here::here(config$path$data, "raw", "eyetracker", "responses"), full.names = TRUE)
  ),
  tar_files(
    behavioral_mat_files,
    list.files(here::here(config$path$data, "raw", "behavioral", "responses"), full.names = TRUE)
  )
)
