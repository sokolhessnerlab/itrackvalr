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
source('R/behavioral_data_models.R')

# Packages
tar_option_set(
  packages = c(
    "here",
    "config",
    "knitr",
    "kableExtra",
    "tibble",
    "R.matlab",
    "stringr",
    "dplyr",
    "purrr"
  )
)

# Config
config <- config::get()

mapped_extraction <- tar_map(
  unlist = FALSE, # Return a nested list from tar_map()
  # Create a dataframe of participants' meta information using raw behavioral data response file names
  values = tibble(
    id = list.files(here::here(config$path$data, "raw", "behavioral", "responses")) %>%
      purrr::map(~ stringr::str_extract(.x, "CSN\\d{3}"))
  ),
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
    extract_behavioral_data(
      config$path$data,
      raw_behavioral_file,
      id_prefix = "CSN"
    )
  )
)

combined_behavioral <- tar_combine(
  extracted_behavioral_data_combined,
  mapped_extraction$extracted_behavioral_data,
  command = dplyr::bind_rows(!!!.x) %>%
    dplyr::mutate(
      # Transform step and response types to 0 or 1 integer values to simulate boolean behavior.
      is_signal = as.integer(step_type > 1),
      is_response = as.integer(resp_type)
    ) %>%
    dplyr::select(-c(resp_type, step_type))
)

summary_reports <- list(
  tar_render(
    incompletes,
    "notebooks/incompletes.Rmd",
    output_dir = "output"
  ),
  tar_render(
    exclusions,
    "notebooks/exclusions.Rmd",
    output_dir = "output"
  )
)

list(
  mapped_extraction,
  combined_behavioral,
  tar_target( #TODO: need to migrate to function in 'R/'
    metadata_behavioral,
    extracted_behavioral_data_combined %>%
      dplyr::select(id, p_signal, clock_side, task_begin, task_end) %>%
      unique()
  ),
  tar_target(
    all_hits_with_reaction_times,
    get_all_hits_with_reaction_times(extracted_behavioral_data_combined)
  ),
  tar_target(
    false_alarms,
    extracted_behavioral_data_combined %>%
      dplyr::filter(is_response == 1) %>%
      dplyr::left_join(all_hits_with_reaction_times, by = c('trial', 'id', 'image_index')) %>%
      tidyr::replace_na(list(is_hit = 0)) %>%
      dplyr::mutate(is_false_alarm = as.integer(!is_hit)) %>%
      dplyr::select(trial, id, image_index, resp_time, is_false_alarm)
  ),
  tar_render(
    behavioral_data_preprocessing_notebook,
    "notebooks/behavioral_data_preprocessing.Rmd",
    output_dir = "output"
  ),
  summary_reports
)
