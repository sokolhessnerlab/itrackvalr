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
    "purrr",
    "lme4",
    "lmerTest"
  )
)

# Config
config <- config::get()

extract_raw_data <- tar_map(
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

combine_extracted_behavioral_data <- tar_combine(
  combined_behavioral_data,
  extract_raw_data$extracted_behavioral_data,
  command = dplyr::bind_rows(!!!.x) %>%
    dplyr::mutate(
      # Transform step and response types to 0 or 1 integer values to simulate boolean behavior.
      is_signal = as.integer(step_type > 1),
      is_response = as.integer(resp_type)
    ) %>%
    dplyr::select(-c(resp_type, step_type))
)

preprocess_behavioral_data <- list (
  combine_extracted_behavioral_data,
  tar_target(
    behavioral_metadata,
    get_behavioral_metadata(combined_behavioral_data)
  ),
  tar_target(
    hits_given_signals,
    get_hits_given_signals(combined_behavioral_data)
  ),
  tar_target(
    false_alarms_given_responses,
    get_false_alarms_given_responses(combined_behavioral_data, hits_given_signals)
  )
)

analyze_behavioral_data <- list(
  tar_target(
    scaled_hits_given_signals,
    hits_given_signals %>% mutate(signal_time = signal_time / 3600)
  ),
  tar_target(
    scaled_false_alarms_given_responses,
    false_alarms_given_responses %>% mutate(resp_time = resp_time / 3600)
  ),
  tar_target(
    model_hit_by_signal_time,
    glmer(
      is_hit_given_signal ~ 1 + signal_time + (1 | id),
      data = scaled_hits_given_signals,
      family = "binomial"
    )
  ),
  tar_target(
    model_hit_by_signal_time_rfx,
    glmer(
      is_hit_given_signal ~ 1 + signal_time + (1 + signal_time | id),
      data = scaled_hits_given_signals,
      family = "binomial"
    )
  ),
  tar_target(
    model_reaction_time_by_signal_time,
    lmer(
      reaction_time ~ 1 + signal_time + (1 | id),
      data = scaled_hits_given_signals %>% na.omit()
    )
  ),
  tar_target(
    model_reaction_time_by_signal_time_rfx,
    lmer(
      reaction_time ~ 1 + signal_time + (1 + signal_time | id),
      data = scaled_hits_given_signals %>% na.omit()
    )
  ),
  tar_target(
    model_false_alarm_by_response_time,
    glmer(
      is_false_alarm_given_response ~ 1 + resp_time + (1 | id),
      data = scaled_false_alarms_given_responses,
      family = "binomial"
    )
  ),
  tar_target(
    model_false_alarm_by_response_time_rfx,
    glmer(
      is_false_alarm_given_response ~ 1 + resp_time + (1 + resp_time | id),
      data = scaled_false_alarms_given_responses,
      family = "binomial"
    )
  )
)

report <- list(
  tar_render(
    incompletes,
    "notebooks/incompletes.Rmd",
    output_dir = "output"
  ),
  tar_render(
    exclusions,
    "notebooks/exclusions.Rmd",
    output_dir = "output"
  ),
  tar_render(
    behavioral_data_preprocessing_notebook,
    "notebooks/behavioral_data_preprocessing.Rmd",
    output_dir = "output"
  ),
  tar_render(
    behavioral_data_analysis_notebook,
    "notebooks/behavioral_data_analysis.Rmd",
    output_dir = "output"
  )
)

list(
  extract_raw_data,
  preprocess_behavioral_data,
  analyze_behavioral_data,
  report
)
