library(targets)
library(tarchetypes)
load_all()

# Globals

# Functions
source("R/functions.R")

# Options
options(tidyverse.quiet = TRUE)

# Packages
tar_option_set(packages = c("tidyverse", "config", "knitr", "kableExtra"))

# TODO: PUT CONFIG STUFF IN A FUNCTION???
# Top level data path for entry
data_path <- config$path$data

# Extracted data subdirectory path
extracted_data_path <- file.path(
  data_path,
  config$path$extracted
)

# Extracted eyetracker data subdirectory path
extracted_eyetracker_data_path <- file.path(
  extracted_data_path,
  config$path$eyetracker
)

# Extracted behavioral data subdirectory path
extracted_behavioral_data_path <- file.path(
  extracted_data_path,
  config$path$behavioral
)

# Extracted eyetracker data file names (to CSV, from MAT)
event_csv <- config$extracted_files$eyetracker_event_csv
sample_csv <- config$extracted_files$eyetracker_sample_csv
ioevent_csv <- config$extracted_files$eyetracker_ioevent_csv
recordings_csv <- config$extracted_files$eyetracker_recordings_csv

path_to_raw_sample_data <- function() {
  here(data_path, "raw", "eyetracker", "responses")
}



# Configure pipeline
list (
  tar_files(files, list.files(path_to_raw_sample_data())), # need actual vector of file paths
  tar_target(data,
             readr::read_csv(files),
             itrackvalr::load_participant_data(extracted_eyetracker_data_path,
                                                                 id_vector,
                                                                 event_csv),
             pattern = map(files))
)
