#' @title Run matlab extractor
#' @description If MATLAB is available on system, run script to create CSV
#'   from MAT files
#' @export
#' @return 0 for success; 1 for error. see matlbr docs.
#' @param path_to_raw_data Character, path to raw data to extract
#' @param mat_file Character, file name with extension ".m"
#' @examples
#' library(matlabr)
#' library(here)
#' extract_csvs_from_mats(here(), "edf_mat_to_csv.m")
extract_csvs_from_mats <- function(mat_file) {
  # TODO: Add args for data path to CSN instead of hard coded in .m files
  if (matlabr::have_matlab()) {
    # run only if matlab is available
    matlabr::run_matlab_script(here('matlab', 'converters', mat_file))
  }
}

#' @title Make dataframe output pretty
#' @description When using `knitr::kable` for documentation output from RMarkdown,
#'   return a pretty table. Otherwise, return the dataframe as is.
#' @export
#' @return dataframe
#' @param df Dataframe
#' @examples
#' make_pretty_df(df)
make_pretty_df <- function(df) {
  if (isTRUE(getOption("knitr.in.progress"))) {
    knitr::kable(df,
                 "simple",
                 format.args = list(big.mark = ",", scientific = FALSE))
  } else {
    df
  }
}

#' @title Get file path to a formal participant identifier
#' @description Participants are identified by order of participation, and
#'   data files for each participant should be stored in a dedicated folder,
#'   such as `CSN004`. This function constructs the file path for a given stage
#'   of data, i.e., `raw` or `extracted`.
#' @export
#' @param path_to_dir Character, a path to a directory of CSV data at a given stage
#' @param id Character, a unique integer identifier
#' @return path to that participant's id
#' @examples
#' get_path_to_id(extracted_eyetracker_data_path, 4)
get_path_to_id <- function(path_to_dir, id) {
  padded_id <- stringr::str_pad(id,
                                participant_id_pad_length,
                                pad = participant_id_pad_character)
  participant_id <- stringr::str_c(participant_id_prefix, padded_id)
  return(file.path(path_to_dir,
                   participant_id))
}

#' @title Get vector of integer id values
#' @description Create and store integer id values in a vector
#'   for participants relative to the existence of that participant's data.
#'   In some cases, participants are not converted from raw MAT data to CSVs
#'   if not considered a complete participant.
#' @export
#' @param path_to_dir Character, a path to a directory of CSV data at a given stage
#' @param min_id Integer, a minimum integer id value. Default is 1.
#' @param max_id Integer, a maximum integer id value. Default is 1.
#' @return vector of integer id values
#' @examples
#' get_id_vector(extracted_eyetracker_data_path, 1, 57)
get_id_vector <- function(path_to_dir, min_id = 1, max_id = 1) {
  id_vector <- c()
  for (i in min_id:max_id) {
    if (dir.exists(get_path_to_id(path_to_dir, i))) {
      id_vector <- c(id_vector, i)
    }
  }
  return(id_vector)
}

#' @title Load participant data
#' @description Read a directory of files, or directory of directories of
#'   files specifying a specific commanly-named file, into a dataframe list.
#'   Specify a `filename_csv` string if the data type is nested within a participant
#'   folder (e.g., eye-tracking data, such as `extracted/eyetracker/CSN004/fsample.csv`).
#'   Otherwise, the method assumes the data is stored as a CSV-per-participant (e.g.,
#'   behavioral data, such as `extracted/behavioral/CSN004.csv`).
#' @export
#' @param path_to_dir A path string to a directory of CSV data
#' @param id_vector A vector of participant IDs as integers
#' @param filename_csv A string name for a file type, including extension.
#' @return list of dataframes
#' @examples
#' load_participant_data(extracted_eyetracker_data_path, id_vector, event_csv)
load_participant_data <- function(path_to_dir, id_vector, filename_csv) {
  # TODO: Include column specfications by type of loading.
  if ("df" %in% ls()) rm("df")
  df <- data.frame()

  df_list <- list()

  for (id in id_vector) {
    if (is_missing(filename_csv)) {
      # Path to file of the participant data as a CSV, such as `path/to/CSN004.csv`.
      path_to_file <- stringr::str_c(get_path_to_id(path_to_dir, id), ".csv")
    } else {
      # Path to files nested within a participant-identified directory, such
      # as with eyetracking data that is split into multiple CSVs, which may
      # be `path/to/CSN004/data.csv`.
      path_to_file <- file.path(get_path_to_id(path_to_dir, id), filename_csv)
    }

    df <- readr::read_csv(path_to_file)
    df_list[[id]] <- df
  }

  return(df_list)
}
