# Functions to extract data from raw files

#################################################
# Behavioral data extraction methods
#################################################

#' @title Extract behavioral data from MAT file
#' @export
extract_behavioral_data <- function(mat_file, id_prefix = "CSN") {
  # Get the padded 3-digit id from the file name, including a prefix
  participant_id <- stringr::str_extract(mat_file, str_c(id_prefix,"\\d{3}"))

  # Read MAT file and extract subject data
  mat_data <- readMat(mat_file)
  struct <- mat_data$subjdata
  struct_data <- struct[,,1]

  # Extract struct fields into dataframe
  df <- tibble(
    trial = 1:struct_data$nTrials,
    id = participant_id, # add the id; may need to make it just integer at some point
    p_signal = struct_data$pSignal[1],
    clock_side = struct_data$lr[1],
    resp_type = struct_data$resps[,1],
    resp_time = struct_data$resps[,2],
    step_type = struct_data$step[,1],
    step_time = struct_data$step[,2],
    image_index = struct_data$img.ind[,1],
    task_begin = struct_data$expBegin[1],
    task_end = struct_data$expEnd
  )

  # Construct output file
  output_file <- stringr::str_c(
    here::here(config$path$data, "extracted", "behavioral", participant_id),
    ".csv"
  )

  # Write dataframe to output file
  readr::write_csv(df, output_file)

  # Return df
  return(df)
}

#################################################
# Eyetracker data extraction methods
#################################################

#' @title Extract eyetracker from MAT file
#' @export
extract_eyetracker_data <- function(mat_file) {
  mat_data <- readMat(mat_file)
  struct <- mat_data$Edf2Mat
  struct_data <- struct[,,1]
  return(struct_data)
}

#' @title Extract recordings data from eyetracker struct
#' @export
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
