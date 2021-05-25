#' @title Get behavioral metadata for each participant
#' @description Creates a dataframe composed of each participant's id, signal
#'   probability, clock side, task begin and end.
#' @export
get_behavioral_metadata <- function(behavioral_data) {
  behavioral_data %>%
    dplyr::select(id, p_signal, clock_side, task_begin, task_end) %>%
    unique()
}

#' @title Get hit times vector from signals and responses per participant
#' @description This function uses input of both signal times and response times
#'   from a single participant's extracted behavioral data to determine the hit
#'   indices, if any, and in turn, the hit times.
#' Get hit timestamp from a vector of signal times and a vector of response times
#' @note Use the .interval variable if 8 seconds is not the desired interval
#' @export
get_hit_times <- function(signal_times, response_times, .interval = 8.0) {

  # Create vector to hold hit indices
  hit_indices <- c()

  signal_times %>%
    purrr::map_dbl(function(signal_time) {

      # Find indices for potential hits within the signal interval
      potential_hit_indices <- which(
        response_times %>%
          purrr::map_lgl(~ between(.x, signal_time, signal_time + .interval)),
        arr.ind = TRUE
      )

      # If there are zero potential hits for this signal time, return NA
      if (!length(potential_hit_indices)) return(NA)

      # Remove existing hit indices from potential hit indices for this signal
      potential_hit_indices <- setdiff(potential_hit_indices, hit_indices)

      # Extract the first hit index (closest to signal time)
      hit_index <- first(potential_hit_indices)

      # Extract hit time from response times using hit index
      hit_time <- response_times[hit_index]

      # Update vector of hit indices
      hit_indices <- c(hit_indices, hit_index)

      # Return the hit time
      return(hit_time)

    })
}

#' @title Get all participants' hits given signal presence
#' @description Creates a dataframe composed of each participant's hit times and
#'   reaction times for those hits, row-by-row with signal times.
#' @export
get_hits_given_signals <- function(behavioral_data) {

  participants <- behavioral_data %>%
    dplyr::pull(id) %>%
    unique()

  # Extract only rows where a signal is present
  only_signals_df <- behavioral_data %>%
    dplyr::filter(is_signal == 1) %>%
    dplyr::mutate(
      signal_time = step_time
    ) %>%
    dplyr::select(trial, id, image_index, signal_time)

  # Extract only rows where a response attempt is present
  only_responses_df <- behavioral_data %>%
    dplyr::filter(is_response == 1) %>%
    dplyr::select(trial, id, image_index, resp_time)

  # Map over the unlisted participants' ids to get the per-participant
  # signals and responses, then return a combined dataframe of all participant
  # including trial rows for signals, and if it exists, hit time and reaction time
  map_dfr(participants, function(participant) {
    signals <- only_signals_df %>%
      dplyr::filter(id == participant)

    responses <- only_responses_df %>%
      dplyr::filter(id == participant)

    signals %>% dplyr::mutate(
      hit_time = get_hit_times(signals$signal_time, responses$resp_time),
      reaction_time = hit_time - signal_time,
      is_hit_given_signal = as.integer(!is.na(hit_time))
    )
  })
}

#'
#'
#'

#' @title Get false alarms given responses for all participants
#' @description TODO
#' @export
get_false_alarms_given_responses <- function(behavioral_data, hits_with_reaction_times) {
  behavioral_data %>%
    dplyr::filter(is_response == 1) %>%
    dplyr::left_join(hits_with_reaction_times, by = c('trial', 'id', 'image_index')) %>%
    tidyr::replace_na(list(is_hit_given_signal = 0)) %>%
    dplyr::mutate(is_false_alarm_given_response = as.integer(!is_hit_given_signal)) %>%
    dplyr::select(trial, id, image_index, resp_time, is_false_alarm_given_response)
}
