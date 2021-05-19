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
    map_dbl(function(signal_time) {

      # Find indices for potential hits within the signal interval
      potential_hit_indices <- which(
        response_times %>%
          map_lgl(~ between(.x, signal_time, signal_time + .interval)),
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

#' @title Get all participants' hits with reaction times
#' @description Creates a dataframe composed of each participant's hit times and
#'   reaction times for those hits, row-by-row with signal times.
#' @export
get_all_hits_with_reaction_times <- function(combined_df) {

  participants <- combined_df %>%
    pull(id) %>%
    unique()

  # Extract only rows where a signal is present
  all_signals_df <- combined_df %>%
    filter(is_signal == 1) %>%
    mutate(
      signal_time = step_time
    ) %>%
    select(trial, id, image_index, signal_time)

  # Extract only rows where a response attempt is present
  all_responses_df <- combined_df %>%
    filter(is_response == 1) %>%
    select(trial, id, image_index, resp_time)

  # Map over the unlisted participants' ids to get the per-participant
  # signals and responses, then return a combined dataframe of all participant
  # including trial rows for signals, and if it exists, hit time and reaction time
  map_dfr(participants, function(participant) {
    signals <- all_signals_df %>%
      filter(id == participant)

    responses <- all_responses_df %>%
      filter(id == participant)

    signals %>% mutate(
      hit_time = get_hit_times(signals$signal_time, responses$resp_time),
      reaction_time = hit_time - signal_time,
      is_hit = as.integer(!is.na(hit_time))
    )
  })
}
