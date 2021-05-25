#' @title Model participant hits by signal time
#' @description TODO
#' @export
hit_by_signal_time_model <- function(df, random_effects = FALSE) {
  if (random_effects) {
    glmer(
      is_hit_given_signal ~ 1 + signal_time + (1 + signal_time | id),
      data = df,
      family = "binomial"
    )
  } else {
    glmer(
      is_hit_given_signal ~ 1 + signal_time + (1 | id),
      data = df,
      family = "binomial"
    )
  }
}

#' @title Model participant reaction time by signal time
#' @description TODO
#' @export
reaction_time_by_signal_time_model <- function(df, random_effects = FALSE) {
  if (random_effects) {
    lmer(
      reaction_time ~ 1 + signal_time + (1 + signal_time | id),
      data = df %>% na.omit()
    )
  } else {
    lmer(
      reaction_time ~ 1 + signal_time + (1 | id),
      data = df %>% na.omit()
    )
  }
}

#' @title Model participant false alarms by response time
#' @description TODO
#' @export
false_alarm_by_response_time_model <- function(df, random_effects = FALSE) {
  if (random_effects) {
    glmer(
      is_false_alarm_given_response ~ 1 + resp_time + (1 + resp_time | id),
      data = df,
      family = "binomial"
    )
  } else {
    glmer(
      is_false_alarm_given_response ~ 1 + resp_time + (1 | id),
      data = df,
      family = "binomial"
    )
  }
}
