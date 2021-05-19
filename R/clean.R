# Functions to clean data from extracted files

#################################################
# Behavioral data cleaning methods
#################################################

#' @title Clean extracted behavioral data
#' @export
clean_behavioral_data <- function(csv_file, id_prefix = "CSN") {
  # Get the padded 3-digit id from the file name, including a prefix
  padded_id <- stringr::str_extract(csv_file, str_c(id_prefix,"\\d{3}"))

  df <- read_csv(csv_file)

  # TODO
  #df <- df %>%
   # mutate(is_double_tick = step_type > 1) # mutate step type to a boolean for double tick

  # Construct output file
  output_file <- stringr::str_c(here::here(config$path$data, "clean", "behavioral", padded_id), ".csv")

  # Write dataframe to output file
  write_csv(df, output_file)

  # Return path
  return(output_file)
}

#################################################
# Eyetracker data cleaning methods
#################################################
