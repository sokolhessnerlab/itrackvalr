Behavioral Data Preprocessing
================
Ari Dyckovsky

  - [Setup](#setup)
      - [Load package with
        dependencies](#load-package-with-dependencies)
      - [Load configuration settings](#load-configuration-settings)
      - [File path definitions](#file-path-definitions)
      - [Participant identification
        settings](#participant-identification-settings)
  - [Load extracted data](#load-extracted-data)
  - [Clock sides](#clock-sides)

## Setup

This notebook requires that the following chunks are run before any
other section chunks can resolve. Please complete the following in
order:

1.  Load the package interface with its dependencies,
2.  Load the configuration settings,
3.  Set file paths,
4.  Set participant identification settings.

### Load package with dependencies

``` r
# Load csn package with devtools
library(devtools)
load_all()

# Load dependencies
library(tidyverse)
library(rlang)
library(ggpubr)
```

### Load configuration settings

We use a configuration file called `config.yml` to handle environment
settings, such as the absolute path to data and subdirectories by type
of data (i.e., “raw”) and participant identification details (i.e., the
prefix “CSN”). For more information on how to use configurable
environment settings, see [this introduction
vignette](https://cran.r-project.org/web/packages/config/vignettes/introduction.html).

``` r
config <- config::get()
```

### File path definitions

File path definitions are set using configured environment settings from
the `config.yml` file. (Note: overrides of configured settings should
occur within the YAML text file, usually via a new environment that
inherits defaults other than explicit overrides).

``` r
# Top level data path for entry
data_path <- config$path$data

# Extracted data subdirectory path
extracted_data_path <- file.path(
  data_path,
  config$path$extracted
)

# Extracted behavioral data subdirectory path
extracted_behavioral_data_path <- file.path(
  extracted_data_path,
  config$path$behavioral
)
```

### Participant identification settings

Participant identification settings are also use configured environment
settings from the `config.yml` file.

``` r
# Participant id settings
participant_id_prefix <- config$participant_id$prefix
participant_id_pad_length <- config$participant_id$pad_length
participant_id_pad_character <- config$participant_id$pad_character
participant_id_min <- config$participant_id$min
participant_id_max <- config$participant_id$max
```

## Load extracted data

Start by establishing a vectorized key of integer identifiers for study
participants. These ids reflect only participants who completed all
required elements of the task.

``` r
# Convenience instantiation of the id vector for later use. Can use
# the getter method at any point for same output. Note: This uses the
# extracted eyetracker data path.
id_vector <- itrackvalr::get_id_vector(extracted_behavioral_data_path,
                                       participant_id_min,
                                       participant_id_max)
```

Then, load behavioral response data for each participant. The following
chunk calls `itrackvalr` functions to load CSVs for participants’
extracted behavioral data. The function returns assigns a list of these
loaded dataframes that we assign to `behavioral_df_list`.

``` r
behavioral_df_list <- itrackvalr::load_participant_data(extracted_behavioral_data_path,
                                            id_vector)
```

## Clock sides

Get the clock sides for each participant and create a dataframe of their
id and side values.

``` r
CLOCK_LEFT <- 0
CLOCK_RIGHT <- 1

get_clock_sides <- function(behavioral_df_list) {
  if ("df" %in% ls()) rm("df")
  df <- data.frame(id = integer(), side = integer())

  for (i in id_vector) {
    side <- behavioral_df_list[[i]] %>%
      slice(1) %>%
      pull(clock_side)
    df <- df %>% dplyr::add_row(tibble(id = i, side))
  }

  return(df)
}

clock_sides_df <- get_clock_sides(behavioral_df_list)

# Count of left and right clock sides
make_pretty_df(
  head(
    clock_sides_df %>%
      group_by(side) %>%
      count()
  )
)
```

| side | n |
| ---: | -: |
