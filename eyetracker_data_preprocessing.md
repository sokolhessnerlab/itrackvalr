Eyetracker Data Preprocessing
=============================

Load packages
-------------

``` r
library(tidyverse)
library(formatR)
```

File path definitions
---------------------

``` r
root_path <- "/Volumes/shlab/Projects/CSN/data/extracted/eyetracker"
event_csv <- "fevent.csv"
sample_csv <- "fsample.csv"
ioevent_csv <- "ioevent.csv"
recordings_csv <- "recordings.csv"

id_prefix <- "CSN"
id_length <- 3
min_id <- 1
max_id <- 57
```

Loading methods
---------------

``` r
get_path_to_id <- function(id) {
    padded_id <- stringr::str_pad(id, 3, pad = "0")
    participant_id <- stringr::str_c(id_prefix, padded_id)
    return(file.path(root_path, participant_id))
}

get_id_vector <- function() {
    # Gets a vector of id integers for participants
    id_vector <- c()
    for (i in min_id:max_id) {
        if (dir.exists(get_path_to_id(i))) {
            id_vector <- c(id_vector, i)
        }
    }
    return(id_vector)
}

load_all_etd_by_filename_csv <- function(id_vector, filename_csv = sample_csv) {
    # Loads all eyetracking data of given filename across participants
    etd_list <- list()
    for (id in id_vector) {
        path_to_etd <- file.path(get_path_to_id(id), filename_csv)
        etd_list[[id]] <- readr::read_csv(path_to_etd)
    }
    return(etd_list)
}

# Convenience instantiation of the id vector for later use. Can use the getter
# method at any point for same output.
id_vector <- get_id_vector()
```

Load each data for all participants
-----------------------------------

``` r
etd_events <- load_all_etd_by_filename_csv(id_vector, event_csv)
etd_ioevents <- load_all_etd_by_filename_csv(id_vector, ioevent_csv)
etd_recordings <- load_all_etd_by_filename_csv(id_vector, recordings_csv)
# TAKES A LOT OF TIME TO LOAD. UNCOMMENT IF WANTED. etd_samples <-
# load_all_etd_by_filename_csv(id_vector, sample_csv)
```

### Events data methods using `edt_events` list

``` r
CALIBRATION_RESULT_MESSAGE <- "!CAL CALIBRATION HV9 R RIGHT"
VALIDATION_RESULT_MESSAGE <- "!CAL VALIDATION HV9 R RIGHT"
CATEGORY_STRING_PATTERN <- "CALIBRATION|VALIDATION"
QUALITY_STRING_PATTERN <- "GOOD|FAIR|POOR"
AVG_ERROR_STRING_INDEX <- 8
MAX_ERROR_STRING_INDEX <- 10
DEG_OFFSET_STRING_INDEX <- 13
PIX_OFFSET_STRING_INDEX <- 15


get_category_from_message <- function(message) {
    # Extract the category of event message
    as.character(str_extract(message, CATEGORY_STRING_PATTERN))
}


get_quality_from_message <- function(message) {
    # Extract the quality of calibration or validation from event message
    as.character(str_extract(message, QUALITY_STRING_PATTERN))
}


get_avg_error_from_message <- function(message) {
    # Extract the avg error of validation
    as.double(word(message, AVG_ERROR_STRING_INDEX))
}


get_max_error_from_message <- function(message) {
    # Extract the max error of validation
    as.double(word(message, MAX_ERROR_STRING_INDEX))
}


get_deg_offset_from_message <- function(message) {
    # Extract the deg offset of validation
    as.double(word(message, DEG_OFFSET_STRING_INDEX))
}


get_pix_offset_from_message <- function(message) {
    # Extract the x coordinate of pix offset of validation
    word(message, PIX_OFFSET_STRING_INDEX)
}


get_event_messages <- function(participant_events) {
    # Get the significant event messages for a single participant events data by
    # sttime, producing extracted columns from message contents.
    participant_events %>% select(message, sttime) %>% transmute(message = str_squish(str_trim(message)), 
        sttime = sttime) %>% filter(str_detect(message, CALIBRATION_RESULT_MESSAGE) | 
        str_detect(message, VALIDATION_RESULT_MESSAGE)) %>% mutate(category = get_category_from_message(message), 
        quality = get_quality_from_message(message), avg_error = get_avg_error_from_message(message), 
        max_error = get_max_error_from_message(message), deg_offset = get_deg_offset_from_message(message), 
        pix_offset = get_pix_offset_from_message(message)) %>% separate(pix_offset, 
        c("pix_x_offset", "pix_y_offset"), ",") %>% mutate(pix_x_offset = as.double(pix_x_offset), 
        pix_y_offset = as.double(pix_y_offset)) %>% relocate(-message)
}
```

``` r
# Output a single participant's important calibration/validation info
get_event_messages(etd_events[[1]])
```

    ## # A tibble: 7 x 9
    ##   sttime category quality avg_error max_error deg_offset pix_x_offset
    ##    <dbl> <chr>    <chr>       <dbl>     <dbl>      <dbl>        <dbl>
    ## 1 1.06e6 CALIBRA… GOOD        NA        NA         NA            NA  
    ## 2 1.08e6 CALIBRA… GOOD        NA        NA         NA            NA  
    ## 3 1.10e6 VALIDAT… POOR         4.66     23.6        4.37        168. 
    ## 4 1.15e6 CALIBRA… GOOD        NA        NA         NA            NA  
    ## 5 1.17e6 CALIBRA… GOOD        NA        NA         NA            NA  
    ## 6 1.20e6 VALIDAT… POOR         2.05      9.27       1.46         39.3
    ## 7 5.12e6 VALIDAT… POOR         2.25      8.98       1.44         55.1
    ## # … with 2 more variables: pix_y_offset <dbl>, message <chr>

### Recordings data methods using `etd_recordings` list

``` r
get_duration <- function(id) {
    # Duration from highest start time and lowest start time, in minutes
    summary <- etd_recordings[[id]] %>% summarize(duration = (max(time) - min(time))/(1000 * 
        60)) %>% unnest(cols = c())
    return(summary$duration)
}

get_durations <- function(id_vector) {
    # Durations in minutes for all participants events' data
    durations <- c()
    for (i in id_vector) {
        durations <- c(durations, get_duration(i))
    }
    return(durations)
}

get_recording_time_matrix <- function(id_vector, dimensional_reducer = 1) {
    # Get a built matrix with a row for each id in id_vector, with four times per
    # row. Optional dimensional reducer to achieve times in seconds, minutes.
    n_ids <- length(id_vector)
    n_recordings <- 1 + length(etd_recordings[[id_vector[[1]]]]$time)
    recording_time_matrix <- matrix(NA, nrow = n_ids, ncol = n_recordings)
    
    for (i in 1:n_ids) {
        id <- id_vector[i]
        recording_time_matrix[i, ] <- c(id, (etd_recordings[[id]]$time/dimensional_reducer))
    }
    
    return(recording_time_matrix)
    
}

get_recording_time_df <- function(id_vector, dimensional_reducer = 1) {
    # Get dataframe using matrix of ids and times from recordings. Optional
    # dimensional reducer to achieve times in seconds, minutes.
    m <- get_recording_time_matrix(id_vector, dimensional_reducer)
    df <- as.data.frame(m)
    recording_time_df_cols <- c("id", "calibration", "validation", "task", "revalidation")
    colnames(df) <- recording_time_df_cols
    
    return(df)
    
}
```

### Look at recording times

Can retrieve all recording moments across participants by seconds or
minutes. In seconds, the difference between revalidation and task time,
then subtracting 3600, provides an idea of how much “overtime” the task
went.

``` r
recording_time_df_seconds <- get_recording_time_df(id_vector, 1000)
recording_time_df_minutes <- get_recording_time_df(id_vector, 1000 * 60)

sort(recording_time_df_seconds$revalidation - recording_time_df_seconds$task - 3600)
```

    ##  [1]  25.149  28.309  31.267  31.483  31.733  32.517  32.635  33.821  34.419
    ## [10]  38.285  39.071  39.623  40.975  44.711  44.805  45.775  45.877  46.277
    ## [19]  47.045  48.175  49.093  49.677  50.481  52.581  53.299  55.383  56.825
    ## [28]  59.169  63.325  64.039  72.231  73.695  74.839  79.483  79.915  80.015
    ## [37]  84.351  95.241  99.463 113.003 115.135 116.207 135.669 139.489 145.689
    ## [46] 155.581 212.885 240.137 263.149

Joint data methods
------------------

``` r
get_times_with_validations_for_id <- function(recording_time_df, i) {
  
  recording_times_for_id <- recording_time_df %>%
    filter(id == i)
  
  validated_participant_details <- get_event_messages(etd_events[[i]]) %>%
    filter(category == "VALIDATION") %>%
    arrange(sttime) %>%
    slice_tail(n = 2) %>%
    mutate(id = as.integer(i)) %>%
    relocate(id)
  
  times_with_validations <- validated_participant_details %>%
    inner_join(recording_times_for_id) %>%
    select(-c(message, category)) %>% # dropped cols from events
    select(-c(calibration, validation)) %>%# dropped cols from recordings
    rename_at(vars(task, revalidation), ~ paste0(., "_start_time")) %>%
    mutate(id = as.integer(id)) %>%
    relocate(id, sttime, task_start_time, revalidation_start_time)
  
  return(times_with_validations)
  
}


get_all_times_with_validations <- function(id_vector) {
  df <- data.frame()
  recording_time_df <- get_recording_time_df(id_vector)
  for (i in id_vector) {
    times_with_validations <- get_times_with_validations_for_id(recording_time_df, i)
    df <- bind_rows(df, times_with_validations)
  }
  return(df)
}
```

### Combine recording times with significant event details per participant

``` r
# For demonstartion purposes, first 10 rows
get_all_times_with_validations(id_vector) %>% slice_head(n = 10)
```

    ##    id   sttime task_start_time revalidation_start_time quality avg_error
    ## 1   1  1195390         1335826                 5081515    POOR      2.05
    ## 2   1  5115802         1335826                 5081515    POOR      2.25
    ## 3   2  1909910         2036216                 5899365    POOR      1.54
    ## 4   2  5928119         2036216                 5899365    POOR      2.95
    ## 5   4  1769410         1840796                 5653681    GOOD      0.84
    ## 6   4  5689157         1840796                 5653681    GOOD      0.88
    ## 7   5  6231071         2554346                 6199151    GOOD      0.85
    ## 8   5  6269103         2554346                 6199151    GOOD      0.82
    ## 9   6  7870548         7935316                11670985    GOOD      0.21
    ## 10  6 11702118         7935316                11670985    GOOD      0.62
    ##    max_error deg_offset pix_x_offset pix_y_offset
    ## 1       9.27       1.46         39.3         40.9
    ## 2       8.98       1.44         55.1         30.7
    ## 3       3.32       0.56        -16.7        -16.3
    ## 4       3.70       1.66         -9.1         76.2
    ## 5       1.10       0.36         15.2          3.2
    ## 6       1.17       0.49         10.5         18.3
    ## 7       1.13       0.70         -3.6         32.7
    ## 8       1.03       0.64        -20.0         21.0
    ## 9       0.42       0.06         -0.7          2.4
    ## 10      0.98       0.57        -14.8         17.7
