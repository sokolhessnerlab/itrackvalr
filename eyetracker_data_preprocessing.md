Eyetracker Data Preprocessing
=============================

Setup
-----

### Libraries

``` r
library(tidyverse)
```

### Formatting

``` r
make_pretty_df <- function(df) {
    if (isTRUE(getOption("knitr.in.progress"))) {
        knitr::kable(df, "simple", format.args = list(big.mark = ",", scientific = FALSE))
    } else {
        df
    }
}
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
make_pretty_df(get_event_messages(etd_events[[5]]))
```

|     sttime| category    | quality |  avg\_error|  max\_error|  deg\_offset|  pix\_x\_offset|  pix\_y\_offset| message                                                                                    |
|----------:|:------------|:--------|-----------:|-----------:|------------:|---------------:|---------------:|:-------------------------------------------------------------------------------------------|
|  2,475,622| CALIBRATION | GOOD    |          NA|          NA|           NA|              NA|              NA| !CAL CALIBRATION HV9 R RIGHT GOOD                                                          |
|  2,499,059| VALIDATION  | GOOD    |        0.44|        1.11|         0.11|            -4.4|            -1.9| !CAL VALIDATION HV9 R RIGHT GOOD ERROR 0.44 avg. 1.11 max OFFSET 0.11 deg. -4.4,-1.9 pix.  |
|  6,231,071| VALIDATION  | GOOD    |        0.85|        1.13|         0.70|            -3.6|            32.7| !CAL VALIDATION HV9 R RIGHT GOOD ERROR 0.85 avg. 1.13 max OFFSET 0.70 deg. -3.6,32.7 pix.  |
|  6,269,103| VALIDATION  | GOOD    |        0.82|        1.03|         0.64|           -20.0|            21.0| !CAL VALIDATION HV9 R RIGHT GOOD ERROR 0.82 avg. 1.03 max OFFSET 0.64 deg. -20.0,21.0 pix. |

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
get_val_reval_by_id <- function(recording_time_df, i) {
    
    recording_times_for_id <- recording_time_df %>% filter(id == i)
    
    all_validations_by_id <- get_event_messages(etd_events[[i]]) %>% filter(category == 
        "VALIDATION") %>% select(-c(message, category))
    
    validation <- all_validations_by_id %>% filter(sttime < recording_times_for_id$task) %>% 
        arrange(sttime) %>% slice_tail(n = 1) %>% mutate(id = as.integer(i)) %>% 
        relocate(id) %>% rename_with(~paste(.x, "val", sep = "_"), -id)
    
    revalidation <- all_validations_by_id %>% filter(sttime > (recording_times_for_id$task + 
        60 * 60 * 1000)) %>% arrange(sttime) %>% slice_tail(n = 1) %>% mutate(id = as.integer(i)) %>% 
        relocate(id) %>% rename_with(~paste(.x, "reval", sep = "_"), -id)
    
    left_join(validation, revalidation, by = c("id"))
    
}


get_all_val_reval_df <- function(id_vector) {
    if ("df" %in% ls()) 
        rm("df")
    df <- data.frame()
    recording_time_df <- get_recording_time_df(id_vector)
    for (i in id_vector) {
        val_reval_df <- get_val_reval_by_id(recording_time_df, i)
        df <- bind_rows(df, val_reval_df)
    }
    return(df)
}

get_recording_and_val_reval_df <- function(recording_time_df, all_val_reval_df) {
    left_join(recording_time_df, all_val_reval_df, by = c("id"))
}
```

### Combine recording times with significant event details per participant

``` r
recording_time_df <- get_recording_time_df(id_vector)
all_val_reval_df <- get_all_val_reval_df(id_vector)

recording_and_val_reval_df <- get_recording_and_val_reval_df(recording_time_df, all_val_reval_df)
```

Visulaize the head of the dataframe:

``` r
make_pretty_df(head(recording_and_val_reval_df, 10))
```

|   id|  calibration|  validation|        task|  revalidation|  sttime\_val| quality\_val |  avg\_error\_val|  max\_error\_val|  deg\_offset\_val|  pix\_x\_offset\_val|  pix\_y\_offset\_val|  sttime\_reval| quality\_reval |  avg\_error\_reval|  max\_error\_reval|  deg\_offset\_reval|  pix\_x\_offset\_reval|  pix\_y\_offset\_reval|
|----:|------------:|-----------:|-----------:|-------------:|------------:|:-------------|----------------:|----------------:|-----------------:|--------------------:|--------------------:|--------------:|:---------------|------------------:|------------------:|-------------------:|----------------------:|----------------------:|
|    1|      881,864|     881,939|   1,335,826|     5,081,515|    1,195,390| POOR         |             2.05|             9.27|              1.46|                 39.3|                 40.9|      5,115,802| POOR           |               2.25|               8.98|                1.44|                   55.1|                   30.7|
|    2|    1,330,220|   1,330,285|   2,036,216|     5,899,365|    1,909,910| POOR         |             1.54|             3.32|              0.56|                -16.7|                -16.3|      5,928,119| POOR           |               2.95|               3.70|                1.66|                   -9.1|                   76.2|
|    4|    1,237,276|   1,237,353|   1,840,796|     5,653,681|    1,769,410| GOOD         |             0.84|             1.10|              0.36|                 15.2|                  3.2|      5,689,157| GOOD           |               0.88|               1.17|                0.49|                   10.5|                   18.3|
|    5|    2,173,786|   2,173,859|   2,554,346|     6,199,151|    2,499,059| GOOD         |             0.44|             1.11|              0.11|                 -4.4|                 -1.9|      6,269,103| GOOD           |               0.82|               1.03|                0.64|                  -20.0|                   21.0|
|    6|    7,524,796|   7,524,855|   7,935,316|    11,670,985|    7,870,548| GOOD         |             0.21|             0.42|              0.06|                 -0.7|                  2.4|     11,702,118| GOOD           |               0.62|               0.98|                0.57|                  -14.8|                   17.7|
|    7|    1,665,884|   1,665,951|   2,051,128|     5,764,131|    1,975,331| GOOD         |             0.26|             0.43|              0.15|                  0.4|                  6.1|      5,793,871| GOOD           |               0.72|               1.33|                0.64|                   -7.6|                   25.3|
|    8|    2,236,502|   2,236,561|   2,722,916|     6,407,267|    2,661,415| FAIR         |             0.89|             1.59|              0.26|                  3.2|                  9.8|      6,428,591| POOR           |               1.18|               3.93|                1.02|                   31.9|                   14.5|
|    9|   12,714,578|  12,714,637|  12,986,604|    16,661,443|   12,929,488| GOOD         |             0.54|             1.44|              0.40|                 -2.4|                 17.9|     16,683,293| FAIR           |               0.74|               1.73|                0.38|                   10.6|                  -14.5|
|   10|    1,653,320|   1,653,401|   2,587,336|     6,233,213|    2,526,494| POOR         |             0.84|             2.31|              0.37|                 13.5|                 -6.2|      6,253,298| POOR           |               2.09|               4.75|                1.89|                  -34.7|                  -81.7|
|   11|    1,071,538|   1,071,611|   1,346,862|     5,061,997|    1,247,087| POOR         |             0.84|             3.07|              0.73|                 17.7|                -24.3|      5,083,297| POOR           |               4.70|              21.47|                4.29|                  129.0|                 -107.7|

Check how average error changes between validation and revalidation:

``` r
avg_error_changes_df <- recording_and_val_reval_df %>% mutate(avg_error_change = avg_error_reval - 
    avg_error_val, max_error_change = max_error_reval - max_error_val) %>% relocate(c(avg_error_change, 
    max_error_change), .after = id) %>% arrange(abs(avg_error_change))

make_pretty_df(head(avg_error_changes_df, 10))
```

|   id|  avg\_error\_change|  max\_error\_change|  calibration|  validation|        task|  revalidation|  sttime\_val| quality\_val |  avg\_error\_val|  max\_error\_val|  deg\_offset\_val|  pix\_x\_offset\_val|  pix\_y\_offset\_val|  sttime\_reval| quality\_reval |  avg\_error\_reval|  max\_error\_reval|  deg\_offset\_reval|  pix\_x\_offset\_reval|  pix\_y\_offset\_reval|
|----:|-------------------:|-------------------:|------------:|-----------:|-----------:|-------------:|------------:|:-------------|----------------:|----------------:|-----------------:|--------------------:|--------------------:|--------------:|:---------------|------------------:|------------------:|-------------------:|----------------------:|----------------------:|
|   21|               -0.01|                1.47|    1,774,038|   1,774,103|   2,252,886|     5,952,349|    2,095,959| POOR         |             1.32|             2.64|              1.18|                 26.9|                 48.3|      6,111,905| POOR           |               1.31|               4.11|                0.55|                    2.5|                   22.9|
|   52|                0.03|               -0.82|    1,304,788|   1,304,855|   1,521,416|     5,176,799|    1,467,984| FAIR         |             0.56|             1.60|              0.25|                  3.8|                 -9.4|      5,205,726| GOOD           |               0.59|               0.78|                0.21|                    6.3|                    6.0|
|    4|                0.04|                0.07|    1,237,276|   1,237,353|   1,840,796|     5,653,681|    1,769,410| GOOD         |             0.84|             1.10|              0.36|                 15.2|                  3.2|      5,689,157| GOOD           |               0.88|               1.17|                0.49|                   10.5|                   18.3|
|   18|               -0.06|                0.75|    1,878,264|   1,878,319|   2,176,006|     5,871,247|    2,131,274| FAIR         |             1.06|             1.72|              0.76|                 24.4|                -18.0|      5,898,916| POOR           |               1.00|               2.47|                0.73|                   22.2|                   16.7|
|   38|               -0.09|               -0.17|    7,163,188|   7,163,245|   7,525,292|    11,157,809|    7,464,045| GOOD         |             0.49|             1.40|              0.41|                -11.9|                -14.1|     11,180,287| GOOD           |               0.40|               1.23|                0.32|                   -6.3|                  -13.1|
|   26|                0.14|                0.27|    2,270,082|   2,270,145|   2,675,926|     6,324,101|    2,597,140| GOOD         |             0.58|             0.97|              0.46|                -15.3|                 -6.5|      6,362,123| GOOD           |               0.72|               1.24|                0.59|                    5.2|                  -24.3|
|   23|               -0.15|                0.22|    1,045,664|   1,045,729|   1,247,312|     4,893,087|    1,218,359| FAIR         |             0.96|             1.93|              0.93|                -12.7|                -37.9|      4,917,114| POOR           |               0.81|               2.15|                0.77|                   -1.2|                  -34.0|
|   54|               -0.19|                1.48|      949,944|     950,013|   1,503,486|     5,135,219|    1,460,781| FAIR         |             1.01|             1.57|              0.67|                -16.0|                -18.7|      5,156,673| POOR           |               0.82|               3.05|                0.39|                   13.0|                    7.9|
|    9|                0.20|                0.29|   12,714,578|  12,714,637|  12,986,604|    16,661,443|   12,929,488| GOOD         |             0.54|             1.44|              0.40|                 -2.4|                 17.9|     16,683,293| FAIR           |               0.74|               1.73|                0.38|                   10.6|                  -14.5|
|   12|               -0.20|               -0.51|    1,124,654|   1,124,721|   1,487,916|     5,160,147|    1,448,114| GOOD         |             0.86|             1.35|              0.76|                -12.6|                -26.8|      5,192,924| GOOD           |               0.66|               0.84|                0.50|                   -1.9|                   20.9|
