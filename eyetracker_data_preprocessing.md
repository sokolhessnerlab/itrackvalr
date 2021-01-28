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
get_all_times_with_validations(id_vector)
```

    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"

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
    ## 11  7  1975331         2051128                 5764131    GOOD      0.26
    ## 12  7  5793871         2051128                 5764131    GOOD      0.72
    ## 13  8  2661415         2722916                 6407267    FAIR      0.89
    ## 14  8  6428591         2722916                 6407267    POOR      1.18
    ## 15  9 12929488        12986604                16661443    GOOD      0.54
    ## 16  9 16683293        12986604                16661443    FAIR      0.74
    ## 17 10  2526494         2587336                 6233213    POOR      0.84
    ## 18 10  6253298         2587336                 6233213    POOR      2.09
    ## 19 11  1247087         1346862                 5061997    POOR      0.84
    ## 20 11  5083297         1346862                 5061997    POOR      4.70
    ## 21 12  1448114         1487916                 5160147    GOOD      0.86
    ## 22 12  5192924         1487916                 5160147    GOOD      0.66
    ## 23 15  1189741         1236256                 4874541    GOOD      0.75
    ## 24 15  4895318         1236256                 4874541    POOR      2.20
    ## 25 16  8255843         8290424                11953749    GOOD      0.88
    ## 26 16 11979423         8290424                11953749    POOR      1.29
    ## 27 17 14512561        14565532                18224701    POOR      0.57
    ## 28 17 18246979        14565532                18224701    POOR      2.05
    ## 29 18  2131274         2176006                 5871247    FAIR      1.06
    ## 30 18  5898916         2176006                 5871247    POOR      1.00
    ## 31 19  1782491         1838636                 5466945    GOOD      0.40
    ## 32 19  5517377         1838636                 5466945    GOOD      0.87
    ## 33 20  1855358         1904632                 5554309    POOR      1.64
    ## 34 20  5573882         1904632                 5554309    POOR      2.24
    ## 35 21  2095959         2252886                 5952349    POOR      1.32
    ## 36 21  6111905         2252886                 5952349    POOR      1.31
    ## 37 22  8187199         8218794                11867887    POOR      0.96
    ## 38 22 11887950         8218794                11867887    POOR      2.10
    ## 39 23  1218359         1247312                 4893087    FAIR      0.96
    ## 40 23  4917114         1247312                 4893087    POOR      0.81
    ## 41 25  1471275         1570970                 5250885    GOOD      0.64
    ## 42 25  5292966         1570970                 5250885    GOOD      0.87
    ## 43 26  2597140         2675926                 6324101    GOOD      0.58
    ## 44 26  6362123         2675926                 6324101    GOOD      0.72
    ## 45 27  3095729         3214436                 6853507    POOR      1.59
    ## 46 27  6875392         3214436                 6853507    POOR      3.33
    ## 47 29  2409855         2463750                 6127789    FAIR      1.07
    ## 48 29  6150941         2463750                 6127789    POOR      1.62
    ## 49 30  2381860         2432284                 6112299    GOOD      0.39
    ## 50 30  6135741         2432284                 6112299    GOOD      0.81
    ## 51 32  2361135         2450194                 6189683    GOOD      0.57
    ## 52 32  6270702         2450194                 6189683    POOR      1.28
    ## 53 33  1076788         1132234                 4785533    GOOD      0.49
    ## 54 33  4809206         1132234                 4785533    POOR      1.83
    ## 55 34  1660381         1761032                 5407309    GOOD      0.79
    ## 56 34  5425374         1761032                 5407309    GOOD      0.48
    ## 57 35  2077112         2138940                 5818423    FAIR      0.53
    ## 58 35  5839338         2138940                 5818423    POOR      2.01
    ## 59 36  1999466         2072262                 5724843    GOOD      0.56
    ## 60 36  5758481         2072262                 5724843    POOR      1.81
    ## 61 37  1508328         1543592                 5183215    POOR      1.20
    ## 62 37  5202251         1543592                 5183215    POOR      1.99
    ## 63 38  7464045         7525292                11157809    GOOD      0.49
    ## 64 38 11180287         7525292                11157809    GOOD      0.40
    ## 65 39  2164086         2214578                 5861623    GOOD      0.53
    ## 66 39  6143466         2214578                 5861623    GOOD      0.75
    ## 67 40  4885254         1191434                 4865129    POOR      3.05
    ## 68 40  4908090         1191434                 4865129    POOR      1.73
    ## 69 41  7490599         7647212                11363419    FAIR      0.60
    ## 70 41 11476259         7647212                11363419    POOR      1.23
    ## 71 42  2349859         2390322                 6031297    GOOD      0.55
    ## 72 42  6057399         2390322                 6031297    FAIR      0.83
    ## 73 43   846280          909672                 4544091    GOOD      0.68
    ## 74 43  4567038          909672                 4544091    POOR      0.89
    ## 75 44  2967643         3025672                 6670383    FAIR      0.61
    ## 76 44  6724748         3025672                 6670383    POOR      1.09
    ## 77 45  6778190         6822626                10454109    GOOD      0.19
    ## 78 45 10482390         6822626                10454109    POOR      1.59
    ## 79 47   315401          355892                 4111473    GOOD      0.70
    ## 80 47  4132569          355892                 4111473    POOR      1.40
    ## 81 48   994193         1039962                 4672597    GOOD      0.57
    ## 82 48  4691363         1039962                 4672597    FAIR      1.29
    ## 83 49  7168814         7209750                10834899    GOOD      0.59
    ## 84 49 10854898         7209750                10834899    POOR      0.84
    ## 85 51  1091056         1162098                 4793365    GOOD      0.57
    ## 86 51  4813281         1162098                 4793365    FAIR      1.07
    ## 87 52  1467984         1521416                 5176799    FAIR      0.56
    ## 88 52  5205726         1521416                 5176799    GOOD      0.59
    ## 89 53  8646308         8732690                12366511    GOOD      0.69
    ## 90 53 12417071         8732690                12366511    POOR      1.79
    ## 91 54  1460781         1503486                 5135219    FAIR      1.01
    ## 92 54  5156673         1503486                 5135219    POOR      0.82
    ## 93 55  1091738         1150236                 4800717    GOOD      0.98
    ## 94 55  4819370         1150236                 4800717    POOR      1.92
    ## 95 56  1023642         1072892                 4729717    FAIR      0.33
    ## 96 56  4788218         1072892                 4729717    FAIR      1.15
    ## 97 57  7114193         7173636                11013773    GOOD      0.51
    ## 98 57 11031374         7173636                11013773    POOR      1.29
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
    ## 11      0.43       0.15          0.4          6.1
    ## 12      1.33       0.64         -7.6         25.3
    ## 13      1.59       0.26          3.2          9.8
    ## 14      3.93       1.02         31.9         14.5
    ## 15      1.44       0.40         -2.4         17.9
    ## 16      1.73       0.38         10.6        -14.5
    ## 17      2.31       0.37         13.5         -6.2
    ## 18      4.75       1.89        -34.7        -81.7
    ## 19      3.07       0.73         17.7        -24.3
    ## 20     21.47       4.29        129.0       -107.7
    ## 21      1.35       0.76        -12.6        -26.8
    ## 22      0.84       0.50         -1.9         20.9
    ## 23      0.96       0.51         -2.4         18.9
    ## 24      3.28       2.12        -82.3          3.9
    ## 25      1.18       0.74         -6.4         26.1
    ## 26      9.71       1.15         34.5        -12.1
    ## 27      2.54       0.44          6.8         15.8
    ## 28      2.64       1.98        -60.4         63.0
    ## 29      1.72       0.76         24.4        -18.0
    ## 30      2.47       0.73         22.2         16.7
    ## 31      1.00       0.27         -5.4         10.8
    ## 32      1.23       0.79         26.2        -31.0
    ## 33      3.45       0.95         22.8         25.5
    ## 34      3.38       1.76         50.2         39.3
    ## 35      2.64       1.18         26.9         48.3
    ## 36      4.11       0.55          2.5         22.9
    ## 37      2.04       0.55         22.7          8.5
    ## 38      2.57       2.02         29.7        -82.4
    ## 39      1.93       0.93        -12.7        -37.9
    ## 40      2.15       0.77         -1.2        -34.0
    ## 41      0.99       0.41        -14.2         -7.4
    ## 42      1.18       0.71         -7.6         26.4
    ## 43      0.97       0.46        -15.3         -6.5
    ## 44      1.24       0.59          5.2        -24.3
    ## 45      4.61       1.43        -39.6         44.0
    ## 46      6.80       3.15       -113.5         43.9
    ## 47      1.30       0.89        -23.5         32.8
    ## 48      2.19       1.44         -4.5         69.0
    ## 49      0.92       0.07          0.1          3.0
    ## 50      0.92       0.26          3.4         11.2
    ## 51      1.19       0.36        -11.7         -7.0
    ## 52      2.14       1.05         -6.9        -43.3
    ## 53      0.84       0.26          0.3        -11.9
    ## 54      2.43       1.77        -58.3        -35.0
    ## 55      1.10       0.58          8.9         23.5
    ## 56      1.39       0.18          7.8          1.9
    ## 57      1.81       0.41          9.9         12.9
    ## 58      2.37       1.91        -74.0          9.9
    ## 59      1.26       0.35          8.6         -9.3
    ## 60      2.52       1.76        -32.5        -57.2
    ## 61      4.92       0.14         -2.5          4.7
    ## 62      5.64       1.37         -5.1         52.6
    ## 63      1.40       0.41        -11.9        -14.1
    ## 64      1.23       0.32         -6.3        -13.1
    ## 65      0.94       0.47         16.3          8.3
    ## 66      1.19       0.71         16.2         25.1
    ## 67     19.27       2.40         -6.3        -87.8
    ## 68      4.90       1.60        -34.7        -46.8
    ## 69      1.55       0.23         10.2          1.6
    ## 70      2.28       0.70        -22.5         16.2
    ## 71      1.40       0.31         -7.3         10.4
    ## 72      1.78       0.57          3.6        -23.3
    ## 73      0.98       0.33         -8.5        -12.3
    ## 74      2.37       0.82         19.8        -32.3
    ## 75      1.65       0.17         -3.0          6.1
    ## 76      2.42       0.86         25.7         28.4
    ## 77      0.42       0.05         -0.3         -2.4
    ## 78     10.51       1.50         68.4         -5.9
    ## 79      1.36       0.35          4.7        -15.7
    ## 80      3.66       1.19         26.6         52.0
    ## 81      1.16       0.10          3.2         -2.6
    ## 82      1.61       1.13         40.0        -21.2
    ## 83      1.22       0.50         -0.8         19.4
    ## 84      2.71       0.72         15.3         26.5
    ## 85      0.79       0.44         17.1          3.7
    ## 86      1.28       0.93         28.7         24.5
    ## 87      1.60       0.25          3.8         -9.4
    ## 88      0.78       0.21          6.3          6.0
    ## 89      1.46       0.25         -0.5          9.3
    ## 90      3.79       1.71         58.9         37.2
    ## 91      1.57       0.67        -16.0        -18.7
    ## 92      3.05       0.39         13.0          7.9
    ## 93      1.40       0.66         25.1          3.3
    ## 94      3.96       1.76         27.6        -77.0
    ## 95      1.57       0.22          6.3         -7.0
    ## 96      1.72       0.88          1.8         35.8
    ## 97      1.25       0.11         -2.2          4.1
    ## 98      2.65       1.18        -31.2         42.2
