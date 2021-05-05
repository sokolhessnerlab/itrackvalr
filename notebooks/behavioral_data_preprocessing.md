Behavioral Data Preprocessing
================
Ari Dyckovsky

  - [Load extracted CSV files](#load-extracted-csv-files)
  - [Sanity checks](#sanity-checks)
  - [Function definitions](#function-definitions)
  - [Get the combined hits using the
    function](#get-the-combined-hits-using-the-function)
  - [Check out a quick preview of the table of
    hits](#check-out-a-quick-preview-of-the-table-of-hits)
  - [Check out the reaction time summary statistics by
    id:](#check-out-the-reaction-time-summary-statistics-by-id)
  - [Show plot of reaction times by signal
    times](#show-plot-of-reaction-times-by-signal-times)

## Load extracted CSV files

Construct a list of the extracted behavioral CSV file targets by
participant.

``` r
extracted_behavioral_csv_files <- participants %>%
  unlist(use.names = FALSE) %>%
  map(~ str_c("extracted_behavioral_csv_file_", .x))
```

Read CSVs into dataframe `combined_df` after using `tar_read_raw` to get
the file path for participants from targets. Will include *all*
participants and include an `id` column to identify the participant
individually.

``` r
withr::with_dir(here::here(), {
  combined_df <- extracted_behavioral_csv_files %>%
    map_df(~ read_csv(tar_read_raw(.)))
})

# Transform step and response types to 0 or 1 integer values to simulate boolean behavior.
combined_df <- combined_df %>%
  mutate(
    is_signal = as.integer(step_type > 1),
    is_response = as.integer(resp_type)
  ) %>%
  select(-c(resp_type, step_type))
```

## Sanity checks

Count of participants by unique id.

``` r
length(unique(combined_df$id))
```

    ## [1] 50

``` r
combined_df %>%
  select(trial, id, is_response, is_signal, resp_time, step_time) %>%
  group_by(id) %>%
  tally(is_response) %>%
  summarise(mean(n), sd(n), min(n), max(n))
```

    ## # A tibble: 1 x 4
    ##   `mean(n)` `sd(n)` `min(n)` `max(n)`
    ##       <dbl>   <dbl>    <int>    <int>
    ## 1      37.3    32.9        4      170

## Function definitions

The function to get each participant’s hit time:

``` r
HIT_INTERVAL <- 8

# Get hit timestampe from a vector of signal times and a vector of response times
# Use the .interval variable if HIT_INTERVAL is not defined or a different hit interval is desired
get_hit_time <- function(signal_times, response_times, .interval = HIT_INTERVAL) {
  signal_times %>% 
    map_dbl(function(signal_time) {
      
      hit_index <- first(which(
        response_times %>% 
          map_lgl(~ between(.x, signal_time, signal_time + .interval)),
        arr.ind = TRUE
      ))
      
      hit_time <- response_times[hit_index]
      
      return(hit_time)
      
    })
}
```

The function to get a dataframe of combined hits, including the hit time
itself, and the reaction time between that hit time and the signal
prompting that response.

``` r
# Get combined hits dataframe composed of each participant's hit times and
# reaction times for those hits, row-by-row with signal times.
# Uses both combined signals and responses
get_combined_hits <- function(participants, combined_df) {
  
  # Extract only rows where a signal is present
  combined_signals_df <- combined_df %>%
    filter(is_signal == 1) %>%
    mutate(
      signal_time = step_time
    ) %>%
    select(trial, id, image_index, signal_time)
  
  # Extract only rows where a response attempt is present
  combined_responses_df <- combined_df %>%
    filter(is_response == 1) %>%
    select(trial, id, image_index, resp_time)
  
  # Map over the unlisted participants' ids to get the per-participant
  # signals and responses, then return a combined dataframe of all participant
  # including trial rows for signals, and if it exists, hit time and reaction time
  map_dfr(unlist(participants), function(participant) {
    participant_signals <- combined_signals_df %>%
      filter(id == participant)
    
    participant_responses <- combined_responses_df %>%
      filter(id == participant)
      
    participant_signals %>% mutate(
        hit_time = get_hit_time(participant_signals$signal_time, participant_responses$resp_time),
        reaction_time = hit_time - signal_time
      )
  })
}
```

## Get the combined hits using the function

``` r
combined_hits_df <- get_combined_hits(participants, combined_df)
```

## Check out a quick preview of the table of hits

``` r
knitr::kable(head(combined_hits_df, 10))
```

| trial | id     | image\_index |   signal\_time |     hit\_time | reaction\_time |
| ----: | :----- | -----------: | -------------: | ------------: | -------------: |
|    81 | CSN001 |          801 |  80.0604399294 |            NA |             NA |
|   117 | CSN001 |          941 | 116.0665163944 | 116.984091845 | 0.917575450165 |
|   119 | CSN001 |         3131 | 118.0679634164 |            NA |             NA |
|   211 | CSN001 |         1325 | 210.0571848214 |            NA |             NA |
|   235 | CSN001 |          752 | 234.0716901990 |            NA |             NA |
|   361 | CSN001 |          103 | 360.0647047530 |            NA |             NA |
|   461 | CSN001 |         2804 | 460.0590288559 |            NA |             NA |
|   591 | CSN001 |           28 | 590.0714484362 |            NA |             NA |
|   823 | CSN001 |          929 | 822.0630183384 | 822.772524834 | 0.709506495599 |
|   845 | CSN001 |          517 | 844.0589960678 | 845.923186561 | 1.864190493347 |

## Check out the reaction time summary statistics by id:

``` r
knitr::kable(head(
  combined_hits_df %>%
    drop_na() %>%
    group_by(id) %>%
    summarise(
      reaction_time_mean = mean(reaction_time),
      reaction_time_min = min(reaction_time),
      reaction_time_max = max(reaction_time),
      reaction_time_sd = sd(reaction_time)
    ),
  10
))
```

| id     | reaction\_time\_mean | reaction\_time\_min | reaction\_time\_max | reaction\_time\_sd |
| :----- | -------------------: | ------------------: | ------------------: | -----------------: |
| CSN001 |        1.71981596174 |      0.030177300678 |       4.73494848317 |     1.185127808038 |
| CSN002 |        1.99359213268 |      0.439399655617 |       7.77171126677 |     1.727003771770 |
| CSN004 |        1.70646593409 |      0.996316162899 |       2.84090208776 |     0.543133623999 |
| CSN005 |        3.06080474118 |      1.533621048930 |       7.64323121076 |     2.120246304387 |
| CSN006 |        1.47237477858 |      0.093593949678 |       7.99434786988 |     1.763412125625 |
| CSN007 |        1.79454994735 |      0.759996981775 |       3.99019392886 |     0.965011326176 |
| CSN008 |        3.09417513841 |      0.850421200456 |       7.92466269398 |     1.627078951623 |
| CSN009 |        1.31759492969 |      0.599157401573 |       2.19912816937 |     0.469031982754 |
| CSN010 |        2.15590923006 |      0.246194777312 |       7.99283455560 |     2.249618738692 |
| CSN011 |        1.36405598454 |      0.692828005016 |       3.64924517583 |     0.651777575973 |

## Show plot of reaction times by signal times

For hits, the reaction time does increase slightly across all
participants.

``` r
combined_hits_df %>%
  drop_na() %>%
  ggplot(aes(x = signal_time, y = reaction_time)) +
    geom_point(color = 'orange') +
    geom_smooth(method=lm) +
    theme_classic()
```

![](behavioral_data_preprocessing_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
