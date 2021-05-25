Behavioral Data Preprocessing
================
Ari Dyckovsky

  - [Read extracted behavioral data](#read-extracted-behavioral-data)
      - [Sanity checks for extracted
        data](#sanity-checks-for-extracted-data)
  - [Analyze all participants’ hits with reaction
    times](#analyze-all-participants-hits-with-reaction-times)
  - [Reaction times per participant centered at the
    median](#reaction-times-per-participant-centered-at-the-median)
  - [Read false alarms target](#read-false-alarms-target)

## Read extracted behavioral data

Use `tar_read` to get the target object
`extracted_behavioral_data_combined` and assign it to `combined_df`.

``` r
withr::with_dir(here::here(), {
  combined_df <- tar_read(combined_behavioral_data)
})
```

### Sanity checks for extracted data

Look at a few rows of data to verify output format.

``` r
knitr::kable(head(combined_df))
```

| trial | id     | p\_signal | clock\_side | resp\_time |     step\_time | image\_index |   task\_begin |     task\_end | is\_signal | is\_response |
| ----: | :----- | --------: | ----------: | ---------: | -------------: | -----------: | ------------: | ------------: | ---------: | -----------: |
|     1 | CSN001 |      0.01 |           1 |          0 | 0.062752873305 |         1731 | 1221.36305753 | 4820.43342514 |          0 |            0 |
|     2 | CSN001 |      0.01 |           1 |          0 | 1.063178328565 |         3364 | 1221.36305753 | 4820.43342514 |          0 |            0 |
|     3 | CSN001 |      0.01 |           1 |          0 | 2.065452163738 |         1208 | 1221.36305753 | 4820.43342514 |          0 |            0 |
|     4 | CSN001 |      0.01 |           1 |          0 | 3.064809437917 |         1887 | 1221.36305753 | 4820.43342514 |          0 |            0 |
|     5 | CSN001 |      0.01 |           1 |          0 | 4.065682470532 |           54 | 1221.36305753 | 4820.43342514 |          0 |            0 |
|     6 | CSN001 |      0.01 |           1 |          0 | 5.069321554899 |         1593 | 1221.36305753 | 4820.43342514 |          0 |            0 |

Check count of participants by unique id.

``` r
length(unique(combined_df$id))
```

    ## [1] 50

## Analyze all participants’ hits with reaction times

Read targets object `all_hits_with_reaction_times` and assign to the
combined hits dataframe `combined_hits_df`.

``` r
withr::with_dir(here::here(), {
  combined_hits_df <- tar_read(hits_given_signals)
})
```

Check out a quick preview of the table of hits

``` r
knitr::kable(head(combined_hits_df))
```

| trial | id     | image\_index |   signal\_time |     hit\_time | reaction\_time | is\_hit\_given\_signal |
| ----: | :----- | -----------: | -------------: | ------------: | -------------: | ---------------------: |
|    81 | CSN001 |          801 |  80.0604399294 |            NA |             NA |                      0 |
|   117 | CSN001 |          941 | 116.0665163944 | 116.984091845 | 0.917575450165 |                      1 |
|   119 | CSN001 |         3131 | 118.0679634164 |            NA |             NA |                      0 |
|   211 | CSN001 |         1325 | 210.0571848214 |            NA |             NA |                      0 |
|   235 | CSN001 |          752 | 234.0716901990 |            NA |             NA |                      0 |
|   361 | CSN001 |          103 | 360.0647047530 |            NA |             NA |                      0 |

Check out the reaction time summary statistics by id:

``` r
combined_hits_df %>%
    drop_na() %>%
    group_by(id) %>%
    summarise(
      reaction_time_mean = mean(reaction_time),
      reaction_time_min = min(reaction_time),
      reaction_time_max = max(reaction_time),
      reaction_time_sd = sd(reaction_time)
    ) %>%
  head() %>%
  knitr::kable()
```

| id     | reaction\_time\_mean | reaction\_time\_min | reaction\_time\_max | reaction\_time\_sd |
| :----- | -------------------: | ------------------: | ------------------: | -----------------: |
| CSN001 |        1.71981596174 |      0.030177300678 |       4.73494848317 |     1.185127808038 |
| CSN002 |        1.99359213268 |      0.439399655617 |       7.77171126677 |     1.727003771770 |
| CSN004 |        1.70646593409 |      0.996316162899 |       2.84090208776 |     0.543133623999 |
| CSN005 |        3.06080474118 |      1.533621048930 |       7.64323121076 |     2.120246304387 |
| CSN006 |        1.47237477858 |      0.093593949678 |       7.99434786988 |     1.763412125625 |
| CSN007 |        1.79454994735 |      0.759996981775 |       3.99019392886 |     0.965011326176 |

Check plot of all reaction times by signal times. (Note the 100ms gaps
where responses were not recorded at 1-1.1, 2-2.1, …)

``` r
combined_hits_df %>%
  drop_na() %>%
  ggplot(aes(x = signal_time, y = reaction_time)) +
    geom_point(color = 'orange') +
    geom_smooth(method=lm) +
    theme_classic()
```

![](/Users/metis/Projects/sokolhessnerlab/itrackvalr/output/behavioral_data_preprocessing_files/figure-gfm/all-reaction-times-by-signal-times-plot-1.png)<!-- -->

## Reaction times per participant centered at the median

``` r
combined_hits_df %>%
  drop_na() %>%
  arrange(reaction_time) %>%
  ggplot(aes(x = reorder(id, reaction_time, FUN = median), y = reaction_time)) +
    geom_point(alpha = 0.8, size = 0.7, color = 'darkblue', position = 'jitter') +
    geom_boxplot(alpha = 0) +
    geom_hline(yintercept = 1, color = 'coral') +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 15),
    plot.margin = margin(18, 18, 18, 18, 'pt')
  ) +
  labs(
    title = 'Reaction times of participants by id',
    subtitle = 'Boxplot per participant anchored at median of reaction time',
    y = 'Reaction time for HIT after double-tick signal',
    x = 'Participant'
  ) +
  coord_flip()
```

![](/Users/metis/Projects/sokolhessnerlab/itrackvalr/output/behavioral_data_preprocessing_files/figure-gfm/boxplot-reaction-times-1.png)<!-- -->

## Read false alarms target

``` r
withr::with_dir(here::here(), {
  false_alarms_df <- tar_read(false_alarms_given_responses)
})
```

``` r
false_alarms_df %>%
  head() %>%
  knitr::kable()
```

| trial | id     | image\_index |    resp\_time | is\_false\_alarm\_given\_response |
| ----: | :----- | -----------: | ------------: | --------------------------------: |
|   117 | CSN001 |          941 | 116.984091845 |                                 0 |
|   357 | CSN001 |         3505 | 356.617249860 |                                 1 |
|   426 | CSN001 |         2139 | 425.957022303 |                                 1 |
|   523 | CSN001 |         2880 | 522.654992917 |                                 1 |
|   739 | CSN001 |         2932 | 738.145843456 |                                 1 |
|   823 | CSN001 |          929 | 822.772524834 |                                 0 |
