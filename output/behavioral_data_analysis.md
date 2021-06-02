Behavioral Data Analysis
================
Ari Dyckovsky

  - [Read extracted behavioral data](#read-extracted-behavioral-data)
  - [Models](#models)
      - [Predict the probability of a hit by signal
        time](#predict-the-probability-of-a-hit-by-signal-time)
      - [Predict the probability of a hit by signal time (with random
        effects on signal
        time)](#predict-the-probability-of-a-hit-by-signal-time-with-random-effects-on-signal-time)
      - [Predict reaction time using signal
        time](#predict-reaction-time-using-signal-time)
      - [Predict reaction time using signal time (with random effects on
        signal
        time)](#predict-reaction-time-using-signal-time-with-random-effects-on-signal-time)
      - [Predict the probability of a false alarm by response
        time](#predict-the-probability-of-a-false-alarm-by-response-time)
      - [Predict the probability of a false alarm by response time (with
        random effects on response
        time)](#predict-the-probability-of-a-false-alarm-by-response-time-with-random-effects-on-response-time)

## Read extracted behavioral data

Use `tar_read` to get the target object `combined_behavioral_data` and
assign it to `combined_df`.

``` r
withr::with_dir(here::here(), {
  combined_df <- tar_read(combined_behavioral_data)
})
```

## Models

We read in and summarize models already evaluated from the
`analyze_behavioral_data` target list in `_targets.R`.

### Predict the probability of a hit by signal time

``` r
withr::with_dir(here::here(), {
  broom.mixed::tidy(tar_read(model_hit_by_signal_time))
})
```

    ## # A tibble: 3 x 7
    ##   effect   group term            estimate std.error statistic     p.value
    ##   <chr>    <chr> <chr>              <dbl>     <dbl>     <dbl>       <dbl>
    ## 1 fixed    <NA>  (Intercept)        0.231     0.157      1.47  0.142     
    ## 2 fixed    <NA>  signal_time       -0.860     0.182     -4.72  0.00000241
    ## 3 ran_pars id    sd__(Intercept)    0.836    NA         NA    NA

``` r
withr::with_dir(here::here(), {
  ggpredict(tar_read(model_hit_by_signal_time), 'signal_time') %>%
    plot()
})
```

![](/Users/metis/Projects/sokolhessnerlab/itrackvalr/output/behavioral_data_analysis_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Predict the probability of a hit by signal time (with random effects on signal time)

``` r
withr::with_dir(here::here(), {
  broom.mixed::tidy(tar_read(model_hit_by_signal_time_rfx))
})
```

    ## # A tibble: 5 x 7
    ##   effect   group term                         estimate std.error statistic     p.value
    ##   <chr>    <chr> <chr>                           <dbl>     <dbl>     <dbl>       <dbl>
    ## 1 fixed    <NA>  (Intercept)                     0.232     0.136      1.71  0.0881    
    ## 2 fixed    <NA>  signal_time                    -0.890     0.197     -4.52  0.00000607
    ## 3 ran_pars id    sd__(Intercept)                 0.633    NA         NA    NA         
    ## 4 ran_pars id    cor__(Intercept).signal_time    0.965    NA         NA    NA         
    ## 5 ran_pars id    sd__signal_time                 0.460    NA         NA    NA

``` r
withr::with_dir(here::here(), {
  ggpredict(tar_read(model_hit_by_signal_time_rfx), 'signal_time') %>%
    plot()
})
```

![](/Users/metis/Projects/sokolhessnerlab/itrackvalr/output/behavioral_data_analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Predict reaction time using signal time

``` r
withr::with_dir(here::here(), {
  broom.mixed::tidy(tar_read(model_reaction_time_by_signal_time))
})
```

    ## # A tibble: 4 x 8
    ##   effect   group    term            estimate std.error statistic    df   p.value
    ##   <chr>    <chr>    <chr>              <dbl>     <dbl>     <dbl> <dbl>     <dbl>
    ## 1 fixed    <NA>     (Intercept)        1.42      0.100     14.2   106.  2.92e-26
    ## 2 fixed    <NA>     signal_time        0.774     0.129      6.02  793.  2.61e- 9
    ## 3 ran_pars id       sd__(Intercept)    0.503    NA         NA      NA  NA       
    ## 4 ran_pars Residual sd__Observation    1.03     NA         NA      NA  NA

``` r
withr::with_dir(here::here(), {
  ggpredict(tar_read(model_reaction_time_by_signal_time), 'signal_time') %>%
    plot()
})
```

![](/Users/metis/Projects/sokolhessnerlab/itrackvalr/output/behavioral_data_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Predict reaction time using signal time (with random effects on signal time)

``` r
withr::with_dir(here::here(), {
  broom.mixed::tidy(tar_read(model_reaction_time_by_signal_time_rfx))
})
```

    ## # A tibble: 6 x 8
    ##   effect   group    term                      estimate std.error statistic    df   p.value
    ##   <chr>    <chr>    <chr>                        <dbl>     <dbl>     <dbl> <dbl>     <dbl>
    ## 1 fixed    <NA>     (Intercept)                  1.39     0.0876     15.9   45.3  4.13e-20
    ## 2 fixed    <NA>     signal_time                  0.877    0.156       5.63  26.0  6.35e- 6
    ## 3 ran_pars id       sd__(Intercept)              0.374   NA          NA     NA   NA       
    ## 4 ran_pars id       cor__(Intercept).signal_…    0.313   NA          NA     NA   NA       
    ## 5 ran_pars id       sd__signal_time              0.586   NA          NA     NA   NA       
    ## 6 ran_pars Residual sd__Observation              1.02    NA          NA     NA   NA

``` r
withr::with_dir(here::here(), {
  ggpredict(tar_read(model_reaction_time_by_signal_time_rfx), 'signal_time') %>%
    plot()
})
```

![](/Users/metis/Projects/sokolhessnerlab/itrackvalr/output/behavioral_data_analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Predict the probability of a false alarm by response time

``` r
withr::with_dir(here::here(), {
  broom.mixed::tidy(tar_read(model_false_alarm_by_response_time))
})
```

    ## # A tibble: 3 x 7
    ##   effect   group term            estimate std.error statistic   p.value
    ##   <chr>    <chr> <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 fixed    <NA>  (Intercept)        1.18      0.183      6.48  9.02e-11
    ## 2 fixed    <NA>  resp_time          1.40      0.246      5.71  1.15e- 8
    ## 3 ran_pars id    sd__(Intercept)    0.936    NA         NA    NA

``` r
withr::with_dir(here::here(), {
  ggpredict(tar_read(model_false_alarm_by_response_time), 'resp_time') %>%
    plot()
})
```

![](/Users/metis/Projects/sokolhessnerlab/itrackvalr/output/behavioral_data_analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Predict the probability of a false alarm by response time (with random effects on response time)

``` r
withr::with_dir(here::here(), {
  broom.mixed::tidy(tar_read(model_false_alarm_by_response_time_rfx))
})
```

    ## # A tibble: 5 x 7
    ##   effect   group term                       estimate std.error statistic      p.value
    ##   <chr>    <chr> <chr>                         <dbl>     <dbl>     <dbl>        <dbl>
    ## 1 fixed    <NA>  (Intercept)                   0.989     0.195      5.07  0.000000402
    ## 2 fixed    <NA>  resp_time                     2.31      0.498      4.64  0.00000351 
    ## 3 ran_pars id    sd__(Intercept)               0.994    NA         NA    NA          
    ## 4 ran_pars id    cor__(Intercept).resp_time   -0.436    NA         NA    NA          
    ## 5 ran_pars id    sd__resp_time                 2.35     NA         NA    NA

``` r
withr::with_dir(here::here(), {
  ggpredict(tar_read(model_false_alarm_by_response_time_rfx), 'resp_time') %>%
    plot()
})
```

![](/Users/metis/Projects/sokolhessnerlab/itrackvalr/output/behavioral_data_analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
