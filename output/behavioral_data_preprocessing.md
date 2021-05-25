Behavioral Data Analysis
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
  - [Models](#models)
      - [Scale times to `[0,1]` interval for
        modeling](#scale-times-to-01-interval-for-modeling)
      - [Predict the probability of a hit by signal
        time](#predict-the-probability-of-a-hit-by-signal-time)
      - [Predict the probability of a hit by signal time with random
        effects](#predict-the-probability-of-a-hit-by-signal-time-with-random-effects)
      - [Predict reaction time using signal
        time](#predict-reaction-time-using-signal-time)
      - [Predict reaction time using signal time with random
        effects](#predict-reaction-time-using-signal-time-with-random-effects)
      - [Predict the probability of a false alarm by response
        time](#predict-the-probability-of-a-false-alarm-by-response-time)
      - [Predict the probability of a false alarm by response time with
        response time random
        effects](#predict-the-probability-of-a-false-alarm-by-response-time-with-response-time-random-effects)
  - [Playground (in progress)](#playground-in-progress)

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

## Models

### Scale times to `[0,1]` interval for modeling

``` r
# TODO: migrate to functions in `R/`
scaled_combined_hits_df <- combined_hits_df %>%
  mutate(
    signal_time = signal_time / 3600
  )

scaled_false_alarms_df <- false_alarms_df %>%
  mutate(
    resp_time = resp_time / 3600
  )
```

### Predict the probability of a hit by signal time

``` r
model_pHit_signal_time <- hit_by_signal_time_model(scaled_combined_hits_df)

summary(model_pHit_signal_time)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod
    ## ]
    ##  Family: binomial  ( logit )
    ## Formula: is_hit_given_signal ~ 1 + signal_time + (1 | id)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2307.5   2323.9  -1150.7   2301.5     1797 
    ## 
    ## Scaled residuals: 
    ##          Min           1Q       Median           3Q          Max 
    ## -2.494411658 -0.784968172 -0.477289551  0.902517615  2.366707656 
    ## 
    ## Random effects:
    ##  Groups Name        Variance    Std.Dev.   
    ##  id     (Intercept) 0.698673966 0.835867194
    ## Number of obs: 1800, groups:  id, 50
    ## 
    ## Fixed effects:
    ##                 Estimate   Std. Error  z value   Pr(>|z|)    
    ## (Intercept)  0.230697540  0.157237994  1.46719    0.14233    
    ## signal_time -0.859634004  0.182285149 -4.71588 2.4067e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## signal_time -0.572

### Predict the probability of a hit by signal time with random effects

``` r
model_pHit_signal_time_rfx <- hit_by_signal_time_model(
  scaled_combined_hits_df, 
  random_effects = TRUE
)

summary(model_pHit_signal_time_rfx)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod
    ## ]
    ##  Family: binomial  ( logit )
    ## Formula: is_hit_given_signal ~ 1 + signal_time + (1 + signal_time | id)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2307.8   2335.3  -1148.9   2297.8     1795 
    ## 
    ## Scaled residuals: 
    ##          Min           1Q       Median           3Q          Max 
    ## -2.195323191 -0.801301603 -0.445534191  0.878428765  2.558663556 
    ## 
    ## Random effects:
    ##  Groups Name        Variance    Std.Dev.    Corr     
    ##  id     (Intercept) 0.400867816 0.633141229          
    ##         signal_time 0.211291736 0.459664808 0.9652377
    ## Number of obs: 1800, groups:  id, 50
    ## 
    ## Fixed effects:
    ##                 Estimate   Std. Error  z value   Pr(>|z|)    
    ## (Intercept)  0.232131157  0.136127360  1.70525   0.088148 .  
    ## signal_time -0.889540392  0.196626226 -4.52402 6.0677e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## signal_time -0.401

### Predict reaction time using signal time

``` r
model_RT_signal_time = reaction_time_by_signal_time_model(
  scaled_combined_hits_df
)

summary(model_RT_signal_time)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
    ## Formula: reaction_time ~ 1 + signal_time + (1 | id)
    ##    Data: df %>% na.omit()
    ## 
    ## REML criterion at convergence: 2463
    ## 
    ## Scaled residuals: 
    ##          Min           1Q       Median           3Q          Max 
    ## -1.856746002 -0.522942508 -0.203867491  0.199189511  5.955977930 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance    Std.Dev.   
    ##  id       (Intercept) 0.253031509 0.503022374
    ##  Residual             1.067215435 1.033061196
    ## Number of obs: 821, groups:  id, 50
    ## 
    ## Fixed effects:
    ##                  Estimate    Std. Error            df  t value   Pr(>|t|)    
    ## (Intercept)   1.424493424   0.100223741 105.550683060 14.21313 < 2.22e-16 ***
    ## signal_time   0.774275825   0.128541525 793.026666766  6.02355 2.6097e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## signal_time -0.585

### Predict reaction time using signal time with random effects

``` r
model_RT_signal_time_rfx = reaction_time_by_signal_time_model(
  scaled_combined_hits_df,
  random_effects = TRUE
)

summary(model_RT_signal_time_rfx)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
    ## Formula: reaction_time ~ 1 + signal_time + (1 + signal_time | id)
    ##    Data: df %>% na.omit()
    ## 
    ## REML criterion at convergence: 2456.1
    ## 
    ## Scaled residuals: 
    ##          Min           1Q       Median           3Q          Max 
    ## -2.131344659 -0.512847902 -0.208950132  0.204067753  6.113072894 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance    Std.Dev.    Corr     
    ##  id       (Intercept) 0.139706110 0.373772805          
    ##           signal_time 0.343581718 0.586158441 0.3126252
    ##  Residual             1.040523614 1.020060594          
    ## Number of obs: 821, groups:  id, 50
    ## 
    ## Fixed effects:
    ##                  Estimate    Std. Error            df  t value   Pr(>|t|)    
    ## (Intercept)  1.3891504991  0.0876039787 45.3029022611 15.85716 < 2.22e-16 ***
    ## signal_time  0.8771143534  0.1556609552 25.9948102193  5.63477 6.3527e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## signal_time -0.455

### Predict the probability of a false alarm by response time

``` r
model_FA_resp_time = false_alarm_by_response_time_model(
  scaled_false_alarms_df,
  random_effects = FALSE
)

summary(model_FA_resp_time)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod
    ## ]
    ##  Family: binomial  ( logit )
    ## Formula: is_false_alarm_given_response ~ 1 + resp_time + (1 | id)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1521.7   1538.3   -757.9   1515.7     1862 
    ## 
    ## Scaled residuals: 
    ##          Min           1Q       Median           3Q          Max 
    ## -6.573183916  0.212407553  0.316268283  0.447433551  1.312333996 
    ## 
    ## Random effects:
    ##  Groups Name        Variance    Std.Dev.   
    ##  id     (Intercept) 0.875903618 0.935897226
    ## Number of obs: 1865, groups:  id, 50
    ## 
    ## Fixed effects:
    ##                Estimate  Std. Error z value   Pr(>|z|)    
    ## (Intercept) 1.183808777 0.182616905 6.48247 9.0232e-11 ***
    ## resp_time   1.403288005 0.245881585 5.70717 1.1487e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr)
    ## resp_time -0.490

### Predict the probability of a false alarm by response time with response time random effects

``` r
model_FA_resp_time_rfx = false_alarm_by_response_time_model(
  scaled_false_alarms_df,
  random_effects = TRUE
)

summary(model_FA_resp_time_rfx)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod
    ## ]
    ##  Family: binomial  ( logit )
    ## Formula: is_false_alarm_given_response ~ 1 + resp_time + (1 + resp_time |      id)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1487.1   1514.8   -738.5   1477.1     1860 
    ## 
    ## Scaled residuals: 
    ##          Min           1Q       Median           3Q          Max 
    ## -7.530117688  0.156638041  0.281783623  0.445188571  1.277232888 
    ## 
    ## Random effects:
    ##  Groups Name        Variance    Std.Dev.    Corr      
    ##  id     (Intercept) 0.987925834 0.993944583           
    ##         resp_time   5.541295874 2.353995725 -0.4357819
    ## Number of obs: 1865, groups:  id, 50
    ## 
    ## Fixed effects:
    ##                Estimate  Std. Error z value   Pr(>|z|)    
    ## (Intercept) 0.988863741 0.195119106 5.06800 4.0202e-07 ***
    ## resp_time   2.309811864 0.497993718 4.63823 3.5140e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr)
    ## resp_time -0.578

``` r
scaled_false_alarms_df$pred <- predict(model_FA_resp_time_rfx, type = "response")
ggplot(scaled_false_alarms_df, aes(x = resp_time)) +
  geom_line(aes(x = resp_time, y = pred))
```

![](/Users/metis/Projects/sokolhessnerlab/itrackvalr/output/behavioral_data_preprocessing_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
scaled_false_alarms_df$pred <- predict(model_FA_resp_time_rfx, type = "response")
scaled_false_alarms_df
```

    ## # A tibble: 1,865 x 6
    ##    trial id     image_index resp_time is_false_alarm_given_response  pred
    ##    <int> <chr>        <dbl>     <dbl>                         <int> <dbl>
    ##  1   117 CSN001         941    0.0325                             0 0.632
    ##  2   357 CSN001        3505    0.0991                             1 0.650
    ##  3   426 CSN001        2139    0.118                              1 0.655
    ##  4   523 CSN001        2880    0.145                              1 0.662
    ##  5   739 CSN001        2932    0.205                              1 0.678
    ##  6   823 CSN001         929    0.229                              0 0.684
    ##  7   846 CSN001        2799    0.235                              1 0.685
    ##  8  1029 CSN001         551    0.286                              0 0.698
    ##  9  1225 CSN001         436    0.340                              1 0.711
    ## 10  1630 CSN001        2639    0.453                              0 0.738
    ## # … with 1,855 more rows

## Playground (in progress)

``` r
combined_hits_df %>%
  mutate(first_half_of_task = as.integer(signal_time < 1800))
```

    ## # A tibble: 1,800 x 8
    ##    trial id     image_index signal_time hit_time reaction_time is_hit_given_signal
    ##    <int> <chr>        <dbl>       <dbl>    <dbl>         <dbl>               <int>
    ##  1    81 CSN001         801        80.1      NA         NA                       0
    ##  2   117 CSN001         941       116.      117.         0.918                   1
    ##  3   119 CSN001        3131       118.       NA         NA                       0
    ##  4   211 CSN001        1325       210.       NA         NA                       0
    ##  5   235 CSN001         752       234.       NA         NA                       0
    ##  6   361 CSN001         103       360.       NA         NA                       0
    ##  7   461 CSN001        2804       460.       NA         NA                       0
    ##  8   591 CSN001          28       590.       NA         NA                       0
    ##  9   823 CSN001         929       822.      823.         0.710                   1
    ## 10   845 CSN001         517       844.      846.         1.86                    1
    ## # … with 1,790 more rows, and 1 more variable: first_half_of_task <int>
