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
  - [Scale times to `[0,1]` interval for
    modeling](#scale-times-to-01-interval-for-modeling)
  - [Models](#models)
      - [Predict the probability of a hit by signal
        time](#predict-the-probability-of-a-hit-by-signal-time)
      - [Predict the probability of a hit by signal time with random
        effects](#predict-the-probability-of-a-hit-by-signal-time-with-random-effects)
      - [Predict reaction time using signal
        time](#predict-reaction-time-using-signal-time)
      - [Predict reaction time using signal time with random
        effects](#predict-reaction-time-using-signal-time-with-random-effects)

## Read extracted behavioral data

Use `tar_read` to get the target object
`extracted_behavioral_data_combined` and assign it to `combined_df`.

``` r
withr::with_dir(here::here(), {
  combined_df <- tar_read(extracted_behavioral_data_combined)
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
  combined_hits_df <- tar_read(all_hits_with_reaction_times)
})
```

Check out a quick preview of the table of hits

``` r
knitr::kable(head(combined_hits_df))
```

| trial | id     | image\_index |   signal\_time |     hit\_time | reaction\_time | is\_hit |
| ----: | :----- | -----------: | -------------: | ------------: | -------------: | ------: |
|    81 | CSN001 |          801 |  80.0604399294 |            NA |             NA |       0 |
|   117 | CSN001 |          941 | 116.0665163944 | 116.984091845 | 0.917575450165 |       1 |
|   119 | CSN001 |         3131 | 118.0679634164 |            NA |             NA |       0 |
|   211 | CSN001 |         1325 | 210.0571848214 |            NA |             NA |       0 |
|   235 | CSN001 |          752 | 234.0716901990 |            NA |             NA |       0 |
|   361 | CSN001 |          103 | 360.0647047530 |            NA |             NA |       0 |

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

## Scale times to `[0,1]` interval for modeling

``` r
# TODO: migrate to function in `R/`
scaled_combined_hits_df <- combined_hits_df %>%
  mutate(
    signal_time = signal_time / 3600,
    reaction_time = reaction_time
  )
```

## Models

### Predict the probability of a hit by signal time

``` r
model_pHit_signal_time <- hit_by_signal_time_model(scaled_combined_hits_df)

summary(model_pHit_signal_time)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod
    ## ]
    ##  Family: binomial  ( logit )
    ## Formula: is_hit ~ 1 + signal_time + (1 | id)
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
    ## Formula: is_hit ~ 1 + signal_time + (1 + signal_time | id)
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
