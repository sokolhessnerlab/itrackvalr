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
summary(tar_read(model_hit_by_signal_time))
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod
    ## ]
    ##  Family: binomial  ( logit )
    ## Formula: is_hit_given_signal ~ 1 + signal_time + (1 | id)
    ##    Data: scaled_hits_given_signals
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

### Predict the probability of a hit by signal time (with random effects on signal time)

``` r
summary(tar_read(model_hit_by_signal_time_rfx))
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod
    ## ]
    ##  Family: binomial  ( logit )
    ## Formula: is_hit_given_signal ~ 1 + signal_time + (1 + signal_time | id)
    ##    Data: scaled_hits_given_signals
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
summary(tar_read(model_reaction_time_by_signal_time))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
    ## Formula: reaction_time ~ 1 + signal_time + (1 | id)
    ##    Data: scaled_hits_given_signals %>% na.omit()
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

### Predict reaction time using signal time (with random effects on signal time)

``` r
summary(tar_read(model_reaction_time_by_signal_time_rfx))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
    ## Formula: reaction_time ~ 1 + signal_time + (1 + signal_time | id)
    ##    Data: scaled_hits_given_signals %>% na.omit()
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
summary(tar_read(model_false_alarm_by_response_time))
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod
    ## ]
    ##  Family: binomial  ( logit )
    ## Formula: is_false_alarm_given_response ~ 1 + resp_time + (1 | id)
    ##    Data: scaled_false_alarms_given_responses
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

### Predict the probability of a false alarm by response time (with random effects on response time)

``` r
summary(tar_read(model_false_alarm_by_response_time_rfx))
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod
    ## ]
    ##  Family: binomial  ( logit )
    ## Formula: is_false_alarm_given_response ~ 1 + resp_time + (1 + resp_time |      id)
    ##    Data: scaled_false_alarms_given_responses
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
