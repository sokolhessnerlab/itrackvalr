Behavioral Data Preprocessing
================
Ari Dyckovsky

Construct a list of the extracted behavioral CSV file targets by
participant.

Read CSVs into dataframe `combined_df` after using `tar_read_raw` to get
the file path for participants from targets. Will include *all*
participants and include an `id` column to identify the participant
individually.

Count of participants by unique id.

    ## [1] 50

    ## # A tibble: 1 x 4
    ##   `mean(n)` `sd(n)` `min(n)` `max(n)`
    ##       <dbl>   <dbl>    <int>    <int>
    ## 1      37.3    32.9        4      170

The function to get each participant’s hit time:

The function to get a dataframe of combined hits, including the hit time
itself, and the reaction time between that hit time and the signal
prompting that response.

Get the combined hits using the function

Check out a quick preview of the table of hits

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
