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

    ## # A tibble: 1,800 x 4
    ##    id     trial response_window_min response_window_max
    ##    <chr>  <dbl>               <dbl>               <dbl>
    ##  1 CSN001    81                80.1                88.1
    ##  2 CSN001   117               116.                124. 
    ##  3 CSN001   119               118.                126. 
    ##  4 CSN001   211               210.                218. 
    ##  5 CSN001   235               234.                242. 
    ##  6 CSN001   361               360.                368. 
    ##  7 CSN001   461               460.                468. 
    ##  8 CSN001   591               590.                598. 
    ##  9 CSN001   823               822.                830. 
    ## 10 CSN001   845               844.                852. 
    ## # … with 1,790 more rows

    ## # A tibble: 1,865 x 3
    ##    id     trial resp_time
    ##    <chr>  <dbl>     <dbl>
    ##  1 CSN001   117      117.
    ##  2 CSN001   357      357.
    ##  3 CSN001   426      426.
    ##  4 CSN001   523      523.
    ##  5 CSN001   739      738.
    ##  6 CSN001   823      823.
    ##  7 CSN001   846      846.
    ##  8 CSN001  1029     1029.
    ##  9 CSN001  1225     1225.
    ## 10 CSN001  1630     1630.
    ## # … with 1,855 more rows
