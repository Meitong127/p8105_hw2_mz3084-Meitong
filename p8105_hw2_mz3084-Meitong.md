p8105_hw2_mz3084 Meitong Zhou
================
Meitong Zhou
2024-09-26

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](p8105_hw2_mz3084-Meitong_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(readr)
```

## Problem 1

Import the data frame

``` r
nyc_transit_df= read_csv("Meitong's data/NYC_Transit.csv", na = c("NA", "", "."))
```

    ## Rows: 1868 Columns: 32
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (22): Division, Line, Station Name, Route1, Route2, Route3, Route4, Rout...
    ## dbl  (8): Station Latitude, Station Longitude, Route8, Route9, Route10, Rout...
    ## lgl  (2): ADA, Free Crossover
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
nyc_transit_df = janitor::clean_names(nyc_transit_df)
```

Select the variables

``` r
select(nyc_transit_df, line:station_latitude,route1:route11,vending,entrance_type,entry,ada)
```

    ## # A tibble: 1,868 × 18
    ##    line  station_name station_latitude route1 route2 route3 route4 route5 route6
    ##    <chr> <chr>                   <dbl> <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    ##  1 4 Av… 25th St                  40.7 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  2 4 Av… 25th St                  40.7 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  3 4 Av… 36th St                  40.7 N      R      <NA>   <NA>   <NA>   <NA>  
    ##  4 4 Av… 36th St                  40.7 N      R      <NA>   <NA>   <NA>   <NA>  
    ##  5 4 Av… 36th St                  40.7 N      R      <NA>   <NA>   <NA>   <NA>  
    ##  6 4 Av… 45th St                  40.6 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  7 4 Av… 45th St                  40.6 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  8 4 Av… 45th St                  40.6 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  9 4 Av… 45th St                  40.6 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 10 4 Av… 53rd St                  40.6 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## # ℹ 1,858 more rows
    ## # ℹ 9 more variables: route7 <chr>, route8 <dbl>, route9 <dbl>, route10 <dbl>,
    ## #   route11 <dbl>, vending <chr>, entrance_type <chr>, entry <chr>, ada <lgl>

``` r
relocate(nyc_transit_df,line, entry)|>
  mutate(nyc_transit_df, entry = case_when(
    entry == "YES" ~ TRUE,
    entry == "NO" ~ FALSE
))
```

    ## # A tibble: 1,868 × 32
    ##    line    entry division station_name station_latitude station_longitude route1
    ##    <chr>   <lgl> <chr>    <chr>                   <dbl>             <dbl> <chr> 
    ##  1 4 Aven… TRUE  BMT      25th St                  40.7             -74.0 R     
    ##  2 4 Aven… TRUE  BMT      25th St                  40.7             -74.0 R     
    ##  3 4 Aven… TRUE  BMT      36th St                  40.7             -74.0 N     
    ##  4 4 Aven… TRUE  BMT      36th St                  40.7             -74.0 N     
    ##  5 4 Aven… TRUE  BMT      36th St                  40.7             -74.0 N     
    ##  6 4 Aven… TRUE  BMT      45th St                  40.6             -74.0 R     
    ##  7 4 Aven… TRUE  BMT      45th St                  40.6             -74.0 R     
    ##  8 4 Aven… TRUE  BMT      45th St                  40.6             -74.0 R     
    ##  9 4 Aven… TRUE  BMT      45th St                  40.6             -74.0 R     
    ## 10 4 Aven… TRUE  BMT      53rd St                  40.6             -74.0 R     
    ## # ℹ 1,858 more rows
    ## # ℹ 25 more variables: route2 <chr>, route3 <chr>, route4 <chr>, route5 <chr>,
    ## #   route6 <chr>, route7 <chr>, route8 <dbl>, route9 <dbl>, route10 <dbl>,
    ## #   route11 <dbl>, entrance_type <chr>, exit_only <chr>, vending <chr>,
    ## #   staffing <chr>, staff_hours <chr>, ada <lgl>, ada_notes <chr>,
    ## #   free_crossover <lgl>, north_south_street <chr>, east_west_street <chr>,
    ## #   corner <chr>, entrance_latitude <dbl>, entrance_longitude <dbl>, …

``` r
nyc_transit_df
```

    ## # A tibble: 1,868 × 32
    ##    division line   station_name station_latitude station_longitude route1 route2
    ##    <chr>    <chr>  <chr>                   <dbl>             <dbl> <chr>  <chr> 
    ##  1 BMT      4 Ave… 25th St                  40.7             -74.0 R      <NA>  
    ##  2 BMT      4 Ave… 25th St                  40.7             -74.0 R      <NA>  
    ##  3 BMT      4 Ave… 36th St                  40.7             -74.0 N      R     
    ##  4 BMT      4 Ave… 36th St                  40.7             -74.0 N      R     
    ##  5 BMT      4 Ave… 36th St                  40.7             -74.0 N      R     
    ##  6 BMT      4 Ave… 45th St                  40.6             -74.0 R      <NA>  
    ##  7 BMT      4 Ave… 45th St                  40.6             -74.0 R      <NA>  
    ##  8 BMT      4 Ave… 45th St                  40.6             -74.0 R      <NA>  
    ##  9 BMT      4 Ave… 45th St                  40.6             -74.0 R      <NA>  
    ## 10 BMT      4 Ave… 53rd St                  40.6             -74.0 R      <NA>  
    ## # ℹ 1,858 more rows
    ## # ℹ 25 more variables: route3 <chr>, route4 <chr>, route5 <chr>, route6 <chr>,
    ## #   route7 <chr>, route8 <dbl>, route9 <dbl>, route10 <dbl>, route11 <dbl>,
    ## #   entrance_type <chr>, entry <chr>, exit_only <chr>, vending <chr>,
    ## #   staffing <chr>, staff_hours <chr>, ada <lgl>, ada_notes <chr>,
    ## #   free_crossover <lgl>, north_south_street <chr>, east_west_street <chr>,
    ## #   corner <chr>, entrance_latitude <dbl>, entrance_longitude <dbl>, …

The cleaned dataset has 1868 and 32 columns. the variables include line,
station, name, station latitude / longitude, routes served, entry,
vending, entrance type, and ADA compliance. These data are tidy, meaning
each variable is in its column, and each row represents a distinct
subway entrance.

**How many distinct stations are there?**

``` r
distinct_stations_df = distinct(nyc_transit_df,line, station_name)
distinct_stations_df
```

    ## # A tibble: 465 × 2
    ##    line     station_name            
    ##    <chr>    <chr>                   
    ##  1 4 Avenue 25th St                 
    ##  2 4 Avenue 36th St                 
    ##  3 4 Avenue 45th St                 
    ##  4 4 Avenue 53rd St                 
    ##  5 4 Avenue 59th St                 
    ##  6 4 Avenue 77th St                 
    ##  7 4 Avenue 86th St                 
    ##  8 4 Avenue 95th St                 
    ##  9 4 Avenue 9th St                  
    ## 10 4 Avenue Atlantic Av-Barclays Ctr
    ## # ℹ 455 more rows

``` r
distinct_stations_count = nrow(distinct_stations_df)

# Display the count of distinct stations
distinct_stations_count
```

    ## [1] 465

**How many stations are ADA compliant?**

``` r
ada_comp_sta = nyc_transit_df|>
  filter(ada == TRUE) |>
  distinct(line, station_name) |>
  count()
ada_comp_sta
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1    84

**What proportion of station entrances / exits without vending allow
entrance?**

``` r
no_vending_proportion = nyc_transit_df |>
  filter(vending == FALSE) |>
  summarise(proportion = n() / nrow(nyc_transit_df))
no_vending_proportion
```

    ## # A tibble: 1 × 1
    ##   proportion
    ##        <dbl>
    ## 1          0

## Problem 2

``` r
library(readxl)
library(dplyr)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
mr_trash_wheel= read_xlsx("Meitong's data/trash_wheel.xlsx", sheet="Mr. Trash Wheel", skip=1,na = c(".", "NA", ""), col_names = TRUE)
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
mr_trash_wheel= 
    janitor::clean_names(mr_trash_wheel) # clean the data into reasonable var name.
drop_na(mr_trash_wheel) # drop rows that do not include dumpster-specific data
```

    ## # A tibble: 0 × 16
    ## # ℹ 16 variables: dumpster <dbl>, month <chr>, year <chr>, date <dttm>,
    ## #   weight_tons <dbl>, volume_cubic_yards <dbl>, plastic_bottles <dbl>,
    ## #   polystyrene <dbl>, cigarette_butts <dbl>, glass_bottles <dbl>,
    ## #   plastic_bags <dbl>, wrappers <dbl>, sports_balls <dbl>,
    ## #   homes_powered <dbl>, x15 <lgl>, x16 <lgl>

``` r
mr_trash_wheel= mr_trash_wheel|>
  mutate(sport_balls= as.integer(round(sports_balls, 0)))
mr_trash_wheel= mr_trash_wheel|>
  mutate(trash_wheel = "Mr. Trash Wheel")
```

``` r
# upload other two sheets and clean them
prof_trash_wheel = read_xlsx("Meitong's data/trash_wheel.xlsx", sheet = "Professor Trash Wheel", skip = 1)
prof_trash_wheel= 
    janitor::clean_names(prof_trash_wheel)
drop_na(prof_trash_wheel)
```

    ## # A tibble: 113 × 13
    ##    dumpster month     year date                weight_tons volume_cubic_yards
    ##       <dbl> <chr>    <dbl> <dttm>                    <dbl>              <dbl>
    ##  1        1 January   2017 2017-01-02 00:00:00        1.79                 15
    ##  2        2 January   2017 2017-01-30 00:00:00        1.58                 15
    ##  3        3 February  2017 2017-02-26 00:00:00        2.32                 18
    ##  4        4 February  2017 2017-02-26 00:00:00        3.72                 15
    ##  5        5 February  2017 2017-02-28 00:00:00        1.45                 15
    ##  6        6 March     2017 2017-03-30 00:00:00        1.71                 15
    ##  7        7 April     2017 2017-04-01 00:00:00        1.82                 15
    ##  8        8 April     2017 2017-04-20 00:00:00        2.37                 15
    ##  9        9 May       2017 2017-05-10 00:00:00        2.64                 15
    ## 10       10 May       2017 2017-05-26 00:00:00        2.78                 15
    ## # ℹ 103 more rows
    ## # ℹ 7 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, homes_powered <dbl>

``` r
prof_trash_wheel= prof_trash_wheel|>
  mutate(trash_wheel = "Professor Trash Wheel")
gwynnda_trash_wheel = read_xlsx("Meitong's data/trash_wheel.xlsx", sheet = "Gwynnda Trash Wheel", skip = 1)
gwynnda_trash_wheel= 
    janitor::clean_names(gwynnda_trash_wheel)
drop_na(gwynnda_trash_wheel)
```

    ## # A tibble: 103 × 12
    ##    dumpster month     year date                weight_tons volume_cubic_yards
    ##       <dbl> <chr>    <dbl> <dttm>                    <dbl>              <dbl>
    ##  1      117 November  2022 2022-11-03 00:00:00        3.06                 15
    ##  2      118 November  2022 2022-11-15 00:00:00        3                    15
    ##  3      119 November  2022 2022-11-19 00:00:00        2.42                 15
    ##  4      120 November  2022 2022-11-22 00:00:00        2.37                 15
    ##  5      121 November  2022 2022-11-30 00:00:00        2.91                 15
    ##  6      122 December  2022 2022-12-13 00:00:00        2.35                 14
    ##  7      123 December  2022 2022-12-17 00:00:00        2.8                  15
    ##  8      124 December  2022 2022-12-17 00:00:00        2.69                 15
    ##  9      125 December  2022 2022-12-19 00:00:00        2.27                 15
    ## 10      126 December  2022 2022-12-19 00:00:00        2.5                  15
    ## # ℹ 93 more rows
    ## # ℹ 6 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, plastic_bags <dbl>, wrappers <dbl>,
    ## #   homes_powered <dbl>

``` r
gwynnda_trash_wheel= gwynnda_trash_wheel|>
  mutate(trash_wheel = "Gwynnda Trash Wheel")
```

``` r
#change all variable "Year" into numeric factor
mr_trash_wheel = mr_trash_wheel |>
  mutate(year = as.numeric(year))
prof_trash_wheel = prof_trash_wheel |>
  mutate(year = as.numeric(year))
gwynnda_trash_wheel = gwynnda_trash_wheel |>
  mutate(year = as.numeric(year))
# combine 3 data set
all_trash_wheel= bind_rows(mr_trash_wheel, prof_trash_wheel, gwynnda_trash_wheel)
all_trash_wheel
```

    ## # A tibble: 1,038 × 18
    ##    dumpster month  year date                weight_tons volume_cubic_yards
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>              <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31                 18
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74                 13
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45                 15
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1                  15
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06                 18
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71                 13
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91                  8
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7                  16
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52                 14
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76                 18
    ## # ℹ 1,028 more rows
    ## # ℹ 12 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, sports_balls <dbl>, homes_powered <dbl>, x15 <lgl>,
    ## #   x16 <lgl>, sport_balls <int>, trash_wheel <chr>

``` r
nrow(all_trash_wheel) 
```

    ## [1] 1038

``` r
total_sum_prof = prof_trash_wheel |>
  summarise(weight_total = sum(weight_tons, na.rm = TRUE))
total_sum_prof
```

    ## # A tibble: 1 × 1
    ##   weight_total
    ##          <dbl>
    ## 1          488

``` r
total_sum_gwynnda_cig_butts = gwynnda_trash_wheel |>
  summarise(cig_butts_total = sum(cigarette_butts, na.rm = TRUE))
total_sum_gwynnda_cig_butts
```

    ## # A tibble: 1 × 1
    ##   cig_butts_total
    ##             <dbl>
    ## 1         1202580

This dataset combines trash collection information from Mr. Trash Wheel,
Professor Trash Wheel, and Gwynnda Trash Wheel. It is include key
variables such as total weight of garbage in each garbage wheel, plastic
bottles, glass bottles, cigarette butts, etc. After cleaning and merging
the data, the resulting dataset contains 1038 observations. Professor
Trash Wheel collected a total of 488 tons of trash. In June 2022,
Gwynnda Trash Wheel collected 1.20258^{6} cigarette butts.

## Problem 3

``` r
library(readr)
library(dplyr)
```

``` r
# read all of the table ans clean there variables' name
bakes_df = read_csv("gbb_datasets/bakes.csv")
```

    ## Rows: 548 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker, Signature Bake, Show Stopper
    ## dbl (2): Series, Episode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
bakes_df= 
    janitor::clean_names(bakes_df)
bakers_df = read_csv("gbb_datasets/bakers.csv")
```

    ## Rows: 120 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker Name, Baker Occupation, Hometown
    ## dbl (2): Series, Baker Age
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
bakers_df= 
    janitor::clean_names(bakers_df)
bakers_df = bakers_df|>
  rename(baker = baker_name)
results_df = read_csv("gbb_datasets/results.csv", skip = 2)
```

    ## Rows: 1136 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): baker, result
    ## dbl (3): series, episode, technical
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
results_df= 
    janitor::clean_names(results_df)
results_df = select(results_df,series, episode, baker, technical, result)
viewers_df = read_csv("gbb_datasets/viewers.csv")
```

    ## Rows: 10 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (11): Episode, Series 1, Series 2, Series 3, Series 4, Series 5, Series ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
viewers_df= 
    janitor::clean_names(viewers_df)
```

``` r
#check if there is any non-matched variable
anti_join(bakes_df, bakers_df, by = c("series", "baker"))
```

    ## # A tibble: 548 × 5
    ##    series episode baker     signature_bake                          show_stopper
    ##     <dbl>   <dbl> <chr>     <chr>                                   <chr>       
    ##  1      1       1 Annetha   "Light Jamaican Black Cakewith Strawbe… Red, White …
    ##  2      1       1 David     "Chocolate Orange Cake"                 Black Fores…
    ##  3      1       1 Edd       "Caramel Cinnamon and Banana Cake"      N/A         
    ##  4      1       1 Jasminder "Fresh Mango and Passion Fruit Humming… N/A         
    ##  5      1       1 Jonathan  "Carrot Cake with Lime and Cream Chees… Three Tiere…
    ##  6      1       1 Lea       "Cranberry and Pistachio Cakewith Oran… Raspberries…
    ##  7      1       1 Louise    "Carrot and Orange Cake"                Never Fail …
    ##  8      1       1 Mark      "Sticky Marmalade Tea Loaf"             Heart-shape…
    ##  9      1       1 Miranda   "Triple Layered Brownie Meringue Cake\… Three Tiere…
    ## 10      1       1 Ruth      "Three Tiered Lemon Drizzle Cakewith F… Classic Cho…
    ## # ℹ 538 more rows

``` r
anti_join(bakes_df, results_df, by = c("series", "episode", "baker"))
```

    ## # A tibble: 8 × 5
    ##   series episode baker    signature_bake                            show_stopper
    ##    <dbl>   <dbl> <chr>    <chr>                                     <chr>       
    ## 1      2       1 "\"Jo\"" Chocolate Orange CupcakesOrange and Card… Chocolate a…
    ## 2      2       2 "\"Jo\"" Caramelised Onion, Gruyere and Thyme Qui… Raspberry a…
    ## 3      2       3 "\"Jo\"" Stromboli flavored with Mozzarella, Ham,… Unknown     
    ## 4      2       4 "\"Jo\"" Lavender Biscuits                         Blueberry M…
    ## 5      2       5 "\"Jo\"" Salmon and Asparagus Pie                  Apple and R…
    ## 6      2       6 "\"Jo\"" Rum and Raisin Baked Cheesecake           Limoncello …
    ## 7      2       7 "\"Jo\"" Raspberry & Strawberry Mousse Cake        Pain Aux Ra…
    ## 8      2       8 "\"Jo\"" Raspberry and Blueberry Mille Feuille     Mini Victor…

``` r
# combine two tables
merged_data = left_join(bakes_df, bakers_df, by = c("series", "baker"))
final_data = left_join(merged_data, results_df, by = c("series", "episode", "baker"))
# download the new table
write_csv(final_data, "gbb_datasets/final_gbbo_dataset.csv")
```

``` r
# build a new column
final_data = final_data |>
  mutate(star_baker_winner = ifelse(result == "STAR BAKER" | result == "WINNER", TRUE, FALSE))
# get the star baker and winner from series 5 to series 10
season_5_to_10 = final_data |>
  filter(series >= 5 & series <= 10 & star_baker_winner == TRUE)
star_baker_winner = season_5_to_10 |>
  select(series, episode, baker, result)
# download the table
print(star_baker_winner)
```

    ## # A tibble: 40 × 4
    ##    series episode baker   result    
    ##     <dbl>   <dbl> <chr>   <chr>     
    ##  1      5       1 Nancy   STAR BAKER
    ##  2      5       2 Richard STAR BAKER
    ##  3      5       3 Luis    STAR BAKER
    ##  4      5       4 Richard STAR BAKER
    ##  5      5       5 Kate    STAR BAKER
    ##  6      5       6 Chetna  STAR BAKER
    ##  7      5       7 Richard STAR BAKER
    ##  8      5       8 Richard STAR BAKER
    ##  9      5       9 Richard STAR BAKER
    ## 10      5      10 Nancy   WINNER    
    ## # ℹ 30 more rows

``` r
write_csv(star_baker_winner, "gbb_datasets/star_baker_winner_dataset.csv")
```

Surprise: I found that in series 5, Nacy was not the one that got the
most star baker, but still become winner.

``` r
head(viewers_df, 10)
```

    ## # A tibble: 10 × 11
    ##    episode series_1 series_2 series_3 series_4 series_5 series_6 series_7
    ##      <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1       1     2.24     3.1      3.85     6.6      8.51     11.6     13.6
    ##  2       2     3        3.53     4.6      6.65     8.79     11.6     13.4
    ##  3       3     3        3.82     4.53     7.17     9.28     12.0     13.0
    ##  4       4     2.6      3.6      4.71     6.82    10.2      12.4     13.3
    ##  5       5     3.03     3.83     4.61     6.95     9.95     12.4     13.1
    ##  6       6     2.75     4.25     4.82     7.32    10.1      12       13.1
    ##  7       7    NA        4.42     5.1      7.76    10.3      12.4     13.4
    ##  8       8    NA        5.06     5.35     7.41     9.02     11.1     13.3
    ##  9       9    NA       NA        5.7      7.41    10.7      12.6     13.4
    ## 10      10    NA       NA        6.74     9.45    13.5      15.0     15.9
    ## # ℹ 3 more variables: series_8 <dbl>, series_9 <dbl>, series_10 <dbl>

``` r
mean_series_1 <- viewers_df |> 
  pull(series_1) |> 
  mean(na.rm = TRUE)
mean_series_5 <- viewers_df |> 
  pull(series_5) |> 
  mean(na.rm = TRUE)
mean_series_1
```

    ## [1] 2.77

``` r
mean_series_5
```

    ## [1] 10.0393

I read the Excel data sheet for each garbage round using `read_xlsx()`
and skipped unnecessary rows using the skip parameter. In addition, I
used the `janitor::clean_names()` function to convert all column names
to a more readable form (e.g., “Weight (tons)” to `weight_tons`. After
checking for mismatched variables using `anti_join`, I joined the three
tables together using `left_join`. Problems encountered: In the dataset,
some observations may be missing. We used `na.rm = TRUE` to ignore these
missing values when calculating the totals. For the viewers data frame,
the mean value of series 1 is 2.77, and the mean value of series 5 is
10.0393
