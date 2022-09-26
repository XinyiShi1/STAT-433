STAT433 HW1
================
Xinyi Shi
2022-09-26

``` r
library(dplyr)
```

    ## 
    ## 载入程辑包：'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(nycflights13)
```

    ## Warning: 程辑包'nycflights13'是用R版本4.2.1 来建造的

``` r
library(ggplot2)
```

How many flights have a missing dep_time? What other variables are
missing? What might these rows represent?

``` r
flights %>% filter(is.na(dep_time)) %>% summarise(nn=n())
```

    ## # A tibble: 1 × 1
    ##      nn
    ##   <int>
    ## 1  8255

Currently dep_time and sched_dep_time are convenient to look at, but
hard to compute with because they’re not really continuous numbers.
Convert them to a more convenient representation of number of minutes
since midnight.

``` r
flights %>% 
  mutate(new_dep_time=dep_time%%100+dep_time%/%100*60,
                   new_sched_dep_time=sched_dep_time%%100+sched_dep_time%/%100*60) %>% 
  summarise(new_dep_time,
            new_sched_dep_time)
```

    ## # A tibble: 336,776 × 2
    ##    new_dep_time new_sched_dep_time
    ##           <dbl>              <dbl>
    ##  1          317                315
    ##  2          333                329
    ##  3          342                340
    ##  4          344                345
    ##  5          354                360
    ##  6          354                358
    ##  7          355                360
    ##  8          357                360
    ##  9          357                360
    ## 10          358                360
    ## # … with 336,766 more rows

Look at the number of canceled flights per day. Is there a pattern? Is
the proportion of canceled flights related to the average delay? Use
multiple dyplr operations, all on one line, concluding with
ggplot(aes(x= ,y=)) + geom_point()

``` r
flights %>%
  mutate(date = lubridate::make_datetime(year, month, day)) %>%
  group_by(date) %>%
  summarise(cancelled = sum(is.na(dep_delay)), 
            n = n(),
            mean_dep_delay = mean(dep_delay,na.rm=TRUE)) %>%
    ggplot(aes(x= cancelled/n)) + 
    geom_point(aes(y=mean_dep_delay), colour='blue', alpha=0.5) + 
    ylab('mean delay (minutes)')
```

![](nice-Readme_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

There is no strong evidence that there is some relationship. But
generally the higher cancelled/n is, the higher the mean delay is.

\`\`\`
