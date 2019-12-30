INTRODUCTION
============

The following project is an Exploratory Data Analysis based on a data
set obtained from kaggle, [US
Accidents](https://www.kaggle.com/sobhanmoosavi/us-accidents), which was
intended to be my first kernel in Kaggle. For that reason I decided to
work the code on my personal computer and upload the updates to kaggle
every now and then. I have decided to keep a copy on github in order to
keep all my work of data processing together in one website. However,
the kaggle kernel that I created can be found
[here](https://www.kaggle.com/teoten/us-accidents-eda)

Let’s get started with the required libraries and calling the data:

``` r
library(tidyverse)
library(lubridate)
library(cowplot)

#US_accidents <- read_csv('../input/us-accidents/US_Accidents_May19.csv')
load('eda.RData')
```

Now we can start with a general overview of the data, the `tibble`, and
a quick summary of the numeric variables using the basic R function
`summary`

``` r
## First view
US_accidents
```

    ## # A tibble: 2,243,939 x 49
    ##    ID    Source   TMC Severity Start_Time          End_Time           
    ##    <chr> <chr>  <dbl>    <dbl> <dttm>              <dttm>             
    ##  1 A-1   MapQu…   201        3 2016-02-08 05:46:00 2016-02-08 11:00:00
    ##  2 A-2   MapQu…   201        2 2016-02-08 06:07:59 2016-02-08 06:37:59
    ##  3 A-3   MapQu…   201        2 2016-02-08 06:49:27 2016-02-08 07:19:27
    ##  4 A-4   MapQu…   201        3 2016-02-08 07:23:34 2016-02-08 07:53:34
    ##  5 A-5   MapQu…   201        2 2016-02-08 07:39:07 2016-02-08 08:09:07
    ##  6 A-6   MapQu…   201        3 2016-02-08 07:44:26 2016-02-08 08:14:26
    ##  7 A-7   MapQu…   201        2 2016-02-08 07:59:35 2016-02-08 08:29:35
    ##  8 A-8   MapQu…   201        3 2016-02-08 07:59:58 2016-02-08 08:29:58
    ##  9 A-9   MapQu…   201        2 2016-02-08 08:00:40 2016-02-08 08:30:40
    ## 10 A-10  MapQu…   201        3 2016-02-08 08:10:04 2016-02-08 08:40:04
    ## # … with 2,243,929 more rows, and 43 more variables: Start_Lat <dbl>,
    ## #   Start_Lng <dbl>, End_Lat <lgl>, End_Lng <lgl>, `Distance(mi)` <dbl>,
    ## #   Description <chr>, Number <dbl>, Street <chr>, Side <chr>, City <chr>,
    ## #   County <chr>, State <chr>, Zipcode <chr>, Country <chr>,
    ## #   Timezone <chr>, Airport_Code <chr>, Weather_Timestamp <dttm>,
    ## #   `Temperature(F)` <dbl>, `Wind_Chill(F)` <dbl>, `Humidity(%)` <dbl>,
    ## #   `Pressure(in)` <dbl>, `Visibility(mi)` <dbl>, Wind_Direction <chr>,
    ## #   `Wind_Speed(mph)` <dbl>, `Precipitation(in)` <dbl>,
    ## #   Weather_Condition <chr>, Amenity <lgl>, Bump <lgl>, Crossing <lgl>,
    ## #   Give_Way <lgl>, Junction <lgl>, No_Exit <lgl>, Railway <lgl>,
    ## #   Roundabout <lgl>, Station <lgl>, Stop <lgl>, Traffic_Calming <lgl>,
    ## #   Traffic_Signal <lgl>, Turning_Loop <lgl>, Sunrise_Sunset <chr>,
    ## #   Civil_Twilight <chr>, Nautical_Twilight <chr>,
    ## #   Astronomical_Twilight <chr>

``` r
## Numeric vars
US_accidents %>%
    select(Severity,
           `Distance(mi)`,
           `Temperature(F)`,
           `Wind_Chill(F)`,
           `Humidity(%)`,
           `Pressure(in)`,
           `Visibility(mi)`,
           `Wind_Speed(mph)`,
           `Precipitation(in)`) %>%
    summary()
```

    ##     Severity      Distance(mi)      Temperature(F)   Wind_Chill(F)    
    ##  Min.   :0.000   Min.   :  0.0000   Min.   :-77.80   Min.   :-65.9    
    ##  1st Qu.:2.000   1st Qu.:  0.0000   1st Qu.: 48.90   1st Qu.: 19.2    
    ##  Median :2.000   Median :  0.0000   Median : 63.00   Median : 28.7    
    ##  Mean   :2.383   Mean   :  0.2879   Mean   : 61.23   Mean   : 26.0    
    ##  3rd Qu.:3.000   3rd Qu.:  0.0100   3rd Qu.: 75.90   3rd Qu.: 36.4    
    ##  Max.   :4.000   Max.   :333.6300   Max.   :170.60   Max.   : 45.2    
    ##                                     NA's   :62265    NA's   :1852370  
    ##   Humidity(%)      Pressure(in)   Visibility(mi)   Wind_Speed(mph) 
    ##  Min.   :  4.00   Min.   : 0.00   Min.   :  0.00   Min.   :  1.2   
    ##  1st Qu.: 50.00   1st Qu.:29.92   1st Qu.: 10.00   1st Qu.:  5.8   
    ##  Median : 68.00   Median :30.03   Median : 10.00   Median :  8.1   
    ##  Mean   : 65.93   Mean   :30.04   Mean   :  9.12   Mean   :  8.8   
    ##  3rd Qu.: 85.00   3rd Qu.:30.15   3rd Qu.: 10.00   3rd Qu.: 11.5   
    ##  Max.   :100.00   Max.   :33.04   Max.   :140.00   Max.   :822.8   
    ##  NA's   :64467    NA's   :57280   NA's   :71360    NA's   :442954  
    ##  Precipitation(in)
    ##  Min.   : 0.0     
    ##  1st Qu.: 0.0     
    ##  Median : 0.0     
    ##  Mean   : 0.1     
    ##  3rd Qu.: 0.0     
    ##  Max.   :10.8     
    ##  NA's   :1979466

By default, calling the data as a tibble gives us already valuable
information on the data set. Using `summary` on selected variables
returns a first description of our data, and the first impressions about
the accidents: \* There are extreme temperatures, from -77 to 170 F \*
Wind chill also extreme to -65 \* Maximum wind speed reaches 822 mph \*
Humidity reaches 100 % \* Precipitation, distance affected and
visibility do not seem to be of big importance

We can go now to the Boolean variables:

``` r
## Boolean vars
US_accidents %>%
    select(Amenity:Turning_Loop) %>%
    summary()
```

    ##   Amenity           Bump          Crossing        Give_Way      
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:2217962   FALSE:2243700   FALSE:2122156   FALSE:2239215  
    ##  TRUE :25977     TRUE :239       TRUE :121783    TRUE :4724     
    ##   Junction        No_Exit         Railway        Roundabout     
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:2056574   FALSE:2241773   FALSE:2225741   FALSE:2243811  
    ##  TRUE :187365    TRUE :2166      TRUE :18198     TRUE :128      
    ##   Station           Stop         Traffic_Calming Traffic_Signal 
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:2207714   FALSE:2222168   FALSE:2243321   FALSE:1885291  
    ##  TRUE :36225     TRUE :21771     TRUE :618       TRUE :358648   
    ##  Turning_Loop   
    ##  Mode :logical  
    ##  FALSE:2243939  
    ## 

``` r
## Graphical representation
signals <- US_accidents %>%
    select(Amenity:Turning_Loop) %>%
    pivot_longer( cols = Amenity:Turning_Loop,
        names_to = 'Annotation',
        values_to = 'Trues') %>%
    filter(Trues == TRUE) %>%
    group_by(Annotation) %>%
    summarise(Total = n())

signals %>%
    ggplot() +
    geom_bar(aes(y = Total,
                 x = reorder(Annotation, Total, FUN = abs),
                 fill = Total),
             stat = 'identity') +
    coord_flip() +
    labs(x = NULL) +
    theme(legend.position="none")
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png) We can
also see that most of the accidents happen at traffic signals, then
junctions and crossing.

``` r
## Other variables
US_accidents %>%
    select(ID, Source, TMC, Severity, Description, Amenity, Stop) %>%
    sample_n(size = 50) %>%
    print(n = 50)
```

    ## # A tibble: 50 x 7
    ##    ID      Source   TMC Severity Description                  Amenity Stop 
    ##    <chr>   <chr>  <dbl>    <dbl> <chr>                        <lgl>   <lgl>
    ##  1 A-1928… Bing      NA        3 At I-294/Exit 277B - Accide… FALSE   FALSE
    ##  2 A-2098… Bing      NA        2 At Curtner Ave/Exit 3 - Acc… FALSE   FALSE
    ##  3 A-1678… MapQu…   201        2 Accident on FL-535 at Lake … FALSE   FALSE
    ##  4 A-2135… Bing      NA        2 At CA-60/Pomona Fwy - Accid… FALSE   FALSE
    ##  5 A-1201… MapQu…   201        3 Left hand shoulder blocked … FALSE   FALSE
    ##  6 A-4097… MapQu…   201        2 Right lane blocked due to a… FALSE   FALSE
    ##  7 A-1691… MapQu…   201        2 Accident on Copper Cove Dr … FALSE   FALSE
    ##  8 A-2796… MapQu…   241        3 #3 lane blocked due to acci… FALSE   FALSE
    ##  9 A-1257… MapQu…   201        3 Slow lane blocked due to ac… FALSE   FALSE
    ## 10 A-3562… MapQu…   201        3 Two right lane blocked due … FALSE   FALSE
    ## 11 A-1686… MapQu…   201        2 Accident on TX-1604 Westbou… FALSE   FALSE
    ## 12 A-1933… Bing      NA        4 At W Indian School Rd - Acc… FALSE   FALSE
    ## 13 A-5159… MapQu…   241        2 Lane blocked due to acciden… FALSE   FALSE
    ## 14 A-2172… Bing      NA        4 Ramp closed to CA-91 - Road… FALSE   FALSE
    ## 15 A-1493… MapQu…   201        2 Accident on Garnett Rd at 5… FALSE   FALSE
    ## 16 A-1760… MapQu…   201        2 Accident on IL-38 Roosevelt… FALSE   FALSE
    ## 17 A-3877… MapQu…   201        2 Accident on Amelia Rd both … FALSE   FALSE
    ## 18 A-5574… MapQu…   201        2 Accident on NV-207 Eastboun… FALSE   FALSE
    ## 19 A-8651… MapQu…   201        2 Accident on US-41 8th St Ea… FALSE   FALSE
    ## 20 A-7374… MapQu…   201        3 Left hand shoulder blocked … FALSE   FALSE
    ## 21 A-1152… MapQu…   201        3 Right lane blocked due to a… FALSE   FALSE
    ## 22 A-5511… MapQu…   201        2 Accident on MA-3 Northbound… FALSE   FALSE
    ## 23 A-1255… MapQu…   201        3 Accident on I-75 Southbound… FALSE   FALSE
    ## 24 A-1877… Bing      NA        4 Closed between US-11/Exit 1… FALSE   FALSE
    ## 25 A-1985… Bing      NA        2 At I-77/West Virginia Tpke … FALSE   FALSE
    ## 26 A-5907… MapQu…   201        2 Accident on RI-103 Child St… FALSE   FALSE
    ## 27 A-2100… Bing      NA        3 At E Alameda Ave/Exit 8 - A… FALSE   FALSE
    ## 28 A-1981… Bing      NA        4 Closed between I-39/I-90/Ex… FALSE   TRUE 
    ## 29 A-1991… Bing      NA        4 Closed at Monroe - Road clo… FALSE   FALSE
    ## 30 A-3679… MapQu…   201        2 Accident on Pierson Rd at S… FALSE   FALSE
    ## 31 A-6272… MapQu…   201        2 Accident on Forum Dr at Six… FALSE   FALSE
    ## 32 A-9661… MapQu…   201        2 Right lane blocked due to a… FALSE   FALSE
    ## 33 A-3447… MapQu…   201        3 Accident on I-15 Southbound… FALSE   FALSE
    ## 34 A-1934… Bing      NA        2 At CA-107/Hawthorne Blvd/Ex… FALSE   FALSE
    ## 35 A-2010… Bing      NA        3 At CO-8/Morrison Rd - Accid… FALSE   FALSE
    ## 36 A-7706… MapQu…   236        3 Heavy traffic left hand sho… FALSE   FALSE
    ## 37 A-5043… MapQu…   201        2 Right hand shoulder blocked… FALSE   FALSE
    ## 38 A-80046 MapQu…   201        2 Accident on CA-57 Northboun… FALSE   FALSE
    ## 39 A-8434… MapQu…   201        3 Right lane blocked and righ… FALSE   FALSE
    ## 40 A-1990… Bing      NA        2 Between US-30/Exit 3 and I-… FALSE   FALSE
    ## 41 A-1813… Bing      NA        3 Between Hudson Ter/Palisade… FALSE   FALSE
    ## 42 A-9416… MapQu…   241        3 Middle lane blocked due to … FALSE   FALSE
    ## 43 A-2806… MapQu…   245        3 Two lanes blocked queueing … FALSE   FALSE
    ## 44 A-3471… MapQu…   201        3 Accident on I-5 Southbound … FALSE   FALSE
    ## 45 A-1290… MapQu…   201        2 Accident on Lakewood Ave at… FALSE   FALSE
    ## 46 A-1426… MapQu…   201        2 Left lane blocked due to ac… FALSE   FALSE
    ## 47 A-3513… MapQu…   201        2 Lane blocked on exit ramp d… FALSE   FALSE
    ## 48 A-2271… MapQu…   201        2 Accident on US-33 Columbus … FALSE   FALSE
    ## 49 A-1836… Bing      NA        3 At US-150/Exit 119 - Accide… FALSE   FALSE
    ## 50 A-1297… MapQu…   201        3 Right and Right center lane… FALSE   FALSE

``` r
## API which reported accident
unique(US_accidents$Source)
```

    ## [1] "MapQuest"      "MapQuest-Bing" "Bing"

``` r
## Traffic message channel
unique(US_accidents$TMC)
```

    ##  [1] 201 241 247 246 341 406 245 248 200 244 203 229 222 202 206 343 236
    ## [18] 239 336 339 351  NA

``` r
## Severity: 1 is lesser, 4 is higher
unique(US_accidents$Severity)
```

    ## [1] 3 2 1 4 0

There are 3 sources of the data, 21 traffic message channels and 4
values of severity.

NUMERIC VARIABLES
=================

Now we are going deeper into the numeric variables that could possibly
have certain influence in the occurrence of accidents, due to their
extreme values. Therefore, we start by observing if the extreme values
(such as too cold or too hot weather) are a normal trend for the
accidents to happen, or they are rather atypical values.

``` r
## Temperature
US_accidents %>%
    ggplot() +
    geom_histogram(aes(x = `Temperature(F)`))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 62265 rows containing non-finite values (stat_bin).

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
## Central view on the main trend
US_accidents %>%
    filter(`Temperature(F)` > 0 &
           `Temperature(F)` <= 100) %>%
    ggplot() +
    geom_histogram(aes(x = `Temperature(F)`))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
## More specific view at the main values
US_accidents %>%
    filter(`Temperature(F)` > 45 &
           `Temperature(F)` <= 80) %>%
    ggplot() +
    geom_histogram(aes(x = `Temperature(F)`))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-markdown_github/unnamed-chunk-5-3.png) In the
first plot we can immediately see that below 0 and above 100°F there is
practically nothing. Therefore we create a second plot centred between
this values. Here we can see that most of the accidents happen between
45 and 80°F, with some particular peaks. Thus, we create a third plot
where we can see those peaks in more detail. There is a particular trend
of a considerable increase in the number of accidents marked by a peak,
and then a drop, follow by a slow increase, and then the same trend.
This could be caused most probably by the instruments of measurement in
the meteorological station, to certain extent.

``` r
## Wind chill
US_accidents %>%
    ggplot() +
    geom_histogram(aes(x = `Wind_Chill(F)`))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1852370 rows containing non-finite values (stat_bin).

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
US_accidents %>%
    select(`Wind_Chill(F)`) %>%
    filter(`Wind_Chill(F)` < 30) %>%
    summarise(n())
```

    ## # A tibble: 1 x 1
    ##    `n()`
    ##    <int>
    ## 1 209936

There is an increase in the number of accidents with the increase of the
wind temperature, with the peak at 75°F. The number of accidents at
really cold temperatures (here I chose 30°F) is rather small compared
with the total accidents: 209,936 out of 2,243,939, or 9.35 %

``` r
## Wind speed
US_accidents %>%
    ggplot() +
    geom_histogram(aes(x = `Wind_Speed(mph)`))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 442954 rows containing non-finite values (stat_bin).

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
US_accidents %>%
    select(`Wind_Speed(mph)`) %>%
    filter(`Wind_Speed(mph)` < 25) %>%
    ggplot() +
    geom_histogram(aes(x = `Wind_Speed(mph)`))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-markdown_github/unnamed-chunk-7-2.png) Despite
of the fact that maximum wind speed reaches, we can see in our first
plot that most of the accidents happened at rather low speed. More
interesting: when we zoom into the values below 25 mph we can see again
a trend of gaps between certain values. This is clearly a measurement
issue. Thus, our theory about the temperature problems could be
certainly related to the measurement instruments.

``` r
## Humidity
US_accidents %>%
    ggplot() +
    geom_histogram(aes(x = `Humidity(%)`))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 64467 rows containing non-finite values (stat_bin).

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
US_accidents %>%
    select(`Humidity(%)`) %>%
    filter(`Humidity(%)` > 35) %>%
    ggplot() +
    geom_histogram(aes(x = `Humidity(%)`))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-markdown_github/unnamed-chunk-8-2.png) Unlike
our previous variables, we can see that the number of accidents
increases when the humidity increases.

STATES
======

Now let’s explore a little bit the accidents in different states: which
state has the main occurrence of accidents? Which one the most severe
ones? Is there a relationship with the traffic lights or the humidity as
we discovered early?

We can start with the basic questions, amount of accidents per state and
severity:

``` r
## Amount of accidents per state
US_accidents %>%
    select(State) %>%
    group_by(State) %>%
    summarise(Total = n()) %>%
    ggplot() +
    geom_bar(aes(y = Total,
                 x = reorder(State, Total, FUN = abs),
                 fill = Total),
             stat = 'identity') +
    coord_flip() +
    labs(x = NULL) +
    theme(legend.position="none")
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
## Amount per state/severity, proportional
US_accidents %>%
    select(Severity, State) %>%
    mutate(State = parse_factor(State),
           Severity = parse_factor(as.character(Severity))) %>%
    group_by(State, Severity) %>%
    summarise(Total = n()) %>%
    ggplot() +
    geom_bar(aes(y = Total,
                 x = State,
                 fill = Severity),
             stat = 'identity',
             position = 'fill') +
    coord_flip() +
    scale_fill_brewer(palette = 'BuPu') +
    labs(x = NULL)
```

![](README_files/figure-markdown_github/unnamed-chunk-9-2.png)

At first glance it is clear that most of the accidents were reported
from CA, then TX and FL. As for the severity, most of the records are
for severity levels 2 and 3. It can be interesting to see if the states
where most of the accidents happen are also correlated with the
severity. For this I have created a function to to plot each severity
value and filter values above a given number of observations. Using this
function we can later adapt each graphic for a better visualisation:

``` r
## Function
plot_SevState <- function(sev, min = 10000) {
    US_accidents %>%
        select(State, Severity) %>%
        filter(Severity == sev) %>%
        group_by(State) %>%
        summarise(Total = n()) %>%
        filter(Total > min) %>%
        ggplot() +
        geom_bar(aes(y = Total,
                     x = reorder(State, Total, FUN = abs),
                     fill = Total),
                 stat = 'identity') +
        coord_flip() +
        labs(x = NULL, y = paste("Severity", sev, "Total")) +
        theme(legend.position="none")
}

## Visualization
plot_grid(
    plot_SevState(1, min = 50),
    plot_SevState(2, min = 50000),
    plot_SevState(3),
    plot_SevState(4, min = 2000))
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

As expected, CA is in the first positions of the four plots, while TX
and FL are also around. When it comes to the accidents with severity
level 4, we see FL in the first position, followed by GA and CA. The
order vary, but the difference between each state is minimal compared
with the number of entries that we have.

We can also check the relationship between states and our most
influencial variables, such as humidity and traffict signals.

``` r
## Relationship with humidity
US_accidents %>%
    select(State, `Humidity(%)`, Severity) %>%
    filter(Severity != 0) %>%
    mutate(State = parse_factor(State),
           Severity = parse_factor(as.character(Severity))) %>%
    group_by(State, Severity) %>%
    summarise(Humidity = mean(`Humidity(%)`, na.rm = T)) %>%
    ggplot(aes(x = State, y = Humidity)) +
    geom_point(aes(colour = Severity, shape = Severity),
               size = 6) +
    scale_fill_brewer(palette = 'BuPu') +
    labs(y = 'Average humidity')
```

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
US_accidents %>%
    select(State, `Humidity(%)`, Severity) %>%
    filter(Severity != 0) %>%
    mutate(State = parse_factor(State),
           Severity = parse_factor(as.character(Severity))) %>%
    group_by(State, Severity) %>%
    summarise(Humidity = mean(`Humidity(%)`, na.rm = T)) %>%
    filter(Humidity > 70) %>%
    ggplot(aes(x = State, y = Humidity)) +
    geom_point(aes(colour = Severity, shape = Severity),
               size = 6) +
    scale_fill_brewer(palette = 'BuPu') +
    labs(y = 'Average humidity')
```

![](README_files/figure-markdown_github/unnamed-chunk-11-2.png)

``` r
## States with main accidents at traffic signals
US_accidents %>%
    select(State, Traffic_Signal) %>%
    filter(Traffic_Signal == TRUE) %>%
    group_by(State) %>%
    summarise(Total = n()) %>%
    filter(Total > 5000) %>%
    ggplot() +
    geom_bar(aes(y = Total,
                 x = reorder(State, Total, FUN = abs),
                 fill = Total),
             stat = 'identity') +
    coord_flip() +
    labs(x = NULL, y = 'No. of accidents at traffic signals') +
    theme(legend.position="none")
```

![](README_files/figure-markdown_github/unnamed-chunk-11-3.png)

Accidents of severity 1 happen at different ranges of humidity, from dry
conditions to very humid. However, most of the accidents happen at
average humidity percentage between 60 and 80. For accidents of level 4
there is and increase in the average humidity, happening between 70-77%
most of the times. At Hum \> 80% happen mostly accidents level 1. ND
state is a particular case, with level 4 accidents of average humidity
of 91%, CA and TX are not among the states with average humidity above
70%.

As for traffic signals, we have again our top states heading the others,
but in a different order. CA comes in third place, it could be that
traffic signal areas are not the mean place for accidents in CA.

``` r
## View at CA only
filter(US_accidents, State == "CA") %>%
    select(Amenity:Turning_Loop) %>%
    summary()
```

    ##   Amenity           Bump          Crossing        Give_Way      
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:481563    FALSE:484676    FALSE:475619    FALSE:484525   
    ##  TRUE :3143      TRUE :30        TRUE :9087      TRUE :181      
    ##   Junction        No_Exit         Railway        Roundabout     
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:420390    FALSE:484495    FALSE:479325    FALSE:484694   
    ##  TRUE :64316     TRUE :211       TRUE :5381      TRUE :12       
    ##   Station           Stop         Traffic_Calming Traffic_Signal 
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:476064    FALSE:478652    FALSE:484655    FALSE:453662   
    ##  TRUE :8642      TRUE :6054      TRUE :51        TRUE :31044    
    ##  Turning_Loop   
    ##  Mode :logical  
    ##  FALSE:484706   
    ## 

It looks like in CA, most of the accidents are reported at junctions
more than at traffic signals.

TIME
====

Now we are going to have a quick view on the occurrence of accidents in
time. The first idea and easiest option would be to look at the
occurrence of accidents in specific moments, say grouped by Month (to
represent time of the year) and by Hour (to obtain time of the day). For
this we can use the package `lubridate` which makes such work easier:

``` r
## Months
US_accidents %>%
    select(Start_Time) %>%
    transmute(Month = month(Start_Time,
                            label = T,
                            locale = 'en_US.utf8')) %>%
    ggplot(aes(x = Month)) +
    geom_bar()
```

![](README_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
## Per hour
US_accidents %>%
    select(Start_Time) %>%
    transmute(Hour = hour(Start_Time)) %>%
    mutate(Hour = factor(as.character(Hour),
                         levels = as.character(c(0:23)))) %>%
    ggplot(aes(Hour)) +
    geom_histogram(stat = 'count')
```

![](README_files/figure-markdown_github/unnamed-chunk-13-2.png)

The amount of accidents reduces between April and July, as well as
during night time. It seems that most of the accidents happen in the
morning, between 7-8 hrs, and at early evening, between 16-17 hrs. In
other view, less accidents are happening during the warmest months and
during the coldest hours. That might answer the question why we did not
find a trend in the occurrence of accidents related to Temperature or
Wind Chill, but Humidity, considering that humidity raises at dusk and
down, with the movements of the sun.

``` r
## Month and humidity
US_accidents %>%
    select(Start_Time, `Humidity(%)`) %>%
    mutate(Month = month(Start_Time,
                            label = T,
                         locale = 'en_US.utf8')) %>%
    group_by(Month) %>%
    summarise(Avg_Hum = mean(`Humidity(%)`, na.rm = T),
              Count = n()) %>%
    ggplot(aes(x = Month, y = Avg_Hum)) +
    geom_point(aes(size = Count))
```

![](README_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
## Hour and Humidity
US_accidents %>%
    select(Start_Time, `Humidity(%)`) %>%
    mutate(Hour = hour(Start_Time),
           Hour = factor(as.character(Hour),
                         levels = as.character(c(0:23)))) %>%
    group_by(Hour) %>%
    summarise(Avg_Hum = mean(`Humidity(%)`, na.rm = T),
              Count = n()) %>%
    ggplot(aes(x = Hour, y = Avg_Hum)) +
    geom_point(aes(size = Count))
```

![](README_files/figure-markdown_github/unnamed-chunk-14-2.png)

As expected, we can easily observe the rise of the humidity at dusk and
down (moments of most accidents), and its reduction between March and
July (months with less accidents).
