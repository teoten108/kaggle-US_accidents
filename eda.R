library(tidyverse)
library(cowplot)
library(lubridate)

#US_accidents <- read_csv('US_Accidents_May19.csv')
load('eda.RData')

## First view to the data:
## -------------------------------------------------------------------

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
## There are extreme temperatures, from -60 to +76 C
## Wind chill also reaches - 54 C
## Visibility does not seem to be an issue, min is 0, but mean is 14 km
## Max wind speed is 1,322 km/h, but mean is 13 km/h
## Humidity reached 100, with mean of 65.93 %
## Precipitation is low, distance affected low (with exceptions).

length(US_accidents$`Wind_Chill(F)`) # 2,243,939 entries

## Boolean vars
US_accidents %>%
    select(Amenity:Turning_Loop) %>%
    summary()
## Bump/hump = baches, topes - 239
##

## Graphical representation
signals <- US_accidents %>%
    select(Amenity:Turning_Loop) %>%
    pivot_longer( cols = Amenity:Turning_Loop,
        names_to = 'Annotation',
        values_to = 'Trues') %>%
    filter(Trues == TRUE) %>%
    group_by(Annotation) %>%
    summarise(Total = n())

### SAVING OBJECTS ------------------------------------|
###                                                    |
###  save(list = ls( all = TRUE), file='eda.RData')   #|
### ---------------------------------------------------|

signals %>%
    ggplot() +
    geom_bar(aes(y = Total,
                 x = reorder(Annotation, Total, FUN = abs),
                 fill = Total),
             stat = 'identity') +
    coord_flip() +
    labs(x = NULL) +
    theme(legend.position="none")

## Most of the accidents happen at Traffic signals, Junctions and
## Roads crossings.

## Other vars
US_accidents %>%
    select(ID, Source, TMC, Severity, Description, Amenity, Stop) %>%
    sample_n(size = 50) %>%
    print(n = 50)

## API which reported accident
unique(US_accidents$Source) # 3 sources
## Traffic message channel
unique(US_accidents$TMC) # 21 channels and NA
## Severity: 1 is lesser, 4 is higher
unique(US_accidents$Severity) # from 0 to 4

### NUMERIC VARIABLES
##
## Evaluate numeric variables that are worth look at

## Temperature
US_accidents %>%
    filter(`Temperature(F)` > 0 &
           `Temperature(F)` <= 100) %>%
    ggplot() +
    geom_histogram(aes(x = `Temperature(F)`))

## More specific view at the main values
US_accidents %>%
    filter(`Temperature(F)` > 45 &
           `Temperature(F)` <= 80) %>%
    ggplot() +
    geom_histogram(aes(x = `Temperature(F)`))

## The main temperatures are between 51-75F, but particular peaks are
## found at 62, 60 and 75. Quite average, nor cold or hot

## Wind chill
US_accidents %>%
    ggplot() +
    geom_histogram(aes(x = `Wind_Chill(F)`))
## Again, most of the accidents happen at 75F. At low temperatures there
## are really few

US_accidents %>%
    select(`Wind_Chill(F)`) %>%
    filter(`Wind_Chill(F)` < 30) %>%
    summarise(n())
## 209,936 to be precise... not so little

## Wind speed
US_accidents %>%
    ggplot() +
    geom_histogram(aes(x = `Wind_Speed(mph)`))

US_accidents %>%
    select(`Wind_Speed(mph)`) %>%
    filter(`Wind_Speed(mph)` < 25) %>%
    ggplot() +
    geom_histogram(aes(x = `Wind_Speed(mph)`))

## Humidity
US_accidents %>%
    ggplot() +
    geom_histogram(aes(x = `Humidity(%)`))

US_accidents %>%
    select(`Humidity(%)`) %>%
    filter(`Humidity(%)` > 35) %>%
    ggplot() +
    geom_histogram(aes(x = `Humidity(%)`))



### STATES AND/OR LOCATION

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
## Main CA, then TX and FL

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
## Main are 2 and 3

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

plot_SevState(0, min = 1)
## There are only 12 entries with severity 0

plot_grid(
    plot_SevState(1, min = 50),
    plot_SevState(2, min = 50000),
    plot_SevState(3),
    plot_SevState(4, min = 2000))

## Relationship with humidity
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
    scale_fill_brewer(palette = 'BuPu')
## Most of the accidents of level 4 happen at average Humidity of 70-77%
## At Hum > 80% happen mostly accidents level 1
## ND state is a particular case, with level 4 accidents of average
## humidity of 91%
## CA and TX are not among the states with average humidity above 70%

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
    labs(x = NULL) +
    theme(legend.position="none")
## TX and FL, CA is in third position, apparently there is a different
## trend in CA

## View at CA only
filter(US_accidents, State == "CA") %>%
    select(Amenity:Turning_Loop) %>%
    summary()
## In CA most of the accidents happen at Junction (64,316), and then
## traffic signal (31,044)

## View at ND
filter(US_accidents, State == "ND") %>%
    select(Amenity:Turning_Loop) %>%
    summary()
## In ND, where the humidity seems to be an issue, there are not enough
## records of the signalization


## TIME

## Months
US_accidents %>%
    select(Start_Time) %>%
    transmute(Month = month(Start_Time,
                            label = T,
                            locale = 'en_US.utf8')) %>%
    ggplot(aes(x = Month)) +
    geom_bar()
## less of accidents happenning between April and July

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
## Between March and July, humidity decreases, and so the amount of
## accidents considerably decreases as well.

## Accidents during a day

## Per hour
US_accidents %>%
    select(Start_Time) %>%
    transmute(Hour = hour(Start_Time)) %>%
    mutate(Hour = factor(as.character(Hour),
                         levels = as.character(c(0:23)))) %>%
    ggplot(aes(Hour)) +
    geom_histogram(stat = 'count')

## Most of the accidents are reported early in the
## morning (7-8 hrs) and early evening (16-17 hrs), while less are
## happening during the night

## Time of the day
US_accidents %>%
    select(Start_Time) %>%
    transmute(Hour = hour(Start_Time)) %>%
    mutate(Day_Time = ifelse(Hour <= 4, "Night",
                      ifelse(Hour > 4 & Hour <= 8, "Dawn",
                      ifelse(Hour > 8 & Hour <= 12, "Morning",
                      ifelse(Hour > 12 & Hour <= 16, "Afternoon",
                      ifelse(Hour > 16 & Hour <= 20, "Dusk",
                             "Evening"))))),
           Day_Time = factor(Day_Time,
                             levels = c("Dawn", "Morning", "Afternoon",
                                        "Dusk", "Evening", "Night"))) %>%
    ggplot(aes(Day_Time)) +
    geom_histogram(stat = 'count')
## In other words, less of the accidents happen at night

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


### CORRELATIONS

US_accidents %>%
    select(Start_Time, `Humidity(%)`) %>%
    mutate(Hour = hour(Start_Time)) %>%
    cor(x = `Humidity(%)`, y = Hour)
