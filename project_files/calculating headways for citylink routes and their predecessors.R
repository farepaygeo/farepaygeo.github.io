##exploring past and present routes
library(sf)
library(tidyverse)
library(ggplot2)
library(gtfstools)
setwd("C:/Users/farep/Documents/GitHub/farepay/ges600_final")

gtfs_18 <- read_gtfs("data/gtfs_6_2018.zip")
gtfs_17 <- read_gtfs ("data/gtfs 3_17.zip")##reading in gtfs

route_ids_18 <- as.character(c("10543", "10539", "10542", "10535", "10468", "10538"))
route_ids_17 <- as.character(c("9336", "9338", "9364", "9365", "9368"))
##we are interested in these routes. the routes in 2017 are the predecessors to those in 2018.


routes_18 <- filter_by_route_id(gtfs_18, route_id = route_ids_18) 
routes_17 <- filter_by_route_id(gtfs_17, route_id = route_ids_17) ##hello

##lets start by comparing headways
##associate each trip with a pattern
patterns_17 <- get_stop_times_patterns(routes_17)
patterns_18 <- get_stop_times_patterns(routes_18)

##add fields for arrivals and departures in seconds after midnight
routes_17 <- convert_time_to_seconds(routes_17)
routes_18 <- convert_time_to_seconds(routes_18)
##associate pattern IDs with trips table
trips_patt_17 <- routes_17$trips %>%
  left_join(patterns_17, by="trip_id")

trips_patt_18 <- routes_18$trips %>%
  left_join(patterns_18, by="trip_id")

##now associate the headways table you just created with stop times, which as the rest of the info we need
trips_patt_17 <- left_join(routes_17$stop_times, trips_patt_17, by = "trip_id")
trips_patt_18 <- left_join(routes_18$stop_times, trips_patt_18, by = "trip_id")
##more trips in 2018. but there's also more routes in my table.

##subset weekday service
weekday_trips_17 <- filter(trips_patt_17, service_id == 1)
weekday_trips_18 <- filter(trips_patt_18, service_id == 1)
##group by stop ID and pattern ID
weekday_trips_17 <- group_by(weekday_trips_17, pattern_id, stop_sequence)
weekday_trips_18 <- group_by(weekday_trips_18, pattern_id, stop_sequence)
##calculate difference in seconds between arrivals at each stop, by trip
weekday_headways_17 <- mutate(weekday_trips_17, diff = departure_time_secs-lag(departure_time_secs))
weekday_headways_18 <- mutate(weekday_trips_18, diff = departure_time_secs-lag(departure_time_secs))
##convert diff column to minutes
weekday_headways_17 <- mutate(weekday_headways_17, headways_m = diff/60)
weekday_headways_18 <- mutate(weekday_headways_18, headways_m = diff/60)
##convert negative values to positive
weekday_headways_17$headways_m <- abs(weekday_headways_17$headways_m) 
weekday_headways_18$headways_m <- abs(weekday_headways_18$headways_m) 

##get rid of NAs
##weekday_headways <- weekday_headways %>% replace(is.na(.), 0)

##filter headways less than 3 and greater than 90 minutes per wong
weekday_headways_17_fil <- filter(weekday_headways_17, headways_m > 3, headways_m < 90) ##this also gets rid of NAs, which feels kind of sketchy but it'll work for this analysis.
weekday_headways_18_fil <- filter(weekday_headways_18, headways_m > 3, headways_m < 90) ##this also gets rid of NAs, which feels kind of sketchy but it'll work for this analysis.

##group by pattern ID
weekday_headways_17_fil <- weekday_headways_17_fil %>% group_by (pattern_id)
weekday_headways_18_fil <- weekday_headways_18_fil %>% group_by (pattern_id)
##summarise by mean headway for each pattern
mean_headways_17_patt <- weekday_headways_17_fil %>% summarise(mean_hw = mean(headways_m)) 
mean_headways_18_patt <- weekday_headways_18_fil %>% summarise(mean_hw = mean(headways_m)) 


##spit out plot
histogram_17 <- ggplot(mean_headways_17_patt, aes(mean_hw)) +
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = mean(mean_hw))       
  )
histogram_18 <- ggplot(mean_headways_18_patt, aes(mean_hw)) +
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = mean(mean_hw))
        
  )

