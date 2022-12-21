##calculating weekday headways
library(sf)
library(tidyverse)
library(ggplot2)
library(gtfstools)
setwd("C:/Users/farep/Documents/GitHub/farepay/ges600_final")

##read in data, filter gtfs by route type and remove commuter and express buses from gtfs
gtfs <- read_gtfs("data/gtfs 7_2015.zip") ##reading in gtfs
gtfs <- filter_by_route_type(gtfs, route_type = 3) ##subset bus routes
names(gtfs)
View(gtfs$routes) ##check out route ids. these change in every GTFS.
route_ids <- as.character(c(7574:7827, "WS", "CS", "ES", "7547", "7563")) ##we are going to get rid of these routes. they include commuter routes and expresslink buses
gtfs_fil <- filter_by_route_id(gtfs, route_id = route_ids, keep = FALSE) ##seeya
View(gtfs_fil$routes)

##associate each trip with a pattern
patterns <- get_stop_times_patterns(gtfs_fil)

##add fields for arrivals and departures in seconds after midnight
gtfs_fil <- convert_time_to_seconds(gtfs_fil)

##associate pattern IDs with trips table
trips_patt <- gtfs_fil$trips %>%
  left_join(patterns, by="trip_id")

##now associate the headways table you just created with stop times, which as the rest of the info we need
trips_patt <- left_join(gtfs_fil$stop_times, trips_patt, by = "trip_id")

##subset weekday service
weekday_trips <- filter(trips_patt, service_id == 1)

##group by stop ID and pattern ID
weekday_trips_grp <- group_by(weekday_trips, route_id, stop_id)

##calculate difference in seconds between arrivals at each stop, by trip
weekday_headways <- mutate(weekday_trips_grp, diff = departure_time_secs-lag(departure_time_secs))

##convert diff column to minutes
weekday_headways <- mutate(weekday_headways, headways_m = diff/60)

##convert negative values to positive
weekday_headways$headways_m <- abs(weekday_headways$headways_m) 

##filter headways less than 3 and greater than 90 minutes per wong
weekday_headways_fil <- filter(weekday_headways, headways_m > 3, headways_m < 90) ##this also gets rid of NAs, which feels kind of sketchy but it'll work for this analysis.

##group by pattern ID
weekday_headways_fil <- weekday_headways_fil %>% group_by (route_id)

##summarise by mean headway for each pattern
mean_headways_patt <- weekday_headways_fil %>% summarise(mean_hw = mean(headways_m))


##spit out plot
histogram <- ggplot(mean_headways_patt, aes(mean_hw)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept = mean(mean_hw))) +
  geom_vline(aes(xintercept = mean(mean_hw) + sd(mean_hw))) +
  geom_vline(aes(xintercept = mean(mean_hw) - sd(mean_hw))         
  )