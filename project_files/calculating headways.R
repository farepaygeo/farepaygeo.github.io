##calculating weekday headways
library(sf)
library(tidyverse)
library(ggplot2)
library(gtfstools)

##read in data, filter gtfs by route type and remove commuter and express buses from gtfs
gtfs <- read_gtfs(##"PATH") ##reading in gtfs
gtfs <- filter_by_route_type(gtfs, route_type = 3) ##subset bus routes
route_ids <- as.character(c(##Express and commuter bus routes)) ##we are going to get rid of these routes. they include commuter routes and expresslink buses
gtfs_fil <- filter_by_route_id(gtfs, route_id = route_ids, keep = FALSE) ##seeya

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
weekday_trips_grp <- group_by(weekday_trips, pattern_id, stop_sequence)

##calculate difference in seconds between arrivals at each stop, by trip
headways_##YEAR <- mutate(weekday_trips_grp, diff = departure_time_secs-lag(departure_time_secs))

##convert diff column to minutes
headways_##YEAR <- mutate(headways_##YEAR, headways_m = diff/60)

##convert negative values to positive
headways_##YEAR##$headways_m <- abs(weekday_headways$headways_m) 

##filter headways less than 3 and greater than 90 minutes per wong
headways_#YEAR_fil <- filter(headways_#YEAR, headways_m > 3, headways_m < 90) ##this also gets rid of NAs, which feels kind of sketchy but it'll work for this analysis.

##group by pattern ID
headways_#YEAR_fil <- headways_#YEAR_fil %>% group_by (pattern_id)

##summarise by mean headway for each pattern
mean_headways_##YEAR <- headways_#YEAR_fil %>% summarise(mean_hw = mean(headways_m))


##spit out plot
histogram_##Year <- ggplot(mean_headways_year, aes(x=mean_hw)) +
  geom_histogram(binwidth = 5, color = "#FDB90B", fill = "#A30330") +
  geom_vline(aes(xintercept = mean(mean_hw)), color = "black", linetype = "dashed", size = 1)+
  scale_x_continuous(name = "Mean Headways by Pattern (minutes)", breaks=seq(5,90,5))+
  ylab("Count (Patterns)") +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())