##sumarize headways and other info by pattern_id
library(sf)
library(tidyverse)
library(ggplot2)
library(gtfstools)
library(rgis)

setwd("C:/Users/farep/Documents/GitHub/farepay/ges600_final")

##read in data, filter gtfs by route type and remove commuter and express buses from gtfs
nov_2018 <- read_gtfs("data/gtfs 11_2018.zip") ##reading in gtfs
nov_2018 <- filter_by_route_type(nov_2018, route_type = 3) ##subset bus routes
route_ids <- as.character(c(10933:10982, "10997")) ##we are going to get rid of these routes. they include commuter routes and expresslink buses
nov_2018_routes <- filter_by_route_id(nov_2018, route_id = route_ids, keep = FALSE) ##seeya

##associate each trip with a pattern
nov_2018_patterns <- get_stop_times_patterns(nov_2018_routes)

##add fields for arrivals and departures in seconds after midnight
nov_2018_routes <- convert_time_to_seconds(nov_2018_routes)

##associate pattern IDs with trips table
headways <- nov_2018_routes$trips %>%
  left_join(nov_2018_patterns, by="trip_id")

##now associate the headways table you just created with stop times, which as the rest of the info we need
headways <- left_join(nov_2018_routes$stop_times, headways, by = "trip_id")

##subset weekday service
weekday_headways <- filter(headways,
                           service_id == 1)

##group by stop ID and pattern ID
weekday_headways_agg <- group_by(weekday_headways, stop_id, pattern_id)

##calculate difference in seconds between arrivals at each stop, by trip
weekday_headways_diff <- mutate(weekday_headways_agg, diff = departure_time_secs-lag(departure_time_secs))

##convert diff column to minutes
weekday_headways_diff <- mutate(weekday_headways_diff, headways_m = diff/60)

##convert negative values to positive
weekday_headways_diff$headways_m <- abs(weekday_headways_diff$headways_m) 

##get rid of NAs
weekday_headways_diff <- weekday_headways_diff %>% replace(is.na(.), 0)

weekday_headways_diff <- weekday_headways_diff %>% group_by (pattern_id)

mean_headways_by_patt <- weekday_headways_diff %>% summarise(mean_hw = mean(headways_m)) 

##filter headways > 3 min and < 90 per tcsw
mean_headways_by_patt_fil <- filter(mean_headways_by_patt, mean_hw > 3, mean_hw < 90 )

twofour_2 <- filter(weekday_headways_diff, pattern_id == 204)  
twoeight <- filter(weekday_headways_diff, pattern_id == 28)  

df10888 <- filter (nov_2018_routes$routes, route_id == 10888)
stop <- filter(nov_2018_routes$stops, stop_id == 7841)

headways_mean <- left_join (weekday_headways_diff, mean_headways_by_patt)




st <- filter(nov_2018_routes$trips, trip_id == 2254870)


mean_hist <- ggplot(mean_headways_by_patt, aes(mean_hw))+
  geom_histogram(binwidth = 10)


##explore weirdly short headways
sup <- filter(headways_mean, mean_hw < 1)
sup <- sup %>% na.omit()


sup_shape <- left_join(sup, nov_2018_routes$shapes)
sup_shape <- na.omit(sup_shape)
sup_sf <- st_as_sf(sup_shape, coords = c("shape_pt_lon", "shape_pt_lat"))
sup_plot <- ggplot()+
  geom_sf(data = sup_sf)
sup_plot
