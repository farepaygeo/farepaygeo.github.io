##exploring my data 2
library(sf)
library(tidyverse)
library(ggplot2)
library(gtfstools)
library(rgis)
setwd("C:/Users/farep/Documents/GitHub/farepay/ges600_final")


list.files()
list.files("data")
nov_2018 <- read_gtfs("data/gtfs 11_2018.zip") ##reading in gtfs
nov_2018 <- filter_by_route_type(nov_2018, route_type = 3) ##subset bus routes
names(nov_2018)
View(nov_2018$routes) ##check out route ids
route_ids <- as.character(c(10933:10982, "10997")) ##we are going to get rid of these routes. they include commuter routes and expresslink buses
nov_2018_routes <- filter_by_route_id(nov_2018, route_id = route_ids, keep = FALSE) ##seeya
View(nov_2018_routes$routes)

##check out the citylink buses
city_link_ids <- as.character(c(10984:10995))##create string for citylink IDs
nov_2018_citylink <- filter_by_route_id(nov_2018_routes, route_id = city_link_ids) ##subset citylink routes
nov_2018_citylink_dur <- get_trip_duration(nov_2018_citylink, unit = "min") ##get citylink trip duration
nov_2018_dur <- get_trip_duration(nov_2018_routes) ##get trip duration for all routes
summary(nov_2018_citylink_dur) ##compare trip durations
summary(nov_2018_dur)

##mess around with this pattern ids thing
nov_2018_patterns <- get_stop_times_patterns(nov_2018_routes)
View(nov_2018_patterns)
nov_2018_shapes <- convert_shapes_to_sf(nov_2018_routes)
View(nov_2018_shapes)
plot (nov_2018_shapes)

##make progress toward calculating headways
nov_2018_routes <- convert_time_to_seconds(nov_2018_routes)
nov_2018_sf <- convert_shapes_to_sf(nov_2018_routes) ##transform nov_2018 routes to sf object
nov_2018_sf$shape_length <- st_length(nov_2018_sf) ##calculate legnths of each shape. returns shape in meters
nov_2018_shapes_length <- as.data.frame(nov_2018_sf) ##convert sf to data frame
nov_2018_shapes_length$shape_length <- gsub("[m]","", as.numeric(nov_2018_shapes_length$shape_length)) #get rid of pesky [m]

pattern_summary <- nov_2018_routes$trips %>%
  left_join(nov_2018_patterns, by="trip_id") %>% 
  left_join(nov_2018_shapes_length, by="shape_id") %>%
  left_join(nov_2018$stop_times, by="trip_id") %>% 
  group_by(pattern_id) %>%
summarise(
  trips = n(),
  routes = n_distinct(route_id)) ##created a table that has num of trips and routes associated with each pattern. each pattern is just associated with 1 route so thats encouraging.

  ##  total_distance_per_day_km = sum(shape_length)/1e3 ,
  ##stops = (n_distinct(stop_id)/2)
## ))


##get more info for your patterns
pattern_summary_2 <- nov_2018_routes$trips %>%
  left_join(nov_2018_patterns, by="trip_id") %>% 
  left_join(nov_2018_shapes_length, by="shape_id") %>%
  left_join(nov_2018_routes$stop_times, by="trip_id") %>% 
  group_by(pattern_id) %>%
  summarise(
    trips = n(),
    routes = n_distinct(route_id),
    total_distance_per_day_km = sum(as.numeric(shape_length), na.rm = TRUE) /1e3,
    total_distance_per_day_km = sum(as.numeric(shape_length), na.rm = TRUE) /1e3 ,
    stops = (n_distinct(stop_id)/2)
  )
headways <- nov_2018_routes$trips %>%
  left_join(nov_2018_patterns, by="trip_id")
View(headways)
selection <- filter(nov_2018_routes$stop_times,
                    trip_id == c("2233431", "2233650"))
selection_by_trip <- selection %>% group_by(trip_id)


headways_2 <- left_join(nov_2018_routes$stop_times, headways, by = "trip_id")

headways_2_sel <- filter(headways_2,
                         pattern_id == c(1:25))
weekday_headways <- filter(headways_2,
                           service_id == 1)
wwekday_headways_agg <- group_by(weekday_headways, stop_id, pattern_id)
wwekday_headways_agg_digg <- mutate(wwekday_headways_agg, diff = departure_time_secs-lag(departure_time_secs))



#unsure if this worked 
#weekday_headways <- weekday_headways %>%
 ## mutate(diff=departure_time_secs-lag(departure_time_secs))# 
  
  
                


wk_hd_sel <- filter(weekday_headways, pattern_id == 1)
wk_hd_stop_sel <- filter(wk_hd_sel, stop_sequence == 1)

headways_sel_agg <- headways_2_sel %>%
                group_by(stop_id, trip_id)%>%
                mutate(diff=departure_time_secs-lag(departure_time_secs))
                
headways_sel_agg <- arrange(headways_sel_agg, stop_sequence, .by_group = TRUE)

headways_sel_agg_2 <- headways_sel_agg %>%
  group_by(stop_id, pattern_id)%>%
  mutate(diff=departure_time_secs-lag(departure_time_secs))

headways_sel_unagg <-  ungroup (headways_sel_agg_2)

headways_sel_unagg_sel <-filter (headways_sel_unagg,  trip_id == c("2233398", "2233399"))

headways_sel_unagg <- headways_sel_unagg %>%
                    group_by(trip_id)
sel <- filter(wwekday_headways_agg_digg, pattern_id == 30)
