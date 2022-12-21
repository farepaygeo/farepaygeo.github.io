##what's up with these outliers
library(mapview)


outliers <- nov_2018_patterns %>% filter(pattern_id %in% 182:290) ##these ones are a little weird

trips_shapes <- select(nov_2018_routes$trips, trip_id, shape_id) ##need to get the matching shape_ids

outliers <- left_join(outliers, trips_shapes)

outliers <- left_join(outliers, nov_2018_routes$shapes)

outliers_sf <- st_as_sf(outliers, coords = c("shape_pt_lon", "shape_pt_lat"))


stop_info <- select(nov_2018_routes$stop_times, trip_id, stop_id, stop_sequence, arrival_time, arrival_time_secs) ##need to get the matching shape_ids

outliers <- left_join(outliers, stop_info)

single_trip <- filter(outliers, trip_id == "2254870")

single_trip_sf <- st_as_sf(single_trip, coords = c("shape_pt_lon", "shape_pt_lat"))
mapview(single_trip_sf)

##group by stop ID and pattern ID
single_trip_agg <- group_by(single_trip, stop_id)

##calculate difference in seconds between arrivals at each stop, by trip
single_trip_hw <- mutate(single_trip_agg, diff = arrival_time_secs-lag(arrival_time_secs))

##convert diff column to minutes
weekday_headways_diff <- mutate(weekday_headways_diff, headways_m = diff/60)
