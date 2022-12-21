##load libraries
library(tidytransit)
library(sf)
library(tidyverse)
library(ggplot2)
library(gtfstools)
library(rgis)
##set working directory
setwd("C:/Users/farep/Documents/GitHub/farepay/farepaygeo/finalproject")
##get gtfs filepaths
list.files("./data")
##read in gtfs data for each year
mta_aug_15 = read_gtfs ("./data/gtfs_08_2015.zip")
mta_dec_17 = read_gtfs("./data/gtfs_12_2017.zip")
mta_feb_20 = read_gtfs("./data/gtfs_2_20.zip")

##pull out bus routes from routes tables.
mta_aug_15_bus_routes <- filter(mta_aug_15[["routes"]], route_type == "3")
mta_dec_17_bus_routes <- filter(mta_dec_17[["routes"]], route_type == "3")
mta_feb_20_bus_routes <- filter(mta_feb_20[["routes"]], route_type == "3")

##hm they're no longer tidygtfs objects. but you can already see how BNR changed the system.
##in 2017 there are 105 observations, in february 2020, 101.

##wait fuck this. let's install gtfstools and filter the whole gtfs by route type.
remove("mta_aug_15", "mta_dec_17", "mta_feb_20" )
install.packages("gtfstools")
remove.packages("tidytransit")
library(gtfstools)
mta_aug_15 = read_gtfs ("./data/gtfs_08_2015.zip")
mta_dec_17 = read_gtfs("./data/gtfs_12_2017.zip")
mta_feb_20 = read_gtfs("./data/gtfs_2_20.zip")
aug_15_bus <- filter_by_route_type(mta_aug_15, route_type = 3)
dec_17_bus <- filter_by_route_type(mta_dec_17, route_type = 3)
feb_20_bus <- filter_by_route_type(mta_feb_20, route_type = 3)

##trying to filter by area now. first have to make a bbox##
box_coords = list(rbind(c(-76.88946, 38.97829),
                        c(-76.88946, 39.50223), 
                        c(-76.37931, 39.50223), 
                        c(-76.37931, 38.97829), 
                        c(-76.88946, 38.97829)))

box <- st_polygon(box_coords)
box <-st_as_sf(box, crs = 4326)
ggplot() +
  geom_sf(data = box, fill = NA)
##you'll use the object "box" later to filter by area##

aug_15_city <- filter_by_sf(aug_15_bus, box, spatial_operation = sf::st_contains)
aug_15_city_shapes <- convert_shapes_to_sf(aug_15_city)
ggplot() +
  geom_sf(data = aug_15_city_shapes) +
  geom_sf(data = box, fill = NA)
##fuck around, find out.
aug_15_city_dur <- get_trip_duration(aug_15_city, unit = "min")
View(aug_15_bus_dur) ##it has a trip_id column, could eventually be joined up with other tables
dec_17_bus_dur <-get_trip_duration(dec_17_bus, unit = "min")
feb_20_bus_dur <- get_trip_duration(feb_20_bus, unit = "min")
summary(aug_15_bus_dur)
summary(feb_20_bus_dur)
summary(dec_17_bus_dur)
## ok so there's your tables with duration of each trip. let's explore headways for the aug_15 data. let's see how bad it supposedly was.
##let's check out weekday service
aug_15_mf <- filter_by_weekday(aug_15_bus, weekday = c("monday", "tuesday", "wednesday", "thursday", "friday"), combine = "and")
aug_15_mf$calendar
##generate pattern ids for all trips
aug_15_mf_serv_id <- get_stop_times_patterns(aug_15_mf, trip_id = NULL, type = "spatial")
##convert these jawns to sf
aug_15_mf_shapes <- convert_shapes_to_sf(aug_15_mf)
aug_15_mf_stops <- convert_stops_to_sf(aug_15_mf)

aug_15_mf_shapes$length <- st_length(aug_15_mf_shapes)

bus_lengths <- aug_15_mf_shapes %>% 
  as.data.frame() %>% 
  select(shape_id, length, -geometry)


##join it up with some other data##


aug_15_service <- aug_15_mf$trips %>%
  left_join(aug_15_mf_serv_id, by="trip_id") %>% 
  left_join(bus_lengths, by="shape_id") %>%
  left_join(aug_15_mf$stop_times, by="trip_id") %>% 
  group_by(pattern_id) %>% 
 
  
   summarise(
    trips = n(), 
    routes = n_distinct(route_id),
    total_distance_per_day_m = sum(as.numeric(length), na.rm=TRUE)/1e3,
    route_avg_distance_m = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
    stops=(n_distinct(stop_id)/2))
View(aug_15_service)
aug_15_service_sf <- left_join(aug_15_service, aug_15_mf_shapes, by = "shape_id")
bus1 <- filter(aug_15_service_sf, pattern_id == '254')
##make a plot of your lil subset
bus1_plot <- 
  ggplot() + 
  geom_sf(data = bus1) 
  theme_bw()
bus1_plot
bus1_plot
bus1 <- st_as_sf(bus1)


##trying to filter by area now. first have to make a bbox##
box_coords = list(rbind(c(-76.88946, 38.97829),
                        c(-76.88946, 39.50223), 
                        c(-76.37931, 39.50223), 
                        c(-76.37931, 38.97829), 
                        c(-76.88946, 38.97829)))

box <- st_polygon(box_coords)
ggplot() +
  geom_sf(data = box, fill = NA)
box <- polygon_bounding_box(-76.88946, -76.37931, 39.1, 39.50223, 4326)
