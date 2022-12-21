##comparing bus routes spatially
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

##routes_17$trips <- routes_17$trips %>%
 ## left_join(patterns_17, by="trip_id")

##routes_18$trips <- routes_18$trips %>%
 ## left_join(patterns_18, by="trip_id")


##get geoms
trip_geom_17 <- get_trip_geometry(routes_17, file = "shapes")
trip_geom_18 <- get_trip_geometry(routes_18, file = "shapes")

##these sf objects could use some attributes
routes_geom_17_sf_mrg <- merge(trip_geom_17, routes_17$trips)
routes_geom_18_sf_mrg <- merge(trip_geom_18, routes_18$trips)

##make plots comparing their extents
plot <- ggplot(routes_geom_17_sf_mrg, mapping = aes(color = route_id)) +
  geom_sf(linewidth = 3)+
  scale_color_manual(values = c("9336" = "#939598",
                                "9338" = "#851F83",
                                "9364" = "#6F4C2F",
                                "9365" =  "#D71921",
                                "9368" = "#6CA144",
                                "10468" = "#939598",
                                "10535" = "#6F4C2F",
                                "10538" = "#6CA144",
                                "10539" =  "#2E3192",
                                "10542" = "#851F83",
                                "10543" = "#D71921"),
                     labels = c("8", "10", "47",
                                "48", "52", "80 (LocalLink)", "Brown", "Lime",
                                "Navy", "Purple", "Red"),
                     name = "Pre-LINK Routes") 



plot <- plot + 
  geom_sf(data = routes_geom_18_sf_mrg, mapping = aes(fill = route_id))+
  geom_sf(linewidth = 1.5)+
  scale_fill_manual(values = c("10468" = "#939598",
                               "10535" = "#6F4C2F",
                               "10538" = "#6CA144",
                               "10539" =  "#2E3192",
                               "10542" = "#851F83",
                               "10543" = "#D71921"),
                    labels = c("80 (LocalLink)", "Brown", "Lime",
                               "Navy", "Purple", "Red"),
                    name = "CityLink Routes") 
