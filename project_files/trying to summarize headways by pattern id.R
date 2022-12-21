##sumarize headways and other info by pattern_id
library(sf)
library(tidyverse)
library(ggplot2)
library(gtfstools)
library(rgis)


##group by pattern ID
weekday_headways_diff <- weekday_headways_diff %>% group_by (pattern_id)

##summarise by mean headway for each pattern
mean_headways_by_patt <- weekday_headways_diff %>% summarise(mean_hw = mean(headways_m)) 

##filter headways > 3 min and < 90 per tcsw
mean_headways_by_patt_fil <- filter(mean_headways_by_patt, mean_hw > 3, mean_hw < 90 )

##spit out plot
histogram <- ggplot(mean_headways_by_patt_fil, aes(mean_hw)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept = mean(mean_hw))) +
  geom_vline(aes(xintercept = mean(mean_hw) + sd(mean_hw))) +
  geom_vline(aes(xintercept = mean(mean_hw) - sd(mean_hw))         
             )
