##tallying patterns
library(sf)
library(tidyverse)
library(ggplot2)
library(gtfstools)
library(rgis)

##spit out plot
histogram_15 <- ggplot(mean_headways_patt_15, aes(mean_hw)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept = mean(mean_hw))       
  )

##which pattern has the most trips associated with it?
tally(weekday_headways_fil)
tally <- tally(weekday_headways_fil)
print(tally)
big_patterns <- filter(weekday_headways_fil, pattern_id == c(45, 43, 326))
##route 8 and route 91 are the big offenders here