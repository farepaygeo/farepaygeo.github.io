##messing with plots
library(ggplot2)

##mean lines
means <- ddply(dat, "cond", summarise, rating.mean=mean(rating))

hist_both <- ggplot(mean_headways_2015, aes (x = mean_hw)) +
  geom_histogram(binwidth = 5, color = "#FDB90B", fill = "#A30330") +
                 
  ##overlay

hist_both <- hist_both +
  geom_histogram(mean_headways_2017, 
                 mapping = aes(x = mean_hw),
                 color = "#A30330",
                 fill = "#FDB90B",
                 binwidth = 5
                ) +
  geom_vline(mean_headways_2017, mapping = aes(xintercept = mean(mean_hw)), show.legend = TRUE) +
  geom_vline(mean_headways_2015, mapping = aes(xintercept = mean(mean_hw)), linetype = "dashed")+
  scale_x_continuous(name = "Mean Headways by Pattern (minutes)", breaks=seq(5,90,5))+
  ylab("Count (Patterns)") +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())



hist_both+scale_color_brewer(palette = "Dark2")