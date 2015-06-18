# Data from
# https://lter.limnology.wisc.edu/data_LR_zoopcatalog/search

library(ggplot2)
library(scales)

data_LR_zoop <- read.csv("./data/little_rock_lake_experiment__zooplankton.csv")

head(data_LR_zoop)

# station 1 = treatment (acidification)
# station 2 = reference (control)

# how many unique species 
length(unique(data_LR_zoop$species_code))
length(unique(data_LR_zoop$species))
sort(unique(data_LR_zoop$species))

# How many observations per species 
table(data_LR_zoop$species)

# save the species names to a .csv
zoop_names <- unique(data_LR_zoop$species)
write.csv(zoop_names, file = "data/LR_zoop_species.csv")

# convert dates to true dates 
data_LR_zoop$date <- as.Date(data_LR_zoop$sample_date)

# plot & save all time series
for (i in unique(data_LR_zoop$species)){
  
  temp_data <- subset(data_LR_zoop, species==i)
  
  temp <- ggplot(temp_data, aes(date,number_per_liter)) 
  temp <- temp + facet_grid(station~.)
  temp <- temp + geom_point() + geom_line()
  temp <- temp + ylab("Number per liter")
  temp <- temp + xlab("Date")
  temp <- temp + theme_bw(base_size=12)
  temp <- temp + ggtitle(i)
  temp <- temp + theme(axis.text.x = element_text(angle=315,hjust=0)) # fix here
  
  # if the taxa name contains > or <, you need to remove that character 
  i <- sub("<", "less", i)
  i <- sub(">", "more", i)
  i <- sub("/", "-", i)
  
  ggsave(paste("time_series_plots/zoop_LR/",i,".jpg",sep=""),temp)
  
}


