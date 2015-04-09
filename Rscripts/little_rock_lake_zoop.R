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

# convert dates to true dates 
data_LR_zoop$date <- as.Date(data_LR_zoop$sample_date)

# plot a time series
temp <- ggplot(subset(data_LR_zoop, species=="POLYARTHRA REMATA"), aes(date,number_per_liter)) 
temp <- temp + geom_point() + geom_line()
temp <- temp + facet_grid(station~.)
temp <- temp + ylab("Number per liter")
temp <- temp + xlab("Date")
temp <- temp + theme_bw(base_size=18)
temp <- temp + scale_x_date(labels=date_format("%Y"),breaks=date_breaks("year"))
temp <- temp + theme(axis.text.x = element_text(angle=315,hjust=0)) # fix here
temp 

##############################################
# match zooplankton species to Martinez 1990 #
##############################################


