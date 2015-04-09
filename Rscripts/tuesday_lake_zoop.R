# Data from
# https://lter.limnology.wisc.edu/data_TL_zoopcatalog/search

library(ggplot2)
library(scales)

data_TL_zoop <- read.csv("./data/cascade_project__zooplankton.csv")

head(data_TL_zoop)

# subset for Tuesday lake only 
data_TL_zoop <- subset(data_TL_zoop, data_TL_zoop$lakeid=="T")

# how many unique species 
length(unique(data_TL_zoop$taxon_name))
sort(unique(data_TL_zoop$taxon_name))

# How many observations per species 
sort(table(data_TL_zoop$taxon_name))

# convert dates to true dates 
data_TL_zoop$date <- as.Date(data_TL_zoop$sampledate)

# plot a time series
for (i in unique(data_TL_zoop$taxon_name)){

  temp_data <- subset(data_TL_zoop, taxon_name==i)
  
  temp <- ggplot(temp_data, aes(date,density)) 
  temp <- temp + geom_point() + geom_line()
  temp <- temp + ylab("Number per liter")
  temp <- temp + xlab("Date")
  temp <- temp + theme_bw(base_size=18)
  temp <- temp + scale_x_date(labels=date_format("%Y"),breaks=date_breaks("year"))
  temp <- temp + theme(axis.text.x = element_text(angle=315,hjust=0)) # fix here
  print(temp)
  
}




