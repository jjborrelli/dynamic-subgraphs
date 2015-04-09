# Data from
# https://lter.limnology.wisc.edu/data_TL_phytocatalog/search

library(ggplot2)
library(scales)

data_TL_phyto <- read.csv("./data/cascade_project__phytoplankton.csv")

head(data_TL_phyto)

# subset for Tuesday lake only 
data_TL_phyto <- subset(data_TL_phyto, data_TL_phyto$lakeid=="T")

# how many unique species 
length(unique(data_TL_phyto$taxon_name))
sort(unique(data_TL_phyto$taxon_name))

# How many observations per species 
sort(table(data_TL_phyto$taxon_name))

# convert dates to true dates 
data_TL_phyto$date <- as.Date(data_TL_phyto$sampledate)

# plot all time series
for (i in unique(data_TL_phyto$taxon_name)){

  temp_data <- subset(data_TL_phyto, taxon_name==i)
  
  temp <- ggplot(temp_data, aes(date,concentration)) 
  temp <- temp + geom_point() + geom_line()
  temp <- temp + ylab("Number per liter")
  temp <- temp + xlab("Date")
  temp <- temp + theme_bw(base_size=18)
  temp <- temp + scale_x_date(labels=date_format("%Y"),breaks=date_breaks("year"))
  temp <- temp + theme(axis.text.x = element_text(angle=315,hjust=0)) # fix here
  print(temp)
  
}

# facet by taxon_name 
temp <- ggplot(data_TL_phyto, aes(date,concentration)) 
temp <- temp + geom_point() + geom_line()
temp <- temp + facet_wrap(~taxon_name)
temp <- temp + ylab("Number per liter")
temp <- temp + xlab("Date")
temp <- temp + theme_bw(base_size=18)
temp <- temp + scale_x_date(labels=date_format("%Y"),breaks=date_breaks("year"))
temp <- temp + theme(axis.text.x = element_text(angle=315,hjust=0)) # fix here
temp

