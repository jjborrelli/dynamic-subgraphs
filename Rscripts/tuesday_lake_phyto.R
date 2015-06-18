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

# save the species names to a .csv
phyto_names <- unique(data_TL_phyto$taxon_name)
write.csv(phyto_names, file = "data/TL_phyto_species.csv")

# convert dates to true dates 
data_TL_phyto$date <- as.Date(data_TL_phyto$sampledate)

# plot an individual time series 
# possible response variables are "concentration" and "total_biovol"
temp_data <- subset(data_TL_phyto, taxon_name=="Cryptomonas 2 (<22)")

temp <- ggplot(temp_data, aes(date,concentration)) 
temp <- temp + geom_point() + geom_line()
temp <- temp + ylab("Number per liter")
temp <- temp + xlab("Date")
temp <- temp + theme_bw(base_size=18)
temp <- temp + scale_x_date(labels=date_format("%Y"),breaks=date_breaks("year"))
temp <- temp + theme(axis.text.x = element_text(angle=315,hjust=0)) # fix here
temp

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

# plot & save all time series
for (i in unique(data_TL_phyto$taxon_name)){

  temp_data <- subset(data_TL_phyto, taxon_name==i)
  
  temp <- ggplot(temp_data, aes(date,concentration)) 
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
  
  ggsave(paste("time_series_plots/phyto_TL/",i,".jpg",sep=""),temp)
  
}



