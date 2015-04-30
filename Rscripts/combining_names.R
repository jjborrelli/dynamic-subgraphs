# vectors for taxa names 
# (these should exist for the next steps)
sp.names # from cheddar webs
phyto_names # empirical phytoplankton 
zoop_names # empirical zooplankton 

# save both empirical phyto and zoop names to a single .csv 
# Use for working on manually resolving names in Excel 
empirical_names <- data.frame(name=c(as.character(phyto_names),as.character(zoop_names)), 
                              type=c(rep("phyto",length(phyto_names)),rep("zoop",length(zoop_names))))

write.csv(empirical_names, file = "data/TL_empirical_species.csv")

#########################################################
# Get a view at some of the taxa that I want to combine #
#########################################################

# convert to a data.table  
library(data.table)
data_table_phyto <- data.table(data_TL_phyto)
data_table_zoop <- data.table(data_TL_zoop)

# these are taxa that I IDed "by hand" that might be good to combine 

# return all rows in empirical data that partially match the string 
# phyto 
data_table_phyto[taxon_name %like% "Arthrodesmus"]
data_table_phyto[taxon_name %like% "Chrysosphaerella"]
data_table_phyto[taxon_name %like% "Cosmarium"]
data_table_phyto[taxon_name %like% "Cryptomonas"]
data_table_phyto[taxon_name %like% "Dinobryon"]
data_table_phyto[taxon_name %like% "Gloeocystis"]
data_table_phyto[taxon_name %like% "Mallomonaas"]
data_table_phyto[taxon_name %like% "Microcystis"]
data_table_phyto[taxon_name %like% "Oocystis"]
data_table_phyto[taxon_name %like% "Peridinium"]
data_table_phyto[taxon_name %like% "Quadrigula"]
data_table_phyto[taxon_name %like% "Selenastrum"]
data_table_phyto[taxon_name %like% "Staurastrum"]
data_table_phyto[taxon_name %like% "Synura"]

# zoop
data_table_zoop[taxon_name %like% "Bosmina"]
data_table_zoop[taxon_name %like% "Kellicotia"]


# the list of unique phyto names that might be combined 
# (sorry the last line is so long. it had problems when it was split)
matching_phyto_names <- sort(as.character(unique(data_TL_phyto$taxon_name[grep("Arthrodesmus|Chrysosphaerella|Cosmarium|Cryptomonas|Dinobryon|Gloeocystis|Mallomonaas|Microcystis|Oocystis|Peridinium|Quadrigula|Selenastrum|Staurastrum|Synura", data_TL_phyto$taxon_name)])))

# plot em 
for (i in matching_phyto_names){
  
  temp_data <- subset(data_TL_phyto, taxon_name==i)
  
  temp <- ggplot(temp_data, aes(date,concentration)) 
  temp <- temp + geom_point() + geom_line()
  temp <- temp + ylab("Number per liter")
  temp <- temp + xlab("Date")
  temp <- temp + theme_bw(base_size=18)
  temp <- temp + ggtitle(i)
  temp <- temp + theme(axis.text.x = element_text(angle=315,hjust=0)) # fix here
  print(temp)
  
}

# the list of unique phyto names that might be combined 
matching_zoop_names <- sort(as.character(unique(data_TL_zoop$taxon_name[grep("Bosmina|Kellicotia", data_TL_zoop$taxon_name)])))

# plot em 
for (i in matching_zoop_names){
  
  temp_data <- subset(data_TL_zoop, taxon_name==i)
  
  temp <- ggplot(temp_data, aes(date,density)) 
  temp <- temp + geom_point() + geom_line()
  temp <- temp + ylab("Number per liter")
  temp <- temp + xlab("Date")
  temp <- temp + theme_bw(base_size=18)
  temp <- temp + ggtitle(i)
  temp <- temp + theme(axis.text.x = element_text(angle=315,hjust=0)) # fix here
  print(temp)
  
}



# more detailed looks @ specific taxa 
data_TL_zoop[data_TL_zoop$taxon_name=="Bosmina",] # only occurs in 2002 
data_TL_zoop[data_TL_zoop$taxon_name=="Bosmina longirostris",] # occurs in multiple years (not 2002)

data_TL_phyto[data_TL_phyto$taxon_name=="Arthrodesmus sp.",] # only occurs in 1988
data_TL_phyto[data_TL_phyto$taxon_name=="Arthrodesmus incus",] # only occurs in 1990
data_TL_phyto[data_TL_phyto$taxon_name=="Arthrodesmus octocornis",] # only occurs in 1988
data_TL_phyto[data_TL_phyto$taxon_name=="Arthrodesmus subulatus",] # occurs in 1988, 1990 

data_TL_phyto[data_TL_phyto$taxon_name=="Chrysosphaerella longispina (colonial)",] # occurs in 1988-1990
data_TL_phyto[data_TL_phyto$taxon_name=="Chrysosphaerella sp.",] # occurs in 1984 - 1990

data_TL_phyto[data_TL_phyto$taxon_name=="Cosmarium sp.",] # occurs in 1988-1989
data_TL_phyto[data_TL_phyto$taxon_name=="Cosmarium sp. (spiny)",] # occurs in 1984 

data_TL_phyto[data_TL_phyto$taxon_name=="Cryptomonas 1 (<14)",] # occurs 1984-1989
data_TL_phyto[data_TL_phyto$taxon_name=="Cryptomonas 2 (<22)",] # occurs 1984-1989
data_TL_phyto[data_TL_phyto$taxon_name=="Cryptomonas caudata",] # 1990
data_TL_phyto[data_TL_phyto$taxon_name=="Cryptomonas marsonii",] # 1985, 1986 (not a lot of data)
data_TL_phyto[data_TL_phyto$taxon_name=="Cryptomonas ovata",] # 1985-1989

data_TL_phyto[data_TL_phyto$taxon_name=="Gloeocystis ampla (ovoid)",] # 1985,1986,1989
data_TL_phyto[data_TL_phyto$taxon_name=="Gloeocystis planctonica",] # 1986

data_TL_phyto[data_TL_phyto$taxon_name=="Oocystis sp. 1",]
data_TL_phyto[data_TL_phyto$taxon_name=="Oocystis sp. 2",]

data_TL_phyto[data_TL_phyto$taxon_name=="Selenastrum minutum",]
data_TL_phyto[data_TL_phyto$taxon_name=="Selenastrum sp.",] # only in 1988
