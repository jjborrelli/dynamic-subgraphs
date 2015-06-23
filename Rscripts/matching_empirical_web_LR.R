library(dplyr)

# import data 
lr.zoop <- read.csv("./data/LR_zoop_species.csv", row.names = 1)
lr.spp <- read.csv("./data/LittleRockSpeciesList.csv")

# convert some things to character 
lr.spp$Species <- as.character(lr.spp$Species)

# fix "nf" to "sp." 
lr.spp$Species[lr.spp$Species=="nf" & lr.spp$Genus!="nf"] <- "sp."

# Add a genus,species column 
lr.spp$taxa <- paste(lr.spp$Genus, lr.spp$Species, sep = " ")

# sort zooplankton alphabetically
lr.zoop$x <- sort(lr.zoop$x)

# write .csv files for manual matching
write.csv(lr.spp,"data/matching_LR_web.csv",row.names=F)
write.csv(lr.zoop,"data/matching_LR_empirical.csv",row.names=F)

#### GO MAKE EDITS MANUALLY IN EXCEL ####

# re-write names according to new names 
zoop_LR_new_names <- read.csv("data/matching_LR_empirical.csv")

# fix a name 
names(zoop_LR_new_names)[1] <- "species"

# merge new names with existing empirical zoop data (data_LR_zoop)
data_LR_zoop <- left_join(data_LR_zoop, zoop_LR_new_names, by="species")
