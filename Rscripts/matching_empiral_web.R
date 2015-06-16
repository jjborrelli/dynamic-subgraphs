# subset the data to only include taxa that have a match in the cheddar package 
data_TL_phyto_match <- subset(data_TL_phyto, data_TL_phyto$taxon_name_new %in% sp.names)
data_TL_zoop_match <- subset(data_TL_zoop, data_TL_zoop$taxon_name_new %in% sp.names)

# List of matching taxa (use to double check)
sort(unique(data_TL_phyto_match$taxon_name_new))
sort(unique(data_TL_zoop_match$taxon_name_new))

# use to double check all matches 
sort(sp.names)
sort(unique(data_TL_phyto$taxon_name_new))
sort(unique(data_TL_zoop$taxon_name_new))
