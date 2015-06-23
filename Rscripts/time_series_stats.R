library(dplyr)

# these two data frames should exist
data_TL_phyto_match
data_TL_zoop_match
# they were maded with "matching_empirical_web_TL.R"

# this data frame should exist 
data_LR_zoop
# it was made with "matching_empirical_web_LR.R" 

#################
# summary stats #
# Tuesday Lake  #
#################
# phyto - by year
ts_stats_TL_phyto_by_year <- 
  data_TL_phyto_match %>%
    group_by(taxon_name_new, year4) %>%
    summarise(
      n_obs = length(concentration),
      avg_conc = mean(concentration),
      sd_conc = sd(concentration),
      cv_conc = sd_conc/avg_conc
      )

# phyto - across all years
ts_stats_TL_phyto_all_yr <- 
  data_TL_phyto_match %>%
    group_by(taxon_name_new) %>%
    summarise(
      n_obs = length(concentration),
      avg_conc = mean(concentration),
      sd_conc = sd(concentration),
      cv_conc = sd_conc/avg_conc
    )

# zoop - by year - TL 
ts_stats_TL_zoop_by_year <- 
  data_TL_zoop_match %>%
    group_by(taxon_name_new, year4) %>%
    summarise(
      n_obs = length(density),
      avg_dens = mean(density),
      sd_dens = sd(density),
      cv_dens = sd_dens/avg_dens
    )

# zoop - across all years - TL 
ts_stats_TL_zoop_all_yr <- 
  data_TL_zoop_match %>%
    group_by(taxon_name_new) %>%
    summarise(
      n_obs = length(density),
      avg_dens = mean(density),
      sd_dens = sd(density),
      cv_dens = sd_dens/avg_dens
    )

#################
# summary stats #
# Little Rock   #
#################
# filter to only include station 2 (control)

# zoop - by year - LR 
ts_stats_LR_zoop_by_year <- 
  data_LR_zoop %>%
  filter(station==2 & new_name != "") %>%
  group_by(new_name, year4) %>%
  summarise(
    n_obs = length(number_per_liter),
    avg_conc = mean(number_per_liter),
    sd_conc = sd(number_per_liter),
    cv_conc = sd_conc/avg_conc
  )

# zoop - across all years - LR
ts_stats_LR_zoop_all_yr <- 
  data_LR_zoop %>%
  filter(station==2 & new_name != "") %>%
  group_by(new_name) %>%
  summarise(
    n_obs = length(number_per_liter),
    avg_conc = mean(number_per_liter),
    sd_conc = sd(number_per_liter),
    cv_conc = sd_conc/avg_conc
  )

################
# save results # 
################ 
# TL 
write.csv(ts_stats_TL_phyto_by_year,"time_series_stats/ts_stats_TL_phyto_by_year.csv",row.names=F)
write.csv(ts_stats_TL_phyto_all_yr,"time_series_stats/ts_stats_TL_phyto_all_yr.csv",row.names=F)
write.csv(ts_stats_TL_zoop_by_year,"time_series_stats/ts_stats_TL_zoop_by_year.csv",row.names=F)
write.csv(ts_stats_TL_zoop_all_yr,"time_series_stats/ts_stats_TL_zoop_all_yr.csv",row.names=F)

# LR 
write.csv(ts_stats_LR_zoop_by_year,"time_series_stats/ts_stats_LR_zoop_by_year.csv",row.names=F)
write.csv(ts_stats_LR_zoop_all_yr,"time_series_stats/ts_stats_LR_zoop_all_yr.csv",row.names=F)
