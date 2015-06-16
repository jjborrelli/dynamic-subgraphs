library(dplyr)

# these two data frames should exist
data_TL_phyto_match
data_TL_zoop_match
# they were maded with "matching_empirical_web.R"

#################
# summary stats #
#################

# phyto - by year
ts_stats_phyto_by_year <- 
  data_TL_phyto_match %>%
    group_by(taxon_name_new, year4) %>%
    summarise(
      n_obs = length(concentration),
      avg_conc = mean(concentration),
      sd_conc = sd(concentration),
      cv_conc = sd_conc/avg_conc
      )

# phyto - across all years
ts_stats_phyto_all_yr <- 
  data_TL_phyto_match %>%
    group_by(taxon_name_new) %>%
    summarise(
      n_obs = length(concentration),
      avg_conc = mean(concentration),
      sd_conc = sd(concentration),
      cv_conc = sd_conc/avg_conc
    )

# zoop - by year
ts_stats_zoop_by_year <- 
  data_TL_zoop_match %>%
    group_by(taxon_name_new, year4) %>%
    summarise(
      n_obs = length(density),
      avg_dens = mean(density),
      sd_dens = sd(density),
      cv_dens = sd_dens/avg_dens
    )

# zoop - across all years
ts_stats_zoop_all_yr <- 
  data_TL_zoop_match %>%
    group_by(taxon_name_new) %>%
    summarise(
      n_obs = length(density),
      avg_dens = mean(density),
      sd_dens = sd(density),
      cv_dens = sd_dens/avg_dens
    )


write.csv(ts_stats_phyto_by_year,"time_series_stats/ts_stats_phyto_by_year.csv",row.names=F)
write.csv(ts_stats_phyto_all_yr,"time_series_stats/ts_stats_phyto_all_yr.csv",row.names=F)
write.csv(ts_stats_zoop_by_year,"time_series_stats/ts_stats_zoop_by_year.csv",row.names=F)
write.csv(ts_stats_zoop_all_yr,"time_series_stats/ts_stats_zoop_all_yr.csv",row.names=F)
