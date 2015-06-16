library(ggplot2)

ts_stats_phyto_by_year
ts_stats_zoop_by_year


phyto_all_yr <- ggplot(ts_stats_phyto_all_yr, aes(x=taxon_name_new,y=cv_conc)) + geom_point()
phyto_all_yr <- phyto_all_yr + theme_classic(base_size=14)
phyto_all_yr <- phyto_all_yr + xlab("Taxa")
phyto_all_yr <- phyto_all_yr + ylab("CV of concentration")
phyto_all_yr <- phyto_all_yr + theme(axis.text.x = element_text(angle = -45, hjust = 0))
phyto_all_yr
ggsave("time_series_stats/plot_phyto_CV_all_yr.jpg",phyto_all_yr,width=8)

zoop_all_yr <- ggplot(ts_stats_zoop_all_yr, aes(x=taxon_name_new,y=cv_dens)) + geom_point()
zoop_all_yr <- zoop_all_yr + theme_classic(base_size=14)
zoop_all_yr <- zoop_all_yr + xlab("Taxa")
zoop_all_yr <- zoop_all_yr + ylab("CV of density")
zoop_all_yr <- zoop_all_yr + theme(axis.text.x = element_text(angle = -45, hjust = 0))
zoop_all_yr
ggsave("time_series_stats/plot_zoop_CV_all_yr.jpg",zoop_all_yr,width=8)

phyto_by_year <- ggplot(ts_stats_phyto_by_year, aes(x=taxon_name_new,y=cv_conc)) + geom_point()
phyto_by_year <- phyto_by_year + theme_classic(base_size=14)
phyto_by_year <- phyto_by_year + xlab("Taxa")
phyto_by_year <- phyto_by_year + ylab("CV of concentration")
phyto_by_year <- phyto_by_year + theme(axis.text.x = element_text(angle = -45, hjust = 0))
phyto_by_year
ggsave("time_series_stats/plot_phyto_CV_by_year.jpg",phyto_by_year,width=8)

zoop_by_year <- ggplot(ts_stats_zoop_by_year, aes(x=taxon_name_new,y=cv_dens)) + geom_point()
zoop_by_year <- zoop_by_year + theme_classic(base_size=14)
zoop_by_year <- zoop_by_year + xlab("Taxa")
zoop_by_year <- zoop_by_year + ylab("CV of density")
zoop_by_year <- zoop_by_year + theme(axis.text.x = element_text(angle = -45, hjust = 0))
zoop_by_year
ggsave("time_series_stats/plot_zoop_CV_by_year.jpg",zoop_by_year,width=8)
