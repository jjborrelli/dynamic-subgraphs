library(ggplot2)

################
# Tuesday Lake # 
################
phyto_TL_all_yr <- ggplot(ts_stats_TL_phyto_all_yr, aes(x=taxon_name_new,y=cv_conc)) + geom_point()
phyto_TL_all_yr <- phyto_TL_all_yr + theme_classic(base_size=14)
phyto_TL_all_yr <- phyto_TL_all_yr + xlab("Taxa")
phyto_TL_all_yr <- phyto_TL_all_yr + ylab("CV of concentration")
phyto_TL_all_yr <- phyto_TL_all_yr + theme(axis.text.x = element_text(angle = -45, hjust = 0))
phyto_TL_all_yr
ggsave("time_series_stats/plot_phyto_TL_CV_all_yr.jpg",phyto_TL_all_yr,width=8)

zoop_TL_all_yr <- ggplot(ts_stats_TL_zoop_all_yr, aes(x=taxon_name_new,y=cv_dens)) + geom_point()
zoop_TL_all_yr <- zoop_TL_all_yr + theme_classic(base_size=14)
zoop_TL_all_yr <- zoop_TL_all_yr + xlab("Taxa")
zoop_TL_all_yr <- zoop_TL_all_yr + ylab("CV of density")
zoop_TL_all_yr <- zoop_TL_all_yr + theme(axis.text.x = element_text(angle = -45, hjust = 0))
zoop_TL_all_yr
ggsave("time_series_stats/plot_zoop_TL_CV_all_yr.jpg",zoop_TL_all_yr,width=8)

phyto_TL_by_year <- ggplot(ts_stats_TL_phyto_by_year, aes(x=taxon_name_new,y=cv_conc)) + geom_point()
phyto_TL_by_year <- phyto_TL_by_year + theme_classic(base_size=14)
phyto_TL_by_year <- phyto_TL_by_year + xlab("Taxa")
phyto_TL_by_year <- phyto_TL_by_year + ylab("CV of concentration")
phyto_TL_by_year <- phyto_TL_by_year + theme(axis.text.x = element_text(angle = -45, hjust = 0))
phyto_TL_by_year
ggsave("time_series_stats/plot_phyto_TL_CV_by_year.jpg",phyto_TL_by_year,width=8)

zoop_TL_by_year <- ggplot(ts_stats_TL_zoop_by_year, aes(x=taxon_name_new,y=cv_dens)) + geom_point()
zoop_TL_by_year <- zoop_TL_by_year + theme_classic(base_size=14)
zoop_TL_by_year <- zoop_TL_by_year + xlab("Taxa")
zoop_TL_by_year <- zoop_TL_by_year + ylab("CV of density")
zoop_TL_by_year <- zoop_TL_by_year + theme(axis.text.x = element_text(angle = -45, hjust = 0))
zoop_TL_by_year
ggsave("time_series_stats/plot_zoop_TL_CV_by_year.jpg",zoop_TL_by_year,width=8)

####################
# Little Rock Lake # 
####################
zoop_LR_all_yr <- ggplot(ts_stats_LR_zoop_all_yr, aes(x=new_name,y=cv_conc)) + geom_point()
zoop_LR_all_yr <- zoop_LR_all_yr + theme_classic(base_size=14)
zoop_LR_all_yr <- zoop_LR_all_yr + xlab("Taxa")
zoop_LR_all_yr <- zoop_LR_all_yr + ylab("CV of concentration")
zoop_LR_all_yr <- zoop_LR_all_yr + theme(axis.text.x = element_text(angle = -45, hjust = 0))
zoop_LR_all_yr
ggsave("time_series_stats/plot_zoop_LR_CV_all_yr.jpg",zoop_LR_all_yr,width=8)

zoop_LR_by_year <- ggplot(ts_stats_LR_zoop_by_year, aes(x=new_name,y=cv_conc)) + geom_point()
zoop_LR_by_year <- zoop_LR_by_year + theme_classic(base_size=14)
zoop_LR_by_year <- zoop_LR_by_year + xlab("Taxa")
zoop_LR_by_year <- zoop_LR_by_year + ylab("CV of concentration")
zoop_LR_by_year <- zoop_LR_by_year + theme(axis.text.x = element_text(angle = -45, hjust = 0))
zoop_LR_by_year
ggsave("time_series_stats/plot_zoop_LR_CV_by_year.jpg",zoop_LR_by_year,width=8)

##############
# Chesapeake # 
##############
data_chesap <- read.csv("./time_series_stats/ts_stats_chesapeake.csv")

chesap_cv_plot <- ggplot(data_chesap, aes(x=taxa,y=cv)) + geom_point()
chesap_cv_plot <- chesap_cv_plot + theme_classic(base_size=14)
chesap_cv_plot <- chesap_cv_plot + xlab("Taxa")
chesap_cv_plot <- chesap_cv_plot + ylab("CV")
chesap_cv_plot <- chesap_cv_plot + theme(axis.text.x = element_text(angle = -45, hjust = 0))
chesap_cv_plot

ggsave("time_series_stats/plot_chesapeake_CV.jpg",chesap_cv_plot,width=8)
