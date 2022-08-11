# ==================================================================================================================================
# ==================================================================================================================================
#
# Modular code for the publication: Heat Risks in Swiss Milk production
#
# Citation: Bucheli, J., Uldry, M. and Finger, R. 2022. Heat Risks in Swiss Milk production. Journal of the Agricultural and Applied
#           Economics Association.
#
# Part 9/9: Figures with descriptive statistics
#
# ==================================================================================================================================
# ==================================================================================================================================

library(ggplot2)
library(tidyverse)
library(ggthemes)
library(egg)
library(cowplot)

plot_titles <- c("a) complete sample","b) plain sample", "c) hill sample", "d) mountain sample")
sub_panels_ID <- list(good_farms_panel,good_farms_panel_plain, good_farms_panel_hill, good_farms_panel_mountain)

# ------------------------------------------------------------------------------------------------
# Accountancy data
# ------------------------------------------------------------------------------------------------

dairy_farms_panel_final$year <- as.factor(dairy_farms_panel_final$year)

boxplots_revenues <- list()
for (i in 1:length(sub_panels_ID)){
  
  temp1 <- dairy_farms_panel_final[dairy_farms_panel_final$farm %in% sub_panels_ID[[i]],]
  temp1$rohMilch <- temp1$rohMilch / 1000  
  
  boxplots_revenues[[i]] <- ggplot(temp1,aes(x=year, y=rohMilch))+
                            theme_hc()+
                          
                            theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90),
                                  axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                                  axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
                                  text = element_text(size=8))+
                                  
                            theme(plot.title = element_text(size=9))+
                            
                            scale_y_continuous(limits = c(0, 600),breaks=seq(0,600,100))+
                            stat_boxplot(geom ='errorbar',width = 0.2)+
 
                      
                            labs(title= plot_titles[i], x="year", y="milk revenues [in 1'000 CHF]")+
                            geom_boxplot(outlier.colour="gray30", outlier.shape=16,
                                         outlier.size=0.5, color="black", fill="gray")
  rm(temp1)
  
}
plot_desc_rev <- plot_grid(boxplots_revenues[[1]],boxplots_revenues[[2]],boxplots_revenues[[3]],boxplots_revenues[[4]])
ggsave(plot=plot_desc_rev,"stats(plot)/rev_boxplots.png", width = 16, height = 20, unit="cm")


boxplots_vet <- list()
for (i in 1:length(sub_panels_ID)){
  
  temp1 <- dairy_farms_panel_final[dairy_farms_panel_final$farm %in% sub_panels_ID[[i]] & dairy_farms_panel_final$sk_Tierarzt > 0,]
  temp1$sk_Tierarzt <- temp1$sk_Tierarzt / 1000  
  
  boxplots_vet[[i]] <- ggplot(temp1,aes(x=year, y=sk_Tierarzt))+
    theme_hc()+
    
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
          text = element_text(size=8))+
    
    theme(plot.title = element_text(size=9))+
    
    scale_y_continuous(limits = c(0, 40),breaks=seq(0,40,10))+
    stat_boxplot(geom ='errorbar',width = 0.2)+
    
    
    labs(title= plot_titles[i], x="year", y="veterinary expenses [in 1'000 CHF]")+
    geom_boxplot(outlier.colour="gray30", outlier.shape=16,
                 outlier.size=0.5, color="black", fill="gray")
  rm(temp1)
  
}
plot_desc_vet <- plot_grid(boxplots_vet[[1]],boxplots_vet[[2]],boxplots_vet[[3]],boxplots_vet[[4]])
ggsave(plot=plot_desc_vet,"stats(plot)/vet_boxplots.png", width = 16, height = 20, unit="cm")



boxplots_feed <- list()
for (i in 1:length(sub_panels_ID)){
  
  temp1 <- dairy_farms_panel_final[dairy_farms_panel_final$farm %in% sub_panels_ID[[i]] & dairy_farms_panel_final$sk_Futter_tot > 0,]
  temp1$sk_Futter_tot <- temp1$sk_Futter_tot / 1000  
  
  boxplots_feed[[i]] <- ggplot(temp1,aes(x=year, y=sk_Futter_tot))+
    theme_hc()+
    
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
          text = element_text(size=8))+
    
    theme(plot.title = element_text(size=9))+
    
    scale_y_continuous(limits = c(0, 220),breaks=seq(0,220,50))+
    stat_boxplot(geom ='errorbar',width = 0.2)+
    
    
    labs(title= plot_titles[i], x="year", y="feed purchases [in 1'000 CHF]")+
    geom_boxplot(outlier.colour="gray30", outlier.shape=16,
                 outlier.size=0.5, color="black", fill="gray")
  rm(temp1)
  
}
plot_desc_feed <- plot_grid(boxplots_feed[[1]],boxplots_feed[[2]],boxplots_feed[[3]],boxplots_feed[[4]])
ggsave(plot=plot_desc_feed,"stats(plot)/feed_boxplots.png", width = 16, height = 20, unit="cm")

# ------------------------------------------------------------------------------------------------
# Farm characteristics
# ------------------------------------------------------------------------------------------------

boxplots_land <- list()
for (i in 1:length(sub_panels_ID)){
  
  temp1 <- dairy_farms_panel_final[dairy_farms_panel_final$farm %in% sub_panels_ID[[i]],]
  
  boxplots_land[[i]] <- ggplot(temp1,aes(x=year, y=LN))+
    theme_hc()+
    
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
          text = element_text(size=8))+
    
    theme(plot.title = element_text(size=9))+
    
    scale_y_continuous(limits = c(0, 160),breaks=seq(0,160,40))+
    stat_boxplot(geom ='errorbar',width = 0.2)+
    
    
    labs(title= plot_titles[i], x="year", y="agricultural land [hectare]")+
    geom_boxplot(outlier.colour="gray30", outlier.shape=16,
                 outlier.size=0.5, color="black", fill="gray")
  rm(temp1)
  
}
plot_desc_land <- plot_grid(boxplots_land[[1]],boxplots_land[[2]],boxplots_land[[3]],boxplots_land[[4]])
ggsave(plot=plot_desc_land,"stats(plot)/land_boxplots.png", width = 16, height = 20, unit="cm")

boxplots_cow <- list()
for (i in 1:length(sub_panels_ID)){
  
  temp1 <- dairy_farms_panel_final[dairy_farms_panel_final$farm %in% sub_panels_ID[[i]],]
  
  boxplots_cow[[i]] <- ggplot(temp1,aes(x=year, y=Stk_Milchkuehe))+
    theme_hc()+
    
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
          text = element_text(size=8))+
    
    theme(plot.title = element_text(size=9))+
    
    scale_y_continuous(limits = c(0, 120),breaks=seq(0,120,30))+
    stat_boxplot(geom ='errorbar',width = 0.2)+
    
    
    labs(title= plot_titles[i], x="year", y="number of milk cows")+
    geom_boxplot(outlier.colour="gray30", outlier.shape=16,
                 outlier.size=0.5, color="black", fill="gray")
  rm(temp1)
  
}
plot_desc_cow <- plot_grid(boxplots_cow[[1]],boxplots_cow[[2]],boxplots_cow[[3]],boxplots_cow[[4]])
ggsave(plot=plot_desc_cow,"stats(plot)/cow_boxplots.png", width = 16, height = 20, unit="cm")

boxplots_productivity <- list()
for (i in 1:length(sub_panels_ID)){
  
  temp1 <- dairy_farms_panel_final[dairy_farms_panel_final$farm %in% sub_panels_ID[[i]],]
  
  boxplots_productivity[[i]] <- ggplot(temp1,aes(x=year, y=kgMilch_jeKuh))+
    theme_hc()+
    
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
          text = element_text(size=8))+
    
    theme(plot.title = element_text(size=9))+
    
    scale_y_continuous(limits = c(0, 15200),breaks=seq(0,15000,3000))+
    stat_boxplot(geom ='errorbar',width = 0.2)+
    
    
    labs(title= plot_titles[i], x="year", y="milk production per cow [kg]")+
    geom_boxplot(outlier.colour="gray30", outlier.shape=16,
                 outlier.size=0.5, color="black", fill="gray")
  rm(temp1)
  
}
plot_desc_productivity <- plot_grid(boxplots_productivity[[1]],boxplots_productivity[[2]],boxplots_productivity[[3]],boxplots_productivity[[4]])
ggsave(plot=plot_desc_productivity,"stats(plot)/productivity_boxplots.png", width = 16, height = 20, unit="cm")

# ------------------------------------------------------------------------------------------------
# Weather data
# ------------------------------------------------------------------------------------------------

load("Meteo/Temperature/farm_T_daily.RData")
load("Meteo/THI/THI_farm.RData")
load("Meteo/Precipitation/farm_precip_daily.RData")
colnames(farm_T_hourly)[5] <- "Temperature"

# -----------------------------------
# Cumulative precipitation
# -----------------------------------

# complete sample
sub_farm_precip_daily <- farm_precip_daily[which(farm_precip_daily$farm %in% good_farms_panel),]
temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)
temp_CP$year <- as.factor(temp_CP$year)
rm (sub_farm_precip_daily)

# plain sample
sub_farm_precip_daily <- farm_precip_daily[which(farm_precip_daily$farm %in% good_farms_panel_plain),]
temp_CP_plain <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)
temp_CP_plain$year <- as.factor(temp_CP_plain$year)
rm (sub_farm_precip_daily)

# hill sample
sub_farm_precip_daily <- farm_precip_daily[which(farm_precip_daily$farm %in% good_farms_panel_hill),]
temp_CP_hill <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)
temp_CP_hill$year <- as.factor(temp_CP_hill$year)
rm (sub_farm_precip_daily)

# mountain sample
sub_farm_precip_daily <- farm_precip_daily[which(farm_precip_daily$farm %in% good_farms_panel_mountain),]
temp_CP_mountain <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)
temp_CP_mountain$year <- as.factor(temp_CP_mountain$year)
rm (sub_farm_precip_daily)

# boxplots
boxplots_CP <- list()
for (i in 1:length(sub_panels_ID)){
  
  temp1 <- temp_CP[temp_CP$farm %in% sub_panels_ID[[i]], ]
  
  boxplots_CP[[i]] <- ggplot(temp1,aes(x=year, y=precip))+
    theme_hc()+
    
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
          text = element_text(size=8))+
    
    theme(plot.title = element_text(size=9))+
    
    scale_y_continuous(limits = c(0, 1700),breaks=seq(0,1800,300))+
    stat_boxplot(geom ='errorbar',width = 0.2)+
    
    
    labs(title= plot_titles[i], x="year", y="Cumulative precipitation [mm]")+
    geom_boxplot(outlier.colour="gray30", outlier.shape=16,
                 outlier.size=0.5, color="black", fill="gray")
  rm(temp1)
  
}
plot_desc_CP <- plot_grid(boxplots_CP[[1]],boxplots_CP[[2]],boxplots_CP[[3]],boxplots_CP[[4]])
ggsave(plot=plot_desc_CP,"stats(plot)/CP_boxplots.png", width = 16, height = 20, unit="cm")

# -----------------------------------
# THI
# -----------------------------------

sub_farm_THI_hourly <- farm_THI_hourly[which(farm_THI_hourly$.id %in% good_farms_panel),]
sub_farm_THI_hourly_plain <- farm_THI_hourly[which(farm_THI_hourly$.id %in% good_farms_panel_plain),]
sub_farm_THI_hourly_hill <- farm_THI_hourly[which(farm_THI_hourly$.id %in% good_farms_panel_hill),]
sub_farm_THI_hourly_mountain <- farm_THI_hourly[which(farm_THI_hourly$.id %in% good_farms_panel_mountain),]

sub_farm_THI_hourly$year <- as.factor(sub_farm_THI_hourly$year)

boxplots_THI <- list()
for (i in 1:length(sub_panels_ID)){
  
  temp1 <- sub_farm_THI_hourly[sub_farm_THI_hourly$.id %in% sub_panels_ID[[i]] & !is.na(sub_farm_THI_hourly$THI), ]
  
  boxplots_THI[[i]] <- ggplot(temp1,aes(x=year, y=THI))+
    theme_hc()+
    
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
          text = element_text(size=8))+
    
    theme(plot.title = element_text(size=9))+
    
    scale_y_continuous(limits = c(-20, 100),breaks=seq(-20,100,30))+
    stat_boxplot(geom ='errorbar',width = 0.2)+
    
    
    labs(title= plot_titles[i], x="year", y="Hourly THI [unitless]")+
    geom_boxplot(outlier.colour="gray30", outlier.shape=16,
                 outlier.size=0.5, color="black", fill="gray")
  rm(temp1)
  
}
plot_desc_THI <- plot_grid(boxplots_THI[[1]],boxplots_THI[[2]],boxplots_THI[[3]],boxplots_THI[[4]])
ggsave(plot=plot_desc_THI,"stats(plot)/THI_boxplots.png", width = 16, height = 20, unit="cm")

# -----------------------------------
# Temperature
# -----------------------------------
sub_farm_T_hourly <- farm_T_hourly[which(farm_T_hourly$farm %in% good_farms_panel),]
sub_farm_T_hourly_plain <- farm_T_hourly[which(farm_T_hourly$farm %in% good_farms_panel_plain),]
sub_farm_T_hourly_hill <- farm_T_hourly[which(farm_T_hourly$farm %in% good_farms_panel_hill),]
sub_farm_T_hourly_mountain <- farm_T_hourly[which(farm_T_hourly$farm %in% good_farms_panel_mountain),]

sub_farm_T_hourly$year <- as.factor(sub_farm_T_hourly$year)
colnames(sub_farm_T_hourly)[5] <- "Temperature"

boxplots_T <- list()
for (i in 1:length(sub_panels_ID)){
  
  temp1 <- sub_farm_T_hourly[sub_farm_T_hourly$farm %in% sub_panels_ID[[i]] & !is.na(sub_farm_T_hourly$Temperature), ]
  
  boxplots_T[[i]] <- ggplot(temp1,aes(x=year, y=Temperature))+
    theme_hc()+
    
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
          text = element_text(size=8))+
    
    theme(plot.title = element_text(size=9))+
    
    scale_y_continuous(limits = c(-30, 40),breaks=seq(-30,40,10))+
    stat_boxplot(geom ='errorbar',width = 0.2)+
    
    
    labs(title= plot_titles[i], x="year", y="Hourly Temperature [°C]")+
    geom_boxplot(outlier.colour="gray30", outlier.shape=16,
                 outlier.size=0.5, color="black", fill="gray")
  rm(temp1)
  
}
plot_desc_T <- plot_grid(boxplots_T[[1]],boxplots_T[[2]],boxplots_T[[3]],boxplots_T[[4]])
ggsave(plot=plot_desc_T,"stats(plot)/T_boxplots.png", width = 16, height = 20, unit="cm")
