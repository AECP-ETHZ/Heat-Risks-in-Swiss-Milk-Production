# ==================================================================================================================================
# ==================================================================================================================================
#
# Modular code for the publication: Heat Risks in Swiss Milk production
#
# Citation: Bucheli, J., Uldry, M. and Finger, R. 2022. Heat Risks in Swiss Milk production. Journal of the Agricultural and Applied
#           Economics Association.
#
# Part 8.2 /9: Figure for feed purchases (figure 4)
# Note: This codes builds on the other parts.
#
# ==================================================================================================================================
# ==================================================================================================================================

library(lmtest)
library(dplyr)
library(ggplot2)
library(egg)

# Firstly, run script 01

plot_titles <- c("a) complete sample","b) plain sample", "c) hill sample", "d) mountain sample")
load("Meteo/Temperature/farm_T_daily.RData")
colnames(farm_T_hourly)[1] <- "farm"
colnames(farm_T_hourly)[5] <- "Temperature"
load("Models/final_knots_feed.RData")
load("Models/final_model_feed.RData")

sub_panels_ID <- list(good_farms_panel,good_farms_panel_plain, good_farms_panel_hill, good_farms_panel_mountain)

# ---------------------
# Plot for each sample
# ---------------------

Temperature_range <- seq(ceiling(min(farm_T_hourly$Temperature, na.rm=T)), floor(max(farm_T_hourly$Temperature, na.rm=T)))
list_absolut_plots <- list()
list_centered_plots <- list()

for (s in 1:length(plot_titles)){
  
  # Data preparation
  temp_model <- list_final_model_feed[[s]]
  temp_knot  <- list_final_knot_combination_feed[[s]]
  
  sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in% sub_panels_ID[[s]] & dairy_farms_panel_final$sk_Futter_tot > 0),]
  number_farms <- length(unique(sub_dairy_farms_panel_final$farm))
  
  sub_farm_T_hourly <- farm_T_hourly[which(farm_T_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
  T_min <- ceiling(min(sub_farm_T_hourly$Temperature, na.rm=T))
  T_max <- floor(max(sub_farm_T_hourly$Temperature, na.rm=T))
  T_mean <- mean(sub_farm_T_hourly$Temperature, na.rm=T)
  
  if(!is.na(temp_knot)){
  # Calculate absolute effects
  effect_T <- lspline(Temperature_range, knots = temp_knot) %*% coef(temp_model)[1:(length(temp_knot) + 1)]
  effect_error_min <- effect_T - ((lspline(Temperature_range, knots=temp_knot)) %*% (abs(coeftest(temp_model))[,2][1:(length(temp_knot) + 1)]*1.96))
  effect_error_max <- effect_T + ((lspline(Temperature_range, knots=temp_knot)) %*% (abs(coeftest(temp_model))[,2][1:(length(temp_knot) + 1)]*1.96))
  summary_effects_T <- as.data.frame(bind_cols(Temperature_range,effect_T,effect_error_min,effect_error_max))
  colnames(summary_effects_T) <- c("Temperature","Effect","lower_band","upper_band")
  } else {
    effect_T <- (Temperature_range) * coef(temp_model)[1]
    effect_error_min <- effect_T - (Temperature_range * (abs(coeftest(temp_model))[,2][1])*1.96)
    effect_error_max <- effect_T + (Temperature_range * (abs(coeftest(temp_model))[,2][1])*1.96)
    summary_effects_T <- as.data.frame(bind_cols(Temperature_range,effect_T,effect_error_min,effect_error_max))
    colnames(summary_effects_T) <- c("Temperature","Effect","lower_band","upper_band")
  }
  
  calculated_effect <- summary_effects_T[which(summary_effects_T$Temperature >= T_min & summary_effects_T$Temperature <= T_max),]
  projected_effect_low <- summary_effects_T[which(summary_effects_T$Temperature <= T_min),]
  projected_effect_up <- summary_effects_T[which(summary_effects_T$Temperature >= T_max),]
  
  list_absolut_plots[[s]] <- ggplot()+ggtitle(paste(plot_titles[s],"\n", "(",number_farms," farms",")",sep=""))+
    theme(panel.grid.major = element_blank(),
          plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
          plot.subtitle = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
          axis.title = element_text(family="Times New Roman", size=12, colour = "grey25"),
          axis.text = element_text(family="Times New Roman", size=12),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none")+
    
    xlab("Temperature [°C]") + ylab("log(feed expenses) [CHF]")+
    scale_x_continuous(breaks = seq(-20,40, by=10), limits = c(min(Temperature_range),max(Temperature_range)))+
    scale_y_continuous(breaks = c(-0.004,-0.002,0,0.002,0.004), limits = c(-0.0045,0.0045))+
    geom_hline(yintercept=c(-0.004,-0.002,0,0.002,0.004), colour="gray", alpha=0.3)+
    geom_hline(yintercept=c(0), colour="darkgray", size=1.05)+
    {if (!is.na(temp_knot))geom_vline(xintercept=temp_knot, linetype="dotted", colour="darkgray", alpha=0.5, size=0.75)}+
    
    
    # Calculated effect
    geom_line(data=calculated_effect, aes(y=Effect, x=Temperature,), size=1, colour="brown1")+
    geom_line(data=calculated_effect, aes(y=upper_band, x=Temperature,), size=0.5, colour="brown1", alpha=1)+
    geom_line(data=calculated_effect, aes(y=lower_band, x=Temperature,), size=0.5, colour="brown1", alpha=1)+
    geom_ribbon(data=calculated_effect,aes(ymin=lower_band, ymax=upper_band, x=Temperature), fill="brown1",alpha=0.1)+

    
  rm(calculated_effect, projected_effect_low, projected_effect_up)
  
  
  # Calculate centered effects
  if (!is.na(temp_knot)){
    effect_T_centered <- effect_T - as.vector(lspline(T_mean, knots = temp_knot)[1,] %*% coef(temp_model)[1:(length(temp_knot) + 1)])
    effect_error_min_centered <- effect_error_min - as.vector(lspline(T_mean, knots = temp_knot)[1,] %*% coef(temp_model)[1:(length(temp_knot)+1)])
    effect_error_max_centered <- effect_error_max - as.vector(lspline(T_mean, knots = temp_knot)[1,] %*% coef(temp_model)[1:(length(temp_knot)+1)])
  } else {
    effect_T_centered <- effect_T - (T_mean * coef(temp_model)[1])
    effect_error_min_centered <- effect_error_min - (T_mean * coef(temp_model)[1])
    effect_error_max_centered <- effect_error_max - (T_mean * coef(temp_model)[1])
  }
  
  summary_effects_T_centered <- as.data.frame(bind_cols(Temperature_range,effect_T_centered,effect_error_min_centered,effect_error_max_centered))
  colnames(summary_effects_T_centered) <- c("Temperature","Effect","lower_band","upper_band")
  
  
  calculated_effect <- summary_effects_T_centered[which(summary_effects_T_centered$Temperature >= T_min & summary_effects_T_centered$Temperature <= T_max),]
  
  T_mean_df <- as.data.frame(matrix(NA,nrow=1, ncol=2))
  colnames(T_mean_df) <- c("Temperature", "Effect")
  T_mean_df[1,1] <- T_mean
  T_mean_df[1,2] <- 0 
  
  list_centered_plots[[s]] <- ggplot()+ggtitle(paste(plot_titles[s],"\n", "(",number_farms," farms",")",sep=""))+
    theme(panel.grid.major = element_blank(),
          plot.title = element_text(family="Times New Roman", size=9, colour = "grey25", hjust=0.5),
          plot.subtitle = element_text(family="Times New Roman", size=7, colour = "grey25", hjust=0.5),
          axis.title = element_text(family="Times New Roman", size=7, colour = "grey25"),
          axis.text = element_text(family="Times New Roman", size=7),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none")+
    
    xlab("Temperature [°C]") + ylab("log(feed purchases) [CHF]")+
    scale_x_continuous(breaks = seq(5,40, by=10), limits = c(5,max(Temperature_range)))+
    scale_y_continuous(breaks = c(-0.004,-0.002,0,0.002,0.004), limits = c(-0.0045,0.0045))+
    geom_hline(yintercept=c(-0.004,-0.002,0,0.002,0.004), colour="gray", alpha=0.3)+
    geom_hline(yintercept=c(0), colour="darkgray", size=0.5)+
    {if (!is.na(temp_knot))geom_vline(xintercept=temp_knot, linetype="dotted", colour="darkgray", alpha=0.5, size=0.75)}+
    
    # Show the average THI and estimated effect
    geom_point(data=T_mean_df,aes(x=Temperature, y= Effect), colour="darkgray", alpha=1)+
    
    # Calculated effect
    geom_line(data=calculated_effect, aes(y=Effect, x=Temperature,), size=0.5, colour="brown1")+
    geom_line(data=calculated_effect, aes(y=upper_band, x=Temperature,), size=0.25, colour="brown1", alpha=1)+
    geom_line(data=calculated_effect, aes(y=lower_band, x=Temperature,), size=0.25, colour="brown1", alpha=1)+
    geom_ribbon(data=calculated_effect,aes(ymin=lower_band, ymax=upper_band, x=Temperature), fill="brown1",alpha=0.3)
    
}

plot_feed_centered <- ggarrange(list_centered_plots[[1]],list_centered_plots[[2]],list_centered_plots[[3]],list_centered_plots[[4]])
ggsave(plot=plot_feed_centered,"Results(plot)/feed_main.pdf", width = 16, height = 16, unit="cm", device = cairo_pdf)
ggsave(plot=plot_feed_centered,"Results(plot)/feed_main.png", width = 16, height = 9, unit="cm")
