# ==================================================================================================================================
# ==================================================================================================================================
#
# Modular code for the publication: Heat Risks in Swiss Milk production
#
# Citation: Bucheli, J., Uldry, M. and Finger, R. 2022. Heat Risks in Swiss Milk production. Journal of the Agricultural and Applied
#           Economics Association.
#
# Part 7.2 /9: Figure for veterinary expenses (figure 3)
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
load("Meteo/THI/THI_farm.RData")
colnames(farm_THI_hourly)[1] <- "farm"
load("Models/final_knots_vet.RData")
load("Models/final_model_vet.RData")

sub_panels_ID <- list(good_farms_panel,good_farms_panel_plain, good_farms_panel_hill, good_farms_panel_mountain)

# ---------------------
# Plot for each sample
# ---------------------

THI_range <- seq(ceiling(min(farm_THI_hourly$THI, na.rm=T)), floor(max(farm_THI_hourly$THI, na.rm=T)))
list_absolut_plots <- list()
list_centered_plots <- list()

for (s in 1:length(plot_titles)){
  
  # Data preparation
  temp_model <- list_final_model_vet[[s]]
  temp_knot  <- list_final_knot_combination_vet[[s]]
  
  sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in% sub_panels_ID[[s]] & dairy_farms_panel_final$sk_Tierarzt > 0),]
  number_farms <- length(unique( sub_dairy_farms_panel_final$farm))
  
  sub_farm_THI_hourly <- farm_THI_hourly[which(farm_THI_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
  THI_min <- ceiling(min(sub_farm_THI_hourly$THI, na.rm=T))
  THI_max <- floor(max(sub_farm_THI_hourly$THI, na.rm=T))
  THI_mean <- mean(sub_farm_THI_hourly$THI, na.rm=T)
  
  # Calculate absolute effects
  if(!is.na(temp_knot)){
  effect_THI <- lspline(THI_range, knots = temp_knot) %*% coef(temp_model)[1:(length(temp_knot) + 1)]
  effect_error_min <- effect_THI - ((lspline(THI_range, knots=temp_knot)) %*% (abs(coeftest(temp_model))[,2][1:(length(temp_knot) + 1)]*1.96))
  effect_error_max <- effect_THI + ((lspline(THI_range, knots=temp_knot)) %*% (abs(coeftest(temp_model))[,2][1:(length(temp_knot) + 1)]*1.96))
  summary_effects_THI <- as.data.frame(bind_cols(THI_range,effect_THI,effect_error_min,effect_error_max))
  colnames(summary_effects_THI) <- c("THI","Effect","lower_band","upper_band")
  } else {
    effect_THI <- (THI_range) * coef(temp_model)[1]
    effect_error_min <- effect_THI - (THI_range * (abs(coeftest(temp_model))[,2][1])*1.96)
    effect_error_max <- effect_THI + (THI_range * (abs(coeftest(temp_model))[,2][1])*1.96)
    summary_effects_THI <- as.data.frame(bind_cols(THI_range,effect_THI,effect_error_min,effect_error_max))
    colnames(summary_effects_THI) <- c("THI","Effect","lower_band","upper_band")
  }
  
  
  calculated_effect <- summary_effects_THI[which(summary_effects_THI$THI >= THI_min & summary_effects_THI$THI <= THI_max),]
  
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
    
    xlab("THI") + ylab("log(veterinary expenses) [CHF]")+
    scale_x_continuous(breaks = seq(-10,90, by=20), limits = c(min(THI_range),max(THI_range)))+
    scale_y_continuous(breaks = c(-0.003,-0.002,-0.001,0,0.001,0.002,0.003), limits = c(-0.004,0.004))+
    geom_hline(yintercept=c(-0.003,-0.002,-0.001,0,0.001,0.002,0.003), colour="gray", alpha=0.3)+
    geom_hline(yintercept=c(0), colour="darkgray", size=1.05)+
    geom_vline(xintercept=temp_knot, linetype="dotted", colour="darkgray", alpha=0.5, size=0.75)+
    
    # Calculated effect
    geom_line(data=calculated_effect, aes(y=Effect, x=THI,), size=1, colour="brown1")+
    geom_line(data=calculated_effect, aes(y=upper_band, x=THI,), size=0.5, colour="brown1", alpha=1)+
    geom_line(data=calculated_effect, aes(y=lower_band, x=THI,), size=0.5, colour="brown1", alpha=1)+
    geom_ribbon(data=calculated_effect,aes(ymin=lower_band, ymax=upper_band, x=THI), fill="brown1",alpha=0.1)+
 
    
  rm(calculated_effect, projected_effect_low, projected_effect_up)
  
  
  # Calculate centered effects
  if(!is.na(temp_knot)){
  effect_THI_centered <- effect_THI - as.vector(lspline(THI_mean, knots = temp_knot)[1,] %*% coef(temp_model)[1:(length(temp_knot) + 1)])
  effect_error_min_centered <- effect_error_min - as.vector(lspline(THI_mean, knots = temp_knot)[1,] %*% coef(temp_model)[1:(length(temp_knot)+1)])
  effect_error_max_centered <- effect_error_max - as.vector(lspline(THI_mean, knots = temp_knot)[1,] %*% coef(temp_model)[1:(length(temp_knot)+1)])
  } else {
    effect_THI_centered <- effect_THI - (THI_mean * coef(temp_model)[1])
    effect_error_min_centered <- effect_error_min - (THI_mean * coef(temp_model)[1])
    effect_error_max_centered <- effect_error_max - (THI_mean * coef(temp_model)[1])
  }
  
  summary_effects_THI_centered <- as.data.frame(bind_cols(THI_range,effect_THI_centered,effect_error_min_centered,effect_error_max_centered))
  colnames(summary_effects_THI_centered) <- c("THI","Effect","lower_band","upper_band")
  
  
  calculated_effect <- summary_effects_THI_centered[which(summary_effects_THI_centered$THI >= THI_min & summary_effects_THI_centered$THI <= THI_max),]
  
  THI_mean_df <- as.data.frame(matrix(NA,nrow=1, ncol=2))
  colnames(THI_mean_df) <- c("THI", "Effect")
  THI_mean_df[1,1] <- THI_mean
  THI_mean_df[1,2] <- 0
  
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
    
    xlab("THI") + ylab("log(veterinary expenses) [CHF]")+
    scale_x_continuous(breaks = seq(40,90, by=10), limits = c(40,max(THI_range)))+
    scale_y_continuous(breaks = c(-0.004,-0.002,0,0.002,0.004), limits = c(-0.0045,0.0045))+
    geom_hline(yintercept=c(-0.004,-0.002,0,0.002,0.004), colour="gray", alpha=0.3)+
    geom_hline(yintercept=c(0), colour="darkgray", size=0.5)+
    {if (!is.na(temp_knot))geom_vline(xintercept=temp_knot, linetype="dotted", colour="darkgray", alpha=0.5, size=0.75)}+
    
    # Show the average THI and estimated effect
    geom_point(data=THI_mean_df,aes(x=THI, y= Effect), colour="darkgray", alpha=1)+
    
    # Calculated effect
    geom_line(data=calculated_effect, aes(y=Effect, x=THI,), size=0.5, colour="brown1")+
    geom_line(data=calculated_effect, aes(y=upper_band, x=THI,), size=0.25, colour="brown1", alpha=1)+
    geom_line(data=calculated_effect, aes(y=lower_band, x=THI,), size=0.25, colour="brown1", alpha=1)+
    geom_ribbon(data=calculated_effect,aes(ymin=lower_band, ymax=upper_band, x=THI), fill="brown1",alpha=0.3)
 
}

plot_vet_centered <- ggarrange(list_centered_plots[[1]],list_centered_plots[[2]],list_centered_plots[[3]],list_centered_plots[[4]])
ggsave(plot=plot_vet_centered,"Results(plot)/vet_main.pdf", width = 16, height = 16, unit="cm", device = cairo_pdf)
ggsave(plot=plot_vet_centered,"Results(plot)/vet_main.png", width = 16, height = 9, unit="cm")
