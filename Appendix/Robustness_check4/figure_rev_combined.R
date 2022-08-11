# ==================================================================================================================================
# ==================================================================================================================================
#
# Modular code for the publication: Heat Risks in Swiss Milk production
#
# Citation: Bucheli, J., Uldry, M. and Finger, R. 2022. Heat Risks in Swiss Milk production. Journal of the Agricultural and Applied
#           Economics Association.
#
# Robustness check 4: figure for milk revenues
#
# ==================================================================================================================================
# ==================================================================================================================================

library(lmtest)
library(dplyr)
library(ggplot2)
library(egg)


plot_titles <- c("a) complete sample","b) plain sample", "c) hill sample", "d) mountain sample")
load("Meteo/THI/THI_farm.RData")
colnames(farm_THI_hourly)[1] <- "farm"

# Load farms with high milk production
load("APPENDIX/Milchleistung/final_knots_rev_highmilk.RData")
load("APPENDIX/Milchleistung/final_model_rev_highmilk.RData")

list_final_model_rev_up <- list_final_model_rev
list_final_knot_combination_rev_up <- list_final_knot_combination_rev
rm(list_final_model_rev,list_final_knot_combination_rev)

# Load farms with low milk production
load("APPENDIX/Milchleistung/final_knots_rev_milkdown.RData")
load("APPENDIX/Milchleistung/final_model_rev_milkdown.RData")

list_final_model_rev_down <- list_final_model_rev
list_final_knot_combination_rev_down <- list_final_knot_combination_rev

# Load these from APP_01_sample_geo_MilchleistungUp.R (first run only this one)
sub_panels_ID_up <- list(good_farms_panel,good_farms_panel_plain, good_farms_panel_hill, good_farms_panel_mountain)

# Calculate again sub-panel from downs from APP_01_geo_Down.R (second run only this one)
sub_panels_ID_down <- list(good_farms_panel,good_farms_panel_plain, good_farms_panel_hill, good_farms_panel_mountain)

# ---------------------
# Plot for each sample
# ---------------------

THI_range <- seq(ceiling(min(farm_THI_hourly$THI, na.rm=T)), floor(max(farm_THI_hourly$THI, na.rm=T)))
list_absolut_plots <- list()
list_centered_plots <- list()

for (s in 1:length(plot_titles)){
  
  # Data preparation
  temp_model_up <- list_final_model_rev_up[[s]]
  temp_knot_up  <- list_final_knot_combination_rev_up[[s]]
  
  sub_dairy_farms_panel_final_up <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in% sub_panels_ID_up[[s]]),]
  number_farms_up <- length(unique( sub_dairy_farms_panel_final_up$farm))
  
  sub_farm_THI_hourly_up <- farm_THI_hourly[which(farm_THI_hourly$farm %in% unique(sub_dairy_farms_panel_final_up$farm)),]
  THI_min_up <- ceiling(min(sub_farm_THI_hourly_up$THI, na.rm=T))
  THI_max_up <- floor(max(sub_farm_THI_hourly_up$THI, na.rm=T))
  THI_mean_up <- mean(sub_farm_THI_hourly_up$THI, na.rm=T)
  
  # Calculate absolute effects
  if(!is.na(temp_knot_up)){
  effect_THI_up <- lspline(THI_range, knots = temp_knot_up) %*% coef(temp_model_up)[1:(length(temp_knot_up) + 1)]
  effect_error_min_up <- effect_THI_up - ((lspline(THI_range, knots=temp_knot_up)) %*% (abs(coeftest(temp_model_up))[,2][1:(length(temp_knot_up) + 1)]*1.96))
  effect_error_max_up <- effect_THI_up + ((lspline(THI_range, knots=temp_knot_up)) %*% (abs(coeftest(temp_model_up))[,2][1:(length(temp_knot_up) + 1)]*1.96))
  summary_effects_THI_up <- as.data.frame(bind_cols(THI_range,effect_THI_up,effect_error_min_up,effect_error_max_up))
  colnames(summary_effects_THI_up) <- c("THI","Effect","lower_band","upper_band")
  } else {
    effect_THI_up <- (THI_range) * coef(temp_model_up)[1]
    effect_error_min_up <- effect_THI_up - (THI_range * (abs(coeftest(temp_model_up))[,2][1])*1.96)
    effect_error_max_up <- effect_THI_up + (THI_range * (abs(coeftest(temp_model_up))[,2][1])*1.96)
    summary_effects_THI_up <- as.data.frame(bind_cols(THI_range,effect_THI_up,effect_error_min_up,effect_error_max_up))
    colnames(summary_effects_THI_up) <- c("THI","Effect","lower_band","upper_band")
  }
  

  temp_model_down <- list_final_model_rev_down[[s]]
  temp_knot_down  <- list_final_knot_combination_rev_down[[s]]
  
  sub_dairy_farms_panel_final_down <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in% sub_panels_ID_down[[s]]),]
  number_farms_down <- length(unique( sub_dairy_farms_panel_final_down$farm))
  
  sub_farm_THI_hourly_down <- farm_THI_hourly[which(farm_THI_hourly$farm %in% unique(sub_dairy_farms_panel_final_down$farm)),]
  THI_min_down <- ceiling(min(sub_farm_THI_hourly_down$THI, na.rm=T))
  THI_max_down <- floor(max(sub_farm_THI_hourly_down$THI, na.rm=T))
  THI_mean_down <- mean(sub_farm_THI_hourly_down$THI, na.rm=T)
  
  # Calculate absolute effects
  if(!is.na(temp_knot_down)){
    effect_THI_down <- lspline(THI_range, knots = temp_knot_down) %*% coef(temp_model_down)[1:(length(temp_knot_down) + 1)]
    effect_error_min_down <- effect_THI_down - ((lspline(THI_range, knots=temp_knot_down)) %*% (abs(coeftest(temp_model_down))[,2][1:(length(temp_knot_down) + 1)]*1.96))
    effect_error_max_down <- effect_THI_down + ((lspline(THI_range, knots=temp_knot_down)) %*% (abs(coeftest(temp_model_down))[,2][1:(length(temp_knot_down) + 1)]*1.96))
    summary_effects_THI_down <- as.data.frame(bind_cols(THI_range,effect_THI_down,effect_error_min_down,effect_error_max_down))
    colnames(summary_effects_THI_down) <- c("THI","Effect","lower_band","upper_band")
  } else {
    effect_THI_down <- (THI_range) * coef(temp_model_down)[1]
    effect_error_min_down <- effect_THI_down - (THI_range * (abs(coeftest(temp_model_down))[,2][1])*1.96)
    effect_error_max_down <- effect_THI_down + (THI_range * (abs(coeftest(temp_model_down))[,2][1])*1.96)
    summary_effects_THI_down <- as.data.frame(bind_cols(THI_range,effect_THI_down,effect_error_min_down,effect_error_max_down))
    colnames(summary_effects_THI_down) <- c("THI","Effect","lower_band","upper_band")
  }
  
  
  # Calculate centered effects
  if(!is.na(temp_knot_up)){
  effect_THI_centered_up <- effect_THI_up - as.vector(lspline(THI_mean_up, knots = temp_knot_up)[1,] %*% coef(temp_model_up)[1:(length(temp_knot_up) + 1)])
  effect_error_min_centered_up <- effect_error_min_up - as.vector(lspline(THI_mean_up, knots = temp_knot_up)[1,] %*% coef(temp_model_up)[1:(length(temp_knot_up)+1)])
  effect_error_max_centered_up <- effect_error_max_up - as.vector(lspline(THI_mean_up, knots = temp_knot_up)[1,] %*% coef(temp_model_up)[1:(length(temp_knot_up)+1)])
  } else {
    effect_THI_centered_up <- effect_THI_up - (THI_mean_up * coef(temp_model_up)[1])
    effect_error_min_centered_up <- effect_error_min_up - (THI_mean_up * coef(temp_model_up)[1])
    effect_error_max_centered_up <- effect_error_max_up - (THI_mean_up * coef(temp_model_up)[1])
  }
  
  summary_effects_THI_centered_up <- as.data.frame(bind_cols(THI_range,effect_THI_centered_up,effect_error_min_centered_up,effect_error_max_centered_up))
  colnames(summary_effects_THI_centered_up) <- c("THI","Effect","lower_band","upper_band")
  
  
  calculated_effect_up <- summary_effects_THI_centered_up[which(summary_effects_THI_centered_up$THI >= THI_min_up & summary_effects_THI_centered_up$THI <= THI_max_up),]
  
  if(!is.na(temp_knot_down)){
    effect_THI_centered_down <- effect_THI_down - as.vector(lspline(THI_mean_down, knots = temp_knot_down)[1,] %*% coef(temp_model_down)[1:(length(temp_knot_down) + 1)])
    effect_error_min_centered_down <- effect_error_min_down - as.vector(lspline(THI_mean_down, knots = temp_knot_down)[1,] %*% coef(temp_model_down)[1:(length(temp_knot_down)+1)])
    effect_error_max_centered_down <- effect_error_max_down - as.vector(lspline(THI_mean_down, knots = temp_knot_down)[1,] %*% coef(temp_model_down)[1:(length(temp_knot_down)+1)])
  } else {
    effect_THI_centered_down <- effect_THI_down - (THI_mean_down * coef(temp_model_down)[1])
    effect_error_min_centered_down <- effect_error_min_down - (THI_mean_down * coef(temp_model_down)[1])
    effect_error_max_centered_down <- effect_error_max_down - (THI_mean_down * coef(temp_model_down)[1])
  }
  
  summary_effects_THI_centered_down <- as.data.frame(bind_cols(THI_range,effect_THI_centered_down,effect_error_min_centered_down,effect_error_max_centered_down))
  colnames(summary_effects_THI_centered_down) <- c("THI","Effect","lower_band","upper_band")
  
  
  calculated_effect_down <- summary_effects_THI_centered_down[which(summary_effects_THI_centered_down$THI >= THI_min_down & summary_effects_THI_centered_down$THI <= THI_max_down),]

  THI_mean_up_df <- as.data.frame(matrix(NA,nrow=1, ncol=2))
  colnames(THI_mean_up_df) <- c("THI", "Effect")
  THI_mean_up_df[1,1] <- THI_mean_up
  THI_mean_up_df[1,2] <- 0
  
  THI_mean_down_df <- as.data.frame(matrix(NA,nrow=1, ncol=2))
  colnames(THI_mean_down_df) <- c("THI", "Effect")
  THI_mean_down_df[1,1] <- THI_mean_down
  THI_mean_down_df[1,2] <- 0
  
  list_centered_plots[[s]] <- ggplot()+ggtitle(paste(plot_titles[s],sep=""))+
    theme(panel.grid.major = element_blank(),
          plot.title = element_text(family="Times New Roman", size=9, colour = "grey25", hjust=0.5),
          plot.subtitle = element_text(family="Times New Roman", size=7, colour = "grey25", hjust=0.5),
          axis.title = element_text(family="Times New Roman", size=7, colour = "grey25"),
          axis.text = element_text(family="Times New Roman", size=7),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none")+
    
    xlab("THI") + ylab("log(milk revenues) [CHF]")+
    scale_x_continuous(breaks = seq(40,90, by=10), limits = c(40,max(THI_range)))+
    scale_y_continuous(breaks = c(-0.004,-0.002,0,0.002,0.004), limits = c(-0.0055,0.0065))+
    geom_hline(yintercept=c(-0.004,-0.002,0,0.002,0.004), colour="gray", alpha=0.3)+
    geom_hline(yintercept=c(0), colour="darkgray", size=0.5)+
    {if (!is.na(temp_knot_down))geom_vline(xintercept=temp_knot_down, linetype="dotted", colour="darkgray", alpha=0.5, size=0.75)}+
    {if (!is.na(temp_knot_up))geom_vline(xintercept=temp_knot_up, linetype="dotted", colour="darkgray", alpha=1.5, size=0.75)}+
    
    geom_rect(aes(xmin=10, xmax=90, ymin=0.0041, ymax=0.0065), color="white",fill="white")+
    annotate("text", x = 55, y = 0.0065, label = paste("high milk yield subsample (",number_farms_up," farms)",sep=""), colour = "brown1",family="Times New Roman",size=2)+
    annotate("text", x = 55, y = 0.0050, label = paste("low milk yield subsample (",number_farms_down," farms)",sep=""), colour = "cornflowerblue",family="Times New Roman",size=2)+
    # Show the average THI and estimated effect
    geom_point(data=THI_mean_down_df,aes(x=THI, y= Effect), colour="darkgray", alpha=1)+
    geom_point(data=THI_mean_up_df, aes(x=THI, y= Effect), colour="gray24", alpha=4)+
    
    # Calculated effect (upper)
    geom_line(data=calculated_effect_up, aes(y=Effect, x=THI,), size=0.5, colour="brown1")+
    geom_line(data=calculated_effect_up, aes(y=upper_band, x=THI,), size=0.25, colour="brown1", alpha=1)+
    geom_line(data=calculated_effect_up, aes(y=lower_band, x=THI,), size=0.25, colour="brown1", alpha=1)+
    geom_ribbon(data=calculated_effect_up,aes(ymin=lower_band, ymax=upper_band, x=THI), fill="brown1",alpha=0.3)+
    
    # Calculated effect (lower)
    geom_line(data=calculated_effect_down, aes(y=Effect, x=THI,), size=0.5, colour="cornflowerblue")+
    geom_line(data=calculated_effect_down, aes(y=upper_band, x=THI,), size=0.25, colour="cornflowerblue", alpha=1)+
    geom_line(data=calculated_effect_down, aes(y=lower_band, x=THI,), size=0.25, colour="cornflowerblue", alpha=1)+
    geom_ribbon(data=calculated_effect_down,aes(ymin=lower_band, ymax=upper_band, x=THI), fill="cornflowerblue",alpha=0.3)
}

plot_rev_centered <- ggarrange(list_centered_plots[[1]],list_centered_plots[[2]],list_centered_plots[[3]],list_centered_plots[[4]])

ggsave(plot=plot_rev_centered,"APPENDIX/Milchleistung/rev_samples.pdf", width = 16, height = 16, unit="cm", device = cairo_pdf)
ggsave(plot=plot_rev_centered,"APPENDIX/Milchleistung/rev_samples.png", width = 16, height = 9, unit="cm")
