# ==================================================================================================================================
# ==================================================================================================================================
#
# Modular code for the publication: Heat Risks in Swiss Milk production
#
# Citation: Bucheli, J., Uldry, M. and Finger, R. 2022. Heat Risks in Swiss Milk production. Journal of the Agricultural and Applied
#           Economics Association.
#
# Robustness check 2: model for temperature effects on milk revenues
#
# ==================================================================================================================================
# ==================================================================================================================================

library(dplyr)
library(plyr)
library(lspline)
library(plm)
library(fixest)

# -------------------------------------------------------------------------
# Load data and preparation of loops
# -------------------------------------------------------------------------

load("Meteo/Temperature/farm_T_daily.RData")
load("Meteo/Precipitation/farm_precip_daily.RData")
colnames(farm_T_hourly)[5] <- "Temperature"

# This is loaded from the first script 01_sample_geo.R
sub_panels_ID <- list(good_farms_panel,good_farms_panel_plain, good_farms_panel_hill, good_farms_panel_mountain)
names(sub_panels_ID) <- c("all zones", "plain zone", "hill zone","mountain zone")

# Define minimum distance between 2 knots
required_space <- 5
# -------------------------------------------------------------------------
# Model resulting in the largest goodness of fit
# -------------------------------------------------------------------------

# List with the knot combination for model with largest goodness of fit
list_final_knot_combination <- list()

# List with model with largest goodness of fit
list_final_model <- list()

# z represents the zone (i.e.sub-panel)
for (z in 1:length(names(sub_panels_ID))){
  
  # Get data for subset z
  sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[z]]),]
  sub_farm_T_hourly <- farm_T_hourly[which(farm_T_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
  sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
 
  # Borders for knot location
  # Define lowest and upper bounds for knot location
  lower_bound <- floor(quantile(sub_farm_T_hourly$Temperature,0.05, type=1, na.rm=T))
  upper_bound <- ceiling(quantile(sub_farm_T_hourly$Temperature,0.95, type=1, na.rm=T))
  knot_range <- seq(lower_bound, upper_bound,1)
  
  # Calculate cumulative precipitation (farm and year) and merge with accounting data
  temp_CP <- sub_farm_precip_daily %>%
    group_by(farm,year)%>%
    summarise_at(vars(precip),sum, na.rm=T)
  
  # Best knot combination for given number of knots
  temp_list_knots <- list()
  
  # List with best model for each number of knots
  temp_list_models <- list()
  
  # ----------------------
  # Linear model
  # ----------------------
  
  # Merge all relevant data
  temp_Temperature<-sub_farm_T_hourly %>%
    group_by(farm,year)%>%
    summarise_at(vars(Temperature),sum, na.rm=T)
  
  sub_farm_panel_T_yearly <- join(sub_dairy_farms_panel_final, temp_Temperature, by=c("farm", "year"))
  
  sub_farm_panel_T_PP_yearly <- join(sub_farm_panel_T_yearly, temp_CP, by=c("farm", "year")) 
  sub_farm_panel_T_PP_yearly$pp2 <- (sub_farm_panel_T_PP_yearly$precip)^2
  rm(temp_Temperature,sub_farm_panel_T_yearly)
  
  temp_list_models[[1]] <- feols(log(rohMilch) ~ Temperature + precip +pp2 |  farm + year, data = sub_farm_panel_T_PP_yearly, cluster=c("farm", "year"))
  rm(sub_farm_panel_T_PP_yearly)
  
  # ----------------------
  # 1 knot
  # ----------------------
  
  temp_RSS_1_knot <- vector(length=length(knot_range))
  
  for (k in 1:length(knot_range)){
    
    temp_new_ts_T_ls <- as.data.frame(lspline(sub_farm_T_hourly$Temperature,knots= knot_range[k]))
    sub_farm_T_hourly_extended <- bind_cols(sub_farm_T_hourly, temp_new_ts_T_ls)
    colnames(sub_farm_T_hourly_extended)[1] <- "farm"
    colnames(sub_farm_T_hourly_extended)[8] <- "s_Temperature_1"
    colnames(sub_farm_T_hourly_extended)[9] <- "s_Temperature_2"
    
    # Aggregate to yearly values
    temp_Temperature<-sub_farm_T_hourly_extended %>%
      group_by(farm,year)%>%
      summarise_at(vars(s_Temperature_1,s_Temperature_2),sum, na.rm=T)
    
    sub_farm_panel_T_yearly <- join(sub_dairy_farms_panel_final, temp_Temperature, by=c("farm", "year"))
    sub_farm_panel_T_PP_yearly <- join(sub_farm_panel_T_yearly, temp_CP, by=c("farm", "year")) 
    sub_farm_panel_T_PP_yearly$pp2 <- (sub_farm_panel_T_PP_yearly$precip)^2
    rm(temp_Temperature,sub_farm_panel_T_yearly)
    
    temp1 <-  feols(log(rohMilch) ~ s_Temperature_1 + s_Temperature_2 + precip +pp2 |  farm + year, data = sub_farm_panel_T_PP_yearly, cluster=c("farm", "year"))
    temp_RSS_1_knot[k] <- sum(resid(temp1)^2)
    rm(temp_new_ts_T_ls, sub_farm_T_hourly_extended, sub_farm_panel_T_PP_yearly,temp1)
    print(k / length(knot_range))
    print(Sys.time())
  }
  
  # Best knot location for 1 knot
  temp_list_knots[[1]] <- knot_range[which.min(temp_RSS_1_knot)]
  rm(temp_RSS_1_knot)
  
  # Best model for 1 knot
  temp_new_ts_T_ls <- as.data.frame(lspline(sub_farm_T_hourly$Temperature,knots=  temp_list_knots[[1]]))
  sub_farm_T_hourly_extended <- bind_cols(sub_farm_T_hourly, temp_new_ts_T_ls)
  colnames(sub_farm_T_hourly_extended)[1] <- "farm"
  colnames(sub_farm_T_hourly_extended)[8] <- "s_Temperature_1"
  colnames(sub_farm_T_hourly_extended)[9] <- "s_Temperature_2"
  
  # Aggregate to yearly values
  temp_Temperature<-sub_farm_T_hourly_extended %>%
    group_by(farm,year)%>%
    summarise_at(vars(s_Temperature_1,s_Temperature_2),sum, na.rm=T)
  
  sub_farm_panel_T_yearly <- join(sub_dairy_farms_panel_final, temp_Temperature, by=c("farm", "year"))
  sub_farm_panel_T_PP_yearly <- join(sub_farm_panel_T_yearly, temp_CP, by=c("farm", "year")) 
  sub_farm_panel_T_PP_yearly$pp2 <- (sub_farm_panel_T_PP_yearly$precip)^2
  rm(temp_Temperature,sub_farm_panel_Temperature_yearly)
  
  temp_list_models[[2]] <- feols(log(rohMilch) ~ s_Temperature_1 + s_Temperature_2 + precip +pp2 |  farm + year, data = sub_farm_panel_T_PP_yearly, cluster=c("farm", "year"))
  rm(temp_new_ts_T_ls, sub_farm_T_hourly_extended, sub_farm_panel_T_PP_yearly,temp1)
  
  # ----------------------
  # 2 knots
  # ----------------------
  
  knots_2_combi <- combn(knot_range, 2, simplify = T)
  diff_1 <- abs(knots_2_combi[1,] - knots_2_combi[2,])
  knots_2_combi_good <- knots_2_combi[,which(diff_1 >= required_space)]
  temp_RSS_2_knot <- vector(length=ncol(knots_2_combi_good))
  
  for (k in 1:ncol(knots_2_combi_good)){
    
    temp_new_ts_T_ls <- as.data.frame(lspline(sub_farm_T_hourly$Temperature,knots= knots_2_combi_good[,k]))
    sub_farm_T_hourly_extended <- bind_cols(sub_farm_T_hourly, temp_new_ts_T_ls)
    colnames(sub_farm_T_hourly_extended)[1] <- "farm"
    colnames(sub_farm_T_hourly_extended)[8] <- "s_Temperature_1"
    colnames(sub_farm_T_hourly_extended)[9] <- "s_Temperature_2"
    colnames(sub_farm_T_hourly_extended)[10] <- "s_Temperature_3"
    
    # Aggregate to yearly values
    temp_Temperature<-sub_farm_T_hourly_extended %>%
      group_by(farm,year)%>%
      summarise_at(vars(s_Temperature_1,s_Temperature_2,s_Temperature_3),sum, na.rm=T)
    
    sub_farm_panel_T_yearly <- join(sub_dairy_farms_panel_final, temp_Temperature, by=c("farm", "year"))
    sub_farm_panel_T_PP_yearly <- join(sub_farm_panel_T_yearly, temp_CP, by=c("farm", "year")) 
    sub_farm_panel_T_PP_yearly$pp2 <- (sub_farm_panel_T_PP_yearly$precip)^2
    rm(temp_Temperature,sub_farm_panel_T_yearly)
    
    temp1 <- feols(log(rohMilch) ~ s_Temperature_1 + s_Temperature_2 + s_Temperature_3 + precip +pp2 |  farm + year, data = sub_farm_panel_T_PP_yearly, cluster=c("farm", "year"))
    temp_RSS_2_knot[k] <- sum(resid(temp1)^2)
    rm(temp_new_ts_T_ls, sub_farm_T_hourly_extended, sub_farm_panel_T_PP_yearly,temp1)
    print(k / ncol(knots_2_combi_good))
    print(Sys.time())
  }
  
  # Best knot location for 2 knots
  temp_list_knots[[2]] <- knots_2_combi_good[,which.min(temp_RSS_2_knot)]
  rm(temp_RSS_2_knot)
  
  # Best model for 2 knots
  temp_new_ts_T_ls <- as.data.frame(lspline(sub_farm_T_hourly$Temperature,knots=  temp_list_knots[[2]]))
  sub_farm_T_hourly_extended <- bind_cols(sub_farm_T_hourly, temp_new_ts_T_ls)
  colnames(sub_farm_T_hourly_extended)[1] <- "farm"
  colnames(sub_farm_T_hourly_extended)[8] <- "s_Temperature_1"
  colnames(sub_farm_T_hourly_extended)[9] <- "s_Temperature_2"
  colnames(sub_farm_T_hourly_extended)[10] <- "s_Temperature_3"
  
  # Aggregate to yearly values
  temp_Temperature<-sub_farm_T_hourly_extended %>%
    group_by(farm,year)%>%
    summarise_at(vars(s_Temperature_1,s_Temperature_2, s_Temperature_3),sum, na.rm=T)
  
  sub_farm_panel_T_yearly <- join(sub_dairy_farms_panel_final, temp_Temperature, by=c("farm", "year"))
  sub_farm_panel_T_PP_yearly <- join(sub_farm_panel_T_yearly, temp_CP, by=c("farm", "year")) 
  sub_farm_panel_T_PP_yearly$pp2 <- (sub_farm_panel_T_PP_yearly$precip)^2
  rm(temp_Temperature,sub_farm_panel_T_yearly)
  
  temp_list_models[[3]] <- feols(log(rohMilch) ~ s_Temperature_1 + s_Temperature_2 + s_Temperature_3 + precip +pp2 |  farm + year, data = sub_farm_panel_T_PP_yearly, cluster=c("farm", "year"))
  rm(temp_new_ts_T_ls, sub_farm_T_hourly_extended, sub_farm_panel_T_PP_yearly)
  
  # ----------------------
  # 3 knots
  # ----------------------
  
  knots_3_combi <- combn(knot_range, 3, simplify = T)
  differences <- matrix(NA, nrow=3, ncol=ncol(knots_3_combi))
  differences[1,] <- abs(knots_3_combi[1,] - knots_3_combi[2,])
  differences[2,] <- abs(knots_3_combi[1,] - knots_3_combi[3,])
  differences[3,] <- abs(knots_3_combi[2,] - knots_3_combi[3,])
  
  knots_3_combi_good <- knots_3_combi[,which(differences[1,] >= required_space & differences[2,] >= required_space & differences[3,] >= required_space)]
  temp_RSS_3_knot <- vector(length=ncol(knots_3_combi_good))
  
  for (k in 1:ncol(knots_3_combi_good)){
    
    temp_new_ts_T_ls <- as.data.frame(lspline(sub_farm_T_hourly$Temperature,knots= knots_3_combi_good[,k]))
    sub_farm_T_hourly_extended <- bind_cols(sub_farm_T_hourly, temp_new_ts_T_ls)
    colnames(sub_farm_T_hourly_extended)[1] <- "farm"
    colnames(sub_farm_T_hourly_extended)[8] <- "s_Temperature_1"
    colnames(sub_farm_T_hourly_extended)[9] <- "s_Temperature_2"
    colnames(sub_farm_T_hourly_extended)[10] <- "s_Temperature_3"
    colnames(sub_farm_T_hourly_extended)[11] <- "s_Temperature_4"
    
    # Aggregate to yearly values
    temp_Temperature<-sub_farm_T_hourly_extended %>%
      group_by(farm,year)%>%
      summarise_at(vars(s_Temperature_1,s_Temperature_2,s_Temperature_3,s_Temperature_4),sum, na.rm=T)
    
    sub_farm_panel_T_yearly <- join(sub_dairy_farms_panel_final, temp_Temperature, by=c("farm", "year"))
    sub_farm_panel_T_PP_yearly <- join(sub_farm_panel_T_yearly, temp_CP, by=c("farm", "year")) 
    sub_farm_panel_T_PP_yearly$pp2 <- (sub_farm_panel_T_PP_yearly$precip)^2
    rm(temp_Temperature,sub_farm_panel_T_yearly)
    
    temp1 <- feols(log(rohMilch) ~ s_Temperature_1 + s_Temperature_2 + s_Temperature_3 +s_Temperature_4 + precip +pp2 |  farm + year, data = sub_farm_panel_T_PP_yearly, cluster=c("farm", "year"))
    temp_RSS_3_knot[k] <- sum(resid(temp1)^2)
    rm(temp_new_ts_T_ls, sub_farm_T_hourly_extended, sub_farm_panel_T_PP_yearly,temp1)
    print(k / ncol(knots_3_combi_good))
    print(Sys.time())
  }
  
  # Best knot location for 3 knots
  temp_list_knots[[3]] <- knots_3_combi_good[,which.min(temp_RSS_3_knot)]
  rm(temp_RSS_3_knot)
  
  # Best model for 3 knot
  temp_new_ts_T_ls <- as.data.frame(lspline(sub_farm_T_hourly$Temperature,knots=  temp_list_knots[[3]]))
  sub_farm_T_hourly_extended <- bind_cols(sub_farm_T_hourly, temp_new_ts_T_ls)
  colnames(sub_farm_T_hourly_extended)[1] <- "farm"
  colnames(sub_farm_T_hourly_extended)[8] <- "s_Temperature_1"
  colnames(sub_farm_T_hourly_extended)[9] <- "s_Temperature_2"
  colnames(sub_farm_T_hourly_extended)[10] <- "s_Temperature_3"
  colnames(sub_farm_T_hourly_extended)[11] <- "s_Temperature_4"
  
  # Aggregate to yearly values
  temp_Temperature<-sub_farm_T_hourly_extended %>%
    group_by(farm,year)%>%
    summarise_at(vars(s_Temperature_1,s_Temperature_2, s_Temperature_3,s_Temperature_4),sum, na.rm=T)
  
  sub_farm_panel_T_yearly <- join(sub_dairy_farms_panel_final, temp_Temperature, by=c("farm", "year"))
  sub_farm_panel_T_PP_yearly <- join(sub_farm_panel_T_yearly, temp_CP, by=c("farm", "year")) 
  sub_farm_panel_T_PP_yearly$pp2 <- (sub_farm_panel_T_PP_yearly$precip)^2
  rm(temp_T,sub_farm_panel_T_yearly)
  
  temp_list_models[[4]] <- feols(log(rohMilch) ~ s_Temperature_1 + s_Temperature_2 + s_Temperature_3 +s_Temperature_4 + precip +pp2 |  farm + year, data = sub_farm_panel_T_PP_yearly, cluster=c("farm", "year"))
  rm(temp_new_ts_T_ls, sub_farm_T_hourly_extended, sub_farm_panel_T_PP_yearly)
  
  # Identify the overall best model
  # Note that AIC does not work for plm models
  best_model_number <- which.min(c(AIC(temp_list_models[[1]]),AIC(temp_list_models[[2]]),AIC(temp_list_models[[3]]),AIC(temp_list_models[[4]])))
  
  # Get knot combination
  if( best_model_number == 1){
    list_final_knot_combination[[z]] <- NA 
  }else{
    list_final_knot_combination[[z]] <- temp_list_knots[[best_model_number - 1]]}
  
  # Save the best model as plm object (easier to control for heteroscedasticity)
  if( best_model_number == 1){
    
    temp_Temperature<-sub_farm_T_hourly %>%
      group_by(farm,year)%>%
      summarise_at(vars(Temperature),sum, na.rm=T)
    
    sub_farm_panel_T_yearly <- join(sub_dairy_farms_panel_final, temp_Temperature, by=c("farm", "year"))
    
    sub_farm_panel_T_PP_yearly <- join(sub_farm_panel_T_yearly, temp_CP, by=c("farm", "year")) 
    sub_farm_panel_T_PP_yearly$pp2 <- (sub_farm_panel_T_PP_yearly$precip)^2
    rm(temp_Temperature,sub_farm_panel_T_yearly)
    
    list_final_model[[z]] <- feols(log(rohMilch) ~ Temperature + precip +pp2 | farm + year, data = sub_farm_panel_T_PP_yearly, cluster=c("farm", "year"))
    rm(sub_farm_panel_T_PP_yearly)
    
  }else if (best_model_number == 2){
    
    temp_new_ts_T_ls <- as.data.frame(lspline(sub_farm_T_hourly$Temperature,knots=  temp_list_knots[[1]]))
    sub_farm_T_hourly_extended <- bind_cols(sub_farm_T_hourly, temp_new_ts_T_ls)
    colnames(sub_farm_T_hourly_extended)[1] <- "farm"
    colnames(sub_farm_T_hourly_extended)[8] <- "s_Temperature_1"
    colnames(sub_farm_T_hourly_extended)[9] <- "s_Temperature_2"
    
    # Aggregate to yearly values
    temp_Temperature<-sub_farm_T_hourly_extended %>%
      group_by(farm,year)%>%
      summarise_at(vars(s_Temperature_1,s_Temperature_2),sum, na.rm=T)
    
    sub_farm_panel_T_yearly <- join(sub_dairy_farms_panel_final, temp_Temperature, by=c("farm", "year"))
    sub_farm_panel_T_PP_yearly <- join(sub_farm_panel_T_yearly, temp_CP, by=c("farm", "year")) 
    sub_farm_panel_T_PP_yearly$pp2 <- (sub_farm_panel_T_PP_yearly$precip)^2
    rm(temp_Temperature,sub_farm_panel_T_yearly)
    
    list_final_model[[z]] <-  feols(log(rohMilch) ~ s_Temperature_1 + s_Temperature_2 + precip +pp2 | farm + year, data = sub_farm_panel_T_PP_yearly, cluster=c("farm", "year"))
    rm(temp_new_ts_T_ls, sub_farm_T_hourly_extended, sub_farm_panel_T_PP_yearly) 
    
    
  }else if (best_model_number == 3){
    
    temp_new_ts_T_ls <- as.data.frame(lspline(sub_farm_T_hourly$Temperature,knots=  temp_list_knots[[2]]))
    sub_farm_T_hourly_extended <- bind_cols(sub_farm_T_hourly, temp_new_ts_T_ls)
    colnames(sub_farm_T_hourly_extended)[1] <- "farm"
    colnames(sub_farm_T_hourly_extended)[8] <- "s_Temperature_1"
    colnames(sub_farm_T_hourly_extended)[9] <- "s_Temperature_2"
    colnames(sub_farm_T_hourly_extended)[10] <- "s_Temperature_3"
    
    # Aggregate to yearly values
    temp_Temperature<-sub_farm_T_hourly_extended %>%
      group_by(farm,year)%>%
      summarise_at(vars(s_Temperature_1,s_Temperature_2, s_Temperature_3),sum, na.rm=T)
    
    sub_farm_panel_T_yearly <- join(sub_dairy_farms_panel_final, temp_Temperature, by=c("farm", "year"))
    sub_farm_panel_T_PP_yearly <- join(sub_farm_panel_T_yearly, temp_CP, by=c("farm", "year")) 
    sub_farm_panel_T_PP_yearly$pp2 <- (sub_farm_panel_T_PP_yearly$precip)^2
    rm(temp_T,sub_farm_panel_T_yearly)
    
    list_final_model[[z]] <-  feols(log(rohMilch) ~ s_Temperature_1 +s_Temperature_2+s_Temperature_3 + precip +pp2 | farm + year, data = sub_farm_panel_T_PP_yearly, cluster=c("farm", "year"))
    rm(temp_new_ts_T_ls, sub_farm_T_hourly_extended, sub_farm_panel_T_PP_yearly) 
    
  } else {
    temp_new_ts_T_ls <- as.data.frame(lspline(sub_farm_T_hourly$Temperature,knots=  temp_list_knots[[3]]))
    sub_farm_T_hourly_extended <- bind_cols(sub_farm_T_hourly, temp_new_ts_T_ls)
    colnames(sub_farm_T_hourly_extended)[1] <- "farm"
    colnames(sub_farm_T_hourly_extended)[8] <- "s_Temperature_1"
    colnames(sub_farm_T_hourly_extended)[9] <- "s_Temperature_2"
    colnames(sub_farm_T_hourly_extended)[10] <- "s_Temperature_3"
    colnames(sub_farm_T_hourly_extended)[11] <- "s_Temperature_4"
    
    # Aggregate to yearly values
    temp_Temperature<-sub_farm_T_hourly_extended %>%
      group_by(farm,year)%>%
      summarise_at(vars(s_Temperature_1,s_Temperature_2, s_Temperature_3,s_Temperature_4),sum, na.rm=T)
    
    sub_farm_panel_T_yearly <- join(sub_dairy_farms_panel_final, temp_Temperature, by=c("farm", "year"))
    sub_farm_panel_T_PP_yearly <- join(sub_farm_panel_T_yearly, temp_CP, by=c("farm", "year")) 
    sub_farm_panel_T_PP_yearly$pp2 <- (sub_farm_panel_T_PP_yearly$precip)^2
    rm(temp_Temperature,sub_farm_panel_T_yearly)
    
    list_final_model[[z]] <-  feols(log(rohMilch) ~ s_Temperature_1 +s_Temperature_2+s_Temperature_3 + s_Temperature_4 + precip +pp2 | farm + year, data = sub_farm_panel_T_PP_yearly, cluster=c("farm", "year"))
    rm(temp_new_ts_T_ls, sub_farm_T_hourly_extended, sub_farm_panel_T_PP_yearly) 
  }
  
  # Tidy up environment
  rm(best_model_number, temp_list_models,temp_list_knots,sub_farm_T_hourly, sub_farm_precip_daily , sub_dairy_farms_panel_final, lower_bound, upper_bound, knot_range, best_model_number,  temp_list_models)
  rm(differences,knots_2_combi, knots_2_combi_good, knots_3_combi, knots_3_combi_good, temp_CP, sub_farm_precip_daily, sub_farm_T_hourly, sub_dairy_farms_panel_final)
  
}

list_final_model_rev <- list_final_model
list_final_knot_combination_rev <- list_final_knot_combination

save(list_final_model_rev, file="Models/final_model_rev.RData")
save(list_final_knot_combination_rev, file="Models/final_knots_rev.RData")