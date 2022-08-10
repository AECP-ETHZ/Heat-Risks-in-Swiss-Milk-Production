# ==================================================================================================================================
# ==================================================================================================================================
#
# Modular code for the publication: Heat Risks in Swiss Milk production
#
# Citation: Bucheli, J., Uldry, M. and Finger, R. 2022. Heat Risks in Swiss Milk production. Journal of the Agricultural and Applied
#           Economics Association.
#
# Part 3/9: Calculate hourly THI for each farm
# Note: Temperature data derived with the scripts found here: https://github.com/AECP-ETHZ/Spatial-extraction-of-Swiss-weather-data
#
# ==================================================================================================================================
# ==================================================================================================================================

library(data.table)

# Load the output from script 02
load("Meteo/Rel Humidity/Interpolated_municipality/interpolated_RH_min_nextstation.RData")
load("Meteo/Rel Humidity/Interpolated_municipality/interpolated_RH_max_nextstation.RData")

# -------------------------------------------------------------------------
# Load temperature data
# -------------------------------------------------------------------------

Tmax_data <- read.csv("Meteo/Temperature/Tmax/daily Tmax_prep.csv", dec=".", header=F,row.names=1, sep=",")[-1,]
colnames(Tmax_data) <- colnames(interpolated_RH_max)

Tmin_data <- read.csv("Meteo/Temperature/Tmin/daily Tmin_prep.csv", dec=".", header=F,row.names=1, sep=",")[-1,]
colnames(Tmin_data) <- colnames(interpolated_RH_max)

# -------------------------------------------------------------------------
# THI formula
# -------------------------------------------------------------------------
#
# Following equation of THI5 in Bohmanova et al. (2007)

THI_5 <- function(Temp,RH){
  (1.8 * Temp+32) - (0.55-0.0055 * RH) * (1.8 * Temp - 26)
}

# -------------------------------------------------------------------------
# Daily minimum THI
# -------------------------------------------------------------------------
#
# # THI_min is at Tmin and RH max

THI_min <- as.data.frame(matrix(NA, nrow=length(unique(farm_data$Gemeinde)), ncol=length(colnames(interpolated_RH_max))))
row.names(THI_min) <- unique(farm_data$Gemeinde)
colnames(THI_min) <- as.Date(as.character(colnames(interpolated_RH_max)), format="%Y-%m-%d")

for (i in 1:nrow(THI_min)){
  for (d in 1:ncol(THI_min)){
    THI_min[i,d] <- THI_5(Temp=as.numeric(as.character(Tmin_data[which(row.names(THI_min)[i]== row.names(Tmin_data)) , d])), RH= as.numeric(as.character(interpolated_RH_max[which(row.names(THI_min)[i] == row.names(interpolated_RH_max)),d])))
  }
}

save(THI_min, file="Meteo/THI/THI_min.RData")

# -------------------------------------------------------------------------
# Daily maximum THI
# -------------------------------------------------------------------------
#
# # THI_max is at Tmax and RH min

THI_max <- as.data.frame(matrix(NA, nrow=length(unique(farm_data$Gemeinde)), ncol=length(colnames(interpolated_RH_max))))
row.names(THI_max) <- unique(farm_data$Gemeinde)
colnames(THI_max) <- as.Date(as.character(colnames(interpolated_RH_max)), format="%Y-%m-%d")

for (i in 1:nrow(THI_max)){
  for (d in 1:ncol(THI_max)){
    THI_max[i,d] <- THI_5(Temp=as.numeric(as.character(Tmax_data[which(row.names(THI_max)[i]== row.names(Tmax_data)) , d])), RH= as.numeric(as.character(interpolated_RH_min[which(row.names(THI_max)[i] == row.names(interpolated_RH_min)),d])))
  }
  print(i / nrow(THI_max))
}


save(THI_max, file="Meteo/THI/THI_max.RData")

# --------------------------------------------
# Calculation of hourly THI values
# -------------------------------------------------------------------------
#
# Based on daily double sine curves. 
# For simplicity the year starts on 01.04.20XX at THI_min and ends on 31.10.20XX at THI_max

# Hourly steps in unit radian
radian_observations <- c(seq(((-0.5*pi)+(2*pi/48)),(0.5*pi-(2*pi/48)), by=(2*pi/24)), seq((( 0.5*pi)+(2*pi/48)),(1.5*pi-(2*pi/48)), by=(2*pi/24)))

# Available years in data (required for subsetting)
available_years <- seq(2003,2015,1)

# Final structure of list is [[farm]][[year]][day of year, hour]
list_THI_farm <- list()

# First loop for farms
for (i in 1:(length(unique(farm_data$Gemeinde)))){
  
  # A new list for each year
  list_THI_farm_year <- list()
  
  # Second loop for years
  for (y in 1:(length(available_years))){
    
    # Subset THI_min and THI_max 
    temp_selected_columns <- which(substr(colnames(THI_min),1,4) == available_years[y])
    
    # Create data frame for hourly THI values
    df_THI_daily_hourly           <- as.data.frame(matrix(NA, nrow= length(temp_selected_columns), ncol=24))
    row.names(df_THI_daily_hourly) <- seq(1, length(temp_selected_columns),1)
    colnames(df_THI_daily_hourly)  <- seq(0,23,1)
    
    # Third loop for each day within a year
    for (d in 1:nrow(df_THI_daily_hourly)){
      
      # Get the data for this day
      THImin_d <- THI_min[which(row.names(THI_min) == unique(farm_data$Gemeinde)[i]),temp_selected_columns[d]]
      THImax_d <- THI_max[which(row.names(THI_max) == unique(farm_data$Gemeinde)[i]),temp_selected_columns[d]]
      THImin_d1 <- THI_min[which(row.names(THI_min) == unique(farm_data$Gemeinde)[i]),temp_selected_columns[d]+1]
      
      # Get daily amplitude of first half (fh) and second half (sh) of the day
      amplitude_fh <- (THImax_d - THImin_d) / 2
      amplitude_sh <- (THImax_d - THImin_d1)/ 2
      
      # Get daily average of first half and second half of the day
      THImean_fh <- (THImax_d + THImin_d) / 2
      THImean_sh <- (THImax_d + THImin_d1)/ 2
      
      # Get hourly THI for the first half of the day (first sine curve)
      for (fh in 1:12){
        df_THI_daily_hourly [d,fh] <- THImean_fh + amplitude_fh * sin(radian_observations[fh])
      }
      
      # Get hourly temperatures for the second half of the day (second sine curve)
      for (sh in 1:12){
        if(!is.null(THImin_d1)){
        df_THI_daily_hourly[d,12+sh] <- THImean_sh + amplitude_sh * sin(radian_observations[12+sh])}
        else{df_THI_daily_hourly[d,12+sh] <- NA}
        
      }
    rm(THImin_d, THImax_d, THImin_d1, amplitude_fh, THImean_fh)
      
    }
    list_THI_farm_year[[y]]  <- df_THI_daily_hourly
    list_THI_farm_year[[y]]$date <- colnames(THI_min)[temp_selected_columns]
    rm(df_THI_daily_hourly, temp_selected_columns)
  }
  
  list_THI_farm[[i]] <- list_THI_farm_year  
  rm(list_THI_farm_year)
  print(i / length(unique(farm_data$Gemeinde)))
}
names(list_THI_farm) <- unique(farm_data$Gemeinde)

# -------------------------------------------------------------------------
# Restructure data for panel regression & match with farm data
# -------------------------------------------------------------------------

# Restructuring data
gemeinde_THI_exposure1 <-rbindlist(lapply(list_THI_farm, rbindlist),use.names = T,idcol=T)

gemeinde_THI_exposure2 <- melt(gemeinde_THI_exposure1, id=c(".id","date"))
colnames(gemeinde_THI_exposure2) <- c("BFS","Date","Hour", "THI")
gemeinde_THI_exposure2$year      <-as.numeric(substr(gemeinde_THI_exposure2$Date,1,4))
gemeinde_THI_exposure2$month      <-as.numeric(substr(gemeinde_THI_exposure2$Date,6,7))

save(gemeinde_THI_exposure2, file="Meteo/THI/THI_gemeinde.RData")

# Match hourly values with farms
# i) Pack it in list

temp_THI_hourly <- list()
farm_ID <- unique(dairy_farms_panel_final$farm)

for(i in 1:length(farm_ID)){
  # Get the farm's BFS number
  temp1_BFS <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm == farm_ID[i])[1],"Gemeinde"]
  # Years with data for specific farm i
  temp1_years <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm == farm_ID[i]),"year"]
  # Extract the data
  temp1_THI_farm <- gemeinde_THI_exposure2[which(gemeinde_THI_exposure2$BFS == temp1_BFS & gemeinde_THI_exposure2$year %in% temp1_years),]
  temp_THI_hourly[[i]] <- temp1_THI_farm
  rm(temp1_BFS, temp1_years, temp1_THI_farm)
  print(i / length(farm_ID))
} 
names(temp_THI_hourly) <- farm_ID

# ii) In one data.frame
farm_THI_hourly <- as.data.frame(matrix(NA))
farm_THI_hourly <- rbindlist(temp_THI_hourly,use.names = T,idcol=T)
rm(temp_THI_hourly, gemeinde_THI_exposure2, gemeinde_THI_exposure1,farm_ID, list_THI_farm)
rm(interpolated_RH_max, interpolated_RH_min, THI_max, THI_min)

save(farm_THI_hourly,file="Meteo/THI/THI_farm.RData")
                          

