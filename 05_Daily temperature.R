# ==================================================================================================================================
# ==================================================================================================================================
#
# Modular code for the publication: Heat Risks in Swiss Milk production
#
# Citation: Bucheli, J., Uldry, M. and Finger, R. 2022. Heat Risks in Swiss Milk production. Journal of the Agricultural and Applied
#           Economics Association.
#
# Part 5/9: Calculate hourly temperatures for each farm
# Note: Precipitation data derived with the scripts found here: https://github.com/AECP-ETHZ/Spatial-extraction-of-Swiss-weather-data
#
# ==================================================================================================================================
# ==================================================================================================================================


Tmax_data <- read.csv("Meteo/Temperature/Tmax/daily Tmax_prep.csv", dec=".", header=F,row.names=1, sep=",")[-1,]
colnames(Tmax_data) <- sort(unique(farm_THI_hourly$Date))

Tmin_data <- read.csv("Meteo/Temperature/Tmin/daily Tmin_prep.csv", dec=".", header=F,row.names=1, sep=",")[-1,]
colnames(Tmin_data) <- sort(unique(farm_THI_hourly$Date))

# -------------------------------------------------------------------------
# Calculation of hourly temperatures
# -------------------------------------------------------------------------
#
# Based on daily double sine curves. 
# For simplicity the year starts on 01.04.20XX at THI_min and ends on 31.10.20XX at THI_max

# Hourly steps in unit radian
radian_observations <-  c(seq(((-0.5*pi)+(2*pi/48)),(0.5*pi-(2*pi/48)), by=(2*pi/24)), seq((( 0.5*pi)+(2*pi/48)),(1.5*pi-(2*pi/48)), by=(2*pi/24)))

# Available years in data (required for subsetting)
available_years <- seq(2003,2015,1)

# Final structure of list is [[farm]][[year]][day of year, hour]
list_T_municipality <- list()

# First loop for municipalitys
for (i in 1:(length(unique(farm_data$Gemeinde)))){
  
  # A new list for each year
  list_T_municipality_year <- list()
  
  # Second loop for years
  for (y in 1:(length(available_years))){
    
    # Subset THI_min and THI_max 
    temp_selected_columns <- which(substr(colnames(Tmin_data),1,4) == available_years[y])
    
    # Create data frame for hourly THI values
    df_T_daily_hourly           <- as.data.frame(matrix(NA, nrow= length(temp_selected_columns), ncol=24))
    row.names(df_T_daily_hourly) <- seq(1, length(temp_selected_columns),1)
    colnames(df_T_daily_hourly)  <- seq(0,23,1)
    
    # Third loop for each day within a year
    for (d in 1:nrow(df_T_daily_hourly)){
      
      # Get the data for this day
      Tmin_d <- as.numeric(Tmin_data[which(row.names(Tmin_data) == unique(farm_data$Gemeinde)[i]),temp_selected_columns[d]])
      Tmax_d <- as.numeric(Tmax_data[which(row.names(Tmax_data) == unique(farm_data$Gemeinde)[i]),temp_selected_columns[d]])
      Tmin_d1 <- as.numeric(Tmin_data[which(row.names(Tmin_data) == unique(farm_data$Gemeinde)[i]),temp_selected_columns[d]+1])
      
      # Get daily amplitude of first half (fh) and second half (sh) of the day
      amplitude_fh <- (Tmax_d - Tmin_d) / 2
      amplitude_sh <- (Tmax_d - Tmin_d1)/ 2
      
      # Get daily average of first half and second half of the day
      Tmean_fh <- (Tmax_d + Tmin_d) / 2
      Tmean_sh <- (Tmax_d + Tmin_d1)/ 2
      
      # Get hourly THI for the first half of the day (first sine curve)
      # THI_max is at the 13 place (after 12 hours from THI_min, whereas THI_min at the first location)
      for (fh in 1:13){
        df_T_daily_hourly [d,fh] <- Tmean_fh + amplitude_fh * sin(radian_observations[fh])
      }
      
      # Get hourly temperatures for the second half of the day (second sine curve)
      # Location 13 is 12 hours after THI_min
      for (sh in 1:12){
        if(length(Tmin_d1) != 0){
          df_T_daily_hourly[d,12+sh] <- Tmean_sh + amplitude_sh * sin(radian_observations[12+sh])}
        else{df_T_daily_hourly[d,12+sh] <- NA}
        
      }
      rm(Tmin_d, Tmax_d, Tmin_d1, amplitude_fh, Tmean_fh)
      
    }
    list_T_municipality_year[[y]]  <- df_T_daily_hourly
    list_T_municipality_year[[y]]$date <- colnames(Tmin_data)[temp_selected_columns]
    rm(df_T_daily_hourly, temp_selected_columns)
  }
  
  list_T_municipality[[i]] <- list_T_municipality_year  
  rm(list_T_municipality_year)
  print(i / length(unique(farm_data$Gemeinde)))
}
names(list_T_municipality) <- unique(farm_data$Gemeinde)

# -------------------------------------------------------------------------
# Restructure data for panel regression & match with farm data
# -------------------------------------------------------------------------

# Restructuring data
gemeinde_T_exposure1 <-rbindlist(lapply(list_T_municipality, rbindlist),use.names = T,idcol=T)

gemeinde_T_exposure2 <- melt(gemeinde_T_exposure1, id=c(".id","date"))
colnames(gemeinde_T_exposure2) <- c("BFS","Date","Hour", "T")
gemeinde_T_exposure2$year      <-as.numeric(substr(gemeinde_T_exposure2$Date,1,4))
gemeinde_T_exposure2$month      <-as.numeric(substr(gemeinde_T_exposure2$Date,6,7))
save(gemeinde_T_exposure2, file="Meteo/Temperature/Gemeinde_T_hourly.RData")
rm(gemeinde_T_exposure1)

# Match hourly values with farms
# i) Pack it in list

temp_T_hourly <- list()
farm_ID <- unique(dairy_farms_panel_final$farm)

for(i in 1:length(farm_ID)){
  # Get the farm's BFS number
  temp1_BFS <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm == farm_ID[i])[1],"Gemeinde"]
  # Years with data for specific farm i
  temp1_years <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm == farm_ID[i]),"year"]
  # Extract the data
  temp1_T_farm <- gemeinde_T_exposure2[which(gemeinde_T_exposure2$BFS == temp1_BFS & gemeinde_T_exposure2$year %in% temp1_years),]
  temp_T_hourly[[i]] <- temp1_T_farm
  rm(temp1_BFS, temp1_years, temp1_T_farm)
  print(i / length(farm_ID))
} 
names(temp_T_hourly) <- farm_ID

# ii) In one data.frame
season_months <- c(4,5,6,7,8,9,10)
farm_T_hourly <- rbindlist(temp_T_hourly,use.names = T,idcol=T)
colnames(farm_T_hourly) <- c("farm","BFS","date","hour","T","year","month")
farm_T_hourly$month <- as.numeric(farm_T_hourly$month)
farm_T_hourly <- farm_T_hourly[farm_T_hourly$month %in% season_months,] 

save(farm_T_hourly, file="Meteo/Temperature/farm_T_daily.RData")
rm(temp_T_hourly, gemeinde_T_exposure2, Tmax_data,Tmin_data,farm_ID, season_months, list_T_municipality)
