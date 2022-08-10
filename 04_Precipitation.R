# ==================================================================================================================================
# ==================================================================================================================================
#
# Modular code for the publication: Heat Risks in Swiss Milk production
#
# Citation: Bucheli, J., Uldry, M. and Finger, R. 2022. Heat Risks in Swiss Milk production. Journal of the Agricultural and Applied
#           Economics Association.
#
# Part 4/9: Calculate cumulative precipitation for each farm
# Note: Precipitation data derived with the scripts found here: https://github.com/AECP-ETHZ/Spatial-extraction-of-Swiss-weather-data
#
# ==================================================================================================================================
# ==================================================================================================================================


library(reshape2)

precip_data <- read.csv("Meteo/Precipitation/daily precipitation.csv", dec=",", sep=";")[,-1]
row.names(precip_data) <- precip_data$BFS

precip_data2 <- precip_data[,-1]
colnames(precip_data2) <- as.Date(seq(as.Date("2003-01-01", format="%Y-%m-%d"), as.Date("2015-12-31", format="%Y-%m-%d"),1))

precip_data2$BFS <- row.names(precip_data2)

gemeinde_precip <- melt(precip_data2, id.vars = "BFS")
gemeinde_precip$year <- substr(gemeinde_precip$variable,1,4)
gemeinde_precip$month <- substr(gemeinde_precip$variable,6,7)
colnames(gemeinde_precip) <- c("BFS", "date", "precip","year","month")

# Match daily precipitation values with farms
# i) Pack it in list

temp_precip_daily <- list()
farm_ID <- unique(dairy_farms_panel_final$farm)

for(i in 1:length(farm_ID)){
  # Get the farm's BFS number
  temp1_BFS <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm == farm_ID[i])[1],"Gemeinde"]
  # Years with data for specific farm i
  temp1_years <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm == farm_ID[i]),"year"]
  # Extract the data
  temp1_precip_farm <- gemeinde_precip[which(gemeinde_precip$BFS == temp1_BFS & gemeinde_precip$year %in% temp1_years),]
  temp_precip_daily[[i]] <- temp1_precip_farm
  rm(temp1_BFS, temp1_years, temp1_precip_farm)
  print(i / length(farm_ID))
} 
names(temp_precip_daily) <- farm_ID

# ii) In one data.frame and only month from April to October
season_months <- c(4,5,6,7,8,9,10)
farm_precip_daily_fullyear <- as.data.frame(matrix(NA))
farm_precip_daily_fullyear <- rbindlist(temp_precip_daily,use.names = T,idcol=T)
colnames(farm_precip_daily_fullyear) <- c("farm","BFS","date","precip","year","month")
farm_precip_daily_fullyear$month <- as.numeric(farm_precip_daily_fullyear$month)
farm_precip_daily <- farm_precip_daily_fullyear[farm_precip_daily_fullyear$month %in% season_months,] 

save(farm_precip_daily, file="Meteo/Precipitation/farm_precip_daily.RData")
rm(precip_data, precip_data2, gemeinde_precip, temp_precip_daily, farm_ID, season_months, farm_precip_daily_fullyear)
