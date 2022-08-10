# ==================================================================================================================================
# ==================================================================================================================================
#
# Modular code for the publication: Heat Risks in Swiss Milk production
#
# Citation: Bucheli, J., Uldry, M. and Finger, R. 2022. Heat Risks in Swiss Milk production. Journal of the Agricultural and Applied
#           Economics Association.
#
# Part 2/9: Matching closest relative humidity station to farms 
#
# ==================================================================================================================================
# ==================================================================================================================================

library(geosphere)
library(lubridate)
library(biogeo)
library(reshape2)

# -------------------------------------------------------------------------
# Minimum relative humidity
# -------------------------------------------------------------------------


# Load & prepare
# Load station rawdata (delivered in 4 packages)
data1 <- read.csv("Meteo/Rel Humidity/Min relative humidity/order_74053_data_prep.txt", sep=";", dec=".")
data2 <- read.csv("Meteo/Rel Humidity/Min relative humidity/order_74054_data_prep.txt", sep=";", dec=".")
data3 <- read.csv("Meteo/Rel Humidity/Min relative humidity/order_74055_data_prep.txt", sep=";", dec=".")
data4 <- read.csv("Meteo/Rel Humidity/Min relative humidity/order_74056_data_prep.txt", sep=";", dec=".")

# Put packages together and remove headings
min_RH_complete <- rbind(data1,data2,data3,data4)
rm(data1,data2,data3,data4)
min_RH_complete <- min_RH_complete[-which(min_RH_complete[,1] == "stn"),] 

# Format relative humidity
min_RH_complete$ure200dn <- as.numeric(as.character(min_RH_complete$ure200dn))

# Format time
min_RH_complete$time <- as.Date(as.character(min_RH_complete$time),format="%Y%m%d")

# Records per station and year
# Focus only on month in which heat stress might occur
# Define these months:
season_months <- c(4,5,6,7,8,9,10)
min_RH_complete$year <- year(min_RH_complete$time)
min_RH_complete$month <- month(min_RH_complete$time)
min_RH_complete <- min_RH_complete[which(min_RH_complete$month %in% season_months),]

stat_availability_RH_min <- as.data.frame(matrix(NA, nrow=length(unique(min_RH_complete[,1])), ncol=length(seq(from=2003,to=2015,by=1))))
colnames(stat_availability_RH_min)  <- seq(2003,2015,1)
row.names(stat_availability_RH_min) <- unique(min_RH_complete[,1])

for(i in 1:nrow(stat_availability_RH_min)){
  # Subset station
  temp1 <- min_RH_complete[which(min_RH_complete$stn == row.names(stat_availability_RH_min)[i]),]
  for (t in 1:ncol(stat_availability_RH_min)){
    # Subset year
    temp2 <- temp1[which(temp1$year == as.numeric(colnames(stat_availability_RH_min)[t])),]
    if(nrow(temp2) == 0) {stat_availability_RH_min[i,t] <- NA}
    else{  stat_availability_RH_min[i,t] <- nrow(temp2[!is.na(temp2[,3]),])}
    rm(temp2)
  }
  rm(temp1)
} 

View(stat_availability_RH_min)

# Subset stations

# Define the critical threshold for days without NA per season
# 214 days from April to October; 0.95 = at least 95% of days without NA
critical_NA <- 214 * 0.95

# Only stations with records in 2003-2015
subset_availability_RH_min <- stat_availability_RH_min[complete.cases(stat_availability_RH_min),]

# Stations with max. 5% NA per year
# 13 = each year fulfills this criterion
temp <- vector()
for (i in 1:nrow(subset_availability_RH_min)){
  temp[i] <- length(which(subset_availability_RH_min[i,] >= critical_NA))
}

temp1 <- which(temp == 13)
subset_availability_RH_min <- subset_availability_RH_min[temp1,]

station_names_available_RH_min <- row.names(subset_availability_RH_min)
rm(temp, temp1, subset_availability_RH_min)


# -------------------------------------------------------------------------
# Maximum relative humidity
# -------------------------------------------------------------------------


# Load & prepare
# Load station rawdata (delivered in 4 packages from MeteoSwiss)

data1 <- read.csv("Meteo/Rel Humidity/Max relative humidity/order_74029_data_prep.txt", sep=";", dec=".")
data2 <- read.csv("Meteo/Rel Humidity/Max relative humidity/order_74030_data_prep.txt", sep=";", dec=".")
data3 <- read.csv("Meteo/Rel Humidity/Max relative humidity/order_74031_data_prep.txt", sep=";", dec=".")
data4 <- read.csv("Meteo/Rel Humidity/Max relative humidity/order_74032_data_prep.txt", sep=";", dec=".")

# Put packages together and remove headings
max_RH_complete <- rbind(data1,data2,data3,data4)
rm(data1,data2,data3,data4)
max_RH_complete <- max_RH_complete[-which(max_RH_complete[,1] == "stn"),] 

# Format relative humidity
max_RH_complete$ure200dx <- as.numeric(as.character(max_RH_complete$ure200dx))

# Format time
max_RH_complete$time <- as.Date(as.character(max_RH_complete$time),format="%Y%m%d")

# Records per station and year

max_RH_complete$year <- year(max_RH_complete$time)
max_RH_complete$month <- month(max_RH_complete$time)
max_RH_complete <- max_RH_complete[which(max_RH_complete$month %in% season_months),]

stat_availability_RH_max <- as.data.frame(matrix(NA, nrow=length(unique(max_RH_complete[,1])), ncol=length(seq(from=2003,to=2015,by=1))))
colnames(stat_availability_RH_max)  <- seq(2003,2015,1)
row.names(stat_availability_RH_max) <- unique(max_RH_complete[,1])

for(i in 1:nrow(stat_availability_RH_max)){
  # Subset station
  temp1 <- max_RH_complete[which(max_RH_complete$stn == row.names(stat_availability_RH_max)[i]),]
  for (t in 1:ncol(stat_availability_RH_max)){
    # Subset year
    temp2 <- temp1[which(temp1$year == as.numeric(colnames(stat_availability_RH_max)[t])),]
    if(nrow(temp2) == 0) {stat_availability_RH_max[i,t] <- NA}
    else{  stat_availability_RH_max[i,t] <- nrow(temp2[!is.na(temp2[,3]),])}
    rm(temp2)
  }
  rm(temp1)
} 

View(stat_availability_RH_max)

# Subset stations
# Here: only stations with records in 2003-2015

subset_availability_RH_max <- stat_availability_RH_max[complete.cases(stat_availability_RH_max),]

# Only stations with max. 5% NA per year
temp <- vector()
for (i in 1:nrow(subset_availability_RH_max)){
  temp[i] <- length(which(subset_availability_RH_max[i,] >= critical_NA))
}

temp1 <- which(temp == 13)
subset_availability_RH_max <- subset_availability_RH_max[temp1,]

station_names_available_RH_max <- row.names(subset_availability_RH_max)
rm(temp, temp1, subset_availability_RH_max)

# Check whether RHmin & RHmax have the same stations
# If result is integer(0), the stations are the same
which(station_names_available_RH_max %in% station_names_available_RH_min == F)

# ----------------------------------------------------------------------------------------------------------------------------------
# Station location
# ----------------------------------------------------------------------------------------------------------------------------------

station_info <- read.csv("Meteo/Rel Humidity/Min relative humidity/Stations1.csv", sep=";")

# Get decimal-degree of coordinates
station_info$long <- dms2dd(dd=station_info$Longitude.deg, mm=station_info$Longitude.min, ss=0, ns=c("E"))
station_info$lat <- dms2dd(dd=station_info$Latitude.deg, mm=station_info$Latitude.min, ss=0, ns=c("N"))

# Subset of stations
station_info_subset <- station_info[which(station_info$stn %in% station_names_available_RH_min),]

# Stations as spatial points
coordinates_stations <- station_info_subset[,c("long","lat")]
coordinates(coordinates_stations) <- c("long" , "lat")
proj4string(coordinates_stations)  <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Check transformation
coordinates_station_subset_map <- spTransform(coordinates_stations,CRS(proj4string(swiss_elevation)))

# Check projection (quality check)
plot(swiss_elevation)
points(coordinates_station_subset_map, col="red")

# ----------------------------------------------------------------------------------------------------------------------------------
# Distances between stations and municipalities
# ---------------------------------------------------------------------------------------------------------------------------------- 

# Weighting factor for elevation
elevation_factor <- 100

# Distances between farms and stations (horizontal & vertical)

# Horizontal distance
distance_matrix_RH <- matrix(NA, nrow=length(unique(farm_data$Gemeinde)), ncol=nrow(station_info_subset))
row.names(distance_matrix_RH) <- sort(unique(farm_data$Gemeinde))
colnames(distance_matrix_RH)  <- station_info_subset$stn

for (i in 1:nrow(distance_matrix_RH)){
  for (s in 1:nrow(station_info_subset)){
    distance_matrix_RH[i,s] <- distm(c(meta_gemeinden[which(meta_gemeinden$BFS == row.names(distance_matrix_RH)[i]), "Longitude"],meta_gemeinden[which(meta_gemeinden$BFS == row.names(distance_matrix_RH)[i]), "Latitude"]),c(station_info_subset[s,"long"],station_info_subset[s,"lat"]), fun=distGeo)
  }
}

# Vertical distance (elevation)
elevation_matrix_RH <- matrix(NA, nrow=length(unique(farm_data$Gemeinde)), ncol=nrow(station_info_subset))
row.names(elevation_matrix_RH) <- sort(unique(farm_data$Gemeinde))
colnames(elevation_matrix_RH)  <- station_info_subset$stn

for (i in 1:nrow(elevation_matrix_RH)){
  for (s in 1:nrow(station_info_subset)){
    elevation_matrix_RH[i,s] <- abs(meta_gemeinden[which(meta_gemeinden$BFS == row.names(distance_matrix_RH)[i]), "elevation"] - station_info_subset[s,"Elevation"])
  }
}

# 3D distances (horizontal and vertical distances)
dim_distance_matrix_RH <- matrix(NA, nrow=length(unique(farm_data$Gemeinde)), ncol=nrow(station_info_subset))
row.names(dim_distance_matrix_RH) <- sort(unique(farm_data$Gemeinde))
colnames(dim_distance_matrix_RH)  <- station_info_subset$stn

for (i in 1:nrow(dim_distance_matrix_RH)){
  for (s in 1:nrow(station_info_subset)){
    dim_distance_matrix_RH[i,s] <- sqrt(distance_matrix_RH[i,s]^2 + (elevation_factor * elevation_matrix_RH[i,s])^2)
  }
}

# ----------------------------------------------------------------------------------------------------------------------------------
# Minimum relative humidity interpolation (municipality center to next station)
# ----------------------------------------------------------------------------------------------------------------------------------

sub_min_RH_complete <- min_RH_complete[which(min_RH_complete$stn %in% station_names_available_RH_min),]

# No negative values allowed for RHmin
sub_min_RH_complete[which(sub_min_RH_complete$ure200dn < 0),3] <- NA

sub_min_RH_complete[,3] <- as.numeric(as.character(sub_min_RH_complete[,3]))
sub_min_RH_complete[,2] <- as.Date(sub_min_RH_complete[,2], format="%Y%m%d")

interpolated_RH_min <- as.data.frame(matrix(NA, nrow=length(unique(farm_data$Gemeinde)), ncol=length(unique(sub_min_RH_complete$time))))
row.names(interpolated_RH_min) <- unique(farm_data$Gemeinde)
colnames(interpolated_RH_min) <- as.Date(unique(sub_min_RH_complete$time), format="%Y%m%d")

for(i in 1:nrow(interpolated_RH_min)){
  
  # Find number of closest stations
  temp_stat_close_name <- sort(dim_distance_matrix_RH[which(row.names(dim_distance_matrix_RH) == row.names(interpolated_RH_min)[i]),])
  
  for (d in 1:ncol(interpolated_RH_min)){
    
    if (!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[1]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[1]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[2]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[2]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[3]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[3]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[4]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[4]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[5]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[5]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[6]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[6]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[7]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[7]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[8]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[8]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[9]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[9]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    }  else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[10]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[10]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    }  else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[11]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[11]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    }  else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[12]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[12]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    }  else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[13]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[13]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    }  else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[14]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[14]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    }  else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[15]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[15]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    }  else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[16]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[16]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    }  else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[17]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[17]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[18]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[18]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[19]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[19]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[20]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[20]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[21]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[21]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[22]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[22]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[23]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[23]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else if(!is.na(sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[24]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3])){
      interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[24]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]
    } else {interpolated_RH_min[i,d] <- sub_min_RH_complete[which(sub_min_RH_complete$stn == names(temp_stat_close_name[25]) & sub_min_RH_complete$time == colnames(interpolated_RH_min)[d]),3]}
    print(paste("# Gemeinde",i,"progress gemeinde (%)",i /nrow(interpolated_RH_min) ,"progress day" ,d / ncol(interpolated_RH_min)))
  }
  rm(temp_stat_close_name)
  print(i / nrow(interpolated_RH_min))
}
save(interpolated_RH_min, file="Meteo/Rel Humidity/interpolated_municipality/interpolated_RH_min_nextstation.RData")

# Long form
interpolated_RH_min1 <- interpolated_RH_min
interpolated_RH_min1$BFS <- row.names(interpolated_RH_min)
interpolated_RH_min1 <- melt(interpolated_RH_min1, id=c("BFS"))
summary(interpolated_RH_min1$value)

# ----------------------------------------------------------------------------------------------------------------------------------
# Maximum relative humidity interpolation (municipality center to next station)
# ----------------------------------------------------------------------------------------------------------------------------------

sub_max_RH_complete <- max_RH_complete[which(max_RH_complete$stn %in% station_names_available_RH_min),]
sub_max_RH_complete[,3] <- as.numeric(as.character(sub_max_RH_complete[,3]))
sub_max_RH_complete[,2] <- as.Date(sub_max_RH_complete[,2], format="%Y%m%d")

interpolated_RH_max <- as.data.frame(matrix(NA, nrow=length(unique(farm_data$Gemeinde)), ncol=length(unique(sub_max_RH_complete$time))))
row.names(interpolated_RH_max) <- unique(farm_data$Gemeinde)
colnames(interpolated_RH_max) <- as.Date(unique(sub_max_RH_complete$time), format="%Y%m%d")

for(i in 1:nrow(interpolated_RH_max)){
  
  # Find number of closest stations
  temp_stat_close_name <- sort(dim_distance_matrix_RH[which(row.names(dim_distance_matrix_RH) == row.names(interpolated_RH_max)[i]),])
  
  for (d in 1:ncol(interpolated_RH_max)){
    
    if (!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[1]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[1]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[2]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[2]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[3]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[3]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[4]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[4]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[5]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[5]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[6]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[6]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[7]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[7]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[8]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[8]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[9]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[9]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    }  else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[10]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[10]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    }  else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[11]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[11]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    }  else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[12]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[12]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    }  else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[13]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[13]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    }  else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[14]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[14]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    }  else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[15]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[15]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    }  else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[16]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[16]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    }  else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[17]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[17]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[18]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[18]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[19]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[19]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[20]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[20]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[21]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[21]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[22]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[22]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[23]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[23]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else if(!is.na(sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[24]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3])){
      interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[24]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]
    } else {interpolated_RH_max[i,d] <- sub_max_RH_complete[which(sub_max_RH_complete$stn == names(temp_stat_close_name[25]) & sub_max_RH_complete$time == colnames(interpolated_RH_max)[d]),3]}
    print(paste("# Gemeinde",i,"progress gemeinde (%)",i /nrow(interpolated_RH_max) ,"progress day" ,d / ncol(interpolated_RH_max)))
  }
  rm(temp_stat_close_name)
  print(i / nrow(interpolated_RH_max))
}
save(interpolated_RH_max, file="Meteo/Rel Humidity/interpolated_municipality/interpolated_RH_max_nextstation.RData")

# Long form
interpolated_RH_max1 <- interpolated_RH_max
interpolated_RH_max1$BFS <- row.names(interpolated_RH_max)
interpolated_RH_max1 <- melt(interpolated_RH_max1, id=c("BFS"))
summary(interpolated_RH_max1$value)

rm(interpolated_RH_max1, interpolated_RH_min1, max_RH_complete, min_RH_complete, sub_max_RH_complete, sub_min_RH_complete)
rm(coordinates_gemeinden, coordinates_gemeinden_map, coordinates_station_subset_map, coordinates_stations)
rm(Switzerland, Switzerland_ch,swiss_elevation, elevation_matrix_RH, distance_matrix_RH, dim_distance_matrix_RH)
