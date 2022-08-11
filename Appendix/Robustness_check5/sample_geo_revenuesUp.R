# ==================================================================================================================================
# ==================================================================================================================================
#
# Modular code for the publication: Heat Risks in Swiss Milk production
#
# Citation: Bucheli, J., Uldry, M. and Finger, R. 2022. Heat Risks in Swiss Milk production. Journal of the Agricultural and Applied
#           Economics Association.
#
# Robustness check 5: sample selection for farms with high share of milk revenues on total revenues
#
# ==================================================================================================================================
# ==================================================================================================================================

library(anchors)
library(raster)
library(rgdal)
library(rgeos)
library(xlsx)

# ----------------------------------------------------------------------------------------------------------------------------------
# Get meta data from Swiss Gemeinden (farms are matched with center of municipalities)
# ----------------------------------------------------------------------------------------------------------------------------------

# Read shapefile with all Gemeinden (municipalities) of Switzerland and convert it to GPS format
data_gemeinden <- readOGR(dsn="Data/Karte/Gemeindekarte", layer = "CH_Gemeinde_new") 
data_gemeinden_GPS <- spTransform(data_gemeinden, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Get coordinates of the center of each Gemeinde
centre_gemeinden <- gCentroid(data_gemeinden_GPS, byid = T)

# Data frame with relevant municipality information
meta_gemeinden           <- data.matrix(matrix(NA, nrow=length(data_gemeinden$NAME), ncol=4))
colnames(meta_gemeinden) <- c("Name", "BFS", "Latitude", "Longitude")

# Adjust format
meta_gemeinden[,1] <- as.character(data_gemeinden$NAME)
meta_gemeinden[,2] <- as.character(data_gemeinden$BFS_NUMMER)
meta_gemeinden[,3] <- as.numeric(centre_gemeinden@coords[,2])
meta_gemeinden[,4] <- as.numeric(centre_gemeinden@coords[,1])

# Save meta_gemeinden and read it to get it as data.frame
write.csv(meta_gemeinden, file="meta_gemeinden.csv")
meta_gemeinden <- read.csv("meta_gemeinden.csv")[,-1]

# Some municipalities have two records (when their area is/was spatially separated)
# We then take the center of the first record in data_gemeinden (usually the larger/ main area)
meta_gemeinden <- meta_gemeinden[-which(duplicated(meta_gemeinden$BFS)),]

# ----------------------------------------------------------------------------------------------------------------------------------
# Get elevation of municipality centers
# ----------------------------------------------------------------------------------------------------------------------------------

# Swiss digital elevation map from: https://opendata.swiss/en/organization/bundesamt-fur-landestopografie-swisstopo
swiss_elevation       <- raster("Data/Karte/Höhenkarte/DHM200.asc")
crs(swiss_elevation)  <- CRS("+init=epsg:21781")

coordinates_gemeinden <- meta_gemeinden[,c("Longitude", "Latitude")]
coordinates(coordinates_gemeinden) <- c("Longitude" , "Latitude")
proj4string(coordinates_gemeinden)  <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Change projection of municipalities to projection of elevation map
coordinates_gemeinden_map <- spTransform(coordinates_gemeinden,CRS(proj4string(swiss_elevation)))

# Check projection (quality check)
plot(swiss_elevation)
points(coordinates_gemeinden_map)

# Get elevation of Gemeinde centers
meta_gemeinden$elevation <-  extract(swiss_elevation,coordinates_gemeinden_map)
rm(data_gemeinden, data_gemeinden_GPS, centre_gemeinden,swiss_elevation, coordinates_gemeinden_map)

# ----------------------------------------------------------------------------------------------------------------------------------
# Load and prepare farm accountancy data 
# ----------------------------------------------------------------------------------------------------------------------------------

farm_data <- read.csv("Data/FADN/Datengesamtbetrieblich_ETH_Finger_MA_impact_weather_milkprod_SpRef.csv", sep=";")

# Manually Correction of BFS number (= Geminde (municipality) ID) for municipality mergers
# i) Identify old BFS-Number

BFS_farms      <- sort(unique(farm_data$Gemeinde))
BFS_gemeinde   <- sort(meta_gemeinden$BFS)
BFS_farms_old  <- BFS_farms[-which(BFS_farms %in% meta_gemeinden$BFS)]

# ii) Change old BFS-Number (after mergers with other municialities) manually
# See https://www.agvchapp.bfs.admin.ch/de/communes/results?Name=Bauma&EntriesFrom=01.01.1960&EntriesTo=01.01.2020

# Farms in municipalities with mergers get latest BFS identification number 
farm_data <- replace.value(farm_data, "Gemeinde", from=171, to=297)
farm_data <- replace.value(farm_data, "Gemeinde", from=174, to=296)
farm_data <- replace.value(farm_data, "Gemeinde", from=330, to=332)
farm_data <- replace.value(farm_data, "Gemeinde", from=343, to=329)
farm_data <- replace.value(farm_data, "Gemeinde", from=417, to=405)
farm_data <- replace.value(farm_data, "Gemeinde", from=537, to=538)
farm_data <- replace.value(farm_data, "Gemeinde", from=539, to=538)
farm_data <- replace.value(farm_data, "Gemeinde", from=542, to=538)
farm_data <- replace.value(farm_data, "Gemeinde", from=545, to=538)
farm_data <- replace.value(farm_data, "Gemeinde", from=555, to=538)
farm_data <- replace.value(farm_data, "Gemeinde", from=618, to=632)
farm_data <- replace.value(farm_data, "Gemeinde", from=621, to=632)
farm_data <- replace.value(farm_data, "Gemeinde", from=625, to=616)
farm_data <- replace.value(farm_data, "Gemeinde", from=631, to=616)
farm_data <- replace.value(farm_data, "Gemeinde", from=764, to=770)
farm_data <- replace.value(farm_data, "Gemeinde", from=765, to=770)
farm_data <- replace.value(farm_data, "Gemeinde", from=781, to=784)
farm_data <- replace.value(farm_data, "Gemeinde", from=851, to=855)
farm_data <- replace.value(farm_data, "Gemeinde", from=854, to=855)
farm_data <- replace.value(farm_data, "Gemeinde", from=862, to=861)
farm_data <- replace.value(farm_data, "Gemeinde", from=864, to=888)
farm_data <- replace.value(farm_data, "Gemeinde", from=871, to=885)
farm_data <- replace.value(farm_data, "Gemeinde", from=882, to=879)
farm_data <- replace.value(farm_data, "Gemeinde", from=887, to=888)
farm_data <- replace.value(farm_data, "Gemeinde", from=926, to=948)
farm_data <- replace.value(farm_data, "Gemeinde", from=974, to=973)
farm_data <- replace.value(farm_data, "Gemeinde", from=984, to=979)
farm_data <- replace.value(farm_data, "Gemeinde", from=986, to=977)
farm_data <- replace.value(farm_data, "Gemeinde", from=994, to=977)
farm_data <- replace.value(farm_data, "Gemeinde", from=1006, to=1010)
farm_data <- replace.value(farm_data, "Gemeinde", from=1622, to=1630)
farm_data <- replace.value(farm_data, "Gemeinde", from=1627, to=1631)
farm_data <- replace.value(farm_data, "Gemeinde", from=2446, to=2465)
farm_data <- replace.value(farm_data, "Gemeinde", from=2447, to=2457)
farm_data <- replace.value(farm_data, "Gemeinde", from=2459, to=2464)
farm_data <- replace.value(farm_data, "Gemeinde", from=2521, to=2535)
farm_data <- replace.value(farm_data, "Gemeinde", from=3333, to=3342)
farm_data <- replace.value(farm_data, "Gemeinde", from=3335, to=3340)
farm_data <- replace.value(farm_data, "Gemeinde", from=3337, to=3342)
farm_data <- replace.value(farm_data, "Gemeinde", from=3351, to=3359)
farm_data <- replace.value(farm_data, "Gemeinde", from=3355, to=3360)
farm_data <- replace.value(farm_data, "Gemeinde", from=3357, to=3359)
farm_data <- replace.value(farm_data, "Gemeinde", from=3358, to=3360)
farm_data <- replace.value(farm_data, "Gemeinde", from=3376, to=3378)
farm_data <- replace.value(farm_data, "Gemeinde", from=3377, to=3379)
farm_data <- replace.value(farm_data, "Gemeinde", from=3391, to=3395)
farm_data <- replace.value(farm_data, "Gemeinde", from=4011, to=4001)
farm_data <- replace.value(farm_data, "Gemeinde", from=4070, to=4080)
farm_data <- replace.value(farm_data, "Gemeinde", from=4101, to=4184)
farm_data <- replace.value(farm_data, "Gemeinde", from=4108, to=4124)
farm_data <- replace.value(farm_data, "Gemeinde", from=4115, to=4125)
farm_data <- replace.value(farm_data, "Gemeinde", from=4119, to=4124)
farm_data <- replace.value(farm_data, "Gemeinde", from=4174, to=4184)
farm_data <- replace.value(farm_data, "Gemeinde", from=4178, to=4170)
farm_data <- replace.value(farm_data, "Gemeinde", from=4180, to=4184)
farm_data <- replace.value(farm_data, "Gemeinde", from=4225, to=4234)
farm_data <- replace.value(farm_data, "Gemeinde", from=4541, to=4545)
farm_data <- replace.value(farm_data, "Gemeinde", from=229, to=298)
farm_data <- replace.value(farm_data, "Gemeinde", from=1003, to=1010)
farm_data <- replace.value(farm_data, "Gemeinde", from=1027, to=1030)
farm_data <- replace.value(farm_data, "Gemeinde", from=1028, to=1030)
farm_data <- replace.value(farm_data, "Gemeinde", from=1029, to=1039)
farm_data <- replace.value(farm_data, "Gemeinde", from=1036, to=1030)
farm_data <- replace.value(farm_data, "Gemeinde", from=1038, to=1030)
farm_data <- replace.value(farm_data, "Gemeinde", from=1042, to=1030)
farm_data <- replace.value(farm_data, "Gemeinde", from=1087, to=1081)
farm_data <- replace.value(farm_data, "Gemeinde", from=1092, to=1081)
farm_data <- replace.value(farm_data, "Gemeinde", from=1096, to=1097)
farm_data <- replace.value(farm_data, "Gemeinde", from=1101, to=1081)
farm_data <- replace.value(farm_data, "Gemeinde", from=1105, to=1104)
farm_data <- replace.value(farm_data, "Gemeinde", from=1106, to=1104)
farm_data <- replace.value(farm_data, "Gemeinde", from=1133, to=1128)
farm_data <- replace.value(farm_data, "Gemeinde", from=1134, to=1140)
farm_data <- replace.value(farm_data, "Gemeinde", from=1141, to=1140)
farm_data <- replace.value(farm_data, "Gemeinde", from=1144, to=1125)
farm_data <- replace.value(farm_data, "Gemeinde", from=1148, to=1151)
farm_data <- replace.value(farm_data, "Gemeinde", from=1602, to=1630)
farm_data <- replace.value(farm_data, "Gemeinde", from=1603, to=1631)
farm_data <- replace.value(farm_data, "Gemeinde", from=1605, to=1631)
farm_data <- replace.value(farm_data, "Gemeinde", from=1606, to=1631)
farm_data <- replace.value(farm_data, "Gemeinde", from=1607, to=1632)
farm_data <- replace.value(farm_data, "Gemeinde", from=1608, to=1630)
farm_data <- replace.value(farm_data, "Gemeinde", from=1609, to=1632)
farm_data <- replace.value(farm_data, "Gemeinde", from=1610, to=1631)
farm_data <- replace.value(farm_data, "Gemeinde", from=1613, to=1631)
farm_data <- replace.value(farm_data, "Gemeinde", from=1614, to=1631)
farm_data <- replace.value(farm_data, "Gemeinde", from=1615, to=1631)
farm_data <- replace.value(farm_data, "Gemeinde", from=1616, to=1631)
farm_data <- replace.value(farm_data, "Gemeinde", from=1617, to=1630)
farm_data <- replace.value(farm_data, "Gemeinde", from=1619, to=1630)
farm_data <- replace.value(farm_data, "Gemeinde", from=1620, to=1632)
farm_data <- replace.value(farm_data, "Gemeinde", from=1623, to=1630)
farm_data <- replace.value(farm_data, "Gemeinde", from=1624, to=1630)
farm_data <- replace.value(farm_data, "Gemeinde", from=1628, to=1631)
farm_data <- replace.value(farm_data, "Gemeinde", from=1629, to=1631)
farm_data <- replace.value(farm_data, "Gemeinde", from=175, to=296)
farm_data <- replace.value(farm_data, "Gemeinde", from=2002, to=2054)
farm_data <- replace.value(farm_data, "Gemeinde", from=2013, to=2053)
farm_data <- replace.value(farm_data, "Gemeinde", from=2015, to=2054)
farm_data <- replace.value(farm_data, "Gemeinde", from=2018, to=2054)
farm_data <- replace.value(farm_data, "Gemeinde", from=2037, to=2044)
farm_data <- replace.value(farm_data, "Gemeinde", from=2040, to=2053)
farm_data <- replace.value(farm_data, "Gemeinde", from=2052, to=2054)
farm_data <- replace.value(farm_data, "Gemeinde", from=2070, to=2114)
farm_data <- replace.value(farm_data, "Gemeinde", from=2074, to=2102)
farm_data <- replace.value(farm_data, "Gemeinde", from=2077, to=2097)
farm_data <- replace.value(farm_data, "Gemeinde", from=2093, to=2114)
farm_data <- replace.value(farm_data, "Gemeinde", from=2094, to=2099)
farm_data <- replace.value(farm_data, "Gemeinde", from=212, to=115)
farm_data <- replace.value(farm_data, "Gemeinde", from=2127, to=2163)
farm_data <- replace.value(farm_data, "Gemeinde", from=2154, to=2125)
farm_data <- replace.value(farm_data, "Gemeinde", from=2172, to=2175)
farm_data <- replace.value(farm_data, "Gemeinde", from=2179, to=2183)
farm_data <- replace.value(farm_data, "Gemeinde", from=2184, to=2236)
farm_data <- replace.value(farm_data, "Gemeinde", from=2192, to=2236)
farm_data <- replace.value(farm_data, "Gemeinde", from=2222, to=2236)
farm_data <- replace.value(farm_data, "Gemeinde", from=2223, to=2236)
farm_data <- replace.value(farm_data, "Gemeinde", from=2227, to=2236)
farm_data <- replace.value(farm_data, "Gemeinde", from=2231, to=2236)
farm_data <- replace.value(farm_data, "Gemeinde", from=2243, to=2254)
farm_data <- replace.value(farm_data, "Gemeinde", from=2244, to=2275)
farm_data <- replace.value(farm_data, "Gemeinde", from=2247, to=2262)
farm_data <- replace.value(farm_data, "Gemeinde", from=2253, to=2254)
farm_data <- replace.value(farm_data, "Gemeinde", from=2263, to=2262)
farm_data <- replace.value(farm_data, "Gemeinde", from=2264, to=2275)
farm_data <- replace.value(farm_data, "Gemeinde", from=2277, to=2275)
farm_data <- replace.value(farm_data, "Gemeinde", from=2279, to=2254)
farm_data <- replace.value(farm_data, "Gemeinde", from=2280, to=2284)
farm_data <- replace.value(farm_data, "Gemeinde", from=2281, to=2284)
farm_data <- replace.value(farm_data, "Gemeinde", from=2283, to=2254)
farm_data <- replace.value(farm_data, "Gemeinde", from=2332, to=2338)
farm_data <- replace.value(farm_data, "Gemeinde", from=2441, to=2465)
farm_data <- replace.value(farm_data, "Gemeinde", from=2443, to=2457)
farm_data <- replace.value(farm_data, "Gemeinde", from=2444, to=2465)
farm_data <- replace.value(farm_data, "Gemeinde", from=2449, to=2465)
farm_data <- replace.value(farm_data, "Gemeinde", from=2452, to=2465)
farm_data <- replace.value(farm_data, "Gemeinde", from=2454, to=2464)
farm_data <- replace.value(farm_data, "Gemeinde", from=2462, to=2465)
farm_data <- replace.value(farm_data, "Gemeinde", from=2496, to=2503)
farm_data <- replace.value(farm_data, "Gemeinde", from=2531, to=2511)
farm_data <- replace.value(farm_data, "Gemeinde", from=2911, to=2920)
farm_data <- replace.value(farm_data, "Gemeinde", from=2912, to=2920)
farm_data <- replace.value(farm_data, "Gemeinde", from=308, to=304)
farm_data <- replace.value(farm_data, "Gemeinde", from=328, to=332)
farm_data <- replace.value(farm_data, "Gemeinde", from=3314, to=3341)
farm_data <- replace.value(farm_data, "Gemeinde", from=3331, to=3341)
farm_data <- replace.value(farm_data, "Gemeinde", from=3332, to=3342)
farm_data <- replace.value(farm_data, "Gemeinde", from=3334, to=3341)
farm_data <- replace.value(farm_data, "Gemeinde", from=3356, to=3360)
farm_data <- replace.value(farm_data, "Gemeinde", from=3371, to=3378)
farm_data <- replace.value(farm_data, "Gemeinde", from=3373, to=3379)
farm_data <- replace.value(farm_data, "Gemeinde", from=3403, to=3395)
farm_data <- replace.value(farm_data, "Gemeinde", from=3406, to=3378)
farm_data <- replace.value(farm_data, "Gemeinde", from=3421, to=3427)
farm_data <- replace.value(farm_data, "Gemeinde", from=3501, to=3542)
farm_data <- replace.value(farm_data, "Gemeinde", from=3511, to=3542)
farm_data <- replace.value(farm_data, "Gemeinde", from=3512, to=3542)
farm_data <- replace.value(farm_data, "Gemeinde", from=3536, to=3543)
farm_data <- replace.value(farm_data, "Gemeinde", from=3538, to=3543)
farm_data <- replace.value(farm_data, "Gemeinde", from=3541, to=3543)
farm_data <- replace.value(farm_data, "Gemeinde", from=3571, to=3619)
farm_data <- replace.value(farm_data, "Gemeinde", from=3573, to=3988)
farm_data <- replace.value(farm_data, "Gemeinde", from=3580, to=3619)
farm_data <- replace.value(farm_data, "Gemeinde", from=3583, to=3619)
farm_data <- replace.value(farm_data, "Gemeinde", from=3584, to=3619)
farm_data <- replace.value(farm_data, "Gemeinde", from=3586, to=3672)
farm_data <- replace.value(farm_data, "Gemeinde", from=3587, to=3672)
farm_data <- replace.value(farm_data, "Gemeinde", from=3592, to=3618)
farm_data <- replace.value(farm_data, "Gemeinde", from=3594, to=3618)
farm_data <- replace.value(farm_data, "Gemeinde", from=3596, to=3618)
farm_data <- replace.value(farm_data, "Gemeinde", from=3599, to=3618)
farm_data <- replace.value(farm_data, "Gemeinde", from=3601, to=3618)
farm_data <- replace.value(farm_data, "Gemeinde", from=3604, to=3618)
farm_data <- replace.value(farm_data, "Gemeinde", from=3606, to=3618)
farm_data <- replace.value(farm_data, "Gemeinde", from=3612, to=3988)
farm_data <- replace.value(farm_data, "Gemeinde", from=3631, to=3673)
farm_data <- replace.value(farm_data, "Gemeinde", from=3639, to=3673)
farm_data <- replace.value(farm_data, "Gemeinde", from=3641, to=3673)
farm_data <- replace.value(farm_data, "Gemeinde", from=3642, to=3673)
farm_data <- replace.value(farm_data, "Gemeinde", from=3651, to=3672)
farm_data <- replace.value(farm_data, "Gemeinde", from=3652, to=3672)
farm_data <- replace.value(farm_data, "Gemeinde", from=3671, to=3673)
farm_data <- replace.value(farm_data, "Gemeinde", from=3743, to=3746)
farm_data <- replace.value(farm_data, "Gemeinde", from=3745, to=3762)
farm_data <- replace.value(farm_data, "Gemeinde", from=3751, to=3764)
farm_data <- replace.value(farm_data, "Gemeinde", from=3763, to=3762)
farm_data <- replace.value(farm_data, "Gemeinde", from=3803, to=3837)
farm_data <- replace.value(farm_data, "Gemeinde", from=384, to=306)
farm_data <- replace.value(farm_data, "Gemeinde", from=3912, to=3911)
farm_data <- replace.value(farm_data, "Gemeinde", from=3913, to=3911)
farm_data <- replace.value(farm_data, "Gemeinde", from=3915, to=3932)
farm_data <- replace.value(farm_data, "Gemeinde", from=3929, to=3921)
farm_data <- replace.value(farm_data, "Gemeinde", from=4103, to=4124)
farm_data <- replace.value(farm_data, "Gemeinde", from=436, to=450)
farm_data <- replace.value(farm_data, "Gemeinde", from=439, to=450)
farm_data <- replace.value(farm_data, "Gemeinde", from=440, to=449)
farm_data <- replace.value(farm_data, "Gemeinde", from=5452, to=5464)
farm_data <- replace.value(farm_data, "Gemeinde", from=5461, to=5451)
farm_data <- replace.value(farm_data, "Gemeinde", from=5463, to=5464)
farm_data <- replace.value(farm_data, "Gemeinde", from=5662, to=5675)
farm_data <- replace.value(farm_data, "Gemeinde", from=5685, to=5693)
farm_data <- replace.value(farm_data, "Gemeinde", from=5787, to=5805)
farm_data <- replace.value(farm_data, "Gemeinde", from=5793, to=5805)
farm_data <- replace.value(farm_data, "Gemeinde", from=5795, to=5805)
farm_data <- replace.value(farm_data, "Gemeinde", from=5814, to=5831)
farm_data <- replace.value(farm_data, "Gemeinde", from=5815, to=5831)
farm_data <- replace.value(farm_data, "Gemeinde", from=5818, to=5831)
farm_data <- replace.value(farm_data, "Gemeinde", from=5829, to=5831)
farm_data <- replace.value(farm_data, "Gemeinde", from=6064, to=6077)
farm_data <- replace.value(farm_data, "Gemeinde", from=6071, to=6076)
farm_data <- replace.value(farm_data, "Gemeinde", from=6073, to=6077)
farm_data <- replace.value(farm_data, "Gemeinde", from=6074, to=6077)
farm_data <- replace.value(farm_data, "Gemeinde", from=6075, to=6077)
farm_data <- replace.value(farm_data, "Gemeinde", from=6115, to=6119)
farm_data <- replace.value(farm_data, "Gemeinde", from=6171, to=6205)
farm_data <- replace.value(farm_data, "Gemeinde", from=6237, to=6252)
farm_data <- replace.value(farm_data, "Gemeinde", from=6471, to=6487)
farm_data <- replace.value(farm_data, "Gemeinde", from=6473, to=6487)
farm_data <- replace.value(farm_data, "Gemeinde", from=6475, to=6487)
farm_data <- replace.value(farm_data, "Gemeinde", from=6477, to=6487)
farm_data <- replace.value(farm_data, "Gemeinde", from=6483, to=6487)
farm_data <- replace.value(farm_data, "Gemeinde", from=6484, to=6487)
farm_data <- replace.value(farm_data, "Gemeinde", from=6486, to=6487)
farm_data <- replace.value(farm_data, "Gemeinde", from=6501, to=6512)
farm_data <- replace.value(farm_data, "Gemeinde", from=6502, to=6512)
farm_data <- replace.value(farm_data, "Gemeinde", from=6509, to=6512)
farm_data <- replace.value(farm_data, "Gemeinde", from=6510, to=6512)
farm_data <- replace.value(farm_data, "Gemeinde", from=6707, to=6729)
farm_data <- replace.value(farm_data, "Gemeinde", from=6714, to=6729)
farm_data <- replace.value(farm_data, "Gemeinde", from=6717, to=6730)
farm_data <- replace.value(farm_data, "Gemeinde", from=6725, to=6729)
farm_data <- replace.value(farm_data, "Gemeinde", from=6726, to=6730)
farm_data <- replace.value(farm_data, "Gemeinde", from=6727, to=6730)
farm_data <- replace.value(farm_data, "Gemeinde", from=6746, to=6808)
farm_data <- replace.value(farm_data, "Gemeinde", from=6747, to=6808)
farm_data <- replace.value(farm_data, "Gemeinde", from=6752, to=6751)
farm_data <- replace.value(farm_data, "Gemeinde", from=6776, to=6790)
farm_data <- replace.value(farm_data, "Gemeinde", from=6777, to=6807)
farm_data <- replace.value(farm_data, "Gemeinde", from=6786, to=6807)
farm_data <- replace.value(farm_data, "Gemeinde", from=6788, to=6809)
farm_data <- replace.value(farm_data, "Gemeinde", from=6791, to=6810)
farm_data <- replace.value(farm_data, "Gemeinde", from=6795, to=6808)
farm_data <- replace.value(farm_data, "Gemeinde", from=6797, to=6808)
farm_data <- replace.value(farm_data, "Gemeinde", from=6798, to=6808)
farm_data <- replace.value(farm_data, "Gemeinde", from=6802, to=6809)
farm_data <- replace.value(farm_data, "Gemeinde", from=684, to=716)
farm_data <- replace.value(farm_data, "Gemeinde", from=697, to=717)
farm_data <- replace.value(farm_data, "Gemeinde", from=710, to=716)
farm_data <- replace.value(farm_data, "Gemeinde", from=712, to=716)
farm_data <- replace.value(farm_data, "Gemeinde", from=721, to=726)
farm_data <- replace.value(farm_data, "Gemeinde", from=722, to=726)
farm_data <- replace.value(farm_data, "Gemeinde", from=725, to=726)
farm_data <- replace.value(farm_data, "Gemeinde", from=753, to=756)


# ii) Calculate aggregated feed and health costs
farm_data$sk_Futter_tot <- farm_data$sk_KraftfuRind + farm_data$sk_uebrigesFutter
farm_data$vet_insem_tot <- farm_data$sk_Tierarzt + farm_data$sk_TiereVersch
length(unique(farm_data$lD))

# iii) Subsetting data
# Remove farms with other animals than cattle
# Because costs are not allocated to specific animals
cattle_data <- farm_data[which(farm_data$GVE_Pferde == 0),]
cattle_data <- cattle_data[which(cattle_data$GVE_Schafe == 0),]
cattle_data <- cattle_data[which(cattle_data$GVE_Ziegen == 0),]
cattle_data <- cattle_data[which(cattle_data$GVE_UebrigeRaufuTiere == 0),]
cattle_data <- cattle_data[which(cattle_data$GVE_Zuchtschweine == 0),]
cattle_data <- cattle_data[which(cattle_data$GVE_Mastsschweine == 0),]
cattle_data <- cattle_data[which(cattle_data$GVE_Ferkel == 0),]
cattle_data <- cattle_data[which(cattle_data$GVE_Mastgefluegel == 0),]
cattle_data <- cattle_data[which(cattle_data$GVE_Legehennen == 0),]
cattle_data <- cattle_data[which(cattle_data$GVE_UebrigeTiere == 0),]
length(unique(cattle_data$lD))

# Remove farms with suckler cow husbandry
# Because these farms are likely to have shifted from milk to meat production
cattle_data_sub <- cattle_data[which(cattle_data$GVE_Mutterkuehe == 0),] 
cattle_data_sub <- cattle_data_sub[which(cattle_data_sub$GVE_MuKuhKalb_1Jminus == 0),]
length(unique(cattle_data_sub$lD))

# Remove farms with fattening
dairy_farms <- cattle_data_sub[which(cattle_data_sub$GVE_Mastvieh_gross==0),]
dairy_farms <- dairy_farms[which(dairy_farms$Stk_Mastvieh_gross_4Mplus==0),]
dairy_farms <- dairy_farms[which(dairy_farms$Stk_Mastkaelber==0),]
length(unique(dairy_farms$lD))

# Subsetting for economic variables
# Farmers need to generate some revenues with milk production
dairy_farms_com <- dairy_farms[which(dairy_farms$rohMilch > 0),]
length(unique(dairy_farms_com$lD))

# ----------------------------------------------------------------------------------------------------------------------------------
# Subsetting the sample
# ----------------------------------------------------------------------------------------------------------------------------------

# Remove farms with less than two records (for main results)
records_per_farm <- as.data.frame(matrix(NA, nrow=length(unique(dairy_farms_com$lD)), ncol=2))
records_per_farm[,1] <- unique(dairy_farms_com$lD)

for(i in 1:nrow(records_per_farm )){
  records_per_farm[i,2] <- nrow(dairy_farms_com[which(dairy_farms_com$lD == records_per_farm[i,1]),])
}

# At least two records (more subsets are below)
good_panel_ID <- records_per_farm[records_per_farm$V2 != 1,]
nrow(good_panel_ID)

dairy_farms_panel_final <- dairy_farms_com[which(dairy_farms_com$lD %in% good_panel_ID[,1]),]

# iv) Subset of relevant variables and aggregation of costs
dairy_farms_panel_final <- dairy_farms_panel_final[,c("lD","Gemeinde", "Jahr","Region","LN","landGruenland_tot","kgMilch_jeKuh","Stk_Milchkuehe","rohMilch","sk_KraftfuRind","sk_uebrigesFutter","sk_Tierarzt","sk_TiereVersch", "sk_Futter_tot", "vet_insem_tot","roh_tot")]
colnames(dairy_farms_panel_final)[1] <- "farm"
colnames(dairy_farms_panel_final)[3] <- "year"
length(unique(dairy_farms_panel_final$farm))

dairy_farms_panel_final$rev_share <- dairy_farms_panel_final$rohMilch / dairy_farms_panel_final$roh_tot

rm(cattle_data, cattle_data_sub, dairy_farms, dairy_farms_com, records_per_farm, good_panel_ID)

'Change this for feed or vet expenses, respectively'
dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$sk_Futter_tot >0),]

# -------
# Subset for each region
# -------

# Threshold for minimum number of years
minimum_years <- 4

# complete panel

length_panel_farm <- as.data.frame(matrix(NA, nrow=length(unique(dairy_farms_panel_final$farm)), ncol=2))
length_panel_farm[,1] <- unique(dairy_farms_panel_final$farm)

for(i in 1:nrow(length_panel_farm)){
  temp1 <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm == length_panel_farm[i,1]),]
  length_panel_farm[i,2] <- nrow(temp1)
  rm(temp1)
}

summary(length_panel_farm[,2])
hist(length_panel_farm[,2], main="sample: obs per farm", xlab="observations per farm", ylab="number of farms")
good_farms_panel <- length_panel_farm[which(length_panel_farm[,2] >= minimum_years ),1]

# Subset for milk production (above 50 quantile of sub-sample)
# Calculate the average revenue per farm

milkrev_share <- as.data.frame(matrix(NA, nrow=length(good_farms_panel), ncol=2))
milkrev_share[,1] <- good_farms_panel

for (i in 1:nrow(milkrev_share)){
  temp1 <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm == milkrev_share[i,1]),]
  milkrev_share[i,2] <- mean(temp1$rev_share, na.rm=T)
  rm(temp1)
}

rm(good_farms_panel)
good_farms_panel <- milkrev_share[which(milkrev_share[,2] >= quantile(milkrev_share[,2], 0.5, type=4)),1]


# i) Plain region
dairy_farms_panel_final_plain <- dairy_farms_panel_final[which(dairy_farms_panel_final$Region==1),]
number_farms_plain <- length(unique(dairy_farms_panel_final_plain$farm))

# length ts per farm
length_panel_farm_plain <- as.data.frame(matrix(NA, nrow=number_farms_plain, ncol=2))
length_panel_farm_plain[,1] <- unique(dairy_farms_panel_final_plain$farm)

for(i in 1:number_farms_plain){
  temp1 <- dairy_farms_panel_final_plain[which(dairy_farms_panel_final_plain$farm == length_panel_farm_plain[i,1]),]
  length_panel_farm_plain[i,2] <- nrow(temp1)
  rm(temp1)
}

summary(length_panel_farm_plain[,2])
hist(length_panel_farm_plain[,2], main="plain region: obs per farm", xlab="obs per farm")

good_farms_panel_plain <- length_panel_farm_plain[which(length_panel_farm_plain[,2] >= minimum_years),1]

# Subset for milk production (above 50 quantile of sub-sample)
# Calculate the average milk revenue per farm

milkrev_share <- as.data.frame(matrix(NA, nrow=length(good_farms_panel_plain), ncol=2))
milkrev_share[,1] <- good_farms_panel_plain

for (i in 1:nrow(milkrev_share)){
  temp1 <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm == milkrev_share[i,1]),]
  milkrev_share[i,2] <- mean(temp1$rev_share, na.rm=T) 
  rm(temp1)
}

rm(good_farms_panel_plain)
good_farms_panel_plain <- milkrev_share[which(milkrev_share[,2] >= quantile(milkrev_share[,2], 0.5, type=4)),1]


# ii) hill region
dairy_farms_panel_final_hill <- dairy_farms_panel_final[which(dairy_farms_panel_final$Region==2),]
number_farms_hill <- length(unique(dairy_farms_panel_final_hill$farm))

# length ts per farm
length_panel_farm_hill <- as.data.frame(matrix(NA, nrow=number_farms_hill, ncol=2))
length_panel_farm_hill[,1] <- unique(dairy_farms_panel_final_hill$farm)

for(i in 1:number_farms_hill){
  temp1 <- dairy_farms_panel_final_hill[which(dairy_farms_panel_final_hill$farm == length_panel_farm_hill[i,1]),]
  length_panel_farm_hill[i,2] <- nrow(temp1)
  rm(temp1)
}

summary(length_panel_farm_hill[,2])
hist(length_panel_farm_hill[,2], main="hill region: obs per farm", xlab="obs per farm")
good_farms_panel_hill <- length_panel_farm_hill[which(length_panel_farm_hill[,2] >= minimum_years),1] 

# Subset for milk production (above 50 quantile of sub-sample)
# Calculate the average revenue per farm

milkrev_share <- as.data.frame(matrix(NA, nrow=length(good_farms_panel_hill), ncol=2))
milkrev_share[,1] <- good_farms_panel_hill

for (i in 1:nrow(milkrev_share)){
  temp1 <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm == milkrev_share[i,1]),]
  milkrev_share[i,2] <- mean(temp1$rev_share, na.rm=T) 
  rm(temp1)
}

rm(good_farms_panel_hill)
good_farms_panel_hill <- milkrev_share[which(milkrev_share[,2] >= quantile(milkrev_share[,2], 0.5, type=4)),1]


# iii) mountain region
dairy_farms_panel_final_mountain <- dairy_farms_panel_final[which(dairy_farms_panel_final$Region==3),]
number_farms_mountain <- length(unique(dairy_farms_panel_final_mountain$farm))

# length ts per farm
length_panel_farm_mountain <- as.data.frame(matrix(NA, nrow=number_farms_mountain, ncol=2))
length_panel_farm_mountain[,1] <- unique(dairy_farms_panel_final_mountain$farm)

for(i in 1:number_farms_mountain){
  temp1 <- dairy_farms_panel_final_mountain[which(dairy_farms_panel_final_mountain$farm == length_panel_farm_mountain[i,1]),]
  length_panel_farm_mountain[i,2] <- nrow(temp1)
  rm(temp1)
}

summary(length_panel_farm_mountain[,2])
hist(length_panel_farm_mountain[,2], main="mountain region: obs per farm", xlab="obs per farm")
good_farms_panel_mountain <- length_panel_farm_mountain[which(length_panel_farm_mountain[,2] >= minimum_years),1] 

# Subset for milk production (above 50 quantile of sub-sample)
# Calculate the average revenue per farm

milkrev_share <- as.data.frame(matrix(NA, nrow=length(good_farms_panel_mountain), ncol=2))
milkrev_share[,1] <- good_farms_panel_mountain

for (i in 1:nrow(milkrev_share)){
  temp1 <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm == milkrev_share[i,1]),]
  milkrev_share[i,2] <- mean(temp1$rev_share, na.rm=T) 
  rm(temp1)
}

rm(good_farms_panel_mountain)
good_farms_panel_mountain <- milkrev_share[which(milkrev_share[,2] >= quantile(milkrev_share[,2], 0.5, type=4)),1]


rm(number_farms_hill, number_farms_mountain, number_farms_plain, length_panel_farm, length_panel_farm_hill, length_panel_farm_mountain, length_panel_farm_plain)

# ----------------------------------------------------------------------------------------------------------------------------------
# Nice elevation map of CH
# ----------------------------------------------------------------------------------------------------------------------------------

Switzerland <- getData("GADM", country="Switzerland", level=0)
Switzerland_ch <- spTransform(Switzerland,  CRS("+init=epsg:21781"))
dem_ch <- mask(swiss_elevation, Switzerland_ch)

plot(dem_ch, col = gray.colors(100, start = 1, end = 0,gamma = 1.8), axes=F, box=FALSE, legend=F)

















