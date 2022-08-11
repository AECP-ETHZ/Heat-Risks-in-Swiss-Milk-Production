# ==================================================================================================================================
# ==================================================================================================================================
#
# Modular code for the publication: Heat Risks in Swiss Milk production
#
# Citation: Bucheli, J., Uldry, M. and Finger, R. 2022. Heat Risks in Swiss Milk production. Journal of the Agricultural and Applied
#           Economics Association.
#
# Minimum detectable slopes
#
# ==================================================================================================================================
# ================================================================================================================================== 

###########################
# revenue all
###########################

sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[1]] & dairy_farms_panel_final$rohMilch > 0),]
sub_farm_THI_hourly <- farm_THI_hourly[which(farm_THI_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]

temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)

temp_new_ts_THI_ls <- as.data.frame(lspline(sub_farm_THI_hourly$THI,knots= c(list_final_knot_combination_rev[[1]])))
sub_farm_THI_hourly_extended <- bind_cols(sub_farm_THI_hourly, temp_new_ts_THI_ls)
colnames(sub_farm_THI_hourly_extended)[1] <- "farm"
colnames(sub_farm_THI_hourly_extended)[8] <- "s_THI_1"
colnames(sub_farm_THI_hourly_extended)[9] <- "s_THI_2"
colnames(sub_farm_THI_hourly_extended)[10] <- "s_THI_3"
colnames(sub_farm_THI_hourly_extended)[11] <- "s_THI_4"

# Aggregate to yearly values
temp_THI<-sub_farm_THI_hourly_extended %>%
  group_by(farm,year)%>%
  summarise_at(vars(s_THI_1,s_THI_2,s_THI_3,s_THI_4),sum, na.rm=T)

sub_farm_panel_THI_yearly <- join(sub_dairy_farms_panel_final, temp_THI, by=c("farm", "year"))
sub_farm_panel_THI_PP_yearly <- join(sub_farm_panel_THI_yearly, temp_CP, by=c("farm", "year")) 
sub_farm_panel_THI_PP_yearly$pp2 <- (sub_farm_panel_THI_PP_yearly$precip)^2
rm(temp_THI,sub_farm_panel_THI_yearly)

temp1 <- lm(log(rohMilch) ~ s_THI_1 + s_THI_2+ s_THI_3 +s_THI_4 + precip +pp2 + as.factor(year) + as.factor(farm)-1 , data =sub_farm_panel_THI_PP_yearly)

'[Zahl ändern]' # usdm package
((1.96 + 0.84) * sd(temp1$residuals)) / (sd(sub_farm_panel_THI_PP_yearly$s_THI_4) * sqrt(nrow(sub_farm_panel_THI_PP_yearly) * vif(sub_farm_panel_THI_PP_yearly[,16:21])[4,2]))

###########################
# revenue plain
###########################

sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[2]] & dairy_farms_panel_final$rohMilch > 0),]
sub_farm_THI_hourly <- farm_THI_hourly[which(farm_THI_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]

temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)

temp_new_ts_THI_ls <- as.data.frame(lspline(sub_farm_THI_hourly$THI,knots= c(list_final_knot_combination_rev[[2]])))
sub_farm_THI_hourly_extended <- bind_cols(sub_farm_THI_hourly, temp_new_ts_THI_ls)
colnames(sub_farm_THI_hourly_extended)[1] <- "farm"
colnames(sub_farm_THI_hourly_extended)[8] <- "s_THI_1"
colnames(sub_farm_THI_hourly_extended)[9] <- "s_THI_2"
colnames(sub_farm_THI_hourly_extended)[10] <- "s_THI_3"
colnames(sub_farm_THI_hourly_extended)[11] <- "s_THI_4"

# Aggregate to yearly values
temp_THI<-sub_farm_THI_hourly_extended %>%
  group_by(farm,year)%>%
  summarise_at(vars(s_THI_1,s_THI_2,s_THI_3,s_THI_4),sum, na.rm=T)

sub_farm_panel_THI_yearly <- join(sub_dairy_farms_panel_final, temp_THI, by=c("farm", "year"))
sub_farm_panel_THI_PP_yearly <- join(sub_farm_panel_THI_yearly, temp_CP, by=c("farm", "year")) 
sub_farm_panel_THI_PP_yearly$pp2 <- (sub_farm_panel_THI_PP_yearly$precip)^2
rm(temp_THI,sub_farm_panel_THI_yearly)

temp1 <- lm(log(rohMilch) ~ s_THI_1 + s_THI_2+ s_THI_3 +s_THI_4 + precip +pp2 + as.factor(year) + as.factor(farm)-1 , data =sub_farm_panel_THI_PP_yearly)

'[Zahl ändern]' # usdm package
((1.96 + 0.84) * sd(temp1$residuals)) / (sd(sub_farm_panel_THI_PP_yearly$s_THI_4) * sqrt(nrow(sub_farm_panel_THI_PP_yearly) * vif(sub_farm_panel_THI_PP_yearly[,16:21])[4,2]))

###########################
# revenue hill
###########################

sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[3]] & dairy_farms_panel_final$rohMilch > 0),]
sub_farm_THI_hourly <- farm_THI_hourly[which(farm_THI_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]

temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)

temp_new_ts_THI_ls <- as.data.frame(lspline(sub_farm_THI_hourly$THI,knots= c(list_final_knot_combination_rev[[3]])))
sub_farm_THI_hourly_extended <- bind_cols(sub_farm_THI_hourly, temp_new_ts_THI_ls)
colnames(sub_farm_THI_hourly_extended)[1] <- "farm"
colnames(sub_farm_THI_hourly_extended)[8] <- "s_THI_1"
colnames(sub_farm_THI_hourly_extended)[9] <- "s_THI_2"
colnames(sub_farm_THI_hourly_extended)[10] <- "s_THI_3"

# Aggregate to yearly values
temp_THI<-sub_farm_THI_hourly_extended %>%
  group_by(farm,year)%>%
  summarise_at(vars(s_THI_1,s_THI_2,s_THI_3),sum, na.rm=T)

sub_farm_panel_THI_yearly <- join(sub_dairy_farms_panel_final, temp_THI, by=c("farm", "year"))
sub_farm_panel_THI_PP_yearly <- join(sub_farm_panel_THI_yearly, temp_CP, by=c("farm", "year")) 
sub_farm_panel_THI_PP_yearly$pp2 <- (sub_farm_panel_THI_PP_yearly$precip)^2
rm(temp_THI,sub_farm_panel_THI_yearly)

temp1 <- lm(log(rohMilch) ~ s_THI_1 + s_THI_2+ s_THI_3 + precip +pp2 + as.factor(year) + as.factor(farm)-1 , data =sub_farm_panel_THI_PP_yearly)

'[Zahl ändern]' # usdm package
((1.96 + 0.84) * sd(temp1$residuals)) / (sd(sub_farm_panel_THI_PP_yearly$s_THI_3) * sqrt(nrow(sub_farm_panel_THI_PP_yearly) * vif(sub_farm_panel_THI_PP_yearly[,16:20])[3,2]))

###########################
# revenue mountain
###########################

sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[4]] & dairy_farms_panel_final$rohMilch > 0),]
sub_farm_THI_hourly <- farm_THI_hourly[which(farm_THI_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]

temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)

temp_new_ts_THI_ls <- as.data.frame(lspline(sub_farm_THI_hourly$THI,knots= c(list_final_knot_combination_rev[[4]])))
sub_farm_THI_hourly_extended <- bind_cols(sub_farm_THI_hourly, temp_new_ts_THI_ls)
colnames(sub_farm_THI_hourly_extended)[1] <- "farm"
colnames(sub_farm_THI_hourly_extended)[8] <- "s_THI_1"
colnames(sub_farm_THI_hourly_extended)[9] <- "s_THI_2"
colnames(sub_farm_THI_hourly_extended)[10] <- "s_THI_3"
colnames(sub_farm_THI_hourly_extended)[11] <- "s_THI_4"

# Aggregate to yearly values
temp_THI<-sub_farm_THI_hourly_extended %>%
  group_by(farm,year)%>%
  summarise_at(vars(s_THI_1,s_THI_2,s_THI_3,s_THI_4),sum, na.rm=T)

sub_farm_panel_THI_yearly <- join(sub_dairy_farms_panel_final, temp_THI, by=c("farm", "year"))
sub_farm_panel_THI_PP_yearly <- join(sub_farm_panel_THI_yearly, temp_CP, by=c("farm", "year")) 
sub_farm_panel_THI_PP_yearly$pp2 <- (sub_farm_panel_THI_PP_yearly$precip)^2
rm(temp_THI,sub_farm_panel_THI_yearly)

temp1 <- lm(log(rohMilch) ~ s_THI_1 + s_THI_2+ s_THI_3 + s_THI_4 + precip +pp2 + as.factor(year) + as.factor(farm)-1 , data =sub_farm_panel_THI_PP_yearly)

'[Zahl ändern]' # usdm package
((1.96 + 0.84) * sd(temp1$residuals)) / (sd(sub_farm_panel_THI_PP_yearly$s_THI_4) * sqrt(nrow(sub_farm_panel_THI_PP_yearly) * vif(sub_farm_panel_THI_PP_yearly[,16:21])[4,2]))

###########################
# vet all
###########################

sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[1]] & dairy_farms_panel_final$sk_Tierarzt > 0),]
sub_farm_THI_hourly <- farm_THI_hourly[which(farm_THI_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]

temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)

temp_new_ts_THI_ls <- as.data.frame(lspline(sub_farm_THI_hourly$THI,knots= c(list_final_knot_combination_vet[[1]])))
sub_farm_THI_hourly_extended <- bind_cols(sub_farm_THI_hourly, temp_new_ts_THI_ls)
colnames(sub_farm_THI_hourly_extended)[1] <- "farm"
colnames(sub_farm_THI_hourly_extended)[8] <- "s_THI_1"
colnames(sub_farm_THI_hourly_extended)[9] <- "s_THI_2"
colnames(sub_farm_THI_hourly_extended)[10] <- "s_THI_3"
colnames(sub_farm_THI_hourly_extended)[11] <- "s_THI_4"

# Aggregate to yearly values
temp_THI<-sub_farm_THI_hourly_extended %>%
  group_by(farm,year)%>%
  summarise_at(vars(s_THI_1,s_THI_2,s_THI_3,s_THI_4),sum, na.rm=T)

sub_farm_panel_THI_yearly <- join(sub_dairy_farms_panel_final, temp_THI, by=c("farm", "year"))
sub_farm_panel_THI_PP_yearly <- join(sub_farm_panel_THI_yearly, temp_CP, by=c("farm", "year")) 
sub_farm_panel_THI_PP_yearly$pp2 <- (sub_farm_panel_THI_PP_yearly$precip)^2
rm(temp_THI,sub_farm_panel_THI_yearly)

temp1 <- lm(log(sk_Tierarzt) ~ s_THI_1 + s_THI_2+ s_THI_3 + s_THI_4 + precip +pp2 + as.factor(year) + as.factor(farm)-1 , data =sub_farm_panel_THI_PP_yearly)

'[Zahl ändern]' # usdm package
((1.96 + 0.84) * sd(temp1$residuals)) / (sd(sub_farm_panel_THI_PP_yearly$s_THI_4) * sqrt(nrow(sub_farm_panel_THI_PP_yearly) * vif(sub_farm_panel_THI_PP_yearly[,16:21])[4,2]))

###########################
# vet plain
###########################

sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[2]] & dairy_farms_panel_final$sk_Tierarzt > 0),]
sub_farm_THI_hourly <- farm_THI_hourly[which(farm_THI_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]

temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)

temp_new_ts_THI_ls <- as.data.frame(lspline(sub_farm_THI_hourly$THI,knots= c(list_final_knot_combination_vet[[2]])))
sub_farm_THI_hourly_extended <- bind_cols(sub_farm_THI_hourly, temp_new_ts_THI_ls)
colnames(sub_farm_THI_hourly_extended)[1] <- "farm"
colnames(sub_farm_THI_hourly_extended)[8] <- "s_THI_1"
colnames(sub_farm_THI_hourly_extended)[9] <- "s_THI_2"


# Aggregate to yearly values
temp_THI<-sub_farm_THI_hourly_extended %>%
  group_by(farm,year)%>%
  summarise_at(vars(s_THI_1,s_THI_2),sum, na.rm=T)

sub_farm_panel_THI_yearly <- join(sub_dairy_farms_panel_final, temp_THI, by=c("farm", "year"))
sub_farm_panel_THI_PP_yearly <- join(sub_farm_panel_THI_yearly, temp_CP, by=c("farm", "year")) 
sub_farm_panel_THI_PP_yearly$pp2 <- (sub_farm_panel_THI_PP_yearly$precip)^2
rm(temp_THI,sub_farm_panel_THI_yearly)

temp1 <- lm(log(sk_Tierarzt) ~ s_THI_1 + s_THI_2 + precip +pp2 + as.factor(year) + as.factor(farm)-1 , data =sub_farm_panel_THI_PP_yearly)

'[Zahl ändern]' # usdm package
((1.96 + 0.84) * sd(temp1$residuals)) / (sd(sub_farm_panel_THI_PP_yearly$s_THI_2) * sqrt(nrow(sub_farm_panel_THI_PP_yearly) * vif(sub_farm_panel_THI_PP_yearly[,16:19])[2,2]))

###########################
# vet hill
###########################

sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[3]] & dairy_farms_panel_final$sk_Tierarzt > 0),]
sub_farm_THI_hourly <- farm_THI_hourly[which(farm_THI_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]

temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)

temp_new_ts_THI_ls <- as.data.frame(lspline(sub_farm_THI_hourly$THI,knots= c(list_final_knot_combination_vet[[3]])))
sub_farm_THI_hourly_extended <- bind_cols(sub_farm_THI_hourly, temp_new_ts_THI_ls)
colnames(sub_farm_THI_hourly_extended)[1] <- "farm"
colnames(sub_farm_THI_hourly_extended)[8] <- "s_THI_1"
colnames(sub_farm_THI_hourly_extended)[9] <- "s_THI_2"
colnames(sub_farm_THI_hourly_extended)[10] <- "s_THI_3"
colnames(sub_farm_THI_hourly_extended)[11] <- "s_THI_4"


# Aggregate to yearly values
temp_THI<-sub_farm_THI_hourly_extended %>%
  group_by(farm,year)%>%
  summarise_at(vars(s_THI_1,s_THI_2,s_THI_3,s_THI_4),sum, na.rm=T)

sub_farm_panel_THI_yearly <- join(sub_dairy_farms_panel_final, temp_THI, by=c("farm", "year"))
sub_farm_panel_THI_PP_yearly <- join(sub_farm_panel_THI_yearly, temp_CP, by=c("farm", "year")) 
sub_farm_panel_THI_PP_yearly$pp2 <- (sub_farm_panel_THI_PP_yearly$precip)^2
rm(temp_THI,sub_farm_panel_THI_yearly)

temp1 <- lm(log(sk_Tierarzt) ~ s_THI_1 + s_THI_2 + s_THI_3+ s_THI_4 + precip +pp2 + as.factor(year) + as.factor(farm)-1 , data =sub_farm_panel_THI_PP_yearly)

'[Zahl ändern]' # usdm package
((1.96 + 0.84) * sd(temp1$residuals)) / (sd(sub_farm_panel_THI_PP_yearly$s_THI_4) * sqrt(nrow(sub_farm_panel_THI_PP_yearly) * vif(sub_farm_panel_THI_PP_yearly[,16:21])[4,2]))

###########################
# vet mountain
###########################

sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[4]] & dairy_farms_panel_final$sk_Tierarzt > 0),]
sub_farm_THI_hourly <- farm_THI_hourly[which(farm_THI_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]

temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)

temp_new_ts_THI_ls <- as.data.frame(lspline(sub_farm_THI_hourly$THI,knots= c(list_final_knot_combination_vet[[4]])))
sub_farm_THI_hourly_extended <- bind_cols(sub_farm_THI_hourly, temp_new_ts_THI_ls)
colnames(sub_farm_THI_hourly_extended)[1] <- "farm"
colnames(sub_farm_THI_hourly_extended)[8] <- "s_THI_1"
colnames(sub_farm_THI_hourly_extended)[9] <- "s_THI_2"
colnames(sub_farm_THI_hourly_extended)[10] <- "s_THI_3"


# Aggregate to yearly values
temp_THI<-sub_farm_THI_hourly_extended %>%
  group_by(farm,year)%>%
  summarise_at(vars(s_THI_1,s_THI_2,s_THI_3),sum, na.rm=T)

sub_farm_panel_THI_yearly <- join(sub_dairy_farms_panel_final, temp_THI, by=c("farm", "year"))
sub_farm_panel_THI_PP_yearly <- join(sub_farm_panel_THI_yearly, temp_CP, by=c("farm", "year")) 
sub_farm_panel_THI_PP_yearly$pp2 <- (sub_farm_panel_THI_PP_yearly$precip)^2
rm(temp_THI,sub_farm_panel_THI_yearly)

temp1 <- lm(log(sk_Tierarzt) ~ s_THI_1 + s_THI_2 + s_THI_3 + precip +pp2 + as.factor(year) + as.factor(farm)-1 , data =sub_farm_panel_THI_PP_yearly)

'[Zahl ändern]' # usdm package
((1.96 + 0.84) * sd(temp1$residuals)) / (sd(sub_farm_panel_THI_PP_yearly$s_THI_3) * sqrt(nrow(sub_farm_panel_THI_PP_yearly) * vif(sub_farm_panel_THI_PP_yearly[,16:20])[3,2]))

###########################
# feed all
###########################

sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[1]] & dairy_farms_panel_final$sk_Futter_tot > 0),]
sub_farm_T_hourly <- farm_T_hourly[which(farm_T_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]

temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)

temp_new_ts_T_ls <- as.data.frame(lspline(sub_farm_T_hourly$Temperature,knots= list_final_knot_combination_feed[[1]]))
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


temp1 <- lm(log(sk_Futter_tot) ~ s_Temperature_1 + s_Temperature_2 + s_Temperature_3 + precip +pp2 + as.factor(year) + as.factor(farm)-1 , data =sub_farm_panel_T_PP_yearly)

'[Zahl ändern]' # usdm package
((1.96 + 0.84) * sd(temp1$residuals)) / (sd(sub_farm_panel_T_PP_yearly$s_Temperature_3) * sqrt(nrow(sub_farm_panel_T_PP_yearly) * vif(sub_farm_panel_T_PP_yearly[,16:20])[3,2]))

###########################
# feed plain
###########################

sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[2]] & dairy_farms_panel_final$sk_Futter_tot > 0),]
sub_farm_T_hourly <- farm_T_hourly[which(farm_T_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]

temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)

temp_Temperature<-sub_farm_T_hourly %>%
  group_by(farm,year)%>%
  summarise_at(vars(Temperature),sum, na.rm=T)

sub_farm_panel_T_yearly <- join(sub_dairy_farms_panel_final, temp_Temperature, by=c("farm", "year"))

sub_farm_panel_T_PP_yearly <- join(sub_farm_panel_T_yearly, temp_CP, by=c("farm", "year")) 
sub_farm_panel_T_PP_yearly$pp2 <- (sub_farm_panel_T_PP_yearly$precip)^2
rm(temp_Temperature,sub_farm_panel_T_yearly)

temp1 <- lm(log(sk_Futter_tot) ~ Temperature + precip +pp2 + as.factor(year) + as.factor(farm)-1 , data =sub_farm_panel_T_PP_yearly)
((1.96 + 0.84) * sd(temp1$residuals)) / (sd(sub_farm_panel_T_PP_yearly$Temperature) * sqrt(nrow(sub_farm_panel_T_PP_yearly) * vif(sub_farm_panel_T_PP_yearly[,16:18])[1,2]))

###########################
# feed hill
###########################

sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[3]] & dairy_farms_panel_final$sk_Futter_tot > 0),]
sub_farm_T_hourly <- farm_T_hourly[which(farm_T_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]

temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)

temp_new_ts_T_ls <- as.data.frame(lspline(sub_farm_T_hourly$Temperature,knots= list_final_knot_combination_feed[[3]]))
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


temp1 <- lm(log(sk_Futter_tot) ~ s_Temperature_1 + s_Temperature_2 + precip +pp2 + as.factor(year) + as.factor(farm)-1 , data =sub_farm_panel_T_PP_yearly)

'[Zahl ändern]' # usdm package
((1.96 + 0.84) * sd(temp1$residuals)) / (sd(sub_farm_panel_T_PP_yearly$s_Temperature_2) * sqrt(nrow(sub_farm_panel_T_PP_yearly) * vif(sub_farm_panel_T_PP_yearly[,16:19])[2,2]))

###########################
# feed mountain
###########################

sub_dairy_farms_panel_final <- dairy_farms_panel_final[which(dairy_farms_panel_final$farm %in%  sub_panels_ID[[4]] & dairy_farms_panel_final$sk_Futter_tot > 0),]
sub_farm_T_hourly <- farm_T_hourly[which(farm_T_hourly$farm %in% unique(sub_dairy_farms_panel_final$farm)),]
sub_farm_precip_daily <-  farm_precip_daily[which(farm_precip_daily$farm %in% unique(sub_dairy_farms_panel_final$farm)),]

temp_CP <- sub_farm_precip_daily %>%
  group_by(farm,year)%>%
  summarise_at(vars(precip),sum, na.rm=T)

temp_new_ts_T_ls <- as.data.frame(lspline(sub_farm_T_hourly$Temperature,knots= list_final_knot_combination_feed[[4]]))
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


temp1 <- lm(log(sk_Futter_tot) ~ s_Temperature_1 + s_Temperature_2+s_Temperature_3+s_Temperature_4 + precip +pp2 + as.factor(year) + as.factor(farm)-1 , data =sub_farm_panel_T_PP_yearly)

'[Zahl ändern]' # usdm package
((1.96 + 0.84) * sd(temp1$residuals)) / (sd(sub_farm_panel_T_PP_yearly$s_Temperature_4) * sqrt(nrow(sub_farm_panel_T_PP_yearly) * vif(sub_farm_panel_T_PP_yearly[,16:19])[4,2]))


