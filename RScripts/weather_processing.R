weather_processing <- function(direct = "./tests/data/") {
  library(CASSIA)
  ###
  # Data printed from the CASSIA processing
  ###
  par(mfrow = c(2, 3))
  plot(data_IIASA_smear$Date, data_IIASA_smear$T168, main = "Temperature", xlab = "Dates", ylab = "Degrees C")
  title(sub = paste("Percentage missing", round(100*sum(is.na(data_IIASA_smear$T168))/nrow(data_IIASA_smear), 2), "%"))
  plot(data_IIASA_smear$Date, data_IIASA_smear$VPD, main = "VPD", xlab = "Dates", ylab = "kPa (should be hPa for PlantFATE)") # TODO: units?
  title(sub = paste("Percentage missing", round(100*sum(is.na(data_IIASA_smear$VPD))/nrow(data_IIASA_smear), 2), "%"))
  # TODO: I think that the VPD data processing is going wrong somehow!
  plot(data_IIASA_smear$Date, data_IIASA_smear$Glob_mean, main = "PAR", xlab = "Dates", ylab = "umol m-2 s-1")
  title(sub = paste("Percentage missing", round(100*sum(is.na(data_IIASA_smear$Glob_mean))/nrow(data_IIASA_smear), 2), "%"))
  plot(data_IIASA_smear$Date, data_IIASA_smear$Glob_max, main = "PAR_max", xlab = "Dates", ylab = "umol m-2 s-1") # TODO: units, what should this value be?
  title(sub = paste("Percentage missing", round(100*sum(is.na(data_IIASA_smear$Glob_max))/nrow(data_IIASA_smear), 2), "%"))
  plot(data_IIASA_smear$Date, data_IIASA_smear$wpsoil_B, main = "SWP", xlab = "Dates", ylab = "-kPa (should be MPa for PlantFATE")
  title(sub = paste("Percentage missing", round(100*sum(is.na(data_IIASA_smear$wpsoil_B))/nrow(data_IIASA_smear), 2), "%"))
  plot(data_IIASA_smear$Glob_mean, data_IIASA_smear$Glob_max, main = "Gobal: Mean vs Max", xlab = "Mean", ylab = "Max")

  ###
  # Daily weather
  ###
  PlantFATE_weather <- data_IIASA_smear
  PlantFATE_weather$VPD <- 10*PlantFATE_weather$VPD
  PlantFATE_weather$SWP <- 0.001*data_IIASA_smear$wpsoil_B
  PlantFATE_weather$SWP[is.na(PlantFATE_weather$SWP)] <- mean(PlantFATE_weather$SWP, na.rm = T) # TODO: better SWP average
  PlantFATE_weather$Year <- as.numeric(substring(data_IIASA_smear$Date, 1, 4))
  PlantFATE_weather$Date <- data_IIASA_smear$Date
  PlantFATE_weather$Days <- 1:nrow(PlantFATE_weather)
  PlantFATE_weather_daily <- PlantFATE_weather[,c("Year", "Date", "Days", "T168", "VPD", "Glob_mean", "Glob_max", "SWP")]
  names(PlantFATE_weather_daily)[4:8] <- c("Temp", "VPD", "PAR", "PAR_max", "SWP")
  PlantFATE_weather_daily <- PlantFATE_weather_daily[PlantFATE_weather_daily$Year >= 2000,]
  
  ### PLOT
  par(mfrow = c(2, 3))
  plot(PlantFATE_weather_daily$Date, PlantFATE_weather_daily$Temp, main = "Temperature", xlab = "Dates", ylab = "Degrees C")
  title(sub = paste("Percentage missing", round(100*sum(is.na(PlantFATE_weather_daily$T168))/nrow(PlantFATE_weather_daily), 2), "%"))
  plot(PlantFATE_weather_daily$Date, PlantFATE_weather_daily$VPD, main = "VPD", xlab = "Dates", ylab = "hPa")
  title(sub = paste("Percentage missing", round(100*sum(is.na(PlantFATE_weather_daily$VPD))/nrow(PlantFATE_weather_daily), 2), "%"))
  # TODO: I think that the VPD data processing is going wrong somehow!
  plot(PlantFATE_weather_daily$Date, PlantFATE_weather_daily$PAR, main = "PAR", xlab = "Dates", ylab = "umol m-2 s-1")
  title(sub = paste("Percentage missing", round(100*sum(is.na(PlantFATE_weather_daily$PAR))/nrow(PlantFATE_weather_daily), 2), "%"))
  plot(PlantFATE_weather_daily$Date, PlantFATE_weather_daily$PAR_max, main = "PAR_max", xlab = "Dates", ylab = "umol m-2 s-1")
  title(sub = paste("Percentage missing", round(100*sum(is.na(PlantFATE_weather_daily$PAR_max))/nrow(PlantFATE_weather_daily), 2), "%"))
  plot(PlantFATE_weather_daily$Date, PlantFATE_weather_daily$SWP, main = "SWP", xlab = "Dates", ylab = "-MPa")
  title(sub = paste("Percentage missing", round(100*sum(is.na(PlantFATE_weather_daily$SWP))/nrow(PlantFATE_weather_daily), 2), "%"))
  
  ###
  # Monthly weather
  ###
  
  # Year, Month, Decimal_year, 
  Amazon_data_reference <- read.csv("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  
  PlantFATE_weather_monthly_temp <- aggregate(. ~ substring(Date, 1, 7), PlantFATE_weather_daily, mean, na.rm = T, na.action = NULL)
  names(PlantFATE_weather_monthly_temp)[1] <- c("YM")
  PlantFATE_weather_monthly_temp$Decimal_year <- seq(PlantFATE_weather_monthly_temp$Year[1], 
                                                     PlantFATE_weather_monthly_temp$Year[nrow(PlantFATE_weather_monthly_temp)],
                                                     length.out = nrow(PlantFATE_weather_monthly_temp))
  PlantFATE_weather_monthly_temp$Month <- rep(1:12, length.out = nrow(PlantFATE_weather_monthly_temp))
  PlantFATE_weather_monthly <- PlantFATE_weather_monthly_temp[,c("Year", "Month", "Decimal_year", "Temp", "VPD", "PAR", "PAR_max", "SWP")]
  # TODO: values added temporarily so that the model can be run
  PlantFATE_weather_monthly$VPD[is.na(PlantFATE_weather_monthly$VPD)] <- mean(PlantFATE_weather_monthly$VPD, na.rm = T)
  PlantFATE_weather_monthly$PAR[is.na(PlantFATE_weather_monthly$PAR)] <- mean(PlantFATE_weather_monthly$PAR, na.rm = T)
  
  ### PLOT
  par(mfrow = c(2, 3))
  dates_hyy = as.Date(paste(PlantFATE_weather_monthly$Year, PlantFATE_weather_monthly$Month, sep = "-"), format = "%Y-%M")
  dates_ama = as.Date(paste(Amazon_data_reference$Year, Amazon_data_reference$Month, sep = "-"), format = "%Y-%M")
  plot(dates_hyy, PlantFATE_weather_monthly$Temp, main = "Temperature", xlab = "Dates", ylab = "Degrees C",
       ylim = c(min(PlantFATE_weather_monthly$Temp, na.rm = T), max(Amazon_data_reference$Temp, na.rm = T)))
  points(dates_ama, Amazon_data_reference$Temp, col = "blue")
  title(sub = paste("Percentage missing", round(100*sum(is.na(PlantFATE_weather_monthly$T168))/nrow(PlantFATE_weather_monthly), 2), "%"))
  plot(dates_hyy, PlantFATE_weather_monthly$VPD, main = "VPD", xlab = "Dates", ylab = "hPa", ylim = c(0, max(Amazon_data_reference$VPD)))
  points(dates_ama, Amazon_data_reference$VPD, col = "blue")
  title(sub = paste("Percentage missing", round(100*sum(is.na(PlantFATE_weather_monthly$VPD))/nrow(PlantFATE_weather_monthly), 2), "%"))
  plot(dates_hyy, PlantFATE_weather_monthly$PAR, main = "PAR", xlab = "Dates", ylab = "umol m-2 s-1", ylim = c(0, max(Amazon_data_reference$PAR)))
  points(dates_ama, Amazon_data_reference$PAR, col = "blue")
  title(sub = paste("Percentage missing", round(100*sum(is.na(PlantFATE_weather_monthly$PAR))/nrow(PlantFATE_weather_monthly), 2), "%"))
  plot(dates_hyy, PlantFATE_weather_monthly$PAR_max, main = "PAR_max", xlab = "Dates", ylab = "umol m-2 s-1", ylim = c(0, max(Amazon_data_reference$PAR_max)))
  points(dates_ama, Amazon_data_reference$PAR_max, col = "blue")
  title(sub = paste("Percentage missing", round(100*sum(is.na(PlantFATE_weather_monthly$PAR_max))/nrow(PlantFATE_weather_monthly), 2), "%"))
  plot(dates_hyy, PlantFATE_weather_monthly$SWP, main = "SWP", xlab = "Dates", ylab = "-MPa", ylim = c(0, max(Amazon_data_reference$SWP)))
  points(dates_ama, Amazon_data_reference$SWP, col = "blue")
  title(sub = paste("Percentage missing", round(100*sum(is.na(PlantFATE_weather_monthly$SWP))/nrow(PlantFATE_weather_monthly), 2), "%"))
  
  ###
  # Save the file
  ###
  
  # TODO: Are the names important or the columns?
  # TODO: What exactly should the names be when the file is daily not monthly
  write.csv(PlantFATE_weather_daily, 
            file = paste0(direct, "PlantFATE_weather_Hyytiala_daily.csv"),
            row.names = F)

  PlantFATE_weather_monthly_temp <- Amazon_data_reference
  PlantFATE_weather_monthly_temp$Year <- rep(PlantFATE_weather_monthly$Year, length.out = nrow(PlantFATE_weather_monthly_temp))
  PlantFATE_weather_monthly_temp$Month <- rep(PlantFATE_weather_monthly$Month, length.out = nrow(PlantFATE_weather_monthly_temp))
  PlantFATE_weather_monthly_temp$Decimal_year <- rep(PlantFATE_weather_monthly$Decimal_year, length.out = nrow(PlantFATE_weather_monthly_temp))
  PlantFATE_weather_monthly_temp$Temp <- rep(PlantFATE_weather_monthly$Temp, length.out = nrow(PlantFATE_weather_monthly_temp))
  PlantFATE_weather_monthly_temp$VPD <- rep(PlantFATE_weather_monthly$VPD, length.out = nrow(PlantFATE_weather_monthly_temp))
  PlantFATE_weather_monthly_temp$PAR <- rep(PlantFATE_weather_monthly$PAR, length.out = nrow(PlantFATE_weather_monthly_temp))
  PlantFATE_weather_monthly_temp$PAR_max <- rep(PlantFATE_weather_monthly$PAR_max, length.out = nrow(PlantFATE_weather_monthly_temp))
  # TODO: found the problem! It doesn't like PAR_max
  
  write.csv(PlantFATE_weather_monthly, 
            file = paste0(direct, "PlantFATE_weather_Hyytiala.csv"),
            row.names = F)
  
}
