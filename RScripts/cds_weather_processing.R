# DEW POINT

calculate_VPD <- function(dew_point_temp, air_temp) {
  # Calculate saturation vapor pressure at air temperature (in hPa)
  e_s <- 6.11 * 10^(7.5 * air_temp / (237.3 + air_temp))
  
  # Calculate vapor pressure at dew point temperature (in hPa)
  e_d <- 6.11 * 10^(7.5 * dew_point_temp / (237.3 + dew_point_temp))
  
  # Calculate relative humidity (in percentage)
  RH <- (e_d / e_s)

  # Calculate VPD
  VPD <- 0.1 * bigleaf::rH.to.VPD(RH, air_temp) # Units kPa to hPa
  return(RH)
}

# READING NC FILE

# Plotting function (reuse for both monthly and daily)
plot_data <- function(data, title_prefix, monthly) {
  par(mfrow = c(3, 3))
  if (monthly) {
    data$YM <- paste0(data$YM, "-01")
    data$date <- as.Date(data$YM, format = "%Y-%m-%d")
  } else {
    data$date <- as.Date(data$YMD, format = "%Y-%m-%d")
  }
  
  # "vpd",
  plot_vars <- c("Temp", "PPFD", "PPFD_max", "swvl2", "VPD", "SWP", "co2")
  for (var in plot_vars) {
    plot(data$date, data[[var]], main = paste(title_prefix, var), 
         xlab = "Dates", ylab = switch(var,
                                       Temp = "Degrees C",
                                       PPFD = "umol m-2 s-1",
                                       PPFD_max = "umol m-2 s-1",
                                       swvl2 = "-kPa",
                                       VPD = "hPa",
                                       SWP = "- Ma",
                                       co2 = "ppm")) # TODO: check units
    title(sub = sprintf("Percentage missing: %.2f%%", 100 * sum(is.na(data[[var]])) / nrow(data)))
  }
  plot(data$PPFD, data$PPFD_max, main = "Global: Mean vs Max", xlab = "Mean", ylab = "Max")
  abline(lm(PPFD_max ~ PPFD, data = data), col = "red", lwd = 2)
  title(sub = sprintf("Gradient: %.2f", coef(lm(PPFD_max ~ PPFD, data = data))[2]))
}

###
# NEW function
###

reading_nc <- function() {
  library(ncdf4)
  library(bigleaf)
  library(data.table)
  library(dplyr)
  library(lubridate)
  
  path_nc <- "/home/josimms/Documents/Austria/eras_data"
  path_test <- "/home/josimms/Documents/Austria/Plant-FATE/tests/data"
  
  variables <- c('t2m', 'ssrd', 'swvl1', 'swvl2')
  
  # Pre-allocate lists
  nc_data <- vector("list", length(c(variables, "d2m")))
  names(nc_data) <- c(variables, "d2m")
  
  nc_files <- list.files(path = path_nc, pattern = "download.nc", full.names = TRUE)
  nc_files_td <- list.files(path = path_nc, pattern = "download_td", full.names = TRUE)
  
  # Coordinates
  lon_target <- 24.29477
  lat_target <- 61.84741
  
  dataset_cds_raw <- list()
  for (i in 1:length(nc_files)) {
    nc <- nc_open(nc_files[i])
    nc_td <- nc_open(nc_files_td[i])
    
    lon <- ncvar_get(nc, "longitude")
    lat <- ncvar_get(nc, "latitude")
    lon_index <- which.min(abs(lon - lon_target))
    lat_index <- which.min(abs(lat - lat_target))
    
    dates <- ncvar_get(nc, nc$dim$time)
    
    for (var in variables) {
      nc_data[[var]] <- ncvar_get(nc, varid = var, 
                                    start = c(lon_index, lat_index, 1),
                                    count = c(1, 1, -1))
      print(length(nc_data[[var]]))
      
    }
    nc_data[["d2m"]] <- ncvar_get(nc_td, varid = "d2m", 
                              start = c(lon_index, lat_index, 1),
                              count = c(1, 1, -1))
    print(length(nc_data[["d2m"]]))
    
    nc_close(nc)
    nc_close(nc_td)
    
    # Create data.table
    dataset_cds_raw_year <- data.table(
      Temp = nc_data$t2m  - 273.15, # 'C
      PPFD = bigleaf::Rg.to.PPFD(nc_data$ssrd/(60*60)), # W m-2 to umol m-2 s-1, PPFD (daily 24-hr mean)
      swvl1 = nc_data$swvl1, # m m-2
      swvl2 = nc_data$swvl2, # m m-2
      Temp_Dew = nc_data$d2m  - 273.15, # 'C
      date = as.POSIXct(dates*3600, origin = "1900-01-01", tz = "GMT")
      
      # TODO: there are too many rows!
    )
    dataset_cds_raw[[i]] <- dataset_cds_raw_year
    print(nrow(dataset_cds_raw_year))
  }
  
  dataset_cds_raw_all <- rbindlist(dataset_cds_raw)
  
  dataset_cds_raw_all[, VPD := calculate_VPD(Temp_Dew, Temp)] # hPa
  # dataset_cds_raw_all$VPD[dataset_cds_raw_all$VPD > 3]
  dataset_cds_raw_all[, YM := format(date, "%Y-%m")]
  dataset_cds_raw_all[, YMD := format(date, "%Y-%m-%d")]
  dataset_cds_raw_all[, Year := as.numeric(format(date, "%Y"))]
  dataset_cds_raw_all[, Month := as.numeric(format(date, "%m"))]
  
  # Monthly aggregation
  monthy_dataset <- dataset_cds_raw_all[, lapply(.SD, mean), by = YM, .SDcols = -c("YMD", "date")]
  monthy_dataset[, PPFD_max := dataset_cds_raw_all[, .(PPFD_max = max(PPFD)), by = YM]$PPFD_max]
  
  # Daily aggregation
  daily_dataset <- dataset_cds_raw_all[, lapply(.SD, mean), by = YMD, .SDcols = -c("YM", "date")]
  daily_dataset[, PPFD_max := dataset_cds_raw_all[, .(PPFD_max = max(PPFD)), by = YMD]$PPFD_max]
  
  # Save datasets
  fwrite(monthy_dataset, file = file.path(path_test, "montly_dataset.csv"))
  fwrite(daily_dataset, file = file.path(path_test, "daily_dataset.csv"))
  
  ####
  # Generate the weather file in the right format
  ####
  
  raw.directory = "/home/josimms/Documents/CASSIA_Calibration/Raw_Data/hyytiala_weather/"
  soil_water_potential_list <- list()
  count = 1
  for (var in c("wpsoil_A", "wpsoil_B", "GPP")) {
    soil_water_potential_list[[count]] <- data.table::rbindlist(lapply(paste0(raw.directory, list.files(raw.directory, var)), data.table::fread))
    count = count + 1 
  }
  soil_water_potential <- data.table::rbindlist(soil_water_potential_list, fill = TRUE)
  
  soil_water_potential[, MD := paste(soil_water_potential$Month, 
                                     soil_water_potential$Day, sep = "-")]
  # Gapfil
  soil_water_potential$HYY_META.wpsoil_B[is.na(soil_water_potential$HYY_META.wpsoil_B)] = soil_water_potential$HYY_META.wpsoil_A[is.na(soil_water_potential$HYY_META.wpsoil_B)] - 
    mean(soil_water_potential$HYY_META.wpsoil_A, na.rm = T) + 
    mean(soil_water_potential$HYY_META.wpsoil_B, na.rm = T)
  
  # Monthly aggregation
  soil_water_potential[, YM := paste(soil_water_potential$Year, sprintf("%02d", soil_water_potential$Month), sep = "-")]
  gpp <- soil_water_potential[, lapply(.SD, mean, na.rm = T), by = YM, .SDcols = -c("MD")]
  names(gpp) <- gsub("HYY_EDDY233.", "", names(gpp))
  
  # Monthly aggregation - for only one year!
  soil_water_potential_montly <- soil_water_potential[, lapply(.SD, mean, na.rm = T), by = Month, .SDcols = -c("MD", "YM")]
  
  # Daily aggregation
  soil_water_potential_daily <- soil_water_potential[, lapply(.SD, mean, na.rm = T), by = MD,  .SDcols = -c("YM")]
  
  # Load the data
  Hyytiala <- fread("/home/josimms/Documents/Austria/Plant-FATE/tests/data/daily_dataframe.csv")
  
  # Extract year and month, and calculate the monthly max Glob for each year
  Hyytiala_monthly <- Hyytiala %>%
    group_by(Year, Month) %>%
    summarise(Mean_Temp = mean(T336_mean, na.rm = T),
              Mean_VPD = mean(VPD, na.rm = T),
              Mean_PPFD = mean(PAR_mean, na.rm = T),
              Mean_PPFD_max = mean(PAR_max, na.rm = T)) %>%
    mutate_all(~replace(., is.infinite(.), NA)) %>%
    group_by(Month) %>%
    summarise(Mean_Temp = mean(Mean_Temp, na.rm = TRUE),
              Mean_VPD = mean(Mean_VPD, na.rm = TRUE),
              Mean_PPFD = mean(Mean_PPFD, na.rm = TRUE),
              Mean_PPFD_max = mean(Mean_PPFD_max, na.rm = TRUE))
  
  plantfate_monthy_dataset <- monthy_dataset
  plantfate_monthy_dataset$co2 <- 380 # ppm TODO: if time get the values from Hyytiala like in the SWP
  plantfate_monthy_dataset$SWP <- - 0.001 * rep(soil_water_potential_montly$HYY_META.wpsoil_B,
                                                length.out = nrow(plantfate_monthy_dataset)) # Soil water potential kPa to - MPa
  plantfate_monthy_dataset$Decimal_year <- seq(plantfate_monthy_dataset$Year[1],
                                               plantfate_monthy_dataset$Year[nrow(plantfate_monthy_dataset)],
                                               length.out = nrow(plantfate_monthy_dataset))
  plantfate_monthy_dataset <- merge(plantfate_monthy_dataset, 
                                    gpp[gpp$Year < plantfate_monthy_dataset$Year[nrow(plantfate_monthy_dataset)],c("YM", "GPP")], 
                                    by = "YM", 
                                    all.x = T)
  
  monthly_means <- plantfate_monthy_dataset %>%
    group_by(Month) %>%
    summarise(Mean_Temp = mean(Temp, na.rm = TRUE),
              Mean_VPD = mean(VPD, na.rm = TRUE),
              Mean_PPFD = mean(PPFD, na.rm = TRUE),
              Mean_PPFD_max = mean(PPFD_max, na.rm = TRUE))
  
  cols = c("Mean_Temp", "Mean_VPD", "Mean_PPFD", "Mean_PPFD_max")
  error = Hyytiala_monthly[,cols] - monthly_means[,cols]
  
  ### Bias correct
  plantfate_monthy_dataset$Temp = plantfate_monthy_dataset$Temp + rep(error$Mean_Temp, length.out = nrow(plantfate_monthy_dataset))
  plantfate_monthy_dataset$VPD = plantfate_monthy_dataset$VPD + rep(error$Mean_VPD, length.out = nrow(plantfate_monthy_dataset))
  plantfate_monthy_dataset$PPFD = plantfate_monthy_dataset$PPFD + rep(error$Mean_PPFD, length.out = nrow(plantfate_monthy_dataset))
  # TODO: can you remove -Inf?
  plantfate_monthy_dataset$PPFD_max = plantfate_monthy_dataset$PPFD_max + rep(error$Mean_PPFD_max, length.out = nrow(plantfate_monthy_dataset))
  
  fwrite(plantfate_monthy_dataset[,c("Year", "Month", "Decimal_year", "Temp", "VPD", "PPFD", "PPFD_max", "SWP", "GPP")],
         file = file.path(path_test, "ERAS_Monthly.csv"))
  
  plantfate_daily_dataset <- daily_dataset
  plantfate_daily_dataset$co2 <- 380
  plantfate_daily_dataset$SWP <- - 0.001 * rep(soil_water_potential_daily$HYY_META.wpsoil_B, 
                                               length.out = nrow(plantfate_daily_dataset)) # Soil water potential kPa to - MPa
  plantfate_daily_dataset$Decimal_year <- seq(plantfate_daily_dataset$Year[1],
                                              plantfate_daily_dataset$Year[nrow(plantfate_daily_dataset)],
                                               length.out = nrow(plantfate_daily_dataset))
  
  # TODO: Bias correct!
  plantfate_daily_dataset$Temp = plantfate_daily_dataset$Temp
  plantfate_daily_dataset$VPD = plantfate_daily_dataset$VPD
  plantfate_daily_dataset$PPFD = plantfate_daily_dataset$PPFD
  # TODO: Hyytiala$Glob67_max why is this -Inf?
  plantfate_daily_dataset$PPFD_max = plantfate_daily_dataset$PPFD_max
  
  fwrite(plantfate_daily_dataset[,c("Year", "Month", "Decimal_year", "Temp", "VPD", "PPFD", "PPFD_max", "SWP")],
         file = file.path(path_test, "ERAS_dataset.csv"))

  # TODO: daily
  
  ####
  # Plot monthly and daily data
  ####
  plot_data(plantfate_monthy_dataset, "Monthly", monthly = TRUE)
  plot_data(plantfate_daily_dataset, "Daily", monthly = FALSE)
  
  return("Your code is done - check the generated files")
}

