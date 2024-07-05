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
  plot_vars <- c("Temp", "PAR", "PAR_max", "swvl2", "VPD", "SWP", "co2")
  for (var in plot_vars) {
    plot(data$date, data[[var]], main = paste(title_prefix, var), 
         xlab = "Dates", ylab = switch(var,
                                       Temp = "Degrees C",
                                       PAR = "umol m-2 s-1",
                                       PAR_max = "umol m-2 s-1",
                                       swvl2 = "-kPa",
                                       VPD = "hPa",
                                       SWP = "- Ma",
                                       co2 = "ppm")) # TODO: check units
    title(sub = sprintf("Percentage missing: %.2f%%", 100 * sum(is.na(data[[var]])) / nrow(data)))
  }
  plot(data$PAR, data$PAR_max, main = "Global: Mean vs Max", xlab = "Mean", ylab = "Max")
  abline(lm(PAR_max ~ PAR, data = data), col = "red")
  title(sub = sprintf("Gradient: %.2f", coef(lm(PAR_max ~ PAR, data = data))[2]))
}

###
# NEW function
###

reading_nc <- function() {
  library(ncdf4)
  library(bigleaf)
  library(data.table)
  
  path_nc <- "/home/josimms/Documents/Austria/eras_data"
  path_test <- "/home/josimms/Documents/Austria/Plant-FATE/tests/data"
  
  variables <- c('t2m', 'ssrd', 'swvl1', 'swvl2')
  
  # Pre-allocate lists
  nc_data <- vector("list", length(variables))
  names(nc_data) <- variables
  
  nc_files <- list.files(path = path_nc, pattern = "download", full.names = TRUE)
  
  # Coordinates
  lon_target <- 17.483333
  lat_target <- 60.083333
  
  dataset_cds_raw <- list()
  for (i in 1:length(nc_files)) {
    nc <- nc_open(nc_files[i])
    
    lon <- ncvar_get(nc, "longitude")
    lat <- ncvar_get(nc, "latitude")
    lon_index <- which.min(abs(lon - lon_target))
    lat_index <- which.min(abs(lat - lat_target))
    
    dates <- ncvar_get(nc, nc$dim$time)
    
    for (var in variables) {
      nc_data[[var]] <- c(nc_data[[var]], 
                          ncvar_get(nc, varid = var, 
                                    start = c(lon_index, lat_index, 1),
                                    count = c(1, 1, -1)))
    }
    
    nc_close(nc)
    
    # Create data.table
    dataset_cds_raw_year <- data.table(
      Temp = nc_data$t2m  - 273.15,
      PAR = bigleaf::Rg.to.PPFD(nc_data$ssrd),
      swvl1 = nc_data$swvl1,
      swvl2 = nc_data$swvl2,
      date = as.POSIXct(dates*3600, origin = "1900-01-01", tz = "GMT")
    )
    
    dataset_cds_raw[[i]] <- dataset_cds_raw_year
  }
  
  dataset_cds_raw_all <- rbindlist(dataset_cds_raw)
  
  #dataset_cds_raw[, vpd := 10 * rH.to.VPD(r, t2m)] # hPa
  dataset_cds_raw_all[, YM := format(date, "%Y-%m")]
  dataset_cds_raw_all[, YMD := format(date, "%Y-%m-%d")]
  dataset_cds_raw_all[, Year := as.numeric(format(date, "%Y"))]
  dataset_cds_raw_all[, Month := as.numeric(format(date, "%m"))]
  
  # Monthly aggregation
  monthy_dataset <- dataset_cds_raw_all[, lapply(.SD, mean), by = YM, .SDcols = -c("YMD", "date")]
  monthy_dataset[, PAR_max := dataset_cds_raw_all[, .(PAR_max = max(PAR)), by = YM]$PAR_max]
  
  # Daily aggregation
  daily_dataset <- dataset_cds_raw_all[, lapply(.SD, mean), by = YMD, .SDcols = -c("YM", "date")]
  daily_dataset[, PAR_max := dataset_cds_raw_all[, .(PAR_max = max(PAR)), by = YMD]$PAR_max]
  
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
  
  plantfate_monthy_dataset <- monthy_dataset
  plantfate_monthy_dataset$VPD <- 0.8
  plantfate_monthy_dataset$co2 <- 380 # TODO: if time get the values from Hyytiala like in the SWP
  plantfate_monthy_dataset$SWP <- - 0.001 * rep(soil_water_potential_montly$HYY_META.wpsoil_B, 
                                     length.out = nrow(plantfate_monthy_dataset))
  plantfate_monthy_dataset$Decimal_year <- seq(plantfate_monthy_dataset$Year[1],
                                               plantfate_monthy_dataset$Year[nrow(plantfate_monthy_dataset)],
                                               length.out = nrow(plantfate_monthy_dataset))
  plantfate_monthy_dataset <- merge(plantfate_monthy_dataset, 
                                    gpp[gpp$Year < plantfate_monthy_dataset$Year[nrow(plantfate_monthy_dataset)],c("YM", "GPP")], 
                                    by = "YM", 
                                    all.x = T)
  fwrite(plantfate_monthy_dataset[,c("Year", "Month", "Decimal_year", "Temp", "VPD", "PAR", "PAR_max", "SWP")],
         file = file.path(path_test, "ERAS_Monthly.csv"))
  
  plantfate_daily_dataset <- daily_dataset
  plantfate_daily_dataset$VPD <- 0.8
  plantfate_daily_dataset$co2 <- 380
  plantfate_daily_dataset$SWP <- - 0.001 * rep(soil_water_potential_daily$HYY_META.wpsoil_B, 
                                    length.out = nrow(plantfate_daily_dataset))
  plantfate_daily_dataset$Decimal_year <- seq(plantfate_daily_dataset$Year[1],
                                              plantfate_daily_dataset$Year[nrow(plantfate_daily_dataset)],
                                               length.out = nrow(plantfate_daily_dataset))
  fwrite(plantfate_daily_dataset[,c("Year", "Month", "Decimal_year", "Temp", "VPD", "PAR", "PAR_max", "SWP")],
         file = file.path(path_test, "ERAS_dataset.csv"))
  
  ####
  # Plot monthly and daily data
  ####
  plot_data(plantfate_monthy_dataset, "Monthly", monthly = TRUE)
  plot_data(plantfate_daily_dataset, "Daily", monthly = FALSE)
  
  return("Your code is done - check the generated files")
}
