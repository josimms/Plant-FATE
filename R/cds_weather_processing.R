# READING NC FILE

###
# Function
###

reading_nc <- function() {
  # Path
  path_nc <- "./inst/python/"
  path_test <- "./tests/data/"
  
  # Variables
  variables <- c('t2m', # 2m_temperature 
                 'lai_hv', # leaf_area_index_high_vegetation
                 'ssrd', # surface_solar_radiation_downwards
                 'tvh', # type_of_high_vegetation
                 'swvl1', # volumetric_soil_water_layer_1
                 'swvl2', # volumetric_soil_water_layer_2
                 'r') # relative humidity
  
  # Reading nc files
  for (variable in variables) {
    # LIST FILES
    nc_files <- list.files(path = path_nc,
                           pattern = "download")
    
    # IMPORT THE FILE
    nc_temp <- list()
    dates_temp <- list()
    for (i in 1:length(nc_files)) {
      temp <- ncdf4::nc_open(paste0(path_nc, nc_files)[i])
      
      lon <- ncdf4::ncvar_get(temp, "longitude")
      lon_index <- which.min(abs(lon-17.483333)) # TODO: replace the coordinate
      lat <- ncdf4::ncvar_get(temp, "latitude", verbose = F)
      lat_index <- which.min(abs(lat-60.083333)) # TODO: replace the coordinate
      
      dates_temp[[i]] <- ncdf4::ncvar_get(temp, temp$dim$time)
      
      nc_temp[[i]] <- ncdf4::ncvar_get(temp, 
                                       varid = variable, 
                                       start = c(lon_index, lat_index, 1),
                                       count = c(1, 1, -1))
    }
    nc_variable <- unlist(nc_temp)
    dates <- unlist(dates_temp)
    
    
    # VARIABLES FORMATTED
    # TODO: units
    if (variable == "t2m") {
      tempature = nc_variable - 273.15 # K to 'C
    } else if (variable == "lai_hv") {
      leaf_area = nc_variable
    } else if (variable == "ssrd") {
      PPDF = bigleaf::Rg.to.PPFD(nc_variable) # J m**-2 s-1 = W m**-2 s-1 to umol m-2 s-1
    } else if (variable == "tvh") {
      vegetation = nc_variable
    } else if (variable == "swvl1") {
      soil_water1 = nc_variable
    } else if (variable == "swvl2") {
      soil_water2 = nc_variable
    } else if (variable == "r") {
      rh = nc_variable
    }
    print(paste(variable, "done! :D"))
  }
  
  # MAKING DATAFRAME
  dataset_cds_raw <- data.frame(tempature, leaf_area, PPDF, vegetation, soil_water1, soil_water2, rh)
  names(dataset_cds_raw) <- c(variables)
  dataset_cds_raw$date <- as.POSIXct(dates*3600, origin = "1900-01-01 00:00:00.0", tz = "GMT")
  dataset_cds_raw$vpd <- 10*bigleaf::rH.to.VPD(dataset_cds_raw$r, dataset_cds_raw$t2m) # TODO: check that rh is right!
  
  # MONTH
  dataset_cds_raw$YM <- substring(dataset_cds_raw$date, 1, 7)
  monthy_dataset <- aggregate(. ~ YM, data = dataset_cds_raw, mean)
  monthy_dataset$ssrd_max <- aggregate(. ~ YM, data = dataset_cds_raw[,c("ssrd", "YM")], max)$ssrd
  save(monthy_dataset, file = paste0(path_test, "montly_dataset.csv"))
  
  # DAY
  dataset_cds_raw$YMD <- substring(dataset_cds_raw$date, 1, 10)
  daily_dataset <- aggregate(. ~ YMD, data = dataset_cds_raw[,-c(8)], mean)
  daily_dataset$ssrd_max <- aggregate(. ~ YMD, data = dataset_cds_raw[,c("ssrd", "YMD")], max)$ssrd
  save(daily_dataset, file = paste0(path_test, "daily_dataset.csv"))
  
  ###
  # PLOT
  ###
  
  # MONTHLY
  par(mfrow = c(2, 3))
  plot(monthy_dataset$date, monthy_dataset$t2m, main = "Temperature", xlab = "Dates", ylab = "Degrees C")
  title(sub = paste("Percentage missing", round(100*sum(is.na(monthy_dataset$t2m))/nrow(monthy_dataset), 2), "%"))
  plot(monthy_dataset$date, monthy_dataset$vpd, main = "VPD", xlab = "Dates", ylab = "hPa")
  title(sub = paste("Percentage missing", round(100*sum(is.na(monthy_dataset$vpd))/nrow(monthy_dataset), 2), "%"))
  plot(monthy_dataset$date, monthy_dataset$ssrd, main = "PAR", xlab = "Dates", ylab = "umol m-2 s-1") # TODO: units!
  title(sub = paste("Percentage missing", round(100*sum(is.na(monthy_dataset$ssrd))/nrow(monthy_dataset), 2), "%"))
  plot(monthy_dataset$date, monthy_dataset$ssrd_max, main = "PAR_max", xlab = "Dates", ylab = "umol m-2 s-1") # TODO: units!
  title(sub = paste("Percentage missing", round(100*sum(is.na(monthy_dataset$ssrd_max))/nrow(monthy_dataset), 2), "%"))
  plot(monthy_dataset$date, monthy_dataset$swvl2, main = "SWP", xlab = "Dates", ylab = "-kPa (should be MPa for PlantFATE") # TODO: units!
  title(sub = paste("Percentage missing", round(100*sum(is.na(monthy_dataset$swvl2))/nrow(monthy_dataset), 2), "%"))
  plot(monthy_dataset$ssrd, monthy_dataset$ssrd_max, main = "Gobal: Mean vs Max", xlab = "Mean", ylab = "Max")
  abline(lm(ssrd_max ~ ssrd, data = monthy_dataset, na.action = na.omit ), col = "red")
  title(sub = paste("Gradient", round(coefficients(lm(ssrd_max ~ ssrd, data = monthy_dataset, na.action = na.omit ))[2], 2)))
  
  # DAILY
  par(mfrow = c(2, 3))
  plot(daily_dataset$date, daily_dataset$t2m, main = "Temperature", xlab = "Dates", ylab = "Degrees C")
  title(sub = paste("Percentage missing", round(100*sum(is.na(daily_dataset$t2m))/nrow(daily_dataset), 2), "%"))
  plot(daily_dataset$date, daily_dataset$vpd, main = "VPD", xlab = "Dates", ylab = "hPa")
  title(sub = paste("Percentage missing", round(100*sum(is.na(daily_dataset$vpd))/nrow(daily_dataset), 2), "%"))
  plot(daily_dataset$date, daily_dataset$ssrd, main = "PAR", xlab = "Dates", ylab = "umol m-2 s-1") # TODO: units!
  title(sub = paste("Percentage missing", round(100*sum(is.na(daily_dataset$ssrd))/nrow(daily_dataset), 2), "%"))
  plot(daily_dataset$date, daily_dataset$ssrd_max, main = "PAR_max", xlab = "Dates", ylab = "umol m-2 s-1") # TODO: units!
  title(sub = paste("Percentage missing", round(100*sum(is.na(daily_dataset$ssrd_max))/nrow(daily_dataset), 2), "%"))
  plot(daily_dataset$date, daily_dataset$swvl2, main = "SWP", xlab = "Dates", ylab = "-kPa (should be MPa for PlantFATE") # TODO: units!
  title(sub = paste("Percentage missing", round(100*sum(is.na(daily_dataset$swvl2))/nrow(daily_dataset), 2), "%"))
  plot(daily_dataset$ssrd, daily_dataset$ssrd_max, main = "Gobal: Mean vs Max", xlab = "Mean", ylab = "Max")
  abline(lm(ssrd_max ~ ssrd, data = daily_dataset, na.action = na.omit ), col = "red")
  title(sub = paste("Gradient", round(coefficients(lm(ssrd_max ~ ssrd, data = daily_dataset, na.action = na.omit ))[2], 2)))
  
  # AGAINST HYYTIALA DATA

  # TODO: check against hyytiälä data
  
  ###
  # DONE AND RETURN
  ###
  
  beepr::beep(3)
  return(dataset_cmip)
}

