# READING NC FILE

###
# NEW function
###

reading_nc <- function() {
  library(ncdf4)
  library(bigleaf)
  library(data.table)
  
  path_nc <- "/home/josimms/Documents/Austria/Plant-FATE/inst/python"
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
      t2m = nc_data$t2m  - 273.15,
      ssrd = Rg.to.PPFD(nc_data$ssrd),
      swvl1 = nc_data$swvl1,
      swvl2 = nc_data$swvl2,
      date = as.POSIXct(dates*3600, origin = "1900-01-01", tz = "GMT")
    )
    
    dataset_cds_raw[[i]] <- dataset_cds_raw_year
  }
  
  dataset_cds_raw_all <- rbindlist(dataset_cds_raw)
  
  #dataset_cds_raw[, vpd := 10 * rH.to.VPD(r, t2m)]
  dataset_cds_raw_all[, YM := format(date, "%Y-%m")]
  dataset_cds_raw_all[, YMD := format(date, "%Y-%m-%d")]
  
  # Monthly aggregation
  monthy_dataset <- dataset_cds_raw_all[, lapply(.SD, mean), by = YM, .SDcols = -c("YMD", "date")]
  monthy_dataset[, ssrd_max := dataset_cds_raw_all[, .(ssrd_max = max(ssrd)), by = YM]$ssrd_max]
  
  # Daily aggregation
  daily_dataset <- dataset_cds_raw_all[, lapply(.SD, mean), by = YMD, .SDcols = -c("YM", "date")]
  daily_dataset[, ssrd_max := dataset_cds_raw_all[, .(ssrd_max = max(ssrd)), by = YMD]$ssrd_max]
  
  # Save datasets
  fwrite(monthy_dataset, file = file.path(path_test, "montly_dataset.csv"))
  fwrite(daily_dataset, file = file.path(path_test, "daily_dataset.csv"))
  
  # Plotting function (reuse for both monthly and daily)
  plot_data <- function(data, title_prefix, monthly) {
    par(mfrow = c(3, 2))
    if (monthly) {
      data$YM <- paste0(data$YM, "-01")
      data$date <- as.Date(data$YM, format = "%Y-%m-%d")
    } else {
      data$date <- as.Date(data$YMD, format = "%Y-%m-%d")
    }
    
    
    # "vpd",
    plot_vars <- c("t2m", "ssrd", "ssrd_max", "swvl2")
    for (var in plot_vars) {
      plot(data$date, data[[var]], main = paste(title_prefix, var), 
           xlab = "Dates", ylab = switch(var,
                                         t2m = "Degrees C",
                                         ssrd = "umol m-2 s-1",
                                         ssrd_max = "umol m-2 s-1",
                                         swvl2 = "-kPa"))
      title(sub = sprintf("Percentage missing: %.2f%%", 100 * sum(is.na(data[[var]])) / nrow(data)))
    }
    plot(data$ssrd, data$ssrd_max, main = "Global: Mean vs Max", xlab = "Mean", ylab = "Max")
    abline(lm(ssrd_max ~ ssrd, data = data), col = "red")
    title(sub = sprintf("Gradient: %.2f", coef(lm(ssrd_max ~ ssrd, data = data))[2]))
  }
  
  # Plot monthly and daily data
  plot_data(monthy_dataset, "Monthly", monthly = TRUE)
  plot_data(daily_dataset, "Daily", monthly = FALSE)
  
  beepr::beep(3)
  return(dataset_cds_raw)
}
