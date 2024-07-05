sort_CMIP <- function() {
  library(ncdf4)
  library(data.table)
  
  # Constants
  TEMP_DIR <- "~/Documents/Austria/cmip6_data/temp"
  ANSWER_DIR <- "~/Documents/Austria/Plant-FATE/tests/data"
  path_test <- "/home/josimms/Documents/Austria/Plant-FATE/tests/data"
  TARGET_LON <- 17.483333
  TARGET_LAT <- 60.083333
  
  for (ssp_scenario in c(245, 585)) {
    # Process each variable
    parameters <- c("tas", "hurs", "rsds") # CO2 Processed Separately
    results <- list()
    
    for (variable in parameters) {
      spp_files <- list.files(path = TEMP_DIR,
                              pattern = paste(variable, ssp_scenario, sep = ".+"),
                              full.names = TRUE)
      
      spp <- process_files(spp_files, variable, TARGET_LON, TARGET_LAT)
      
      results[[variable]] <- transform_variable(variable, spp)
      message(paste(variable, "processing complete."))
    }
    
    # Compute VPD
    results$VPD <- 10 * bigleaf::rH.to.VPD(results$hurs * 0.01, results$tas) # kPa to hPa
    
    # Create date sequence
    date <- seq(as.Date("2015-01-01"), 
                as.Date("2100-12-30"), 
                length.out = length(results$tas)) # TODO: dates are wrong here, but want graphs
    
    # Get soil potential from historical data
    monthy_dataset <- fread(file.path(path_test, "ERAS_Monthly.csv"))
    daily_dataset <- fread(file.path(path_test, "ERAS_dataset.csv"))
    
    # CO2 importing
    spp_files <- list.files(path = TEMP_DIR,
                            pattern = paste("co2", ssp_scenario, sep = ".+"),
                            full.names = TRUE)
    co2 <- process_files_co2(spp_files, "co2mass")
    
    # Create final dataset
    dataset_cmip <- data.table::data.table(Year = as.numeric(substring(date, 1, 4)),
                                           Month = as.numeric(substring(date, 6, 7)),
                                           Decimal_year = seq(as.numeric(substring(date[1], 1, 4)), 
                                                              as.numeric(substring(date[length(date)], 1, 4)), 
                                                              length.out = length(results$tas)),
                                           Temp = results$tas,
                                           VPD = results$VPD,
                                           PAR = results$rsds,
                                           SWP = - 0.001 * rep(daily_dataset$SWP, length.out = length(results$tas)), # TODO: why are these longer?
                                           date = date,
                                           co2 = rep(daily_to_average(co2), length.out = length(results$tas))) # TODO: why are these longer?
    dataset_cmip[, YMD := format(date, "%Y-%m-%d")]
    dataset_cmip[, YM := format(date, "%Y-%m")]
    # NOTE: Mean and max the same as the values are from a daily source!
    dataset_cmip[, PAR_max := dataset_cmip[, .(PAR_max = 3*max(PAR)), by = YMD]$PAR_max] 
    
    # Monthly
    dataset_cmip_monthly <- dataset_cmip[, lapply(.SD, mean), by = YM, .SDcols = -c("date", "YMD", "PAR_max", "Decimal_year")]
    dataset_cmip_monthly[, PAR_max := dataset_cmip[, .(PAR_max = max(PAR)), by = YM]$PAR_max]
    dataset_cmip_monthly$Decimal_year <- seq(1000, 1050, length.out = nrow(dataset_cmip_monthly))
    
    # Yearly CO2
    dataset_cmip_yearly <- dataset_cmip[, lapply(.SD, mean), by = Year, .SDcols = -c("date", "YMD", "YM", "PAR_max", "Decimal_year")]
    
    # Save results
    fwrite(dataset_cmip[,c("Year", "Month", "Decimal_year", "Temp", "VPD", "PAR", "PAR_max", "SWP")], 
           file = file.path(ANSWER_DIR, paste0("PlantFATE", ssp_scenario, ".csv")))
    fwrite(dataset_cmip_monthly[,c("Year", "Month", "Decimal_year", "Temp", "VPD", "PAR", "PAR_max", "SWP")], 
           file = file.path(ANSWER_DIR, paste0("PlantFATE_monthly", ssp_scenario, ".csv")))
    fwrite(dataset_cmip_yearly[,c("Year", "co2")], 
           file = file.path(ANSWER_DIR, paste0("PlantFATE_yearly", ssp_scenario, ".csv")))
    
    # Plot results
    plot_data(dataset_cmip_monthly, "Monthly", monthly = TRUE)
    plot_data(dataset_cmip, "Daily", monthly = FALSE)
  }
  
  return("Data processed look to files for the datasets")
}

###
# Helper functions
###

process_files <- function(files, variable, target_lon, target_lat) {
  spp_temp <- lapply(files, function(file) {
    temp <- nc_open(file)
    on.exit(nc_close(temp))
    
    lon <- ncvar_get(temp, "lon")
    lat <- ncvar_get(temp, "lat")
    lon_index <- which.min(abs(lon - target_lon))
    lat_index <- which.min(abs(lat - target_lat))
    
    ncvar_get(temp, varid = variable, 
              start = c(lon_index, lat_index, 1), 
              count = c(1, 1, -1))
  })
  unlist(spp_temp)
}

process_files_co2 <- function(files, variable) {
  spp_temp <- lapply(files, function(file) {
    temp <- nc_open(file)
    on.exit(nc_close(temp))
    
    ncvar_get(temp, varid = "co2mass", 
              start = 1, 
              count = c(-1))
  })
  
  out <- 0.0000000000001 * unlist(spp_temp)
  return(out)
} # TODO: although these are now the right scale, make sure that there is a logic here!

transform_variable <- function(variable, spp) {
  switch(variable,
         "pr" = {
           result <- spp * 86400  # mm day-1
           if (any(result < 0)) warning("Precip contains negative values")
           result
         },
         "tas" = spp - 273.15,  # 'C
         "rsds" = {
           result <- bigleaf::Rg.to.PPFD(spp)  # umol m-2 s-1
           if (any(result < 0)) warning("PAR contains negative values")
           result
         },
         "hurs" = {
           if (any(spp < 0)) warning("RH contains negative values")
           spp  # %
         },
         stop("Unsupported variable: ", variable)
  )
}

daily_to_average <- function(daily) {
  month_length <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  month_length_leapyear <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  four_years_start_2015 <- c(month_length, month_length, month_length_leapyear, month_length)
  
  monthly <- rep(daily, times = rep(four_years_start_2015, length.out = length(co2)))
  return(monthly)
}
