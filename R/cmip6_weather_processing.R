sort_CMIP <- function() {
  # Constants
  TEMP_DIR <- "~/Documents/Austria/Plant-FATE/inst/cmip6_data/temp"
  ANSWER_DIR <- "~/Documents/Austria/Plant-FATE/inst/cmip6_data/"
  path_test <- "/home/josimms/Documents/Austria/Plant-FATE/tests/data"
  TARGET_LON <- 17.483333
  TARGET_LAT <- 60.083333
  
  # Process each variable
  ssp_scenario = 245
  parameters <- c("tas", "hurs", "rsds")
  results <- list()
  
  # CO2 Processed Seperately
  
  for (variable in parameters) {
    spp_files <- list.files(path = TEMP_DIR,
                            pattern = variable,
                            full.names = TRUE)
    
    spp <- process_files(spp_files, variable, TARGET_LON, TARGET_LAT)
    
    results[[variable]] <- transform_variable(variable, spp)
    message(paste(variable, "processing complete."))
  }
  
  # Compute VPD
  results$VPD <- bigleaf::rH.to.VPD(results$hurs * 0.01, results$tas)
  
  # Create date sequence
  date <- seq(as.Date("2015-01-01"), 
              as.Date("2100-12-30"), 
              length.out = length(results$tas)) # TODO: dates are wrong here, but want graphs
  
  # Get soil potenital from historical data
  monthy_dataset <- fread(file.path(path_test, "ERAS_Monthly.csv"))
  daily_dataset <- fread(file.path(path_test, "ERAS_dataset.csv"))

  # Create final dataset
  dataset_cmip <- data.table::data.table(date = date,
                                         t2m = results$tas,
                                         ssrd = results$rsds,
                                         vpd = results$VPD,
                                         sp = rep(daily_dataset$sp, length.out = length(results$tas)))
  dataset_cmip[, YMD := format(date, "%Y-%m-%d")]
  dataset_cmip[, YM := format(date, "%Y-%m")]
  # NOTE: Mean and max the same as the values are from a daily source!
  dataset_cmip[, ssrd_max := dataset_cmip[, .(ssrd_max = 3*max(ssrd)), by = YMD]$ssrd_max] 
  dataset_cmip$co2 <- 380 # TODO: replace with real values!
  
  # Monthly
  dataset_cmip_monthly <- dataset_cmip[, lapply(.SD, mean), by = YM, .SDcols = -c("date", "YMD", "ssrd_max")]
  dataset_cmip_monthly[, ssrd_max := dataset_cmip[, .(ssrd_max = max(ssrd)), by = YM]$ssrd_max]
  
  # Save results
  fwrite(dataset_cmip, 
       file = file.path(ANSWER_DIR, paste0("PlantFATE", ssp_scenario, ".csv")))
  fwrite(dataset_cmip_monthly, 
         file = file.path(ANSWER_DIR, paste0("PlantFATE_monthly", ssp_scenario, ".csv")))
  
  # Plot results
  plot_data(dataset_cmip_monthly, "Monthly", monthly = TRUE)
  plot_data(dataset_cmip, "Daily", monthly = FALSE)
  
  return("Data processed look to files for the datasets")
}

# Helper functions
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

transform_variable <- function(variable, spp) {
  switch(variable,
         "pr" = {
           result <- spp * 86400  # mm day-1
           if (any(result < 0)) warning("Precip contains negative values")
           result
         },
         "tas" = spp - 273.15,  # 'C
         "rsds" = {
           result <- bigleaf::Rg.to.PPFD(spp) * 0.000001 * 86400  # umol m-2 s-1 to sum mmol m-2 day-1
           if (any(result < 0)) warning("PAR contains negative values")
           result
         },
         "hurs" = {
           if (any(spp < 0)) warning("RH contains negative values")
           spp  # %
         },
         "co2" = {
           spp
         },
         stop("Unsupported variable: ", variable)
  )
}
