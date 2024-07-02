get_CMIP <- function(urls_ssp, source_id, ssp_scenario, download = FALSE) {
  urls_ssp
  source_id
  ssp_scenario = 245
  
  # Constants
  TEMP_DIR <- "~/Documents/Austria/Plant-FATE/cmip6_data/temp/"
  ANSWER_DIR <- "~/Documents/Austria/Plant-FATE/cmip6_data/"
  TARGET_LON <- 17.483333
  TARGET_LAT <- 60.083333
  
  # Filter data for the specified source
  urls_ssp_source <- urls_ssp[urls_ssp$source_id %in% source_id, ]
  
  if (nrow(urls_ssp_source) == 0) {
    stop("No data found for the specified source_id")
  }
  
  if (any(urls_ssp_source$source_id != source_id)) {
    stop("Taking data from the wrong source_id!")
  }
  
  # Download files if requested
  if (download) {
    options(timeout = max(3000, getOption("timeout")))
    files <- file.path(TEMP_DIR, 
                       paste(ssp_scenario, 
                             urls_ssp_source$source_id, 
                             urls_ssp_source$variable_id, 
                             urls_ssp_source$member_id, 
                             urls_ssp_source$datetime_start, 
                             urls_ssp_source$datetime_end, 
                             sep = "_"), 
                       ".nc")
    files_missing <- files[!file.exists(files)]
    if (length(files_missing) > 0) {
      mapply(download.file, urls_ssp_source$file_url[!file.exists(files)], files_missing)
    }
    message("Download Complete!")
  }
  
  # Process each variable
  parameters <- unique(urls_ssp_source$variable_id)
  results <- list()
  
  for (variable in parameters) {
    spp_files <- list.files(path = TEMP_DIR,
                            pattern = paste(ssp_scenario, 
                                            urls_ssp_source$source_id[1], 
                                            variable, 
                                            urls_ssp_source$member_id[1], 
                                            sep = "_"),
                            full.names = TRUE)
    
    spp <- process_files(spp_files, variable, TARGET_LON, TARGET_LAT)
    
    results[[variable]] <- transform_variable(variable, spp)
    message(paste(variable, "processing complete."))
  }
  
  # Compute VPD
  results$VPD <- bigleaf::rH.to.VPD(results$hurs * 0.01, results$tas)
  
  # Create date sequence
  date <- seq(as.Date(urls_ssp_source$datetime_start[1]), 
              as.Date(tail(urls_ssp_source$datetime_end, n = 1)), 
              by = "day")
  
  # Adjust for non-leap years if necessary
  if (length(date) != length(results$tas)) {
    date <- date[!find_leap(date)]
    if (source_id == "CESM2-WACCM") {
      results <- lapply(results, function(x) x[-length(x)])
    }
  }
  
  # Create final dataset
  dataset_cmip <- data.frame(date = date,
                             TAir = results$tas,
                             PAR = results$rsds,
                             VPD = results$VPD)
  
  # Save results
  save(dataset_cmip, 
       file = file.path(ANSWER_DIR, paste0(source_id, "_", ssp_scenario, ".RData")))
  
  beepr::beep(3)
  return(dataset_cmip)
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
         stop("Unsupported variable: ", variable)
  )
}