whole_weather_process <- function(raw.directory = "/home/josimms/Documents/CASSIA_Calibration/Raw_Data/hyytiala_weather/",
                                  output_directory = "/home/josimms/Documents/Austria/Plant-FATE/tests/data") {
  
  variables <- c("RH672", "RH1250", "RHTd", "PAR", "CO2168", "T168", "T336", "Precip", 
                 "tsoil_5", "tsoil_10", "wsoil_B1", "wsoil_B2", "Glob", "Glob67", 
                 "Pamb336", "wpsoil_A", "wpsoil_B", "GPP")
  
  daily_result <- data.table(Date = character())
  
  for (variable in variables) {
    cat("Processing", variable, "\n")
    
    # Read and process files for each variable separately
    files <- list.files(raw.directory, pattern = variable, full.names = TRUE)
    
    variable_data <- rbindlist(lapply(files, function(file) {
      dt <- fread(file)
      
      # Simplify column names
      setnames(dt, gsub("HYY_META.|HYY_EDDY233.", "", names(dt)))
      
      # Add Date column
      dt[, Date := paste(Year, Month, Day, sep = "-")]
      
      return(dt)
    }))
    
    # Calculate daily statistics
    if (variable %in% c("Glob", "Glob67", "PAR")) {
      daily_stats <- variable_data[, .(
        mean = mean(get(variable), na.rm = TRUE),
        max = max(get(variable), na.rm = TRUE)
      ), by = Date]
      setnames(daily_stats, c("mean", "max"), paste0(variable, c("_mean", "_max")))
    } else if (variable == "Precip") {
      daily_stats <- variable_data[, .(sum = sum(get(variable), na.rm = TRUE)), by = Date]
      setnames(daily_stats, "sum", paste0(variable, "_sum"))
    } else {
      daily_stats <- variable_data[, .(mean = mean(get(variable), na.rm = TRUE)), by = Date]
      setnames(daily_stats, "mean", paste0(variable, "_mean"))
    }
    
    # Merge with the result
    daily_result <- merge(daily_result, daily_stats, by = "Date", all = TRUE)
    
    # Clear memory
    rm(variable_data, daily_stats)
    gc()
  }
  
  # Add Year, Month, and Day columns
  daily_result[, c("Year", "Month", "Day") := tstrsplit(Date, "-", fixed=TRUE)]
  
  # Convert Year, Month, and Day to numeric for proper sorting
  daily_result[, ':='(Year = as.numeric(Year), 
                      Month = as.numeric(Month), 
                      Day = as.numeric(Day))]
  daily_result[, VPD := 10 * bigleaf::rH.to.VPD(0.01*RH672_mean, T336_mean)]
  
  # Add Monthly column
  daily_result[, Monthly := paste(Year, sprintf("%02d", Month), sep = "-")]
  
  # Sort the data by date
  setorder(daily_result, Year, Month, Day)
  
  # Reorder columns
  setcolorder(daily_result, c("Year", "Month", "Day", "Date", "Monthly"))
  
  # Save the result
  fwrite(daily_result, file.path(output_directory, "daily_dataframe.csv"))
  
  cat("Processing complete. Results saved to", file.path(output_directory, "daily_dataframe.csv"), "\n")
}