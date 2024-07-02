###
# Fast weather processing
###

downloading_data <- function(raw.directory = "/home/josimms/Documents/CASSIA_Calibration/Raw_Data/hyytiala_weather/",
                             year_start = 1995,
                             year_end = 2023) {
  
  http.origin = "https://smear-backend.rahtiapp.fi/search/timeseries/csv?tablevariable=HYY_"
  for (variable in c(paste0("META.", c("RH672", "RH1250", "RHTd", "PAR", "CO2168", "T168", "T336", "Precip", "tsoil_5", "tsoil_10", "wsoil_B1", "wsoil_B2", "Glob", "Glob67", "Pamb336", "wpsoil_A", "wpsoil_B")), "EDDY233.GPP")[16:17]) {
    from = "&from="
    year1 = seq(year_start, year_end, by = 2)
    to = "-01-01T00%3A00%3A00.000&to="
    year2 = seq(year_start+1, year_end, by = 2)
    if (length(year2) != length(year1)) {year2 <- c(year2, year_end)}
    end = "-12-31T23%3A59%3A59.999&quality=ANY&aggregation=NONE&interval=1"
    
    files = as.list(paste0(http.origin, variable, from, year1, to, year2, end))
    imported_file = as.list(paste0(raw.directory, variable, from, year1, "to", year2))
    
    mapply(download.file, files, imported_file)
  }

  # Doesn't return as it is downloading the files
}


###
# inporting_hyytiala_raw_data_into_list
###

inporting_hyytiala_raw_data_into_list <- function(raw.directory = "/home/josimms/Documents/CASSIA_Calibration/Raw_Data/hyytiala_weather/") {
  
  environmental.variable.list <- list()
  count = 1
  for (variable in c("RH672", "RH1250", "RHTd", "PAR", 
                     "CO2168", "T168", "T336", "Precip", 
                     "tsoil_5", "tsoil_10", "wsoil_B1", "wsoil_B2", 
                     "Glob", "Glob67", "Pamb336", "wpsoil_A", "wpsoil_B",
                     "GPP")) {
    environmental.variable.list[[count]] <- data.table::rbindlist(lapply(paste0(raw.directory, list.files(raw.directory, variable)), data.table::fread))
    environmental.variable.list[[count]]$Date <- paste(environmental.variable.list[[count]]$Year,
                                                               environmental.variable.list[[count]]$Month, 
                                                               environmental.variable.list[[count]]$Day,
                                                               sep = "-")
    environmental.variable.list[[count]]$Monthly <- paste(environmental.variable.list[[count]]$Year,
                                                       environmental.variable.list[[count]]$Month,
                                                       sep = "-")
    count = count + 1
  }
  
  return(environmental.variable.list)
}

###
# Simplifying the names!
### 

simplifying_names <- function(environmental.variable.list) {
  names(environmental.variable.list) <- gsub("HYY_META.", "", names(environmental.variable.list))
  names(environmental.variable.list) <- gsub("HYY_EDDY233.", "", names(environmental.variable.list))
  return(environmental.variable.list)
}

###
# Generating mean values
###

generating_mean_values_daily <- function(environmental.variable.list) {
  RH672_out <- environmental.variable.list[["RH672"]][, RH672_mean := mean(RH672, na.rm = T), by = Date]
  RH1250_out <- environmental.variable.list[["RH1250"]][, RH1250_mean := mean(RH1250, na.rm = T), by = Date]
  RHTd_out <- environmental.variable.list[["RHTd"]][, RHTd_mean := mean(RHTd, na.rm = T), by = Date]
  CO2168_out <- environmental.variable.list[["CO2168"]][, CO2168_mean := mean(CO2168, na.rm = T), by = Date]
  T168_out <- environmental.variable.list[["T168"]][, T168_mean := mean(T168, na.rm = T), by = Date]
  T336_out <- environmental.variable.list[["T336"]][, T336_mean := mean(T336, na.rm = T), by = Date]
  wpsoil_A_out <- environmental.variable.list[["wpsoil_A"]][, wpsoil_A_mean := mean(wpsoil_A, na.rm = T), by = Date]
  wpsoil_B_out <- environmental.variable.list[["wpsoil_B"]][, wpsoil_B_mean := mean(wpsoil_B, na.rm = T), by = Date]
  GPP_out <- environmental.variable.list[["GPP"]][, GPP_mean := mean(GPP, na.rm = T), by = Date]
  Glob_out <- environmental.variable.list[["Glob"]][, Glob_mean := mean(Glob, na.rm = T), by = Date]
  Glob67_out <- environmental.variable.list[["Glob67"]][, Glob67_mean := mean(Glob67, na.rm = T), by = Date]
  # Max
  Glob_max <- environmental.variable.list[["Glob"]][, Glob_max := max(Glob, na.rm = T), by = Date]
  Glob67_max <- environmental.variable.list[["Glob67"]][, Glob67_max := max(Glob67, na.rm = T), by = Date]
  # Sum
  Precip_out <- environmental.variable.list[["Precip"]][, Precip_sum := sum(Precip, na.rm = T), by = Date]
  
  ## Make into list
  daily.list <- list(RH672_out, RH1250_out, RHTd_out, CO2168_out, T168_out, T336_out,
                     wpsoil_A_out, wpsoil_B_out, GPP_out, Glob_out, Glob67_out, Glob_max, Glob67_max,
                     Precip_out)
  return(daily.list)
}

generating_mean_values_monthly <- function(environmental.variable.list) {
  out <- environmental.variable.list[, mean_value := mean(7, na.rm = T), by = 9]
  return(out)
}

generating_max_values_monthly <- function(environmental.variable.list) {
  out <- environmental.variable.list[, max_value := max(7, na.rm = T), by = 9]
  return(out)
}

generating_sum_values_monthly <- function(environmental.variable.list) {
  out <- environmental.variable.list[, sum_value := sum(7, na.rm = T), by = 9]
  return(out)
}

###
# Compiling the mounthly and daily values
###

daily_list <- function(environmental.variable.list) {
  test <- lapply(environmental.variable.list, generating_mean_values_daily)
  
  return(environmental.variable.list)
}

###
#
###

whole_weather_process <- function(raw.directory = "/home/josimms/Documents/CASSIA_Calibration/Raw_Data/hyytiala_weather/") {
  library(data.table)
  
  ###
  # HYYTIÄLÄ
  ###
  
  ## Big files only run if really really nesicary!
  # downloading_data()
  
  ### READING FILES
  environmental.variable.list <- inporting_hyytiala_raw_data_into_list()
  
  ### RENAMING VARIABLE NAMES
  environmental.variable.list <- lapply(environmental.variable.list, simplifying_names)
  
  names(environmental.variable.list) <- c("RH672", "RH1250", "RHTd", "PAR", 
                                          "CO2168", "T168", "T336", "Precip", 
                                          "tsoil_5", "tsoil_10", "wsoil_B1", "wsoil_B2", 
                                          "Glob", "Glob67", "Pamb336", "wpsoil_A", "wpsoil_B",
                                          "GPP")
  ### GETTING DAILY VALUES
  daily.list <- generating_mean_values_daily(environmental.variable.list)
  daily.dataframe <- rbinddatatable(daily.list)
    
  ### saving dataframe!
  data.direct <- "/home/josimms/Documents/Austria/PlantFATE/tests/data"
  fwrite(daily.dataframe, paste(data.direct, "daily.dataframe.csv"))
  
  ### GETTING MONTHLY VALUES
  monthly.list <- generating_mean_values_daily(environmental.variable.list)
  
  ### saving dataframe!
  
  ###
  # GAPFILLING
  ### 
  
  ## TODO: parameters from this? 
  # ProfoundData
  
  
}
