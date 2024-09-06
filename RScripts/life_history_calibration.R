boreal_calibration_lho <- function() {
  ####
  # Weather comparison
  ####
  amazon <- read.delim("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", sep = ",")
  boreal <- read.delim("tests/data/ERAS_Monthly.csv", sep = ",")
  
  ### WEATHER
  
  par(mfrow = c(2, 2))
  plot(as.Date(paste(boreal$Year, boreal$Month, "01", sep = "-"), format = "%Y-%m-%d"), boreal$Temp, type = "l",
       ylim = range(boreal$Temp, amazon$Temp), ylab = "Temp", xlab = "Dates")
  lines(as.Date(paste(amazon$Year, amazon$Month, "01", sep = "-"), format = "%Y-%m-%d"), amazon$Temp, col = "blue")
  
  plot(as.Date(paste(boreal$Year, boreal$Month, "01", sep = "-"), format = "%Y-%m-%d"), boreal$VPD, type = "l",
       ylim = range(boreal$VPD, amazon$VPD), ylab = "VPD", xlab = "Dates")
  lines(as.Date(paste(amazon$Year, amazon$Month, "01", sep = "-"), format = "%Y-%m-%d"), amazon$VPD, col = "blue")
  
  plot(as.Date(paste(boreal$Year, boreal$Month, "01", sep = "-"), format = "%Y-%m-%d"), boreal$PPFD, type = "l",
       ylim = range(boreal$PPFD, amazon$PAR), ylab = "PPFD", xlab = "Dates")
  lines(as.Date(paste(amazon$Year, amazon$Month, "01", sep = "-"), format = "%Y-%m-%d"), amazon$PAR, col = "blue")
  
  plot(as.Date(paste(boreal$Year, boreal$Month, "01", sep = "-"), format = "%Y-%m-%d"), boreal$SWP, type = "l",
       ylim = range(boreal$SWP, amazon$SWP), ylab = "SWP", xlab = "Dates")
  lines(as.Date(paste(amazon$Year, amazon$Month, "01", sep = "-"), format = "%Y-%m-%d"), amazon$SWP, col = "blue")
  
  
  ####
  # Finial Initialisation files
  ####
  lho_amazon <- create_lho("tests/params/p_test_v2.ini", 
                           "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv",
                           "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv",
                           init_co2 = 365)
  lho_weather <- create_lho("tests/params/p_test_boreal_only_weather_changed.ini", 
                           "tests/data/ERAS_Monthly.csv", 
                           "tests/data/ERAS_Monthly.csv", 
                           init_co2 = 365)
  lho_phydro <- create_lho("tests/params/p_test_boreal_weather_traits.ini", 
                            "tests/data/ERAS_Monthly.csv", 
                            "tests/data/ERAS_Monthly.csv", 
                            init_co2 = 365)
  lho_core_traits <- create_lho("tests/params/p_test_boreal_weather_core_traits.ini", 
                                "tests/data/ERAS_Monthly.csv", 
                                "tests/data/ERAS_Monthly.csv", 
                                init_co2 = 365)
  lho_resp <- create_lho("tests/params/p_test_boreal_weather_resp.ini", 
                         "tests/data/ERAS_Monthly.csv", 
                         "tests/data/ERAS_Monthly.csv", 
                         init_co2 = 365)
  lho_boreal <- create_lho("tests/params/p_test_boreal.ini", 
                           "tests/data/ERAS_Monthly.csv", 
                           "tests/data/ERAS_Monthly.csv", 
                           init_co2 = 365)
  
  ####
  # Actual run outputs
  ####
  dt <- 1/12
  df_amazon <- run_for_dataset(lho_amazon, 1965, 2022, dt) # Note: weather repeats when it runs out
  df_weather <- run_for_dataset(lho_weather, 1965, 2022, dt)
  df_phydro <- run_for_dataset(lho_phydro, 1965, 2022, dt)
  df_core_traits <- run_for_dataset(lho_core_traits, 1965, 2022, dt)
  df_resp <- run_for_dataset(lho_resp, 1965, 2022, dt)
  df_boreal <- run_for_dataset(lho_boreal, 1965, 2022, dt) # TODO: this doesn't run! Why?
  
  ####
  # Plotting lho
  ####

  ### OUTPUTS
  
  # TODO: add data
  par(mfrow = c(3, 2))
  plot(df_weather$date, df_weather$assim_gross, type = "l", col = "red",
      xlab = "Date", ylab = "GPP, kg C m-2", 
      ylim = range(c(df_weather$assim_gross, df_resp$assim_gross, df_core_traits$assim_gross, df_amazon$assim_gross))) 
  lines(df_phydro$date, df_phydro$assim_gross, col = "orange")
  lines(df_core_traits$date, df_core_traits$assim_gross, col = "yellow")
  lines(df_resp$date, df_resp$assim_gross, col = "green")
  lines(df_boreal$date, df_boreal$assim_gross, col = "blue")
  lines(df_amazon$date, df_amazon$assim_gross, lty = 2)
  
  plot(df_weather$date, df_weather$height, type = "l", col = "red",
       xlab = "Date", ylab = "Height, m",
       ylim = range(c(df_weather$height, df_resp$height, df_core_traits$height, df_amazon$height))) 
  lines(df_phydro$date, df_phydro$height, col = "orange")
  lines(df_core_traits$date, df_core_traits$height, col = "yellow")
  lines(df_resp$date, df_resp$height, col = "green")
  lines(df_boreal$date, df_boreal$height, col = "blue")
  lines(df_amazon$date, df_amazon$height, lty = 2)
  
  plot(df_weather$date, df_weather$nitrogen, type = "l", col = "red",
       xlab = "Date", ylab = "Leaf Nitrogen, g g-1", # TODO: units!
       ylim = range(c(df_weather$nitrogen, df_resp$nitrogen, df_core_traits$nitrogen, df_amazon$nitrogen))) 
  lines(df_phydro$date, df_phydro$nitrogen, col = "orange")
  lines(df_core_traits$date, df_core_traits$nitrogen, col = "yellow")
  lines(df_resp$date, df_resp$nitrogen, col = "green")
  lines(df_boreal$date, df_boreal$nitrogen, col = "blue")
  lines(df_amazon$date, df_amazon$nitrogen, lty = 2)
  
  plot(df_weather$date, df_weather$rs, type = "l", col = "red",
       xlab = "Date", ylab = "Respiration", # TODO: is this respiration?
       ylim = range(c(df_weather$rs, df_resp$rs, df_core_traits$rs, df_amazon$rs))) 
  lines(df_phydro$date, df_phydro$rs, col = "orange")
  lines(df_core_traits$date, df_core_traits$rs, col = "yellow")
  lines(df_resp$date, df_resp$rs, col = "green")
  lines(df_boreal$date, df_boreal$total_rep, col = "blue")
  lines(df_amazon$date, df_amazon$rs, lty = 2)
  
  plot(df_weather$date, df_weather$root_mass, type = "l", col = "red", # TODO: is root mass, actually fine root mass, or is there another parameter here?
       xlab = "Date", ylab = "Fine Root Mass",
       ylim = range(c(df_weather$root_mass, df_resp$rs, df_core_traits$rs, df_amazon$rs))) 
  lines(df_phydro$date, df_phydro$root_mass, col = "orange")
  lines(df_core_traits$date, df_core_traits$root_mass, col = "yellow")
  lines(df_resp$date, df_resp$root_mass, col = "green")
  lines(df_boreal$date, df_boreal$root_mass, col = "blue")
  lines(df_amazon$date, df_amazon$root_mass, lty = 2)
  
  ####
  # Sensitivity analysis of certian parameters
  #####
  
  create_lho_params <- function(params_file, i_metFile, a_metFile, co2File = "", init_co2 = NULL, param) {
    lho <- new(LifeHistoryOptimizer, params_file)
    lho$set_i_metFile(i_metFile)
    lho$set_a_metFile(a_metFile)
    lho$set_co2File(co2File)
    if (!is.null(init_co2)) lho$init_co2(init_co2)
    print(c(lho$env$clim_inst$co2, lho$env$clim_acclim$co2))
    lho$par0$ <- param
    lho$init()
    return(lho)
  }
  
  potential_parameters <- seq(0.01, 0.3, length.out = 40)
  
  # Use mclapply for parallel processing
  sa_rs <- mclapply(potential_parameters, function(i) {
    lho_temp <- create_lho_params("tests/params/p_test_v2.ini", 
                                  "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                                  "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                                  co2File = "",
                                  init_co2 = 365,
                                  i)
    dt <- 1/12
    df <- run_for_dataset(lho_temp, 1960, 2022, dt)
    result$rs_value <- i
    return(result)
  }, mc.cores = detectCores()-2)
  
  # Combine the results into a single data frame
  sa_rs_all <- do.call(rbind, sa_rs)
  
  # Plot
  color_scale <- scale_color_gradientn(colours = rainbow(5), name = "Sapwood Respiration")
  combined_plot <- create_combined_plot(df_original, sa_rs_all, color_scale)
  print(combined_plot)
  
  
  
  # TODO: fine root data?
}
