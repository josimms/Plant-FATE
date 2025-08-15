cmip_applied_lho <- function() {
  
  ####
  # Weather comparison
  ####
  
  amazon <- read.delim("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", sep = ",")
  boreal_245 <- read.delim("tests/data/PlantFATE_monthly245.csv", sep = ",")
  boreal_585 <- read.delim("tests/data/PlantFATE_monthly585.csv", sep = ",")
  
  ### WEATHER
  
  par(mfrow = c(2, 2))
  plot(as.Date(paste(boreal_585$Year, boreal_585$Month, "01", sep = "-"), format = "%Y-%m-%d"), boreal_585$Temp, type = "l",
       ylim = range(boreal_245$Temp, boreal_585$Temp, amazon$Temp), ylab = "Temp, 'C", xlab = "Dates", col = "red")
  lines(as.Date(paste(boreal_245$Year, boreal_245$Month, "01", sep = "-"), format = "%Y-%m-%d"), boreal_245$Temp, col = "yellow")
  lines(as.Date(paste(amazon$Year, amazon$Month, "01", sep = "-"), format = "%Y-%m-%d"), amazon$Temp, col = "blue")
  
  plot(as.Date(paste(boreal_585$Year, boreal_585$Month, "01", sep = "-"), format = "%Y-%m-%d"), boreal_585$VPD, type = "l",
       ylim = range(boreal_245$VPD, boreal_585$VPD, amazon$VPD), ylab = "VPD, hPa", xlab = "Dates", col = "red")
  lines(as.Date(paste(boreal_245$Year, boreal_245$Month, "01", sep = "-"), format = "%Y-%m-%d"), boreal_245$VPD, col = "yellow")
  lines(as.Date(paste(amazon$Year, amazon$Month, "01", sep = "-"), format = "%Y-%m-%d"), amazon$VPD, col = "blue")
  
  plot(as.Date(paste(boreal_585$Year, boreal_585$Month, "01", sep = "-"), format = "%Y-%m-%d"), boreal_585$PPFD, type = "l",
       ylim = range(boreal_245$PPFD, boreal_585$PPFD, amazon$PAR), ylab = "PPFD, umol m-2 s-1", xlab = "Dates", col = "red")
  lines(as.Date(paste(boreal_245$Year, boreal_245$Month, "01", sep = "-"), format = "%Y-%m-%d"), boreal_245$PPFD, col = "yellow")
  lines(as.Date(paste(amazon$Year, amazon$Month, "01", sep = "-"), format = "%Y-%m-%d"), amazon$PAR, col = "blue")
  
  plot(as.Date(paste(boreal_585$Year, boreal_585$Month, "01", sep = "-"), format = "%Y-%m-%d"), boreal_585$SWP, type = "l",
       ylim = range(boreal_245$SWP, boreal_585$SWP, amazon$SWP), ylab = "SWP", xlab = "Dates", col = "red")
  lines(as.Date(paste(boreal_245$Year, boreal_245$Month, "01", sep = "-"), format = "%Y-%m-%d"), boreal_245$SWP, col = "yellow")
  lines(as.Date(paste(amazon$Year, amazon$Month, "01", sep = "-"), format = "%Y-%m-%d"), amazon$SWP, col = "blue")
  
  ####
  # Finial Initialisation files
  ####
  lho_amazon <- create_lho("tests/params/p_test_v2.ini", 
                           "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv",
                           "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv",
                           init_co2 = 365)
  
  lho_high_co2 <- create_lho("tests/params/p_test_boreal_monthly_co2_high.ini", 
                             "tests/data/PlantFATE_monthly585.csv", 
                             "tests/data/PlantFATE_monthly585.csv", 
                             "tests/data/PlantFATE_yearly585.csv")
  
  lho_medium_co2 <- create_lho("tests/params/p_test_boreal_monthly_co2_medium.ini", 
                               "tests/data/PlantFATE_monthly245.csv", 
                               "tests/data/PlantFATE_monthly245.csv", 
                               "tests/data/PlantFATE_yearly245.csv")
  
  ### Actual run outputs
  dt <- 1/12
  df_amazon <- run_for_dataset(lho_amazon, 2000, 2015, dt)
  df_high_co2 <- run_for_dataset(lho_high_co2, 2015, 2100, dt)
  df_medium_co2 <- run_for_dataset(lho_medium_co2, 2015, 2100, dt)
  
  #####
  # Plots
  #####
  
  ### OUTPUTS
  
  par(mfrow = c(2, 2))
  plot(df_high_co2$date, df_high_co2$assim_gross, type = "l", col = "red",
       xlab = "Date", ylab = "GPP, kg C m-2", 
       ylim = range(c(df_high_co2$assim_gross, df_medium_co2$assim_gross, df_amazon$assim_gross))) 
  lines(df_medium_co2$date, df_medium_co2$assim_gross, col = "yellow")
  lines(df_amazon$date, df_amazon$assim_gross, col = "blue")
  
  plot(df_high_co2$date, df_high_co2$height, type = "l", col = "red",
       xlab = "Date", ylab = "Height, m",
       ylim = range(c(df_high_co2$height, df_resp$height, df_core_traits$height, df_amazon$height))) 
  lines(df_medium_co2$date, df_medium_co2$height, col = "yellow")
  lines(df_amazon$date, df_amazon$height, col = "blue")
  
  plot(df_high_co2$date, df_high_co2$nitrogen, type = "l", col = "red",
       xlab = "Date", ylab = "Leaf Nitrogen, g g-1", # TODO: units!
       ylim = range(c(df_high_co2$nitrogen, df_medium_co2$nitrogen, df_amazon$nitrogen))) 
  lines(df_medium_co2$date, df_medium_co2$nitrogen, col = "yellow")
  lines(df_amazon$date, df_amazon$nitrogen, col = "blue")
  
  plot(df_high_co2$date, df_high_co2$rs, type = "l", col = "red",
       xlab = "Date", ylab = "Respiration", # TODO: is this respiration?
       ylim = range(c(df_high_co2$rs, df_medium_co2$rs, df_amazon$rs))) 
  lines(df_medium_co2$date, df_medium_co2$rs, col = "yellow")
  lines(df_amazon$date, df_amazon$rs, col = "blue")
  
}