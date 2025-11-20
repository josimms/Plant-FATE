blank <- function() {
  library(tidyverse)
  library(lubridate)  # convenient for date operations
  library(ggplot2)
  library(zoo)
  library(data.table)
  library(rphydro)
  library(Rprebasso)
  library(patchwork)
  
  ###
  # PREBAS Comparison
  ###

  prebas <- TransectRun(defaultThin = 0, ClCut = 0)
  
  # Rprebasso::varNames
  
  ###
  # Validation data
  ###
  
  data_directory <- "~/Documents/CASSIA_Calibration/Processed_Data/"
  loaded_data <- CASSIA::load_data(data_directory)
  
  halme_et_al_2022 <- data.frame(matrix(nrow = 9, ncol = 2))
  halme_et_al_2022$height <- c(17.1, 21.0, 20.5, 18.3, 13.2, 20.7, 16.8, 23.6, 13.6)
  halme_et_al_2022$crown_diameter <- c(3.4, 4.0, 4.1, 3.3, 2.9, 3.7, 3.4, 4.2, 2.8)
  
  input_climate <- read.csv("~/Documents/Austria/Plant-FATE/tests/data/ERAS_Monthly.csv")
  
  raw.directory = "/home/josimms/Documents/CASSIA_Calibration/Raw_Data/hyytiala_weather/"
  environmental.variable.list <- list()
  count = 1
  for (variable in c("GPP", "NEE", "F_CO2_leaf", "F_H2O_leaf", "ET_gapf")) {
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
  names(environmental.variable.list) <- c("GPP", "NEE", "F_CO2_leaf", "F_H2O_leaf", "ET")
  
  # Original Eddy Covarience files in umol CO2 m-2 s-1
  DT <- copy(environmental.variable.list[["GPP"]])   # copy to avoid modifying original
  DT2 <- copy(environmental.variable.list[["NEE"]])   # copy to avoid modifying original
  DT3 <- copy(environmental.variable.list[["F_CO2_leaf"]])   # copy to avoid modifying original
  DT4 <- copy(environmental.variable.list[["F_H2O_leaf"]])   # copy to avoid modifying original
  DT5 <- copy(environmental.variable.list[["ET"]])   # copy to avoid modifying original
  
  GPP_out <- DT[, .(GPP_mean = mean(HYY_EDDY233.GPP, na.rm = TRUE)), by = Monthly]
  NEE_out <- DT2[, .(NEE_mean = mean(HYY_EDDY233.NEE, na.rm = TRUE)), by = Monthly]
  F_CO2_leaf_out <- DT3[, .(F_CO2_leaf_mean = mean(HYY_TREE.F_CO2_leaf, na.rm = TRUE)), by = Monthly]
  F_H2O_leaf_out <- DT4[, .(F_H2O_leaf_mean = mean(HYY_TREE.F_H2O_leaf, na.rm = TRUE)), by = Monthly]
  ET_out <- DT5[, .(ET_mean = mean(HYY_EDDY233.ET_gapf, na.rm = TRUE)), by = Monthly]
  
  GPP_out <- GPP_out[ , Monthly := zoo::as.yearmon(Monthly, "%Y-%m") ]
  NEE_out <- NEE_out[ , Monthly := zoo::as.yearmon(Monthly, "%Y-%m") ]
  F_CO2_leaf_out <- F_CO2_leaf_out[ , Monthly := zoo::as.yearmon(Monthly, "%Y-%m") ]
  F_H2O_leaf_out <- F_H2O_leaf_out[ , Monthly := zoo::as.yearmon(Monthly, "%Y-%m") ]
  ET_out <- ET_out[ , Monthly := zoo::as.yearmon(Monthly, "%Y-%m") ]
  
  # If Month is numeric (1..12):
  # TODO: is there transpiration data?
  setorder(GPP_out, Monthly)
  setorder(NEE_out, Monthly)
  setorder(F_CO2_leaf_out, Monthly)
  setorder(F_H2O_leaf_out, Monthly)
  setorder(ET_out, Monthly)
  
  GPP_out$GPP_mean = GPP_out$GPP_mean
  GPP_out$GPP_mean_kg = GPP_out$GPP_mean * 12.11 * 1e-9 * 60 * 60 * 24 * 365.25 # kg per year, as in PlantFATE
  NEE_out$NEE_mean = NEE_out$NEE_mean
  NEE_out$NEE_mean_kg = NEE_out$NEE_mean * 12.11 * 1e-9 * 60 * 60 * 24 * 365.25 # kg per year, as in PlantFATE
  F_CO2_leaf_out$F_CO2_leaf_mean = F_CO2_leaf_out$F_CO2_leaf_mean
  F_CO2_leaf_out$F_CO2_leaf_mean_kg = F_CO2_leaf_out$F_CO2_leaf_mean * 2.45e-8 * 60 * 60 * 24 * 365.25 # kg per year, as in PlantFATE
  F_H2O_leaf_out$F_H2O_leaf_mean = F_H2O_leaf_out$F_H2O_leaf_mean
  F_H2O_leaf_out$F_H2O_leaf_mean_kg = F_H2O_leaf_out$F_H2O_leaf_mean * 1.801528e-8 * 60 * 60 * 24 * 365.25 # kg per year, as in PlantFATE
  ET_out$ET_mean = ET_out$ET_mean
  ET_out$ET_mean_kg = ET_out$ET_mean * 1.801528e-8 * 60 * 60 * 24 * 365.25 # kg per year, as in PlantFATE
  
  GPP_out$NEE_mean <- NEE_out$NEE_mean
  GPP_out$NEE_mean_kg <- NEE_out$NEE_mean_kg
  GPP_out <- merge(GPP_out, F_CO2_leaf_out[, c("Monthly", "F_CO2_leaf_mean")], 
                   by = "Monthly", all.x = TRUE)
  GPP_out <- merge(GPP_out, F_H2O_leaf_out[, c("Monthly", "F_H2O_leaf_mean")], 
                   by = "Monthly", all.x = TRUE)
  GPP_out$ET <- ET_out$ET_mean
  Eddy_covariance <- GPP_out
  Eddy_covariance[, MonthlyDate := as.Date(paste0("01 ", Monthly), format = "%d %b %Y")]
  Eddy_covariance[, Month := as.integer(format(MonthlyDate, "%m"))]
  
  
  ###
  # Simulation!
  ###
  
  # ------------------------------------------------------------
  # Add a third simulation (lho_3) for set_soil_nitrogen(0.5)
  # ------------------------------------------------------------
  
  lho <- new(LifeHistoryOptimizer, "tests/params/p_test_boreal.ini")
  lho$set_i_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  lho$set_a_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  lho$set_co2File("")
  lho$set_soil_nitrogen(1.1)
  lho$init()
  
  lho_2 <- new(LifeHistoryOptimizer, "tests/params/p_test_boreal.ini")
  lho_2$set_i_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  lho_2$set_a_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  lho_2$set_co2File("")
  lho_2$set_soil_nitrogen(0.9)
  lho_2$init()
  
  lho_3 <- new(LifeHistoryOptimizer, "tests/params/p_test_boreal.ini")
  lho_3$set_i_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  lho_3$set_a_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  lho_3$set_co2File("")
  lho_3$set_soil_nitrogen(0.7)
  lho_3$init()
  
  dt <- 1/12
  start_year <- 1960
  end_year <- 2022+60
  years_seq <- seq(start_year, end_year, dt)
  
  df <- df_2 <- df_3 <- data.frame(matrix(ncol = length(lho$get_header()), nrow = 0))
  col_names <- lho$get_header()
  
  results  <- results_2 <- results_3 <- vector("list", length(years_seq))
  i <- 1
  
  for (t in years_seq) {
    # --- Simulation 1 (N = 0.9)
    results[[i]] <- tryCatch({
      lho$grow_for_dt(t, dt)
      state <- lho$get_state(t + dt)
      df_row <- as.data.frame(t(state))
      names(df_row) <- col_names
      df_row
    }, error = function(e) {
      message("Error at t=", t, " (lho): ", e$message)
      setNames(as.data.frame(as.list(rep(NA, length(col_names)))), col_names)
    })
    
    # --- Simulation 2 (N = 0.7)
    results_2[[i]] <- tryCatch({
      lho_2$grow_for_dt(t, dt)
      state_2 <- lho_2$get_state(t + dt)
      df_row_2 <- as.data.frame(t(state_2))
      names(df_row_2) <- col_names
      df_row_2
    }, error = function(e) {
      message("Error at t=", t, " (lho_2): ", e$message)
      setNames(as.data.frame(as.list(rep(NA, length(col_names)))), col_names)
    })
    
    # --- Simulation 3 (N = 0.5)
    results_3[[i]] <- tryCatch({
      lho_3$grow_for_dt(t, dt)
      state_3 <- lho_3$get_state(t + dt)
      df_row_3 <- as.data.frame(t(state_3))
      names(df_row_3) <- col_names
      df_row_3
    }, error = function(e) {
      message("Error at t=", t, " (lho_3): ", e$message)
      setNames(as.data.frame(as.list(rep(NA, length(col_names)))), col_names)
    })
    
    i <- i + 1
  }
  
  # --- Combine to data frames
  df   <- do.call(rbind, results)
  df_2 <- do.call(rbind, results_2)
  df_3 <- do.call(rbind, results_3)
  names(df) <- names(df_2) <- names(df_3) <- col_names
  
  # --- Add date columns
  dates <- seq(as.Date(paste0(start_year, "-01-01")),
               as.Date(paste0(end_year, "-01-01")), by = "month")[1:nrow(df)]
  
  df$date   <- dates
  df_2$date <- dates
  df_3$date <- dates
  
  # Set color scheme
  cols <- c("red", "orange", "yellow")
  N_labels <- c("N=0.9", "N=0.7", "N=0.5")
  
  #####
  # Weather
  #####
  p1 <- ggplot(input_climate, aes(x = 1:nrow(input_climate), y = Temp)) + 
    geom_line() + 
    labs(title = "Temperature", x = "Time", y = "Temp (°C)")
  
  p2 <- ggplot(input_climate, aes(x = 1:nrow(input_climate), y = VPD)) + 
    geom_line() + 
    labs(title = "VPD", x = "Time", y = "VPD (hPa)")
  
  p3 <- ggplot(input_climate, aes(x = 1:nrow(input_climate), y = PPFD)) + 
    geom_line() + 
    labs(title = "PPFD", x = "Time", y = "μmol m⁻² s⁻¹")
  
  p4 <- ggplot(input_climate, aes(x = 1:nrow(input_climate), y = PPFD_max)) + 
    geom_line() + 
    labs(title = "PPFD_max", x = "Time", y = "μmol m⁻² s⁻¹")
  
  p5 <- ggplot(input_climate, aes(x = 1:nrow(input_climate), y = SWP)) + 
    geom_line() + 
    labs(title = "Soil Water Potential", x = "Time", y = "SWP (MPa)")
  
  # Combine all plots vertically (or use / for rows, | for columns)
  (p1 + p2 / p3 + p4 / p5) +
    plot_annotation(title = "Climate Inputs")

  # Amazon
  import_amazon_data <- fread("~/Documents/Austria/Plant-FATE/tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  
  p1 <- ggplot(import_amazon_data, aes(x = 1:nrow(import_amazon_data), y = Temp)) + 
    geom_line() + 
    labs(title = "Temperature", x = "Time", y = "Temp (°C)")
  
  p2 <- ggplot(import_amazon_data, aes(x = 1:nrow(import_amazon_data), y = VPD)) + 
    geom_line() + 
    labs(title = "VPD", x = "Time", y = "VPD (hPa)")
  
  p3 <- ggplot(import_amazon_data, aes(x = 1:nrow(import_amazon_data), y = PAR)) + 
    geom_line() + 
    labs(title = "PAR", x = "Time", y = "μmol m⁻² s⁻¹")
  
  p4 <- ggplot(import_amazon_data, aes(x = 1:nrow(import_amazon_data), y = PAR_max)) + 
    geom_line() + 
    labs(title = "PAR_max", x = "Time", y = "μmol m⁻² s⁻¹")
  
  p5 <- ggplot(import_amazon_data, aes(x = 1:nrow(import_amazon_data), y = SWP)) + 
    geom_line() + 
    labs(title = "Soil Water Potential", x = "Time", y = "SWP (MPa)")
  
  # Combine all plots vertically (or use / for rows, | for columns)
  (p1 + p2 / p3 + p4 / p5) +
    plot_annotation(title = "Climate Inputs")
  
  #####
  # Model
  #####
  
  # ----------------------------
  # 1. Gross Assimilation & Tree Growth
  # ----------------------------
  par(mfrow = c(3, 2))
  
  # 1. Gross Assimilation
  plot(df$date, df$assim_gross, type = "l", col = cols[1],
       ylab = "Assimilation gross", xlab = "Date",
       main = "Gross Assimilation", ylim = range(df$assim_gross, df_2$assim_gross, df_3$assim_gross, Eddy_covariance$GPP_mean_kg,
                    prebas$multiOut[3,,10,,1][1:62], na.rm = TRUE))
  lines(df_2$date, df_2$assim_gross, col = cols[2])
  lines(df_3$date, df_3$assim_gross, col = cols[3])
  points(Eddy_covariance$MonthlyDate, Eddy_covariance$GPP_mean_kg, col = "black", pch = 16)
  points(as.Date(as.character(1960 + prebas$multiOut[3,,7,,1]), format = "%Y")[1:62], 
         prebas$multiOut[3,,10,,1][1:62], col = "green", pch = 17)
  legend("topleft", legend = c(N_labels, "Eddy Covariance", "Preles"), 
         col = c(cols, "black", "green"), lty = c(1,1,1,NA,NA), pch = c(NA,NA,NA,16,17), bty = "n")
  
  # 2. NPP (assim_net)
  plot(df$date, df$assim_net, type = "l", col = cols[1],
       ylab = "NPP (kg C / month)", xlab = "Date",
       main = "Net Primary Production",
       ylim = range(df$assim_net, df_2$assim_net, df_3$assim_net,
                    -Eddy_covariance$NEE_mean_kg, prebas$multiOut[3,,18,,1]/prebas$multiOut[3,,17,,1], na.rm = TRUE))
  lines(df_2$date, df_2$assim_net, col = cols[2])
  lines(df_3$date, df_3$assim_net, col = cols[3])
  points(Eddy_covariance$MonthlyDate, -Eddy_covariance$NEE_mean_kg, col = "black", pch = 16)
  points(as.Date(as.character(1960 + prebas$multiOut[3,,7,,1]), format = "%Y")[1:62], 
         prebas$multiOut[3,,18,,1][1:62]/prebas$multiOut[3,,17,,1][1:62], col = "green", pch = 17)
  legend("topleft", legend = c(N_labels, "Eddy Covariance", "Preles"), 
         col = c(cols, "black", "green"), lty = c(1,1,1,NA,NA), pch = c(NA,NA,NA,16,17), bty = "n")
  
  # 4. Height with validation data
  plot(df$date, df$height, type = "l", col = cols[1],
       ylab = "Height (m)", main = "Tree Height",
       ylim = range(df$height, loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine height BA weighted mean"],
                    prebas$multiOut[3,,11,,1][1:62], na.rm = TRUE))
  lines(df_2$date, df_2$height, col = cols[2])
  lines(df_3$date, df_3$height, col = cols[3])
  points(as.Date(paste0(loaded_data$smearII_data$date[loaded_data$smearII_data$variable == "pine height BA weighted mean"], "-01-01")), 
         loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine height BA weighted mean"], col = "blue", pch = "x")
  points(as.Date(as.character(1960 + prebas$multiOut[3,,7,,1]), format = "%Y")[1:62], 
         prebas$multiOut[3,,11,,1][1:62], col = "green", pch = 17)
  legend("topleft", legend = c(N_labels, "ICOS Data", "Preles"), 
         col = c(cols, "blue", "green"), lty = c(1,1,1,NA,NA), pch = c(NA,NA,NA,16,17), bty = "n")
  
  plot(df$date, df$diameter, type = "l", col = cols[1],
       ylab = "Diameter (m)", main = "Tree Diameter",
       ylim = range(df$diameter, df_2$diameter, df_3$diameter, 0.01*loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine diameter BA weighted mean"], prebas$multiOut[3,,12,,1][1:62]/100, na.rm = TRUE))
  lines(df_2$date, df_2$diameter, col = cols[2])
  lines(df_3$date, df_3$diameter, col = cols[3])
  points(as.Date(paste0(loaded_data$smearII_data$date[loaded_data$smearII_data$variable == "pine diameter BA weighted mean"], "-01-01")), 0.01*loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine diameter BA weighted mean"], col = "blue", pch = "x")
  points(as.Date(as.character(1960 + prebas$multiOut[3,,7,,1]), format = "%Y")[1:62], prebas$multiOut[3,,12,,1][1:62]/100, col = "green", pch = 17)
  legend("topleft", legend = c(N_labels, "ICOS Data", "Preles"), col = c(cols, "blue", "green"), lty = c(1,1,1,NA,NA), pch = c(NA,NA,NA,16,17), bty = "n")
  
  plot(df$date, df$root_mass, type = "l", col = cols[1],
       ylab = "Root Mass (kg C)", main = "Root Carbon Pool",
       ylim = range(df$root_mass, df_2$root_mass, df_3$root_mass, 0.4, na.rm = TRUE))
  lines(df_2$date, df_2$root_mass, col = cols[2])
  lines(df_3$date, df_3$root_mass, col = cols[3])
  abline(h = 0.4)
  
  plot(df$date, df$crown_area, type = "l", col = cols[1],
       ylab = "Crown Area (m²)", main = "Crown Area",
       ylim = range(df$crown_area, df_2$crown_area, df_3$crown_area, na.rm = TRUE))
  lines(df_2$date, df_2$crown_area, col = cols[2])
  lines(df_3$date, df_3$crown_area, col = cols[3])
  
  # ----------------------------
  # 2. Nitrogen Variables
  # ----------------------------
  date_point <- as.Date("2007-06-21")
  
  par(mfrow = c(2, 2))
  plot(df$date, df$tree_nitrogen, type = "l", col = cols[1],
       ylab = "kg N", main = "Free Tree Nitrogen", xlab = "Date",
       ylim = range(df$tree_nitrogen, df_2$tree_nitrogen, df_3$tree_nitrogen, na.rm = TRUE))
  title(sub = "This is \"free\" nitrogen not used nitrogen")
  lines(df_2$date, df_2$tree_nitrogen, col = cols[2])
  lines(df_3$date, df_3$tree_nitrogen, col = cols[3])
  legend("topleft", legend = c(N_labels), col = c(cols), lty = 1, bty = "n")
  
  plot(df$date, df$optimal_leaf_nitrogen, type = "p", col = cols[1], pch = ".",
       ylab = "Leaf N Dynamics (g g⁻¹)", main = "Optimal Leaf N", xlab = "Date",
       ylim = range(df$optimal_leaf_nitrogen, df$potential_leaf_nitrogen, df_2$optimal_leaf_nitrogen, df_3$optimal_leaf_nitrogen, na.rm = TRUE))
  points(df_2$date, df_2$optimal_leaf_nitrogen, col = cols[2], pch = ".")
  points(df_3$date, df_3$optimal_leaf_nitrogen, col = cols[3], pch = ".")
  points(df$date, df$potential_leaf_nitrogen, col = cols[1], pch = "-")
  points(df_2$date, df_2$potential_leaf_nitrogen, col = cols[2], pch = "-")
  points(df_3$date, df_3$potential_leaf_nitrogen, col = cols[3], pch = "-")
  abline(h = 12/1000, col = "blue", lty = 1)
  legend("topright", legend = c("Potential", "Optimal"), col = "black", pch = c("-", "."), bty = "n", title = "Leaf Nitrogen")
  title(sub = "Korhonen 2012: Socts Pine Needle N", col.sub = "blue")
  
  # Lower and upper values in g per tree per year
  low  <- 12 / 1010      # kg tree-1 year-1
  high <- 23.8 /2000    # kg tree-1 year-1
  # Plot your lines
  plot(df$date, df$nitrogen_uptake, type = "l", col = cols[1],
       ylab = "kg N per kg biomass", main = "Nitrogen Uptake", xlab = "Date",
       ylim = range(df$nitrogen_uptake, df_2$nitrogen_uptake, df_3$nitrogen_uptake, high, low, na.rm = TRUE))
  lines(df_2$date, df_2$nitrogen_uptake, col = cols[2])
  lines(df_3$date, df_3$nitrogen_uptake, col = cols[3])
  points(date_point, low,  pch = "x", col = "blue")
  points(date_point, high, pch = "x", col = "blue")
  # Optional: draw a vertical line between them to show the full range
  segments(date_point, low, date_point, high, col = "blue", lwd = 2)
  legend(
    "topleft",
    legend = c(N_labels, "Korhonen 2012 (range)"),
    col = c(cols, "blue"),
    pch = c(rep(NA, length(N_labels) + 1), "x"),   # use 16 for the range points
    lty = c(rep(1, length(N_labels) + 1), 1),     # keep a line for consistency
    bty = "n"
  )
  title(sub = "Korhonen 2012: kg tree-1 year-1", col.sub = "blue")

  low  <- 210 / 1010    # kg tree-1
  high <- 210 / 2000    # kg tree-1
  plot(df$date, df$nitrogen_in_biomass, type = "l", col = cols[1],
       ylab = "kg", main = "Nitorgen In Biomass", xlab = "Date",
       ylim = range(df$nitrogen_in_biomass, df_2$nitrogen_in_biomass, df_3$nitrogen_in_biomass, na.rm = TRUE))
  lines(df_2$date, df_2$nitrogen_in_biomass, col = cols[2])
  lines(df_3$date, df_3$nitrogen_in_biomass, col = cols[3])
  points(date_point, low,  pch = "x", col = "blue")
  points(date_point, high, pch = "x", col = "blue")
  # Optional: draw a vertical line between them to show the full range
  segments(date_point, low, date_point, high, col = "blue", lwd = 2)
  title(sub = "Korhonen 2012: Standing Biomass / Tree Number", col.sub = "blue")
  
  # ----------------------------
  # 3. Respiration
  # ----------------------------
  par(mfrow = c(2, 2))
  
  plot(df$date, df$rl, type = "l", col = cols[1], main = "Leaf Respiration", ylab = "kg/month")
  lines(df_2$date, df_2$rl, col = cols[2])
  lines(df_3$date, df_3$rl, col = cols[3])
  
  plot(df$date, df$rr, type = "l", col = cols[1], main = "Root Respiration", ylab = "kg/month",
       ylim = range(df$rr, df_2$rr, df_3$rr, 30.44*4/1000, na.rm = TRUE))
  lines(df_2$date, df_2$rr, col = cols[2])
  lines(df_3$date, df_3$rr, col = cols[3])
  abline(h = 30.44*4/1000, col = "blue")
  
  plot(df$date, df$rs, type = "l", col = cols[1], main = "Sapwood Respiration", ylab = "kg/month")
  lines(df_2$date, df_2$rs, col = cols[2])
  lines(df_3$date, df_3$rs, col = cols[3])
  
  plot(df$date, df$tl, type = "l", col = cols[1], main = "Leaf Turnover", ylab = "kg/month")
  lines(df_2$date, df_2$tl, col = cols[2])
  lines(df_3$date, df_3$tl, col = cols[3])
  
  # ----------------------------
  # 4. Biomass: Leaf, Root, Stem
  # ----------------------------
  par(mfrow = c(2, 2))
  
  plot(df$date, df$leaf_mass, type = "l", col = cols[1], main = "Leaf Mass", ylab = "kg",
       ylim = range(df$leaf_mass, df_2$leaf_mass, df_3$leaf_mass, loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine_foliage_biomass_ICOS"]/1000, na.rm = TRUE))
  lines(df_2$date, df_2$leaf_mass, col = cols[2])
  lines(df_3$date, df_3$leaf_mass, col = cols[3])
  points(as.Date(paste0(loaded_data$smearII_data$date[loaded_data$smearII_data$variable == "pine_foliage_biomass_ICOS"], "-01-01")), 
         loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine_foliage_biomass_ICOS"]/1000, col = "blue", pch = "x")
  
  plot(df$date, df$root_mass, type = "l", col = cols[1], main = "Root Mass", ylab = "kg",
       ylim = range(df$root_mass, df_2$root_mass, df_3$root_mass, na.rm = TRUE))
  lines(df_2$date, df_2$root_mass, col = cols[2])
  lines(df_3$date, df_3$root_mass, col = cols[3])
  
  plot(df$date, df$stem_mass, type = "l", col = cols[1], main = "Stem Mass", ylab = "kg",
       ylim = range(df$stem_mass, df_2$stem_mass, df_3$stem_mass, na.rm = TRUE))
  lines(df_2$date, df_2$stem_mass, col = cols[2])
  lines(df_3$date, df_3$stem_mass, col = cols[3])
  
  plot(df$date, df$coarse_root_mass, type = "l", col = cols[1], main = "Coarse Stem Mass", ylab = "kg",
       ylim = range(df$coarse_root_mass, df_2$coarse_root_mass, df_3$coarse_root_mass, na.rm = TRUE))
  lines(df_2$date, df_2$coarse_root_mass, col = cols[2])
  lines(df_3$date, df_3$coarse_root_mass, col = cols[3])
  
  # ----------------------------
  # 5. Additional variables: LAI, Crown, Lifespan, Mortality
  # ----------------------------
  par(mfrow = c(2, 2))
  
  plot(df$date, df$lai, type = "l", col = cols[1], main = "Leaf Area Index", ylab = "?",
       ylim = range(df$lai, df_2$lai, df_3$lai, loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "LAI_pine_ICOS"], na.rm = TRUE))
  lines(df_2$date, df_2$lai, col = cols[2])
  lines(df_3$date, df_3$lai, col = cols[3])
  points(as.Date(paste0(loaded_data$smearII_data$date[loaded_data$smearII_data$variable == "LAI_pine_ICOS"], "-01-01")), 
         loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "LAI_pine_ICOS"], col = "blue", pch = "x")
  abline(h = 4.0, col = "green")
  
  plot(df$date, df$leaf_lifespan, type = "l", col = cols[1], main = "Leaf Lifespan", ylab = "days")
  lines(df_2$date, df_2$leaf_lifespan, col = cols[2])
  lines(df_3$date, df_3$leaf_lifespan, col = cols[3])
  
  plot(df$date, df$fineroot_lifespan, type = "l", col = cols[1], main = "Fineroot Lifespan", ylab = "days")
  lines(df_2$date, df_2$fineroot_lifespan, col = cols[2])
  lines(df_3$date, df_3$fineroot_lifespan, col = cols[3])
  
  plot(df$date, df$mortality, type = "l", col = cols[1], main = "Mortality", ylab = "?")
  lines(df_2$date, df_2$mortality, col = cols[2])
  lines(df_3$date, df_3$mortality, col = cols[3])
  
  # ----------------------------
  # 6. Vcmax, Jmax, dpsi, Transpiration
  # ----------------------------
  par(mfrow = c(2, 2))
  
  plot(df$date, df$vcmax, type = "l", col = cols[1], main = "Vcmax", ylab = "µmol m⁻² s⁻¹",
       ylim = range(df$vcmax, df_2$vcmax, df_3$vcmax, na.rm = TRUE))
  lines(df_2$date, df_2$vcmax, col = cols[2])
  lines(df_3$date, df_3$vcmax, col = cols[3])
  
  plot(df$date, df$dpsi, type = "l", col = cols[1], main = "Δψ", ylab = "MPa",
       ylim = range(df$dpsi, df_2$dpsi, df_3$dpsi, na.rm = TRUE))
  lines(df_2$date, df_2$dpsi, col = cols[2])
  lines(df_3$date, df_3$dpsi, col = cols[3])
  
  plot(df$date, df$transpiration, type = "l", col = cols[1], main = "Transpiration", ylab = "kg H2O / month",
       ylim = range(df$transpiration, df_2$transpiration, df_3$transpiration, Eddy_covariance$ET/100, na.rm = TRUE))
  lines(df_2$date, df_2$transpiration, col = cols[2])
  lines(df_3$date, df_3$transpiration, col = cols[3])
  points(Eddy_covariance$MonthlyDate, Eddy_covariance$ET/100, col = "black", pch = 16)
  legend("topleft", legend = c(N_labels, "Eddy ET"), col = c(cols, "black"), lty = 1, bty = "n")
  
  plot(df$date, df$assim_net, type = "l", col = cols[1], main = "Net Assimilation", ylab = "kg C / month",
       ylim = range(df$assim_net, -Eddy_covariance$NEE, prebas$multiOut[3,,18,,1][1:62]/prebas$multiOut[3,,17,,1][1:62], na.rm = TRUE))
  lines(df_2$date, df_2$assim_net, col = cols[2])
  lines(df_3$date, df_3$assim_net, col = cols[3])
  points(Eddy_covariance$MonthlyDate, -Eddy_covariance$NEE, col = "black", pch = 16)
  points(as.Date(as.character(1960 + prebas$multiOut[3,,7,,1]), format = "%Y")[1:62], prebas$multiOut[3,,18,,1][1:62]/prebas$multiOut[3,,17,,1][1:62], col = "green", pch = 17)
  legend("topleft", legend = c(N_labels, "Eddy Covariance", "Preles"), col = c(cols, "black", "green"), lty = 1, pch = c(NA,NA,NA,16,17), bty = "n")
  
  # ----------------------------
  # 7. Total Production, Fitness, Mortality
  # ----------------------------
  par(mfrow = c(2, 2))
  
  plot(df$date, df$total_prod, type = "l", col = cols[1], main = "Total Production", ylab = "?",
       ylim = range(df$total_prod, df_2$total_prod, df_3$total_prod, na.rm = TRUE))
  lines(df_2$date, df_2$total_prod, col = cols[2])
  lines(df_3$date, df_3$total_prod, col = cols[3])
  
  plot(df$date, df$total_rep, type = "l", col = cols[1], main = "Total Reproduction", ylab = "?",
       ylim = range(df$total_rep, df_2$total_rep, df_3$total_rep, prebas$multiOut[3,,9,,1][1:62], na.rm = TRUE))
  lines(df_2$date, df_2$total_rep, col = cols[2])
  lines(df_3$date, df_3$total_rep, col = cols[3])
  points(as.Date(as.character(1960 + prebas$multiOut[3,,7,,1]), format = "%Y")[1:62], prebas$multiOut[3,,9,,1][1:62], col = "green", pch = 17)
  legend("topleft", legend = c(N_labels, "Preles"), col = c(cols, "green"), lty = 1, pch = c(NA,NA,NA,17), bty = "n")
  
  plot(df$date, df$fitness, type = "l", col = cols[1], main = "Fitness", ylab = "?",
       ylim = range(df$fitness, df_2$fitness, df_3$fitness, na.rm = TRUE))
  lines(df_2$date, df_2$fitness, col = cols[2])
  lines(df_3$date, df_3$fitness, col = cols[3])
  
  plot(df$date, df$mortality, type = "l", col = cols[1], main = "Mortality", ylab = "?",
       ylim = range(df$mortality, df_2$mortality, df_3$mortality, na.rm = TRUE))
  lines(df_2$date, df_2$mortality, col = cols[2])
  lines(df_3$date, df_3$mortality, col = cols[3])
  
  # ----------------------------
  # 8. Mortality breakdown (growth, hyd, d, inst)
  # ----------------------------
  par(mfrow = c(2, 2))
  
  plot(df$date, df$mortrate_growth, type = "l", col = cols[1], main = "Mortality Growth", ylab = "?",
       ylim = range(df$mortrate_growth, df_2$mortrate_growth, df_3$mortrate_growth, na.rm = TRUE))
  lines(df_2$date, df_2$mortrate_growth, col = cols[2])
  lines(df_3$date, df_3$mortrate_growth, col = cols[3])
  
  plot(df$date, df$mortrate_d, type = "l", col = cols[1], main = "Mortality D", ylab = "?",
       ylim = range(df$mortrate_d, df_2$mortrate_d, df_3$mortrate_d, na.rm = TRUE))
  lines(df_2$date, df_2$mortrate_d, col = cols[2])
  lines(df_3$date, df_3$mortrate_d, col = cols[3])
  
  plot(df$date, df$mortrate_hyd, type = "l", col = cols[1], main = "Mortality Hyd", ylab = "?",
       ylim = range(df$mortrate_hyd, df_2$mortrate_hyd, df_3$mortrate_hyd, na.rm = TRUE))
  lines(df_2$date, df_2$mortrate_hyd, col = cols[2])
  lines(df_3$date, df_3$mortrate_hyd, col = cols[3])
  
  plot(df$date, df$mortality_inst, type = "l", col = cols[1], main = "Mortality Inst", ylab = "?",
       ylim = range(df$mortality_inst, df_2$mortality_inst, df_3$mortality_inst, na.rm = TRUE))
  lines(df_2$date, df_2$mortality_inst, col = cols[2])
  lines(df_3$date, df_3$mortality_inst, col = cols[3])
  
  ###
  # Daylight hours
  ###
  
  day_length_hours <- function(lat_deg, day_of_year) {
    lat <- lat_deg * pi / 180
    N <- day_of_year
    
    # Day angle
    ## Transformation of the angle into a circular form for the trig equations
    gamma <- 2 * pi * (N - 1) / 365
    
    # Solar declination (radians)
    delta <- -asin(0.39779 * cos(0.98565 * pi/180 * (N + 10) +
                                   1.914 * pi/180 * sin(0.98565 * pi/180 * (N - 2))))
    
    # Zenith angle for sunrise/sunset (90.833° = includes refraction + solar radius)
    z0 <- 90.833 * pi / 180
    
    cos_h0 <- (cos(z0) - sin(lat) * sin(delta)) / (cos(lat) * cos(delta))
    
    # Initialize result
    D <- numeric(length(cos_h0))
    
    # Handle polar day/night
    D[cos_h0 <= -1] <- 24  # Sun never sets
    D[cos_h0 >=  1] <- 0   # Sun never rises
    
    mask <- (cos_h0 > -1) & (cos_h0 < 1)
    H0 <- acos(cos_h0[mask])
    D[mask] <- (24 / pi) * H0  # Convert radians to hours
    
    # Fractional Day length
    L <- D/24
    
    return(L)
  }
  
  plot(1:365, day_length_hours(60, 1:365), main = "Correction for day length", xlab = "Day of Year", ylab = "Hours of sunlight", ylim = c(0, 1), type = "l", col = "green")
  lines(1:365, day_length_hours(0, 1:365), main = "Correction for day length", xlab = "Day of Year", ylab = "Hours of sunlight", col = "blue")
  lines(1:365, day_length_hours(-60, 1:365), main = "Correction for day length", xlab = "Day of Year", ylab = "Hours of sunlight", col = "red")
  legend("top", legend = c(60, 0, -60), col = c("green", "blue", "red"), lty = 1, bty = "n", title = "Latitude", horiz = TRUE)
 
  ###
  # PHYDRO
  ###
  
  # ---- PARAMETERS ----
  kphio = 0.08 # 0.026263945805926
  kphio_amazon = 0.055
  rdark = 0.011
  vwind = 3
  a_jmax = 80
  par_cost = list(alpha=0.1008, gamma=0.180496537959982, nitrogen_store_conversion = 1)
  par_cost_2 = list(alpha=0.1008, gamma=0.180496537959982, nitrogen_store_conversion = 1.5)
  par_cost_3 = list(alpha=0.1008, gamma=0.180496537959982, nitrogen_store_conversion = 0.5)
  par_cost_amazon = list(alpha=0.1008, gamma=1.1875, nitrogen_store_conversion = 1)
  par_plant = list(conductivity=4.1311874912949e-17, psi50=-0.857817410110663, b=1)
  par_plant_amazon = list(conductivity=0.5e-16, psi50=-2.29, b=1)
  co2 = 400
  elv  = 181
  pa = rpmodel::calc_patm(elv)
  pa_amazon = 101325
  psi_soil = -0.01
  fapar = 0.7
  nitrogen = 1000000
  
  options = list(
    gs_method = "GS_IGF", 
    et_method = "ET_DIFFUSION",
    ftemp_vj_method = "FV_kumarathunge19",
    ftemp_rd_method = "FR_heskel16",
    ftemp_br_method = "FB_atkin15",
    scale_alpha = FALSE
  )
  
  # ---- STORAGE ----
  phydro_results           <- vector("list", nrow(input_climate))
  phydro_results_2         <- vector("list", nrow(input_climate))
  phydro_results_3         <- vector("list", nrow(input_climate))
  phydro_results_amazon    <- vector("list", nrow(import_amazon_data))
  phydro_results_analytical <- vector("list", nrow(input_climate))
  phydro_results_amazon_analytical <- vector("list", nrow(import_amazon_data))
  
  # ---- SAFE WRAPPER ----
  safe_try <- function(expr, month) {
    tryCatch(expr,
             error = function(e) {
               message("⚠️ Error in month ", month, ": ", e$message)
               return(NULL)
             })
  }
  
  # ---- MAIN LOOPS ----
  
  # Boreal (numerical + analytical)
  for (month in 1:nrow(input_climate)) {
    phydro_results[[month]]   <- safe_try(rphydro::rphydro_nitrogen(
      input_climate$Temp[month], input_climate$Temp[month],
      input_climate$PPFD[month], input_climate$PPFD_max[month],
      0.1 * input_climate$VPD[month],
      co2, pa, nitrogen, fapar, kphio, psi_soil,
      rdark, vwind, a_jmax, par_plant, par_cost, options), month)
    
    phydro_results_2[[month]] <- safe_try(rphydro::rphydro_nitrogen(
      input_climate$Temp[month], input_climate$Temp[month],
      input_climate$PPFD[month], input_climate$PPFD_max[month],
      0.1 * input_climate$VPD[month],
      co2, pa, nitrogen, fapar, kphio, psi_soil,
      rdark, vwind, a_jmax, par_plant, par_cost_2, options), month)
    
    phydro_results_3[[month]] <- safe_try(rphydro::rphydro_nitrogen(
      input_climate$Temp[month], input_climate$Temp[month],
      input_climate$PPFD[month], input_climate$PPFD_max[month],
      0.1 * input_climate$VPD[month],
      co2, pa, nitrogen, fapar, kphio, psi_soil,
      rdark, vwind, a_jmax, par_plant, par_cost_3, options), month)
    
    # ---- Analytical Boreal ----
    phydro_results_analytical[[month]] <- safe_try(
      rphydro::rphydro_numerical(
        input_climate$Temp[month], input_climate$Temp[month],
        input_climate$PPFD[month], input_climate$PPFD_max[month],
        0.1 * input_climate$VPD[month],
        co2, pa, fapar, kphio, psi_soil,
        rdark, vwind, par_plant, par_cost, options), month)
  }
  
  # ---- AMAZON (numerical + analytical) ----
  for (month in 1:nrow(import_amazon_data)) {
    phydro_results_amazon[[month]] <- safe_try(rphydro::rphydro_nitrogen(
      import_amazon_data$Temp[month], import_amazon_data$Temp[month],
      import_amazon_data$PAR[month], import_amazon_data$PAR_max[month],
      0.1 * import_amazon_data$VPD[month],
      co2, pa_amazon, nitrogen, fapar, kphio_amazon, psi_soil,
      rdark, vwind, a_jmax, par_plant_amazon, par_cost_amazon, options), month)
    
    # ---- Analytical Amazon ----
    phydro_results_amazon_analytical[[month]] <- safe_try(
      rphydro::rphydro_numerical(
        import_amazon_data$Temp[month], import_amazon_data$Temp[month],
        import_amazon_data$PAR[month], import_amazon_data$PAR_max[month],
        0.1 * import_amazon_data$VPD[month],
        co2, pa_amazon, fapar, kphio_amazon, psi_soil,
        rdark, vwind, par_plant_amazon, par_cost_amazon, options), month)
  }
  
  # ---- EXTRACT RESULTS ----
  extract_safe <- function(x, var) if (is.null(x) || is.null(x[[var]])) NA else x[[var]]
  
  decimal_to_date <- function(decimal_year) {
    year <- floor(decimal_year)
    rem  <- decimal_year - year
    # approximate number of days in year
    days_in_year <- ifelse((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0), 366, 365)
    as.Date(paste0(year, "-01-01")) + round(rem * days_in_year)
  }
  
  make_df <- function(results, version, amazon) {
    data.frame(
      Month = 1:length(results),
      Dates = if (amazon) decimal_to_date(import_amazon_data$Decimal_year) else decimal_to_date(input_climate$Decimal_year),
      a      = sapply(results, extract_safe, "a"),
      jmax   = sapply(results, extract_safe, "jmax"),
      vcmax  = sapply(results, extract_safe, "vcmax"),
      dpsi   = sapply(results, extract_safe, "dpsi"),
      n_leaf = sapply(results, extract_safe, "n_leaf"),
      Version = version
    )
  }
  
  df1  <- make_df(phydro_results, "1", FALSE)
  df2  <- make_df(phydro_results_2, "1.5", FALSE)
  df3  <- make_df(phydro_results_3, "0.5", FALSE)
  df4  <- make_df(phydro_results_amazon, "Amazon", TRUE)
  df5  <- make_df(phydro_results_analytical, "Numerical", FALSE)
  df6  <- make_df(phydro_results_amazon_analytical, "Amazon_Numerical", TRUE)
  
  # ---- CONVERT TO KG C ----
  conversion_factor <- 12.11 * 1e-9 * 24 * 60 * 60 * 365.25
  
  df1$a_scaled <- df1$a * conversion_factor
  df2$a_scaled <- df2$a * conversion_factor
  df3$a_scaled <- df3$a * conversion_factor
  df4$a_scaled <- df4$a * conversion_factor
  df5$a_scaled <- df5$a * conversion_factor
  df6$a_scaled <- df6$a * conversion_factor
  
  df_combined <- rbind(df1, df2, df3, df4, df5, df6)
  
  # ---- VALIDATION DATA ----
  GPP_out_df <- data.frame(
    Month = 1:length(GPP_out$GPP_mean),
    Dates = GPP_out$MonthlyDate,
    GPP_mean = GPP_out$GPP_mean,
    GPP_mean_kg = GPP_out$GPP_mean_kg
  )
  
  df_long <- df_combined %>%
    tidyr::pivot_longer(
      cols = c(a, jmax, vcmax, a_scaled, dpsi, n_leaf),
      names_to = "Variable",
      values_to = "Value"
    )
  # For model outputs
  df_long <- df_long %>%
    mutate(Dates = as.Date(format(Dates, "%Y-%m-01")))
  
  facet_labels <- c(
    a = "a (µmol m⁻² s⁻¹)",
    a_scaled = "a (kg C year⁻¹)",
    jmax = "Jmax (µmol m⁻² s⁻¹)",
    vcmax = "Vcmax (µmol m⁻² s⁻¹)",
    dpsi = "Δψ (MPa)",
    n_leaf = "Leaf N (g g⁻¹)"
  )
  
  GPP_long <- GPP_out_df %>%
    pivot_longer(
      cols = c(GPP_mean, GPP_mean_kg),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    mutate(
      Variable = dplyr::recode(
        Variable,
        "GPP_mean"    = "a",
        "GPP_mean_kg" = "a_scaled"
      )
    )
  
  # ---- PLOT ----
  ggplot(df_long, aes(x = Month, y = Value, color = Version, group = Version)) +
    geom_line() +
    geom_point() +
    geom_point(data = GPP_long,
               aes(x = Month, y = Value),
               color = "black",
               size = 2,
               shape = 16,
               inherit.aes = FALSE) +
    facet_wrap(~ Variable, scales = "free_y", ncol = 2,
               labeller = as_labeller(facet_labels)) +
    labs(
      title = "Comparison of Model Outputs vs Validation GPP",
      x = "Iteration (Month)",
      y = "Value",
      color = "Version",
      subtitle = "Black dots = validation GPP data"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  start_date <- max(min(df_long$Dates), min(GPP_long$Dates))
  end_date   <- min(max(df_long$Dates), max(GPP_long$Dates))
  
  df_long_sub <- df_long %>%
    filter(Dates >= start_date & Dates <= end_date)

  # Join modeled + GPP validation by Month and Variable
  df_scatter <- df_long_sub %>%
    filter(Variable == "a", Version %in% c("1","1.5","0.5")) %>%
    left_join(
      GPP_long %>% filter(Variable == "a", !is.na(Value)),
      by = c("Dates","Variable"),
      suffix = c("_model","_gpp")
    ) %>%
    rename(Modeled = Value_model, GPP = Value_gpp)
  
  # ---- NEW SCATTER PLOT ----
  ggplot(df_scatter, aes(x = GPP, y = Modeled, color = Version)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    facet_wrap(~ Variable, scales = "free",
               labeller = as_labeller(facet_labels)) +
    labs(
      title = "Modeled vs Observed GPP (Boreal Only, Nitrogen-Cost Variants)",
      x = "Observed GPP",
      y = "Modeled GPP",
      color = "Version"
    ) +
    theme_minimal()
  
                               
}

Testing_the_architecture <- function() {
  ###
  # Geometry as in the model
  ### 
  
  geom_from_cpp <- function(a, c, m, n, fg, H_m,
                            D_seq = seq(0.01, 0.5, length.out = 100),
                            z_len = 100) {
    
    ### --- C++ GEOMETRY TRANSLATION --- ###
    pic_4a <- pi * c / (4 * a)
    
    zm_H <- ((n - 1) / (m * n - 1))^(1/n)
    
    qm_cpp <- m * n *
      ((n - 1) / (m * n - 1))^(1 - 1/n) *
      (( (m - 1) * n / (m * n - 1) )^(m - 1))
    
    eta_c <- zm_H -
      (m*m*n)/(qm_cpp*qm_cpp) *
      beta(2 - 1/n, 2*m - 1) *
      (pbeta((n - 1)/(m*n - 1), 2 - 1/n, 2*m - 1) - (1 - fg))
    
    ### --- HEIGHT MODEL (matching C++ version) --- ###
    height_fun <- function(D) {
      H_m * (1 - exp(-a * D / H_m))
    }
    height_output <- height_fun(D_seq)
    
    ### --- CROWN AREA MODEL --- ###
    crown_area_fun <- function(D) {
      (pi * c)/(4 * a) * D * height_fun(D)
    }
    A_c_output <- crown_area_fun(D_seq)
    
    ### --- CROWN RADIUS MODEL (normalized by tree height) --- ###
    crown_radius_fun <- function(z, A_c, q_m, H_i) {
      (1/q_m) * sqrt(A_c/pi) * m * n *
        (z/H_i)^(n-1) * (1 - (z/H_i)^n)^(m-1)
    }
    
    ### --- Common z sequence for all trees (for plotting) --- ###
    z_seq_common <- seq(0, max(height_output), length.out = z_len)
    
    ### --- CROWN RADIUS PROFILES (all columns same length) --- ###
    crown_radius_outputs <- sapply(1:length(A_c_output), function(i) {
      H_i <- height_output[i]
      # scale z to actual tree height, capped at H_i
      z_scaled <- pmin(z_seq_common, H_i)
      crown_radius_fun(z_scaled, A_c_output[i], qm_cpp, H_i)
    })
    
    colnames(crown_radius_outputs) <- paste0("D=", round(D_seq, 3))
    rownames(crown_radius_outputs) <- paste0("z=", round(z_seq_common, 2))
    
    ### Return everything
    list(
      cpp_geom = list(
        pic_4a = pic_4a,
        zm_H = zm_H,
        qm_cpp = qm_cpp,
        eta_c = eta_c
      ),
      input_sequences = list(
        D_seq = D_seq,
        z_seq = z_seq_common
      ),
      height = height_output,
      crown_area = A_c_output,
      crown_radius = crown_radius_outputs
    )
  }
  
  
  #######
  ### Base values
  #######
  
  a = 150
  c = 450
  m = 10
  n = 4.25
  H_m = 30
  
  out <- geom_from_cpp(a, c, m, n, fg = 0.15, H_m)
  
  layout(matrix(c(1,3,
                  2,3), nrow = 2, byrow = TRUE), widths = c(1,1.2))
  
  par(mar = c(4,4,2,1))  # adjust margins
  
  ### ============================================================
  ### (1) Crown area vs diameter (top-left)
  ### ============================================================
  diameter_from_height <- function(H, H_m, a) {
    - (H_m / a) * log(1 - H / H_m)
  }
  
  plot(out$input_sequences$D_seq, out$crown_area + out$input_sequences$D_seq,
       xlab = "Diameter, m", ylab = "Crown Area, A_c, m²",
       main = "Projected Crown Area from C++ Model", type = "l")
  for (i in seq_len(nrow(halme_et_al_2022))) {
    h <- halme_et_al_2022$height[i]
    x <- diameter_from_height(halme_et_al_2022$height[i], H_m, a)
    A_c <- (halme_et_al_2022$crown_diameter[i] / 2)^2 * pi
    
    # Points
    points(x, A_c, pch = "x", col = "blue")
  }
  
  legend("bottomright",
         legend = "Halme 2022 (max crown width transformed)",
         col = "blue", pch = "x", bty = "n")
  
  
  ### ============================================================
  ### (2) Height vs diameter (bottom-left)
  ### ============================================================
  plot(out$input_sequences$D_seq, out$height,
       xlab = "Diameter, m", ylab = "Height, H, m",
       main = "Height from C++ Model", type = "l")
  
  points(
    0.01 * loaded_data$smearII_data$amount[
      loaded_data$smearII_data$variable == "pine diameter BA weighted mean"
    ],
    loaded_data$smearII_data$amount[
      loaded_data$smearII_data$variable == "pine height BA weighted mean"
    ],
    pch = "x", col = "blue"
  )
  
  legend("topleft",
         legend = c("Hyytiälä Data Set"),
         col = "blue", pch = "x", bty = "n")
  
  ### ============================================================
  ### (3) Radius(z) for two diameters (right, spanning both rows)
  ### ============================================================
  Ds <- seq(0.05, 0.5, by = 0.05)
  cols <- rainbow(length(Ds))  # colors for model diameters
  obs_col <- "lightgrey"       # light grey for Halme crown boxes
  
  # Determine x-range for symmetric profiles
  max_r <- max(out$crown_radius)  # crown radius in meters
  x_range <- c(-max_r, max_r)
  y_range <- c(0, max(out$input_sequences$z_seq))  # height in meters
  
  # Initialize plot with same scale on both axes
  plot(NULL,
       xlim = x_range,
       ylim = y_range,
       xlab = "Crown Radius (m)",
       ylab = "Height (m)",
       main = "Symmetric Crown Profiles for Multiple Diameters",
       asp = 1)  # ensures 1:1 aspect ratio
  
  # Draw symmetric crown curves for model diameters
  for (k in seq_along(Ds)) {
    j <- which.min(abs(out$input_sequences$D_seq - Ds[k]))
    
    r_prof <- out$crown_radius[, j]        # model radius (tapers to zero)
    z_vals <- out$input_sequences$z_seq
    
    # Stem radius
    stem_radius <- Ds[k] / 2
    
    # Apply stem radius only until crown exceeds it
    r_prof_adjusted <- r_prof
    exceed_index <- which(r_prof_adjusted >= stem_radius)[1]  # first index crown > stem
    if (!is.na(exceed_index)) {
      r_prof_adjusted[1:(exceed_index-1)] <- stem_radius
    } else {
      # If crown never exceeds stem radius, keep stem_radius
      r_prof_adjusted[] <- stem_radius
    }
    
    # Right and left sides
    lines(r_prof_adjusted, z_vals, col = cols[k], lty = 2, lwd = 2)
    lines(-r_prof_adjusted, z_vals, col = cols[k], lty = 2, lwd = 2)
  }
  
  # Draw Halme 2022 crowns as light grey boxes
  for (i in seq_len(nrow(halme_et_al_2022))) {
    y <- halme_et_al_2022$height[i]
    r_max <- halme_et_al_2022$crown_diameter[i] / 2
    
    # horizontal line at top of crown
    lines(c(-r_max, r_max), c(y, y), col = obs_col, lwd = 2)
    
    # vertical lines from crown top to x-axis
    lines(c(-r_max, -r_max), c(0, y), col = obs_col, lwd = 2)
    lines(c(r_max, r_max), c(0, y), col = obs_col, lwd = 2)
  }
  
  # Legend for model diameters
  legend("topleft",
         legend = paste("D =", Ds),
         col = cols, lty = 2, lwd = 2, bty = "n")
  
  # Legend for Halme crowns
  legend("bottomright",
         legend = "Halme 2022 (max crown width)",
         col = obs_col, lwd = 2, bty = "n")
  
  
  
  # ----------------------------------------------------------
  # Wrapper: run geom_from_cpp() for many m and n combinations
  # ----------------------------------------------------------
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  
  scan_mn <- function(a, c, H_m, fg = 0.15,
                      m_vals = seq(1.0, 3.0, length.out = 5),
                      n_vals = seq(1.05, 2.0, length.out = 5),
                      D_seq = seq(0.4, 0.4, length.out = 30),
                      z_len = 60) {
    
    grid <- expand.grid(m = m_vals, n = n_vals)
    
    results <- pmap(grid, function(m, n) {
      out <- geom_from_cpp(a=a, c=c, m=m, n=n, fg=fg,
                           H_m=H_m, D_seq=D_seq, z_len=z_len)
      
      # Convert radius profile to long tidy format
      df <- as.data.frame(out$crown_radius)
      df$z <- out$input_sequences$z_seq
      
      df_long <- df %>%
        pivot_longer(cols = -z,
                     names_to = "D",
                     values_to = "radius") %>%
        mutate(D = as.numeric(sub("D=", "", D)),
               m = m, n = n)
    })
    
    bind_rows(results)
  }
  
  # ----------------------------------------------------------
  # Example: run through many m and n
  # ----------------------------------------------------------
  mn_data <- scan_mn(
    a = a,
    c = c,
    H_m = H_m,
    fg = 0.15,
    m_vals = seq(2.75, 10, length.out = 6),
    n_vals = seq(2.75, 10, length.out = 6)
  )
  
  # ----------------------------------------------------------
  # Visualization: Faceted crown profiles for each (m, n)
  # ----------------------------------------------------------
  
  ggplot(mn_data, aes(x = radius, y = z, group = D, color = D)) +
    geom_point(alpha = 0.6) +
    facet_grid(m ~ n, labeller = label_both) +
    theme_bw() +
    labs(
      title = "Crown Radius Profiles Across m–n Parameter Space",
      x = "Crown Radius",
      y = "Height (z)",
      color = "Diameter (D)"
    )
    
}

Getting_assimilation_to_the_right_levels <- function() {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(lubridate)  # for date rounding if needed
  
  lho <- new(LifeHistoryOptimizer, "tests/params/p_test_boreal.ini")
  lho$set_i_metFile("tests/data/ERAS_Monthly.csv")
  lho$set_a_metFile("tests/data/ERAS_Monthly.csv")
  lho$set_co2File("")
  lho$set_soil_nitrogen(0.9)
  lho$init()
  
  dt <- 1/12
  col_names <- lho$get_header()
  df <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  names(df) <- col_names
  
  start_year <- 1960
  end_year <- 1970
  
  # Parameter grids
  kphio_values <- seq(0.0001, 0.01, length = 5)   # smaller grid for illustration
  alpha_values <- seq(0.0001, 0.01, length = 5)
  gamma_values <- seq(0.0001, 0.01, length = 5)
  a_jmax_values <- seq(50, 110, length = 5)
  
  time_seq <- seq(start_year, end_year, dt)
  
  # Pre-allocate list (approximation)
  results <- list()
  i <- 1
  
  # Loop over all combinations of the four parameters
  for (k in kphio_values) {
    for (a in alpha_values) {
      for (g in gamma_values) {
        for (jmax in a_jmax_values) {
          
          # Set parameters
          lho$par0$kphio <- k
          lho$par0$alpha <- a
          lho$par0$gamma <- g
          lho$par0$a_jmax <- jmax
          
          for (t in time_seq) {
            results[[i]] <- tryCatch({
              lho$grow_for_dt(t, dt)
              state <- lho$get_state(t + dt)
              df_row <- as.data.frame(t(state), stringsAsFactors = FALSE)
              names(df_row) <- col_names
              # Add parameter values to the row
              df_row$kphio <- k
              df_row$alpha <- a
              df_row$gamma <- g
              df_row$a_jmax <- jmax
              df_row$date <- as.Date(as.yearmon(t))
              df_row
            }, error = function(e) {
              message("Error at t = ", t, ": ", conditionMessage(e))
              na_row <- as.data.frame(as.list(rep(NA, length(col_names))), stringsAsFactors = FALSE)
              names(na_row) <- col_names
              na_row$kphio <- k
              na_row$alpha <- a
              na_row$gamma <- g
              na_row$a_jmax <- jmax
              na_row$date <- as.Date(as.yearmon(t))
              na_row
            })
            i <- i + 1
          }
        }
      }
    }
  }
  
  df <- do.call(rbind, results)
  
  # Select only variables you want to plot
  variables_to_plot <- c("assim_gross", "assim_net")
  df_long <- df %>%
    select(date, kphio, alpha, gamma, a_jmax, all_of(variables_to_plot)) %>%
    pivot_longer(cols = all_of(variables_to_plot), names_to = "variable", values_to = "value")
  
  # Ensure date is a Date object (it might already be)
  df_long$date <- as.Date(df_long$date)
  
  # Aggregate by day and parameter combination
  df_daily <- df_long %>%
    group_by(date, kphio, alpha, gamma, a_jmax, variable) %>%
    summarise(value = max(value, na.rm = TRUE), .groups = "drop")
  
  ggplot(df_daily, aes(x = date, y = value, color = factor(kphio))) +
    geom_line() +
    facet_wrap(~variable, scales = "free_y") +
    labs(
      title = "Daily-aggregated time series of variables",
      x = "Date",
      y = "Value",
      color = "kphio"
    ) +
    theme_minimal()
  
  # Filter only the variable of interest
  df_agg <- df_daily %>%
    filter(variable == "assim_gross")
  
  # Example: x = date, y = assim_gross
  # color = kphio, linetype = alpha, facet by gamma and a_jmax
  ggplot(df_agg, aes(x = date, y = value, color = factor(kphio), linetype = factor(alpha))) +
    geom_line() +
    facet_grid(gamma ~ a_jmax) +  # rows = gamma, cols = a_jmax
    labs(
      title = "Assim Gross over Time for Different Parameters",
      x = "Date",
      y = "assim_gross",
      color = "kphio",
      linetype = "alpha",
      subtitle = "Facets: rows = gamma, columns = a_jmax"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ####
  
  # Aggregate by date and parameter combination
  df_daily <- df_long %>%
    group_by(date, kphio, alpha, gamma, a_jmax, variable) %>%
    summarise(
      value = if(all(is.na(value))) NA else max(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Filter only the variable of interest
  df_agg <- df_daily %>%
    filter(variable == "assim_gross") %>%
    mutate(
      kphio_f = factor(kphio),
      alpha_f = factor(alpha),
      gamma_f = factor(gamma),
      a_jmax_f = factor(a_jmax)
    )
  
  # Plot
  ggplot(df_agg, aes(x = date, y = value, color = kphio_f, linetype = alpha_f)) +
    geom_line() +
    facet_grid(gamma_f ~ a_jmax_f) +
    labs(
      title = "Assim Gross over Time for Different Parameters",
      x = "Date",
      y = "assim_gross",
      color = "kphio",
      linetype = "alpha",
      subtitle = "Facets: rows = gamma, columns = a_jmax"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  ###
  # Old plots
  ###
  
  #######
  ### PHOTOSYNTHESIS
  #######
  par(mfrow = c(2, 2))
  
  #--------------------------------------
  # 1. Compute month from dates
  #--------------------------------------
  df <- df %>%
    mutate(Month = month(date))   # month 1–12 from model date
  
  input_climate <- input_climate %>%
    mutate(
      Date = as.Date(paste(Year, Month, "01", sep = "-")),  # construct full date
      Month = as.integer(Month)
    )
  
  #--------------------------------------
  # 2. Summarise monthly PPFD statistics
  #--------------------------------------
  monthly_ppfd_model <- df %>%
    group_by(Month) %>%
    summarise(
      mean_ppfd = mean(ppfd, na.rm = TRUE),
      min_ppfd  = min(ppfd, na.rm = TRUE),
      max_ppfd  = max(ppfd, na.rm = TRUE)
    )
  
  monthly_ppfd_input <- input_climate %>%
    group_by(Month) %>%
    summarise(
      mean_ppfd = mean(PPFD, na.rm = TRUE),
      min_ppfd  = min(PPFD, na.rm = TRUE),
      max_ppfd  = max(PPFD, na.rm = TRUE)
    )
  
  #--------------------------------------
  # 3. Set y-axis limits across both datasets
  #--------------------------------------
  ylim <- range(c(
    monthly_ppfd_model$min_ppfd, monthly_ppfd_model$max_ppfd,
    monthly_ppfd_input$min_ppfd, monthly_ppfd_input$max_ppfd
  ), na.rm = TRUE)
  
  #--------------------------------------
  # 4. Base plot for model (grey shaded range)
  #--------------------------------------
  plot(monthly_ppfd_model$Month, monthly_ppfd_model$mean_ppfd, type = "l",
       ylim = ylim, xlab = "Month", ylab = expression("PPFD ("*mu*"mol m"^-2*" s"^-1*")"),
       main = "Monthly Mean and Range of PPFD")
  
  polygon(
    c(monthly_ppfd_model$Month, rev(monthly_ppfd_model$Month)),
    c(monthly_ppfd_model$min_ppfd, rev(monthly_ppfd_model$max_ppfd)),
    col = "grey80", border = NA
  )
  lines(monthly_ppfd_model$Month, monthly_ppfd_model$mean_ppfd, lwd = 2, col = "black")
  
  #--------------------------------------
  # 5. Overlay input_climate data (blue shaded area + line)
  #--------------------------------------
  polygon(
    c(monthly_ppfd_input$Month, rev(monthly_ppfd_input$Month)),
    c(monthly_ppfd_input$min_ppfd, rev(monthly_ppfd_input$max_ppfd)),
    col = rgb(0, 0, 1, 0.2), border = NA
  )
  lines(monthly_ppfd_input$Month, monthly_ppfd_input$mean_ppfd, col = "blue", lwd = 2)
  
  #--------------------------------------
  # 6. Add axis and legend
  #--------------------------------------
  axis(1, at = 1:12, labels = month.abb)
  legend("topright", legend = c("Model PPFD", "Input Climate PPFD"),
         col = c("black", "blue"), lty = 1, lwd = 2, bty = "n")
  
  #--------------------------------------
  # 7. Add caption
  #--------------------------------------
  mtext("Instantaneous climate data (blue) compared with model output (black)",
        side = 3, line = 0.5, cex = 0.8)
  
  
  ylim = range(df$ppfd, input_climate$PPFD, na.rm = T)
  plot(as.Date(paste(input_climate$Year, input_climate$Month, 01), format = "%Y%m%d"), input_climate$PPFD, col = "green", main = "PPFD", xlab = "Date", ylab = "umol m-2 s-1")
  mtext("Instantenous Climate is outputted. Green: weather data in", 
        side = 3, line = 0.5, cex = 0.8)
  points(df$date, df$ppfd)
  
}

