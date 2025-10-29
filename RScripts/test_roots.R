blank <- function() {
  library(dplyr)
  library(lubridate)  # convenient for date operations
  library(ggplot2)
  library(zoo)
  library(data.table)
  library(rphydro)
  library(tidyverse)
  library(Rprebasso)
  
  ###
  # PREBAS Comparison
  ###

  prebas <- TransectRun(defaultThin = 0, ClCut = 0)
  
  # Rprebasso::varNames
  
  ###
  # Simulation!
  ###
  
  lho <- new(LifeHistoryOptimizer, "tests/params/p_test_boreal.ini")
  lho$set_i_metFile("tests/data/ERAS_Monthly.csv")
  lho$set_a_metFile("tests/data/ERAS_Monthly.csv")
  lho$set_co2File("")
  # lho$set_soil_nitrogen(0.5)
  lho$init()
  
  dt <- 1/12
  
  df <- data.frame(matrix(ncol = length(lho$get_header()), nrow = 0))
  col_names <- lho$get_header()
  
  start_year <- 1960
  end_year <- 2022

  results <- vector("list", length(seq(start_year, end_year, dt)))
  i <- 1
  
  # end_year
  for (t in seq(start_year, end_year, dt)) {
    results[[i]] <- tryCatch({
      lho$grow_for_dt(t, dt)
      state <- lho$get_state(t + dt)
      df_row <- as.data.frame(t(state), stringsAsFactors = FALSE)
      names(df_row) <- col_names   # force the names here
      df_row
    },
    error = function(e) {
      message("Error at t = ", t, ": ", conditionMessage(e))
      na_row <- as.data.frame(as.list(rep(NA, length(col_names))), stringsAsFactors = FALSE)
      names(na_row) <- col_names
      na_row
    })
    i <- i + 1
  }
  
  df <- do.call(rbind, results)
  names(df) <- col_names
  
  df$date <- seq(
    as.Date(paste0(start_year, "-01-01")),
    as.Date(paste0(end_year, "-01-01")),
    by = "month"
  )[1:nrow(df)]
  
  par(mfrow = c(1, 2))
  plot(df$assim_gross)
  plot(df$diameter)
  
  ###
  # Validation data
  ###
  
  data_directory <- "~/Documents/CASSIA_Calibration/Processed_Data/"
  loaded_data <- CASSIA::load_data(data_directory)

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
  
  GPP_out$GPP_mean = GPP_out$GPP_mean * 12 * 1e-9 * 30.44 * 24 * 60 * 60 * 10000/1000
  NEE_out$NEE_mean = NEE_out$NEE_mean * 12 * 1e-9 * 30.44 * 24 * 60 * 60 * 10000/1000
  F_CO2_leaf_out$F_CO2_leaf_mean = F_CO2_leaf_out$F_CO2_leaf_mean * 12 * 1e-9 * 30.44 * 24 * 60 * 60 * 10000/1000
  F_H2O_leaf_out$F_H2O_leaf_mean = F_H2O_leaf_out$F_H2O_leaf_mean * 18 * 1e-6 * 30.44 * 24 * 60 * 60 * 10000/1000
  ET_out$ET_mean = ET_out$ET_mean * 18 * 1e-6 * 30.44 * 24 * 60 * 60 * 10000/1000
  
  GPP_out$NEE <- NEE_out$NEE_mean
  GPP_out <- merge(GPP_out, F_CO2_leaf_out[, c("Monthly", "F_CO2_leaf_mean")], 
                    by = "Monthly", all.x = TRUE)
  GPP_out <- merge(GPP_out, F_H2O_leaf_out[, c("Monthly", "F_H2O_leaf_mean")], 
                   by = "Monthly", all.x = TRUE)
  GPP_out$ET <- ET_out$ET_mean
  Eddy_covariance <- GPP_out
  Eddy_covariance[, MonthlyDate := as.Date(paste0("01 ", Monthly), format = "%d %b %Y")]
  Eddy_covariance[, Month := as.integer(format(MonthlyDate, "%m"))]
  
  ###
  # Plots to get parameters
  ###
  
  ### Checking the shape of the canopy
  crown_radius <- function(z, q_m, A_c, m, n, H_m) {
    r = (1/q_m) * (A_c/pi)^(1/2) * m * n * (z/H_m)^(n-1) * (1 - (z/H_m)^n)^(m-1)
    return(r)
  }
  
  height <- function(D, a, H_m) {
    H = H_m * (1 - exp(-a * D/H_m))
    return(H)
  }
  
  crown_area <- function(D, a, H_m, c) {
    A_c = (pi * c)/(4 * a) * D * height(D, a, H_m)
  }

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
  
  # prebas$multiOut[3,,18,,1][1:62]/12/prebas$multiOut[3,,17,,1][1:62]
  ylim = range(df$assim_net, -Eddy_covariance$NEE, na.rm = T)
  plot(df$date, df$assim_net, main = "Net Assimilation", xlab = "Date", ylab = "kg C per month", ylim = ylim)
  mtext("Eddy (kg C per month)", 
        side = 3, line = 0.5, cex = 0.7, col = "blue")
  points(Eddy_covariance$MonthlyDate, -Eddy_covariance$NEE, col = "blue")
  points(as.Date(as.character(1960 + prebas$multiOut[3,,7,,1]), format = "%Y")[1:62], prebas$multiOut[3,,18,,1][1:62]/prebas$multiOut[3,,17,,1][1:62], col = "red")
  mtext("Preles (kg C yearly value per tree)", side = 1, line = 2, cex = 0.7, col = "red")
  
  # PRELES, units: g C m-2 to kg per month per canopy of one tree (assumed ground as canopy model)
  # Eddy_covariance_monthly$GPP_mean / 1000,
  # 1000 as a divider to make per tree and then 1000 from g C to kg C
  ylim = range(df$assim_gross, Eddy_covariance$GPP_mean, prebas$multiOut[3,,10,,1][1:62], na.rm = T)
  # TODO: is this net or gross assimilation?
  # TODO: although there isn't any nitrogen uptake limitation could this be changing something somehow?
  plot(df$date, df$assim_gross, main = "Gross Assimilation", xlab = "Date", ylab = "kg C per month", ylim = ylim)
  points(Eddy_covariance$MonthlyDate, Eddy_covariance$GPP_mean, col = "blue")
  points(as.Date(as.character(1960 + prebas$multiOut[3,,7,,1]), format = "%Y")[1:62], prebas$multiOut[3,,10,,1][1:62], col = "red")
  mtext("Eddy (kg C per month)", side = 3, line = 0.5, cex = 0.7, col = "blue")
  mtext("Preles (kg C yearly value per tree)", side = 1, line = 2, cex = 0.7, col = "red")
  
  df <- df %>%
    mutate(Month = month(date))  # month() returns 1–12
  
  monthly_stats <- df %>%
    group_by(Month) %>%
    summarise(
      mean_assim = mean(assim_gross, na.rm = TRUE),
      min_assim = min(assim_gross, na.rm = TRUE),
      max_assim = max(assim_gross, na.rm = TRUE)
    )
  
  Eddy_covariance_monthly_stats <- Eddy_covariance %>%
    group_by(Month) %>%
    summarise(
      mean_assim = mean(GPP_mean, na.rm = TRUE),
      min_assim = min(GPP_mean, na.rm = TRUE),
      max_assim = max(GPP_mean, na.rm = TRUE)
    )
  
  # Set y-axis limits to include both your model data and Eddy data
  ylim <- range(c(monthly_stats$min_assim, monthly_stats$max_assim
                  #Eddy_covariance_monthly_stats$min_assim, 
                  #Eddy_covariance_monthly_stats$max_assim
                  ),
                na.rm = TRUE)
  # Base plot: model monthly mean and range
  plot(monthly_stats$Month, monthly_stats$mean_assim, type = "l",
       ylim = ylim, xlab = "Month", ylab = "Assimilation (kg C per month)",
       main = "Monthly Gross Assimilation with Range")
  polygon(c(monthly_stats$Month, rev(monthly_stats$Month)),
          c(monthly_stats$min_assim, rev(monthly_stats$max_assim)),
          col = "grey", border = NA)
  lines(monthly_stats$Month, monthly_stats$mean_assim, lwd = 2)
  # Overlay Eddy covariance monthly stats (shaded area + mean line)
  polygon(c(Eddy_covariance_monthly_stats$Month, rev(Eddy_covariance_monthly_stats$Month)),
          c(Eddy_covariance_monthly_stats$min_assim, rev(Eddy_covariance_monthly_stats$max_assim)),
          col = rgb(0,0,1,0.2), border = NA)
  lines(Eddy_covariance_monthly_stats$Month, Eddy_covariance_monthly_stats$mean_assim,
        col = "blue", lwd = 2)
  # Add x-axis labels as month abbreviations
  axis(1, at = 1:12, labels = month.abb)
  # Add a legend
  legend("topright", legend = c("Model", "Eddy Covariance"),
         col = c("black", "blue"), lty = 1, lwd = 2, bty = "n")
  
  ### PHYDRO
  kphio = 0.026263945805926
  rdark = 0.011
  vwind = 3
  a_jmax = 80
  par_cost = list(alpha=0.1008, gamma=0.180496537959982, nitrogen_store_conversion = 1)
  par_cost_2 = list(alpha=0.1008, gamma=0.180496537959982, nitrogen_store_conversion = 1.5)
  par_cost_3 = list(alpha=0.1008, gamma=0.180496537959982, nitrogen_store_conversion = 0.5)
  par_plant = list(conductivity=3e-17, psi50=-2, b=2)
  co2 = 400
  elv  = 181
  pa = rpmodel::calc_patm(elv)
  psi_soil = -0.1
  fapar = 0.7
  nitrogen = 100
  options = list(gs_method = "GS_IGF", 
                 et_method = "ET_DIFFUSION",
                 ftemp_vj_method = "FV_kumarathunge19",
                 ftemp_rd_method = "FR_heskel16",
                 ftemp_br_method = "FB_atkin15",
                 scale_alpha = F)
  
  phydro_results   <- vector("list", nrow(input_climate))
  phydro_results_2 <- vector("list", nrow(input_climate))
  phydro_results_3 <- vector("list", nrow(input_climate))
  
  for (month in 1:nrow(input_climate)) {
    # message("Running month ", month, " of ", nrow(input_climate), "...")
    
    # Define a helper function for safe model calls
    safe_rphydro <- function(par_cost) {
      tryCatch({
        rphydro::rphydro_nitrogen(
          input_climate$Temp[month], input_climate$Temp[month],
          input_climate$PPFD[month], input_climate$PPFD_max[month],
          1000 * input_climate$VPD[month],
          co2, pa, nitrogen, fapar, kphio, psi_soil,
          rdark, vwind, a_jmax, par_plant, par_cost, options
        )
      }, error = function(e) {
        message("⚠️  Error in month ", month, ": ", e$message)
        return(NULL)  # or NA if you prefer
      })
    }
    
    # Run each parameterization safely
    phydro_results[[month]]   <- safe_rphydro(par_cost)
    phydro_results_2[[month]] <- safe_rphydro(par_cost_2)
    phydro_results_3[[month]] <- safe_rphydro(par_cost_3)
  }
  
  # Helper to extract safely
  extract_safe <- function(x, var) {
    if (is.null(x) || is.null(x[[var]])) return(NA)
    x[[var]]
  }
  
  # Extract variables for each parameterization
  df1 <- data.frame(
    Month = 1:length(phydro_results),
    a      = sapply(phydro_results, extract_safe, "a"),
    jmax   = sapply(phydro_results, extract_safe, "jmax"),
    vcmax  = sapply(phydro_results, extract_safe, "vcmax"),
    dpsi   = sapply(phydro_results, extract_safe, "dpsi"),
    n_leaf = sapply(phydro_results, extract_safe, "n_leaf"),
    Version = "1"
  )
  
  df2 <- data.frame(
    Month = 1:length(phydro_results_2),
    a      = sapply(phydro_results_2, extract_safe, "a"),
    jmax   = sapply(phydro_results_2, extract_safe, "jmax"),
    vcmax  = sapply(phydro_results_2, extract_safe, "vcmax"),
    dpsi   = sapply(phydro_results_2, extract_safe, "dpsi"),
    n_leaf = sapply(phydro_results_2, extract_safe, "n_leaf"),
    Version = "1.5"
  )
  
  df3 <- data.frame(
    Month = 1:length(phydro_results_3),
    a      = sapply(phydro_results_3, extract_safe, "a"),
    jmax   = sapply(phydro_results_3, extract_safe, "jmax"),
    vcmax  = sapply(phydro_results_3, extract_safe, "vcmax"),
    dpsi   = sapply(phydro_results_3, extract_safe, "dpsi"),
    n_leaf = sapply(phydro_results_3, extract_safe, "n_leaf"),
    Version = "0.5"
  )
  
  # Combine both data frames
  df_combined <- bind_rows(df1, df2, df3)
  df_combined$a_scaled <- df_combined$a * 12 * 1e-9 * 20 * 60 * 60 * 24 * 30.44
  
  # Convert to long format for facet plotting
  df_long <- df_combined %>%
    pivot_longer(
      cols = c(a, jmax, vcmax, a_scaled, dpsi, n_leaf),
      names_to = "Variable",
      values_to = "Value"
    )
  
  # Create a named vector for facet labels
  facet_labels <- c(
    a = "a (µmol m⁻² s⁻¹)",
    a_scaled = "a (kg C month⁻¹)",
    jmax = "Jmax (µmol m⁻² s⁻¹)",
    vcmax = "Vcmax (µmol m⁻² s⁻¹)",
    dpsi = "Δψ (MPa)",
    n_leaf = "Leaf N (g g-1)"
  )
  
  ggplot(df_long,
         aes(x = Month, y = Value, color = Version, group = Version)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ Variable, scales = "free_y", ncol = 2,
               labeller = as_labeller(facet_labels)) +
    labs(
      title = "Comparison of Model Outputs: Infrastructure",
      x = "Iteration (Month)",
      y = "Value",
      color = "Infrastructure"
    ) +
    theme_minimal()
  
  ### Respiration
  
  plot(df$date, df$rl, main = "Leaf respiration", xlab = "Date", ylab = "kg/month")
  
  ylim = range(df$rr, 30.44 * 4/1000, na.rm = T)
  plot(df$date, df$rr, main = "Root respiration", xlab = "Date", ylab = "kg/month", ylim = ylim)
  # ploints()
  abline(h = 30.44 * 4/1000, col = "blue")
  # TODO: find the actual flux and add it here, also m2 to root system?
  mtext("Kira Ryhti dissertation, g C m-2 d-1 x 30.44 / 1000 = kg C m-2 month-1 \n TODO PlantFATE = kg/month (entire root system)", side = 3, line = -1, cex = 0.8)
  
  plot(df$date, df$rs, main = "Sapwwod respiration", xlab = "Date", ylab = "kg/month")
  
  plot(df$date, df$tl, main = "turnover leaf", xlab = "Date", ylab = "kg/month")
  
  plot(df$date, df$tr, main = "turnover roots", xlab = "Date", ylab = "kg/month")
  
  plot(df$date, df$dpsi, main = "dpsi", xlab = "Date", ylab = "average Pa / month (paper)")
  
  plot(df$date, df$vcmax, main = "Vcmax", xlab = "Date", ylab = "Average umol m-2 s-1 per month")
  
  ylim = range(df$transpiration, Eddy_covariance$ET/100, na.rm = T)
  plot(df$date, df$transpiration, main = "Transpiration", xlab = "Date", ylab = "kg-H2O month-1", ylim = ylim)
  points(Eddy_covariance$MonthlyDate, Eddy_covariance$ET/100, col = "blue")
  
  #######
  ### Biomass
  #######
  # TODO: why isn't this increasing very quickly...? Is this in the boreal calibration or the construction?
  ylim = range(loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine height BA weighted mean"], df$height, na.rm = T)
  plot(df$date, df$height, main = "Height", xlab = "Date", ylab = "m", ylim = ylim)
  points(as.Date(paste0(loaded_data$smearII_data$date[loaded_data$smearII_data$variable == "pine height BA weighted mean"], "-01-01")), loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine height BA weighted mean"], col = "blue", pch = "x")
  legend("topleft", c("Output", "Calibration Data", "Initial Value"), 
         pch = c("o", "x", ""), lty = c(0, 0, 1), bty = "n", col = c("black", "blue", "green"))
  points(as.Date(as.character(1960 + prebas$multiOut[3,,7,,1]), format = "%Y")[1:62], prebas$multiOut[3,,11,,1][1:62], col = "red")
  mtext("Preles (m average tree)", side = 1, line = 2, cex = 0.7, col = "red")

  # TODO: why isn't this increasing, why are there NAs?
  ylim = range(0.01*loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine diameter BA weighted mean"], df$diameter, na.rm = T)
  plot(df$date, df$diameter, main = "Diameter", xlab = "Date", ylab = "m", ylim = ylim)
  points(as.Date(paste0(loaded_data$smearII_data$date[loaded_data$smearII_data$variable == "pine diameter BA weighted mean"], "-01-01")), 0.01 * loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine diameter BA weighted mean"], col = "blue", pch = "x")
  points(as.Date(as.character(1960 + prebas$multiOut[3,,7,,1]), format = "%Y")[1:62], prebas$multiOut[3,,12,,1][1:62]/100, col = "red")
  mtext("Preles (D, cm / 100, average tree)", 
        side = 1, line = 2, cex = 0.7, col = "red")
  
  ylim = range(df$crown_area, na.rm = T)
  plot(df$date, df$crown_area, main = "Crown Area", xlab = "Date", ylab = "m² ?", ylim = ylim)
  abline(h = 20, col = "blue") # NOTE: this is just a reference value to get the scale approx right
  
  ylim = range(loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "LAI_pine_ICOS"], df$lai, na.rm = T)
  plot(df$date, df$lai, main = "Leaf Area Index", xlab = "Date", ylab = "?", ylim = ylim)
  points(as.Date(paste0(loaded_data$smearII_data$date[loaded_data$smearII_data$variable == "LAI_pine_ICOS"], "-01-01")), loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "LAI_pine_ICOS"], col = "blue", pch = "x")
  abline(h = 4.0, col = "green")
  
  plot(df$date, df$sapwood_fraction, main = "Sapwood Fraction", xlab = "Date", ylab = "?")
  
  # Hyytiälä original: kg/ha, Life History: tonne
  ylim = range(loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine_foliage_biomass_ICOS"]/1000, df$leaf_mass, na.rm = T)
  plot(df$date, df$leaf_mass, main = "Leaf Mass", xlab = "Date", ylab = "kg", ylim = ylim)
  points(as.Date(paste0(loaded_data$smearII_data$date[loaded_data$smearII_data$variable == "pine_foliage_biomass_ICOS"], "-01-01")), loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine_foliage_biomass_ICOS"]/1000, col = "blue", pch = "x")
  
  ### Roots
  plot(df$date, df$root_mass, main = "Root Mass", xlab = "Date", ylab = "kg")
  
  plot(df$date, df$ectomycorrhiza_mass, main = "Ectomycorrhiza Mass", xlab = "Date", ylab = "kg")
  
  plot(df$date, df$stem_mass, main = "Stem Mass", xlab = "Date", ylab = "kg")
  
  plot(df$date, df$coarse_stem_mass, main = "Coarse Mass", xlab = "Date", ylab = "kg")
  
  #######
  ### NITROGEN BALANCE
  #######
  
  plot(df$date, df$tree_nitrogen, main = "Tree Nitrogen", xlab = "Date", ylab = "gN", ylim = range(c(df$tree_nitrogen, 39), na.rm = T))
  abline(h = 39, col = "green")
  
  plot(df$date, df$potential_leaf_nitrogen, main = "Leaf Nitrogen", pch = "x", xlab = "Date", ylab = "gN", ylim = range(c(df$optimal_leaf_nitrogen, df$potential_leaf_nitrogen), na.rm = T))
  points(df$date, df$optimal_leaf_nitrogen, xlab = "Date", pch = "x", col = "red")
  legend("topleft", c("Output", "Optimisation Maximum Bound", "Optimal Value", "Calibration Data", "Initial Value"), 
         pch = c("o", "x", "x", "x", ""), lty = c(0, 0, 0, 0, 1), bty = "n", col = c("black", "black", "red", "blue", "green"))
  
  #######
  ### MORTALITY, FITNESS AND TOTAL PROD
  #######
  ylim = range(df$total_rep, prebas$multiOut[3,,9,,1][1:62], na.rm = T)
  plot(df$date, df$total_rep, main = "Total Rep", xlab = "Date", ylab = "?", ylim = ylim)
  points(as.Date(as.character(1960 + prebas$multiOut[3,,7,,1]), format = "%Y")[1:62], prebas$multiOut[3,,9,,1][1:62], col = "red")
  mtext("Preles = g C per year, Respi_tot", 
        side = 1, line = 2, cex = 0.7, col = "red")
  
  plot(df$date, df$fitness, main = "Fitness", xlab = "Date", ylab = "?")
  
  plot(df$date, df$total_prod, main = "Total Prod", xlab = "Date", ylab = "?")
  
  plot(df$date, df$mortality, main = "Mortality", xlab = "Date", ylab = "?")
  
  plot(df$date, df$mortality_inst, main = "Mortality Inst", xlab = "Date", ylab = "?")
  
  plot(df$date, df$mortrate_0, main = "Mortailty rate 0", xlab = "Date", ylab = "?")
  
  plot(df$date, df$mortrate_growth, main = "Mortality growth", xlab = "Date", ylab = "?")
  
  plot(df$date, df$mortrate_d, main = "Mortality d", xlab = "Date", ylab = "?")
  
  plot(df$date, df$mortrate_hyd, main = "Mortality hyd", xlab = "Date", ylab = "?")
  
  #######
  ### LIFESPAN + ROOTS
  #######
  
  plot(df$date, df$leaf_lifespan, main = "Leaf Lifespan", xlab = "Date", ylab = "?")
  
  plot(df$date, df$fineroot_lifespan, main = "Fineroot Lifespan", xlab = "Date", ylab = "?")
  
  plot(df$date, df$root_length, main = "Root Length", xlab = "Date", ylab = "mm")
  abline(h = 1.5, col = "green")
  
  plot(df$date, df$root_no, main = "Root No", xlab = "Date", ylab = "no")
  abline(h = 20, col = "green")
  
  plot(df$date, df$nitrogen_uptake, main = "Nitrogen Uptake", xlab = "Date", ylab = "gN per biomass per Date")
  
  #######
  ### Geometry
  #######
  
  a = 150
  c = 3000
  m = 2.0
  n = 1.1
  z = 0.2
  H_m = 30
  # Function to find q_m such that max r(z) = sqrt(A_c/pi)
  find_qm <- function(q_m, A_c, m, n, H_m) {
    # maximize r(z) over z
    max_r <- optimize(function(z) crown_radius(z, q_m, A_c, m, n, H_m),
                      interval = c(0, H_m), maximum = TRUE)$objective
    # difference from target
    return(max_r - sqrt(A_c/pi))
  }
  
  # Solve for q_m numerically
  qm_solution <- uniroot(function(q) find_qm(q, A_c, m, n, H_m),
                         interval = c(0.01, 10))  # adjust interval if needed
  
  qm <- qm_solution$root
  
  halme_et_al_2022 <- data.frame(height = c(17.1, 21.0, 20.5, 18.3, 13.2, 20.7, 16.8, 23.6, 13.6),
                                 crown_diameter = c(3.4, 4.0, 4.1, 3.3, 2.9, 3.7, 3.4, 4.2, 2.8))
  
  ### PARAMETER TESTS
  
  par(mfrow = c(2, 2))
  # TODO: crown area data
  plot(0.01*(1:30), crown_area(0.01*(1:30), a, H_m, c), xlab = "Diameter, m", ylab = "Crown Area, A_cm, m²", main = "")
  points(0.105, 28, col = "blue", pch = "x")
  legend("topleft", c("Equations", "Mencuccini and Bonosi (2001)"), col = c("black", "blue"), pch = c(1, "x"), bty = "n")
  
  plot(0.01*(1:30), height(0.01*(1:30), a, H_m), xlab = "Diameter, m", ylab = "Height, H, m", main = "")
  points(0.01*loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine diameter BA weighted mean"], 
         loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine height BA weighted mean"], 
         pch = "x", col = "blue")
  legend("topleft", legend = c("Hyytiälä Data Set"), col = c("blue","green"), pch = "x", bty = "n")
  
  plot(height(0.01*(1:30), a, H_m), crown_radius(z, q_m, crown_area(0.01*(1:30), a, H_m, c), m, n, H_m), 
       ylab = "Crown Radius, m", xlab = "Height, m", main = "")
  points(halme_et_al_2022$height, halme_et_al_2022$crown_diameter/2, col = "blue", pch = "x")
  legend("topleft", legend = c("Halme 2022 (height vs crown radius)"), col = c("blue"), pch = "x", bty = "n")
  
  plot(crown_radius(0.01*(1:30), q_m, crown_area(0.1, a, H_m, c), m, n, H_m), seq(1, 30, length.out = 30), xlim = c(0, 2.5), ylab = "z", xlab = "Crown Radius", col = "red", type = "l", lty = 2)
  lines(crown_radius(0.01*(1:30), q_m, crown_area(0.2, a, H_m, c), m, n, H_m), seq(1, 30, length.out = 30), col = "orange", lty = 2)
  points(halme_et_al_2022$crown_diameter/2, halme_et_al_2022$height, col = "blue", pch = "x")
  legend("topleft", legend = c(0.1, 0.2), col = c("red", "orange"), title = "Diameter", lty = 2, bty = "n")
  legend("bottomright", legend = c("Halme 2022 (crown radius vs height)"), col = c("blue"), pch = "x", bty = "n")
  
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
  lho$set_soil_nitrogen(0.5)
  lho$init()
  
  dt <- 1/12
  col_names <- lho$get_header()
  df <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  names(df) <- col_names
  
  start_year <- 1960
  end_year <- 1970
  
  # Parameter grids
  kphio_values <- seq(0.01, 0.03, length = 5)   # smaller grid for illustration
  alpha_values <- c(0.08, 0.1, 0.12)
  gamma_values <- c(0.15, 0.18, 0.2)
  a_jmax_values <- c(60, 80, 100)
  
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
  
  
  
}


