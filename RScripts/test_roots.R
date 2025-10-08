blank <- function() {
  library(dplyr)
  library(lubridate)  # convenient for date operations
  library(ggplot2)
  library(zoo)
  library(data.table)
  
  ###
  # Simulation!
  ###
  
  lho <- new(LifeHistoryOptimizer, "tests/params/p_test_boreal.ini")
  lho$set_i_metFile("tests/data/ERAS_dataset_plantfate.csv")
  lho$set_a_metFile("tests/data/ERAS_dataset_plantfate.csv")
  lho$set_co2File("")
  lho$set_soil_nitrogen(0.5)
  lho$init()
  
  dt <- 1/12
  
  df <- data.frame(matrix(ncol = length(lho$get_header()), nrow = 0))
  col_names <- lho$get_header()
  
  start_year <- 1960
  end_year <- 2022

  results <- vector("list", length(seq(start_year, end_year, dt)))
  i <- 1
  
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
  
  ###
  # Validation data
  ###
  
  data_directory <- "~/Documents/CASSIA_Calibration/Processed_Data/"
  loaded_data <- CASSIA::load_data(data_directory)

  input_climate <- read.csv("~/Documents/Austria/Plant-FATE/tests/data/ERAS_Monthly.csv")
  
  raw.directory = "/home/josimms/Documents/CASSIA_Calibration/Raw_Data/hyytiala_weather/"
  environmental.variable.list <- list()
  count = 1
  for (variable in c("GPP", "NEE")) {
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
  names(environmental.variable.list) <- c("GPP", "NEE")
  
  # Original Eddy Covarience files in umol CO2 m-2 s-1
  DT <- copy(environmental.variable.list[["GPP"]])   # copy to avoid modifying original
  DT2 <- copy(environmental.variable.list[["NEE"]])   # copy to avoid modifying original
  GPP_out <- DT[, .(GPP_mean = mean(HYY_EDDY233.GPP, na.rm = TRUE)), by = Monthly]
  NEE_out <- DT2[, .(NEE_mean = mean(HYY_EDDY233.NEE, na.rm = TRUE)), by = Monthly]
  
  GPP_out <- GPP_out[ , Monthly := zoo::as.yearmon(Monthly, "%Y-%m") ]
  NEE_out <- NEE_out[ , Monthly := zoo::as.yearmon(Monthly, "%Y-%m") ]
  
  # If Month is numeric (1..12):
  # TODO: is there transpiration data?
  setorder(GPP_out, Monthly)
  setorder(NEE_out, Monthly)
  
  GPP_out$GPP_mean = GPP_out$GPP_mean * 12 * 1e-6 * 1e-3 * 30 * 24 * 60
  NEE_out$NEE_mean = NEE_out$NEE_mean * 12 * 1e-6 * 1e-3 * 30 * 24 * 60
  
  GPP_out$NEE <- NEE_out$NEE_mean
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
  plot(as.Date(paste(input_climate$Year, input_climate$Month, 01), format = "%Y%m%d"), input_climate$PPFD, col = "green", main = "PPFD", xlab = "Date", ylab = "?")
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
  
  ylim = range(df$assim_net, Eddy_covariance$NEE, na.rm = T)
  # TODO: i have this data no?
  plot(df$date, df$assim_net, main = "Net Assimilation", xlab = "Date", ylab = "kg C per month", ylim = ylim)
  mtext("Eddy, umol CO2 m-2 s-1 * seconds * 12 * 1e-9 = kg C per month", 
        side = 3, line = 0.5, cex = 0.7, col = "blue")
  points(Eddy_covariance$MonthlyDate, Eddy_covariance$NEE, col = "blue")
  
  # PRELES, units: g C m-2 to kg per month per canopy of one tree (TODO: is this m-2 of ground or leaves?)
  # Eddy_covariance_monthly$GPP_mean / 1000,
  # 1000 as a divider to make per tree and then 1000 from g C to kg C
  ylim = range(df$assim_gross, Eddy_covariance$GPP_mean, na.rm = T)
  # TODO: is this net or gross assimilation?
  # TODO: although there isn't any nitrogen uptake limitation could this be changing something somehow?
  plot(df$date, df$assim_gross, main = "Gross Assimilation", xlab = "Date", ylab = "kg C per month", ylim = ylim)
  points(Eddy_covariance$MonthlyDate, Eddy_covariance$GPP_mean, col = "blue")
  mtext("Eddy, umol CO2 m-2 s-1 * seconds * 12 * 1e-9 = kg C per month", 
        side = 3, line = 0.5, cex = 0.7, col = "blue")
  
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
  ylim <- range(c(monthly_stats$min_assim, monthly_stats$max_assim,
                  Eddy_covariance_monthly_stats$min_assim, 
                  Eddy_covariance_monthly_stats$max_assim),
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
  
  plot(df$date, df$transpiration, main = "Transpiration", xlab = "Date", ylab = "kg-h2o month-1")
  
  #######
  ### Biomass
  #######
  # TODO: why isn't this increasing very quickly...? Is this in the boreal calibration or the construction?
  ylim = range(loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine height BA weighted mean"], df$height, na.rm = T)
  plot(df$date, df$height, main = "Height", xlab = "Date", ylab = "m", ylim = ylim)
  points(as.Date(paste0(loaded_data$smearII_data$date[loaded_data$smearII_data$variable == "pine height BA weighted mean"], "-01-01")), loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine height BA weighted mean"], col = "blue", pch = "x")
  legend("topleft", c("Output", "Calibration Data", "Initial Value"), 
         pch = c("o", "x", ""), lty = c(0, 0, 1), bty = "n", col = c("black", "blue", "green"))

  # TODO: why isn't this increasing, why are there NAs?
  ylim = range(0.01*loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine diameter BA weighted mean"], df$diameter, na.rm = T)
  plot(df$date, df$diameter, main = "Diameter", xlab = "Date", ylab = "m", ylim = ylim)
  points(as.Date(paste0(loaded_data$smearII_data$date[loaded_data$smearII_data$variable == "pine diameter BA weighted mean"], "-01-01")), 0.01 * loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine diameter BA weighted mean"], col = "blue", pch = "x")
  
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
  
  plot(df$date, df$steam_mass, main = "Root Mass", xlab = "Date", ylab = "kg")
  
  plot(df$date, df$coarse_steam_mass, main = "Root Mass", xlab = "Date", ylab = "kg")
  
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
  
  plot(df$date, df$total_rep, main = "Total Rep", xlab = "Date", ylab = "?")
  
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
  abline(h = 20, col = "blue")
  
  plot(0.01*(1:30), height(0.01*(1:30), a, H_m), xlab = "Diameter, m", ylab = "Height, H, m", main = "")
  points(0.01*loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine diameter BA weighted mean"], 
         loaded_data$smearII_data$amount[loaded_data$smearII_data$variable == "pine height BA weighted mean"], 
         pch = "x", col = "blue")
  
  plot(height(0.01*(1:30), a, H_m), crown_radius(z, q_m, crown_area(0.01*(1:30), a, H_m, c), m, n, H_m), 
       ylab = "Crown Radius, m", xlab = "Height, m", main = "")
  points(halme_et_al_2022$height, halme_et_al_2022$crown_diameter/2, col = "blue", pch = "x")
  
  plot(crown_radius(0.01*(1:30), q_m, crown_area(0.1, a, H_m, c), m, n, H_m), seq(1, 30, length.out = 30), ylab = "z", xlab = "Crown Radius", col = "red", type = "l", lty = 1)
  lines(crown_radius(0.01*(1:30), q_m, crown_area(0.5, a, H_m, c), m, n, H_m), seq(1, 30, length.out = 30), col = "orange")
  points(halme_et_al_2022$crown_diameter/2, halme_et_al_2022$height, col = "blue", pch = "x")
  legend("topleft", legend = c(0.1, 0.5), col = c("red", "orange"), title = "Diameter", lty = 2, bty = "n")
  
  ### Photosynthesis
  par(mfrow = c(2, 1))
  plot(df$date, inst_df$a, main = "Assimilation per leaf", xlab = "Dates", ylab = "(umol m-2 s-1)")
  plot(df$date, crown_area(0.1, a, H_m, c) * inst_df$a, main = "Assimilation multiplied by canopy area", xlab = "Dates", ylab = "(kg C)", lty = 2, col = "red")
  lines(df$date, crown_area(0.2, a, H_m, c) * inst_df$a, lty = 2, col = "orange")
  
}


