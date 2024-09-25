boreal_calibration_lho <- function() {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(parallel)
  library(lhs) # Generate all combinations of potential parameters
  library(logger)
  
  #devtools::install_github("jaideep777/Plant-FATE@develop")
  #library(PlantFATE)
  
  
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
  df_boreal <- run_for_dataset(lho_boreal, 1965, 2022, dt)
  
  ####
  # Plotting lho - with each section changed
  ####
  
  ### SMEAR Data
  hyyitala <- CASSIA::load_data()
  
  ### TRY data for n content generally
  data_try_leaf_n <- data.table::fread(paste0("/home/josimms/Documents/Austria/TRY_data/", "35525.txt"))
  data_try_scots_pine <- data_try_leaf_n[data_try_leaf_n$SpeciesName == "Pinus sylvestris"]
  
  scots_pine_pivot_string_all_cols <- data_try_scots_pine %>%
    dplyr::mutate(DataName = stringi::stri_trans_general(DataName, "Latin-ASCII")) %>%
    tidyr::pivot_wider(names_from = DataName, 
                       values_from = StdValue, 
                       id_cols = ObservationID, 
                       values_fn = ~mean(.x, na.rn = T))

  ### OUTPUTS
  par(mfrow = c(2, 2))
  plot(df_weather$date, df_weather$assim_gross, type = "l", col = "red",
      xlab = "Date", ylab = "GPP, kg C m-2", 
      ylim = range(c(df_weather$assim_gross, df_resp$assim_gross, df_core_traits$assim_gross, df_amazon$assim_gross))) 
  lines(df_phydro$date, df_phydro$assim_gross, col = "orange")
  lines(df_core_traits$date, df_core_traits$assim_gross, col = "yellow")
  lines(df_resp$date, df_resp$assim_gross, col = "green")
  lines(df_boreal$date, df_boreal$assim_gross, col = "blue")
  lines(df_amazon$date, df_amazon$assim_gross, lty = 2)
  points(as.Date(paste(boreal$Year, boreal$Month, "01", sep = "-")), boreal$GPP, col = "purple", pch = "-") # TODO: units
  legend("topleft", c("GPP, Measured Hyyitälä"), col = c("purple"), lty = 1, bty = "n")
  
  plot(df_weather$date, df_weather$height, type = "l", col = "red",
       xlab = "Date", ylab = "Height, m",
       ylim = range(c(df_weather$height, df_resp$height, df_core_traits$height, df_amazon$height, 22))) 
  lines(df_phydro$date, df_phydro$height, col = "orange")
  lines(df_core_traits$date, df_core_traits$height, col = "yellow")
  lines(df_resp$date, df_resp$height, col = "green")
  lines(df_boreal$date, df_boreal$height, col = "blue")
  lines(df_amazon$date, df_amazon$height, lty = 2)
  points(as.Date(paste(hyyitala$smearII_data$date[hyyitala$smearII_data$variable == "pine height arithmetic mean"], "01-01", sep = "-")), 
         hyyitala$smearII_data$amount[hyyitala$smearII_data$variable == "pine height arithmetic mean"], pch = "x")
  points(as.Date(paste(hyyitala$smearII_data$date[hyyitala$smearII_data$variable == "BA weighted mean height pine"], "01-01", sep = "-")), 
         hyyitala$smearII_data$amount[hyyitala$smearII_data$variable == "BA weighted mean height pine"], pch = "x")
  points(as.Date(paste(hyyitala$smearII_data$date[hyyitala$smearII_data$variable == "Arithmetic mean height pine"], "01-01", sep = "-")), 
         hyyitala$smearII_data$amount[hyyitala$smearII_data$variable == "Arithmetic mean height pine"], pch = "x")
  points(as.Date(paste(hyyitala$smearII_data$date[hyyitala$smearII_data$variable == "pine height BA weighted mean"], "01-01", sep = "-")), 
         hyyitala$smearII_data$amount[hyyitala$smearII_data$variable == "pine height BA weighted mean"], pch = "x")
  points(as.Date(paste(hyyitala$smearII_data$date[hyyitala$smearII_data$variable == "pine height arithmetic mean"], "01-01", sep = "-")), 
         hyyitala$smearII_data$amount[hyyitala$smearII_data$variable == "pine height arithmetic mean"], pch = "x")
  legend("topleft", c("Measured Data Hyyitälä"), col = c("black"), pch = "x", bty = "n")
  
  # tree_height_baseal_area_weighted_mean
  # dominant_height
  # BA weighted mean height spruce
  # Arithmetic mean height spruce
  # spruce height BA weighted mean
  # spruce height arithmetic mean
  
  plot(df_weather$date, df_weather$nitrogen, type = "l", col = "red",
       xlab = "Date", ylab = "Leaf Nitrogen, g g-1", # TODO: units!
       ylim = range(c(df_weather$nitrogen, df_resp$nitrogen, df_core_traits$nitrogen, df_amazon$nitrogen))) 
  lines(df_phydro$date, df_phydro$nitrogen, col = "orange")
  lines(df_core_traits$date, df_core_traits$nitrogen, col = "yellow")
  lines(df_resp$date, df_resp$nitrogen, col = "green")
  lines(df_boreal$date, df_boreal$nitrogen, col = "blue")
  lines(df_amazon$date, df_amazon$nitrogen, lty = 2)
  legend("topleft", 
         paste("Mean N per dry leaf mass (TRY)", unique(data_try_leaf_n$UnitName[data_try_leaf_n$DataName == "Leaf nitrogen content per dry mass (Nmass)"])),
         col = "purple", lty = 3, bty = "n")
  abline(h = mean(scots_pine_pivot_string_all_cols$`Leaf nitrogen content per dry mass (Nmass)`, na.rm = T), col = "purple", lty = 3)
  
  plot(df_weather$date, df_weather$rs, type = "l", col = "red",
       xlab = "Date", ylab = "Respiration",
       ylim = range(c(df_weather$rs, df_resp$rs, df_core_traits$rs, df_amazon$rs))) 
  lines(df_phydro$date, df_phydro$rs, col = "orange")
  lines(df_core_traits$date, df_core_traits$rs, col = "yellow")
  lines(df_resp$date, df_resp$rs, col = "green")
  lines(df_boreal$date, df_boreal$rs, col = "blue")
  lines(df_amazon$date, df_amazon$rs, lty = 2)
  
  par(new = TRUE)
  par(mfrow = c(1, 1))
  plot.new()
  legend(0.05, 0.5, c("Only weather changed", "Jaideep's respiration parameters", "Core Traits", "Respiration", "All", "Amazon"), 
         col = c("red", "orange", "yellow", "green", "blue", "black"), lty = c(1, 1, 1, 1, 1, 2), bty = "n", ncol = 3, title = "Prameters", cex = 0.8, lwd = 2)
  
  
  ####
  # Sensitivity analysis of certain parameters, building the processes
  #####
  
  ### Plot function
  # Define the function
  create_combined_plot <- function(sa_ib_all, color_scale, boreal, hyyitala, scots_pine_pivot_string_all_cols, param) {
    sa_ib_all$parameter <- sa_ib_all[,c(param)]
    
    # Plot 1: Height
    hyytiala_pine_heights <- hyyitala$smearII_data %>%
      filter(variable %in% c("pine height arithmetic mean", 
                             "BA weighted mean height pine", 
                             "Arithmetic mean height pine", 
                             "pine height BA weighted mean")) %>%
      mutate(date = as.Date(paste(date, "01-01", sep = "-")))
    p1 <- ggplot() +
      geom_point(data = sa_ib_all, aes(x = date, y = height, color = parameter), shape = 45, size = 3) +
      geom_point(data = hyytiala_pine_heights, aes(x = date, y = amount), shape = 4, size = 3) +
      # geom_line(data = df_original, aes(x = date, y = height), color = "black") +
      labs(x = "Date", y = "Height (m)") +
      color_scale +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.box = "vertical",
            plot.title = element_text(hjust = 0.5)) +
      guides(color = guide_legend(nrow = 2))
    
    # Plot 2: Root Mass
    hyytiala_pine_coarse_root <- hyyitala$smearII_data %>%
      filter(variable %in% c("pine_stump_coarse_roots_biomass",
                             "Stumps and coarse roots of living trees before thinning",
                             "Stumps and coarse roots of living trees after thinning")) %>%
      mutate(date = as.Date(paste(date, "01-01", sep = "-")))
    p2 <- ggplot() +
      geom_point(data = sa_ib_all, aes(x = date, y = stem_mass + coarse_root_mass, color = parameter), shape = 45, size = 3) +
      geom_point(data = hyytiala_pine_coarse_root, aes(x = date, y = amount), shape = 4, size = 3) +
      # geom_line(data = df_original, aes(x = date, y = stem_mass + coarse_root_mass), color = "black") +
      labs(x = "Date", y = "Root Mass") +
      color_scale +
      theme_minimal()
    
    # Plot 3: Diameter
    hyytiala_pine_diameters <- hyyitala$smearII_data %>%
      filter(variable %in% c("pine DBH arithmetic mean", 
                             "BA weighted DBH pine", 
                             "Arithmetic DBH pine", 
                             "pine diameter BA weighted mean")) %>%
      mutate(date = as.Date(paste(date, "01-01", sep = "-")))
    p3 <- ggplot() +
      geom_point(data = sa_ib_all, aes(x = date, y = 100*diameter, color = parameter), shape = 45, size = 3) +
      geom_point(data = hyytiala_pine_diameters, aes(x = date, y = amount), shape = 4, size = 3) +
      # geom_line(data = df_original, aes(x = date, y = 100*diameter), color = "black") +
      labs(x = "Date", y = "Diameter (cm)") +
      color_scale +
      theme_minimal()
    
    # Plot 3: Diameter vs height
    # Combine the datasets
    combined_data <- bind_rows(
      hyytiala_pine_heights %>% mutate(measure_type = "height"),
      hyytiala_pine_diameters %>% mutate(measure_type = "diameter")
    )
    
    # Add a unique identifier for each row within each date-variable-measure_type group
    combined_data <- combined_data %>%
      group_by(date, variable, measure_type) %>%
      mutate(row_id = row_number()) %>%
      ungroup()
    
    # Reshape the data
    wide_data <- combined_data %>%
      mutate(column_name = paste(variable, measure_type, row_id, sep = "_")) %>%
      pivot_wider(
        id_cols = date,
        names_from = column_name,
        values_from = amount
      )
    
    
    p35 <- ggplot() +
      geom_point(data = sa_ib_all, aes(x = height, y = 100*diameter, color = parameter), shape = 45, size = 3) +
      geom_point(data = wide_data, aes(x = `BA weighted mean height pine_height_1`,
                                     y = `pine diameter BA weighted mean_diameter_1`), shape = 4, size = 3) + 
      geom_point(data = wide_data, aes(x = `BA weighted mean height pine_height_2`,
                                       y = `pine diameter BA weighted mean_diameter_1`), shape = 4, size = 3) + 
      geom_point(data = wide_data, aes(x = `Arithmetic mean height pine_height_1`,
                                       y = `pine DBH arithmetic mean_diameter_1`), shape = 4, size = 3) + 
      geom_point(data = wide_data, aes(x = `Arithmetic mean height pine_height_2`,
                                       y = `pine DBH arithmetic mean_diameter_2`), shape = 4, size = 3) + 
      # geom_line(data = df_original, aes(x = height, y = 100*diameter), color = "black") +
      labs(x = "Height (m)", y = "Diameter (cm)") +
      color_scale +
      theme_minimal()
    
    # Plot 4: Total Mass
    p4 <- ggplot() +
      geom_point(data = sa_ib_all, aes(x = date, y = total_mass, color = parameter), shape = 45, size = 3) +
      # geom_line(data = df_original, aes(x = date, y = total_mass), color = "black") +
      labs(x = "Date", y = "Total Mass") +
      color_scale +
      theme_minimal()
    
    boreal$date <- as.Date(paste(boreal$Year, boreal$Month, "01", sep = "-"))
    # Plot 5: Assim gross
    p5 <- ggplot() +
      geom_point(data = sa_ib_all, aes(x = date, y = assim_gross, color = parameter), shape = 45, size = 3) +
      # geom_line(data = df_original, aes(x = date, y = assim_gross), color = "black") + # TODO: make a different line type!
      geom_line(data = boreal, aes(x = date, y = GPP), color = "black") +
      labs(x = "Date", y = "Assim gross") +
      color_scale +
      theme_minimal()
    
    hyytiala_pine_LAI <- hyyitala$smearII_data %>%
      filter(variable %in% c("LAI_pine_allsided", 
                             "LAI_pine_ICOS")) %>%
      mutate(date = as.Date(paste(date, "01-01", sep = "-")))
    # Plot 4: LAI
    p6 <- ggplot() +
      geom_point(data = sa_ib_all, aes(x = date, y = lai, color = parameter), shape = 45, size = 3) +
      geom_point(data = hyytiala_pine_LAI, aes(x = date, y = amount), shape = 4, size = 3) +
      # geom_line(data = df_original, aes(x = date, y = lai), color = "black") +
      labs(x = "Date", y = "LAI") +
      color_scale +
      theme_minimal()
    
    hyytiala_pine_foliage <- hyyitala$smearII_data %>%
      filter(variable %in% c("pine_foliage_biomass", 
                             "pine_foliage_biomass_ICOS", 
                             "Needles / Leaves before thinning", # TODO: think about this! TODO: Units?
                             "Needles / Leaves after thinning")) %>%
      mutate(date = as.Date(paste(date, "01-01", sep = "-")))
    # Plot 4: Leaf Mass
    p7 <- ggplot() +
      geom_point(data = sa_ib_all, aes(x = date, y = leaf_mass, color = parameter), shape = 45, size = 3) +
      geom_point(data = hyytiala_pine_foliage, aes(x = date, y = amount), shape = 4, size = 3) +
      # geom_line(data = df_original, aes(x = date, y = leaf_mass), color = "black") +
      labs(x = "Date", y = "Leaf Mass") +
      color_scale +
      theme_minimal()
    
    # Combine plots
    combined_plot <- (p1 + p3 + p35) / (p6 + p4 + p2) / p5 +
      plot_layout(guides = "collect") & 
      theme(legend.position = "bottom")
    
    # Return the combined plot
    return(combined_plot)
  }
  
  create_lho_zeta <- function(params_file, i_metFile, a_metFile, co2File = "", init_co2 = NULL, param) {
    lho <- new(LifeHistoryOptimizer, params_file)
    lho$set_i_metFile(i_metFile)
    lho$set_a_metFile(a_metFile)
    lho$set_co2File(co2File)
    if (!is.null(init_co2)) lho$init_co2(init_co2)
    #print(c(lho$env$clim_inst$co2, lho$env$clim_acclim$co2))
    lho$traits0$a <- param
    lho$init()
    return(lho)
  }
  
  potential_parameters <- seq(50, 400, length.out = 40)
  
  # Use mclapply for parallel processing
  sa_rs <- mclapply(potential_parameters, function(i) {
    lho_temp <- create_lho_zeta("tests/params/p_test_boreal.ini", 
                                "tests/data/ERAS_Monthly.csv", 
                                "tests/data/ERAS_Monthly.csv",
                                co2File = "",
                                init_co2 = 365,
                                i)
    dt <- 1/12
    df <- run_for_dataset(lho_temp, 1960, 2022, dt)
    df$a <- i
    return(df)
  }, mc.cores = detectCores()-2)
  
  not_errors <- c()
  count = 1
  for (j in 1:length(sa_rs)) {
    if (is.data.frame(sa_rs[[j]])) {
      not_errors[count] <- j
      count = count + 1
    }
  }
  
  sa_rs_not_errors <- sa_rs[not_errors]
  
  # Combine the results into a single data frame
  sa_rs_all <- do.call(rbind, sa_rs_not_errors)
  
  # Plot
  color_scale <- scale_color_gradientn(colours = rainbow(5), name = "Zeta (fine root leaf ratio)")
  combined_plot <- create_combined_plot(df_boreal, sa_rs_all, color_scale, boreal, hyyitala, scots_pine_pivot_string_all_cols, "a")
  print(combined_plot)
  
  plot(sa_rs_all$height)
  plot(sa_rs_all$assim_gross)
  
  ####
  # Core parameters sensitivity analysis
  #
  # Was told to change:
  # lma, zeta, fcr, hmat, fhmat, seed_mass, wood_density, a, c, m, n, fg
  ####
  
  # Function to create LifeHistoryOptimizer objects
  create_lho_params_core_traits <- function(params_file, i_metFile, a_metFile, co2File = "", init_co2 = NULL, param_values) {
    lho <- new(LifeHistoryOptimizer, params_file)
    lho$set_i_metFile(i_metFile)
    lho$set_a_metFile(a_metFile)
    lho$set_co2File(co2File)
    if (!is.null(init_co2)) lho$init_co2(init_co2)
    print(c(lho$env$clim_inst$co2, lho$env$clim_acclim$co2))
    
    lho$traits0$lma <- param_values$lma
    lho$traits0$zeta <- param_values$zeta
    lho$traits0$fcr <- param_values$fcr
    lho$traits0$hmat <- param_values$hmat
    lho$traits0$fhmat <- param_values$fhmat
    lho$traits0$seed_mass <- param_values$seed_mass
    lho$traits0$wood_density <- param_values$wood_density
    lho$traits0$a <- param_values$a
    lho$traits0$c <- param_values$c
    lho$traits0$m <- param_values$m
    lho$traits0$n <- param_values$n
    lho$traits0$p50_xylem <- param_values$p50_xylem
    lho$traits0$K_xylem <- param_values$K_xylem
    
    lho$par0$fg <- param_values$fg
    lho$par0$rs <- param_values$rs
    lho$par0$rr <- param_values$rr
    lho$par0$lai0 <- param_values$lai
    lho$par0$kphio <- param_values$kphio
    lho$par0$gamma <- param_values$gamma
    
    lho$init()
    return(lho)
  }
  
  n_samples <- 1000  # n_samples
  n_params <- 19     # Number of parameters
  
  # Generate LHS samples
  lhs_samples <- randomLHS(n_samples, n_params)
  
  # Scale the samples to your desired ranges
  param_names <- c("lma", "zeta", "fcr", "hmat", "fhmat", "seed_mass", 
                   "wood_density", "a", "c", "m", "n", "fg", "rs", "rr",
                   "lai0", "kphio", "gamma", "p50_xylem", "K_xylem")
  
  param_ranges <- list(
    lma = c(0.1, 0.3),
    zeta = c(0.1, 0.9),
    fcr = c(0.2, 0.3),
    hmat = c(30, 50),
    fhmat = c(0.1, 0.9),
    seed_mass = c(0.005, 0.006),
    wood_density = c(390.3314, 410), # CASSIA has 400
    a = c(0.001, 600),
    c = c(4000, 10000),
    m = c(1.9, 2.1),
    n = c(1.0, 1.2),
    fg = c(0.2, 0.5),
    rs = c(0.01, 0.9),
    rr = c(0.01, 0.9),
    lai0 = c(1, 5),
    kphio = c(0.02, 0.04),
    gamma = c(0.1, 0.3),
    p50_xylem = c(-4, -0.5),
    K_xylem = c(4.1311874912949E-18, 5E-17)
  )
  
  scaled_samples <- sapply(1:n_params, function(i) {
    range <- param_ranges[[i]]
    lhs_samples[,i] * (range[2] - range[1]) + range[1]
  })
  
  # Create the data frame
  param_grid <- as.data.frame(scaled_samples)
  colnames(param_grid) <- param_names
  
  sa_ib_all <- mclapply(1:nrow(param_grid), function(i) {
    params <- param_grid[i, ]
    
    # Initialize result as NULL, to be filled later
    result <- NULL
    
    tryCatch({
      # Input validation
      if (!all(c("lma", "zeta", "fcr", "hmat", "fhmat", "seed_mass", "wood_density", "a", "c", "m", "n", "fg", "p50_xylem", "K_xylem", "rs", "rr", "lai0", "kphio", "gamma") %in% names(params))) {
        stop("Missing required parameters in param_grid")
      }
      
      
      lho_temp <- create_lho_params_core_traits(
        "tests/params/p_test_v2.ini", 
        "tests/data/ERAS_Monthly.csv", 
        "tests/data/ERAS_Monthly.csv", 
        co2File = "",
        init_co2 = 365,
        params
      )
      
      if (is.null(lho_temp)) {
        stop("Failed to create lho_params object")
      }
      
      result <- run_for_dataset(lho_temp, 2015, 2100, dt)
      
      if (is.null(result)) {
        stop("run_for_dataset returned NULL")
      }
      
      names(params)[names(params) %in% c("rs", "rr")] <- c("rs_param", "rr_param")
      
      result <- cbind(result, params)
      
    }, error = function(e) {
      log_error(paste("Error in iteration", i, ":", e$message))
      return(NULL)
    }, warning = function(w) {
      log_warn(paste("Warning in iteration", i, ":", w$message))
    })
    
    return(result)
  }, mc.cores = max(1, detectCores() - 2))
  
  # Remove NULL results (failed iterations)
  sa_all_not_errors <- sa_ib_all[!sapply(sa_ib_all, is.null)]
  
  # Log summary
  log_info(paste("Completed", length(sa_ib_all), "out of", nrow(param_grid), "iterations successfully"))
  
  sa_ib_all_all <- do.call(rbind, sa_all_not_errors)
  
  for (param in param_names) {
    color_scale <- scale_color_gradientn(colours = rainbow(5), name = param)
    # df_boreal 
    combined_plot <- create_combined_plot(sa_ib_all_all, color_scale, boreal, hyyitala, scots_pine_pivot_string_all_cols, param)
    ggsave(filename = file.path("pspm_output_test/boreal_monthly_calibration_high_CO2/parameters", 
                                paste0(param, "_core_traits_sensitivity_analysis.png")), 
           plot = combined_plot, width = 10, height = 6, dpi = 300)
  }
  
  ####
  # Respiration parameters sensitivity analysis
  #
  # Was told to change:
  # lma, zeta, fcr, hmat, fhmat, seed_mass, wood_density, a, c, m, n, fg
  ####
  
  # # CHNAGED PARAMETER: VERSION 1, 0.35 Growth respiration in CASSIA
  # rr   = 0.35      # Fine root respiration rate per unit biomass per unit productivity [m2 kg-1]    0.4*1e3*1e-4*12/0.6/6.5 = 0.4 [MgC ha-1 mo-1] * 1e-3 [kgC/MgC] * 12 [mo yr-1] * 1e-4 [ha m-2] / 0.6 [m_fr, kgC m-2] / 6.5 [GPP kg m-2 yr-1] = [yr-1] / [GPP kg m-2 yr-1]   (0.4 from Doughty et al 2017 biotropica)
  # CHNAGED PARAMETER: VERSION 1, 0.3 Wood respiration in CASSIA
  # rs   = 0.03      # sapwood respiration rate per unit sapwood mass [yr-1] ---> Calibrated to NPP
  
  
  ####
  # 
  ####
  

} # End of the entire function
