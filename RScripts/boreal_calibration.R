###
# Library
###

library(ggplot2)
library(patchwork)
library(parallel)

###
# Life history
###

create_lho <- function(params_file, i_metFile, a_metFile, co2File = "", init_co2 = NULL) {
  lho <- new(LifeHistoryOptimizer, params_file)
  lho$set_i_metFile(i_metFile)
  lho$set_a_metFile(a_metFile)
  lho$set_co2File(co2File)
  if (!is.null(init_co2)) lho$init_co2(init_co2)
  print(c(lho$env$clim_inst$co2, lho$env$clim_acclim$co2))
  lho$init()
  return(lho)
}

run_for_dataset <- function(lho, start_year, end_year, dt) {
  df <- data.frame(matrix(ncol = length(lho$get_header()), nrow = 0))
  colnames(df) <- lho$get_header()
  
  for (t in seq(start_year, end_year, dt)) {
    lho$grow_for_dt(t, dt)
    df[nrow(df) + 1, ] <- lho$get_state(t + dt)
  }
  df$date <- seq(as.Date(paste0(start_year, "-01-01")), 
                 as.Date(paste0(end_year, "-01-01")),
                 by = "month")
  
  return(df)
}


life_histroy_calibration <- function() {
  
  create_lho_params <- function(params_file, i_metFile, a_metFile, co2File = "", init_co2 = NULL, param) {
    lho <- new(LifeHistoryOptimizer, params_file)
    lho$set_i_metFile(i_metFile)
    lho$set_a_metFile(a_metFile)
    lho$set_co2File(co2File)
    if (!is.null(init_co2)) lho$init_co2(init_co2)
    print(c(lho$env$clim_inst$co2, lho$env$clim_acclim$co2))
    lho$par0$rs <- param
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
}

###
# PlantFATE
###

# Helper function to create and run simulation
run_simulation <- function(param_file, start_year, end_year, output_prefix) {
  sim <- new(Patch, param_file)
  sim$init(start_year, end_year)
  sim$simulate()
  sim$close()
  
  output_dir <- "pspm_output_test"
  dir_path <- file.path(output_dir, output_prefix)
  
  return(list(sim = sim, dir_path = dir_path))
}

# Helper function to read simulation data
read_sim_data <- function(dir_path) {
  a <- read.delim(file.path(dir_path, "D_PFATE.csv"), sep = ",")
  b <- read.delim(file.path(dir_path, "Y_PFATE.csv"), sep = ",")
  return(list("D" = a,
              "Y" = b))
}

# Plot function
plot_gpp_npp <- function(dat, title, color, data = TRUE) {
  ERAS_Monthly <- data.table::fread("tests/data/ERAS_Monthly.csv")
  
  matplot(y = cbind(dat$GPP, dat$NPP) * 1e-3 * 365,
          x = dat$YEAR,
          type = "l", col = color, lty = c(1, 2),
          main = title, ylab = "GPP, NPP (kgC/m2/yr)", xlab = "Time (years)")
  points(as.Date(paste(ERAS_Monthly$Year, ERAS_Monthly$Month, "01", sep = "-")), 0.000001 * ERAS_Monthly$GPP, pch = 4)
}

# Size distribution analysis
analyze_size_dist <- function(dir_path) {
  dist <- read.delim(file.path(dir_path, "size_distributions.csv"), header = FALSE, sep = ",")
  dist <- dist[, -ncol(dist)]
  names(dist)[1:2] <- c("YEAR", "SPP")
  
  dist %>%
    filter(YEAR == max(YEAR)) %>%
    pivot_longer(cols = -(YEAR:SPP), names_to = "size_class") %>%
    group_by(YEAR, size_class) %>%
    summarize(de = sum(value, na.rm = TRUE)) %>%
    pivot_wider(names_from = size_class, values_from = de) %>%
    colMeans(na.rm = TRUE)
}

make_csv <- function() {
  col1 = c("A", "B", "C")
  col2 = 1:3
  col3 = rep(NA, 3)
  col4 = rep(NA, 3)
  col5 = rep(0.7, 3)
  height = c(25, 32, 25)
  lma = c(150, 215, 85)
  p50 = c(-3.6, -3.4, -2.25)
  col8 = rep(NA, 3)
  col9 = rep(NA, 3)
  allx <- data.frame("Family" = col1, 
                     "Species" = col2, 
                     "Number of indirviduals" = col3, 
                     "Total BasalArea_2017(cm2)" = col4, 
                     "meanWoodDensity (g/cm3)" = col5, 
                     "Height_Max(m)" = height, 
                     "Leaf LMA (g/m2)" = lma,
                     "P50 (Mpa)" = p50,
                     "P50 source" = col8,
                     "P50 site" = col9)
  data.table::fwrite(allx, file = "./tests/data/traits_boreal_canopy_understorey.csv")
}

# Stomatal conductance plots
plot_gs <- function(dat, title, color) {
  matplot(y = cbind(dat$GS), x = dat$YEAR, type = "l", lty = 1, col = color,
          main = title, ylab = "Stomatal conductance (mol/m2/s)", xlab = "Time (years)")
}

boreal_calibration <- function() {
  # Run simulations
  amazon_sim <- run_simulation("tests/params/p_test_v2.ini", 1000, 1050, "test_3spp_100yr")
  boreal_sim_weather <- run_simulation("tests/params/p_test_boreal_only_weather_changed.ini", 1960, 1995, "boreal_monthly_calibration_weather")
  boreal_sim_resp <- run_simulation("tests/params/p_test_boreal_weather_resp.ini", 1960, 1995, "boreal_monthly_calibration_weather")
  boreal_sim_core_traits <- run_simulation("tests/params/p_test_boreal_weather_core_traits.ini", 1960, 1995, "boreal_monthly_calibration_weather_core_traits")
  boreal_sim_traits <- run_simulation("tests/params/p_test_boreal_weather_traits.ini", 1960, 1995, "boreal_monthly_calibration_weather_traits")
  boreal_sim_all <- run_simulation("tests/params/p_test_boreal.ini", 1960, 1995, "boreal_monthly_calibration")
  co2_high_sim <- run_simulation("tests/params/p_test_boreal_monthly_co2_high.ini", 2015, 2100, "boreal_monthly_calibration_high_CO2")
  co2_medium_sim <- run_simulation("tests/params/p_test_boreal_monthly_co2_medium.ini", 2015, 2100, "boreal_monthly_calibration_medium_CO2")
  
  boreal_sim_all <- run_simulation("tests/params/p_test_boreal.ini", 1960, 1995, "boreal_monthly_calibration")
  plot_gpp_npp(read_sim_data(boreal_sim_all$dir_path), "Boreal", "cyan3")
  
  # Read simulation data
  dat_list <- list(
    amazon = read_sim_data(amazon_sim$dir_path),
    boreal = read_sim_data(boreal_sim_all$dir_path), #TODO: plot means
    boreal_weather = read_sim_data(boreal_sim_weather$dir_path),
    boreal_resp = read_sim_data(boreal_sim_resp$dir_path),
    boreal_core_traits = read_sim_data(boreal_sim_core_traits$dir_path),
    boreal_traits = read_sim_data(boreal_sim_traits$dir_path)
    # co2_high = read_sim_data(co2_high_sim$dir_path),
    # co2_medium = read_sim_data(co2_medium_sim$dir_path)
  )
  
  # Read ERAS data
  ERAS_Monthly <- data.table::fread("tests/data/ERAS_Monthly.csv")
  ERAS_Year <- ERAS_Monthly[, lapply(.SD, mean, na.rm = TRUE), by = Year]
  
  # Calibration
  par(mfrow = c(3, 2))
  plot_gpp_npp(dat_list$amazon$D, "Amazon:\nOriginal", c("green4", "green3"))
  plot_gpp_npp(dat_list$boreal_weather$D, "Boreal:\nOnly Weather", "cyan3")
  plot_gpp_npp(dat_list$boreal_resp$D, "Boreal:\nRespiration Parameters", "cyan3")
  plot_gpp_npp(dat_list$boreal_core_traits$D, "Boreal:\nCore Traits Parameters", "cyan3")
  plot_gpp_npp(dat_list$boreal_traits$D, "Boreal:\nTraits File Parameters", "cyan3")
  plot_gpp_npp(dat_list$boreal$D, "Boreal", "cyan3")
  
  # Scenarios
  par(mfrow = c(2, 2))
  plot_gpp_npp(dat_list$co2_medium$D, "Boreal, Medium CO2", "orange")
  plot_gpp_npp(dat_list$co2_high$D, "Boreal, High CO2", "red")
  plot_gpp_npp(dat_list$boreal$D, "Boreal", "cyan3")
  
  # Nitrogen
  par(mfrow = c(2, 2))
  
  par(mfrow = c(2, 2))
  mapply(plot_gs, dat_list, 
         names(dat_list), 
         c("green4", "cyan3", "cyan3", "red", "orange"))
  
  #dist_amb_v2 <- analyze_size_dist(amazon_sim$dir_path)
  #dist_amb_boreal <- analyze_size_dist(boreal_sim$dir_path)
  
  # Return results if needed
  #list(
  #  simulations = list(amazon = amazon_sim, boreal = boreal_sim, 
  #                     co2_high = co2_high_sim, co2_medium = co2_medium_sim),
  #  data = dat_list,
  #  size_distributions = list(amazon = dist_amb_v2, boreal = dist_amb_boreal)
  #)
}


