###
# Library
###

library(ggplot2)
library(patchwork)

###
# PlantFATE
###

# Helper function to create and run simulation
run_simulation <- function(param_file, start_year, end_year, output_prefix) {
  sim <- new(pfate::Patch, param_file)
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
  plot(as.Date(paste(ERAS_Monthly$Year, ERAS_Monthly$Month, "01", sep = "-")), ERAS_Monthly$GPP, pch = 4)
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
    boreal_traits = read_sim_data(boreal_sim_traits$dir_path),
    co2_high = read_sim_data(co2_high_sim$dir_path),
    co2_medium = read_sim_data(co2_medium_sim$dir_path)
  )
  
  # Read ERAS data
  ERAS_Monthly <- data.table::fread("tests/data/ERAS_Monthly.csv")
  ERAS_Year <- ERAS_Monthly[, lapply(.SD, mean, na.rm = TRUE), by = Year]
  
  # Calibration
  par(mfrow = c(3, 2))
  plot_gpp_npp(dat_list$amazon, "Amazon:\nOriginal", c("green4", "green3"))
  plot_gpp_npp(dat_list$boreal_weather, "Boreal:\nOnly Weather", "cyan3")
  plot_gpp_npp(dat_list$boreal_resp, "Boreal:\nRespiration Parameters", "cyan3")
  plot_gpp_npp(dat_list$boreal_core_traits, "Boreal:\nCore Traits Parameters", "cyan3")
  plot_gpp_npp(dat_list$boreal_traits, "Boreal:\nTraits File Parameters", "cyan3")
  plot_gpp_npp(dat_list$boreal, "Boreal", "cyan3")
  
  # Scenarios
  par(mfrow = c(2, 2))
  plot_gpp_npp(dat_list$co2_medium, "Boreal, Medium CO2", "orange")
  plot_gpp_npp(dat_list$co2_high, "Boreal, High CO2", "red")
  plot_gpp_npp(dat_list$boreal, "Boreal", "cyan3")
  
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

###
# Plotting function - not for Life History
###

plot_plant_trajectory = function(df, df_high_co2, df_medium_co2, start_year, end_year){
  # TODO: Fix the calibration data
  
  ####
  # Reading Data
  ####
  boreal <- CASSIA::load_data()
  boreal$smearII_data$date <- as.Date(paste0(boreal$smearII_data$date, "-01-01"))
  ERAS_Monthly <- data.table::fread("tests/data/ERAS_Monthly.csv")
  ERAS_Monthly$date <- as.Date(paste(ERAS_Monthly$Year, ERAS_Monthly$Month, "01", sep = "-"))
  
  ####
  # Calculating extra traits
  ####
  df$leaf_area = df$crown_area * df$lai
  df$heartwood_fraction = 1-df$sapwood_fraction
  
  df$date <- as.Date(df$date)
  
  ####
  # Plotting
  ####
  par(mfrow=c(4,2), mar=c(4,4,1,1), oma=c(1,1,1,1))
  plot(df$height~df$diameter, type="l", ylab="Height", xlab="Diameter")
  points(0.01*boreal$smearII_data$amount[boreal$smearII_data$variable == "pine diameter BA weighted mean"],
         boreal$smearII_data$amount[boreal$smearII_data$variable == "pine height BA weighted mean"])
  points(0.01*boreal$smearII_data$amount[boreal$smearII_data$variable == "pine DBH arithmetic mean"],
         boreal$smearII_data$amount[boreal$smearII_data$variable == "pine height arithmetic mean"], pch = 2)
  points(0.01*boreal$smearII_data$amount[boreal$smearII_data$variable == "BA weighted mean DBH pine"],
         boreal$smearII_data$amount[boreal$smearII_data$variable == "BA weighted mean height pine"], pch = 3)
  lines(df_high_co2$height~df_high_co2$diameter, col = "red")
  lines(df_medium_co2$height~df_medium_co2$diameter, col = "orange")
  
  plot(df$crown_area~I(df$height*df$diameter), type="l", ylab="Crown area", xlab="DH")
  plot(I(df$total_rep/df$total_prod)~df$height, type="l", ylab="Frac alloc to\nreproduction", xlab="Height")
  plot(I(df$total_rep/df$total_prod)~df$diameter, type="l", ylab="Frac alloc to\nreproduction", xlab="Diameter")
  
  par(mfrow=c(4,4), mar=c(4,8,1,1), oma=c(1,1,1,1), mgp=c(4,1,0), cex.lab=1.2)
  plot(df$height~df$date, ylab="Height", xlab="Year", type="l", lwd=2)
  points(boreal$smearII_data$date[boreal$smearII_data$variable == "pine height BA weighted mean"],
         boreal$smearII_data$amount[boreal$smearII_data$variable == "pine height BA weighted mean"])
  points(boreal$smearII_data$date[boreal$smearII_data$variable == "pine height arithmetic mean"],
         boreal$smearII_data$amount[boreal$smearII_data$variable == "pine height arithmetic mean"], pch = 2)
  points(boreal$smearII_data$date[boreal$smearII_data$variable == "BA weighted mean height pine"],
         boreal$smearII_data$amount[boreal$smearII_data$variable == "BA weighted mean height pine"], pch = 3)
  
  plot(df$diameter~df$date, ylab="Diameter", xlab="Year", col="brown", type="l", lwd=2)
  points(boreal$smearII_data$date[boreal$smearII_data$variable == "pine diameter BA weighted mean"],
         0.01 * boreal$smearII_data$amount[boreal$smearII_data$variable == "pine diameter BA weighted mean"])
  points(boreal$smearII_data$date[boreal$smearII_data$variable == "pine DBH arithmetic mean"],
         0.01 * boreal$smearII_data$amount[boreal$smearII_data$variable == "pine DBH arithmetic mean"], pch = 2)
  points(boreal$smearII_data$date[boreal$smearII_data$variable == "BA weighted mean DBH pine"],
         0.01 * boreal$smearII_data$amount[boreal$smearII_data$variable == "BA weighted mean DBH pine"], pch = 3)
  
  matplot(y=cbind(df$total_mass,
                  df$total_rep,
                  df$litter_mass,
                  df$total_mass+df$total_rep+df$litter_mass,
                  df$total_prod), 
          x=df$date, col=c("green4", "purple", "yellow4", "black", "red"), log="", lty=c(1,1,1,1,2), lwd=c(1,1,1,2,1), type="l",
          ylab="Biomass pools", xlab="Year")
  abline(h=0, col="grey")
  points(boreal$smearII_data$date[boreal$smearII_data$variable == "pine diameter BA weighted mean"],
         0.01 * boreal$smearII_data$amount[boreal$smearII_data$variable == "pine diameter BA weighted mean"])
  points(boreal$smearII_data$date[boreal$smearII_data$variable == "pine DBH arithmetic mean"],
         0.01 * boreal$smearII_data$amount[boreal$smearII_data$variable == "pine DBH arithmetic mean"], pch = 2)
  points(boreal$smearII_data$date[boreal$smearII_data$variable == "BA weighted mean DBH pine"],
         0.01 * boreal$smearII_data$amount[boreal$smearII_data$variable == "BA weighted mean DBH pine"], pch = 3)
  
  
  matplot(y=cbind(df$total_mass,
                  df$leaf_mass,
                  df$root_mass,
                  df$coarse_root_mass,
                  df$stem_mass),
          x=df$date, col=c("black", "green3", "purple", "purple3", "brown"), log="", lty=c(1,1,1,1,1), lwd=c(1,1,1,1,1), type="l",
          ylab="Biomass pools", xlab="Year")
  abline(h=0, col="grey")
  
  
  matplot(y=cbind(df$fitness), 
          x=df$date, col=c("purple2", "magenta", "pink"), log="", lty=c(1,1,1), lwd=c(1,1,2), type="l",
          ylab="Fitness", xlab="Year")
  abline(h=0, col="grey")
  
  # plot(dat$total_mass~dat$i, ylab="Total biomass", xlab="Year")
  # plot(dat$total_mass~dat$i, ylab="Total biomass", xlab="Year")
  # 
  # points(y=dat$total_prod-dat$litter_mass, x=dat$i, type="l", col="red")
  
  # plot(y=1 - (dat$total_prod-dat$litter_mass)/dat$total_mass, x=dat$i, ylab="Total biomass", xlab="Year")
  
  # plot(dat$ppfd[01:1000]~dat$i[01:1000], type="l")
  # plot(dat$assim_gross[901:1000]~dat$ppfd[901:1000], type="l")
  
  
  matplot(y=cbind(df$assim_gross/df$crown_area,
                  df$assim_net/df$crown_area,
                  df$assim_net/df$assim_gross * max(df$assim_gross/df$crown_area)), 
          x=df$date, col=c("green3", "green4", scales::alpha("yellow3", 0.5)), log="", lty=1, type="l", lwd=c(1,1,3),
          ylab=expression(atop("GPP, NPP", "(kg m"^"-2"*"Yr"^"-1"*")")), xlab="Year")
  abline(h=0, col="grey")
  lines(ERAS_Monthly$date, ERAS_Monthly$GPP) # TODO:  µmol m⁻² s⁻¹
  
  matplot(y=cbind(df$rr/df$crown_area,
                  df$rs/df$crown_area,
                  df$rl/df$crown_area), 
          x=df$date, col=c("pink2", "pink3", "pink4"), log="", lty=1, type="l",
          ylab="Respiration", xlab="Year")
  points(boreal$trenching_co2_fluxes$V1,
         boreal$trenching_co2_fluxes$V2, pch = 3) # TODO: check that the units are right and that the right fluxes are used
  
  matplot(y=cbind(df$tr/df$crown_area,
                  df$tl/df$crown_area), 
          x=df$date, col=c("orange3", "orange4"), log="", lty=1, type="l",
          ylab="Turnover", xlab="Year",
          add=F)
  
  plot(I(df$transpiration/df$crown_area/1000*1000)~df$date, type="l", col="blue", ylab="Transpitation\n(mm/yr)")
  
  plot(df$dpsi~df$date, type="l", col="cyan", ylab=expression(atop(Delta*psi, "(MPa)")), xlab="Year")
  
  plot(df$vcmax~df$date, type="l", col="limegreen", ylab=expression(atop(V[cmax], "("*mu*"mol m"^"-2"*"s"^"-1"*")")), xlab="Year")
  
  plot(I(df$leaf_area/df$crown_area)~df$date, ylab="LAI", xlab="Year", type="l")
  
  summary(df$mortality)
  
  plot(I(df$mortality)~df$date, ylab="Cumulative\nMortality", xlab="Year", type="l")
  
  plot(I(df$mortality_inst[df$diameter<0.5])~df$diameter[df$diameter<0.5], ylab="Instantaneous\nmortality rate", xlab="Diameter", type="l")
  
  matplot(y=cbind(df$sapwood_fraction, 
                  df$heartwood_fraction),
          x=df$date, 
          ylab="Sap/Heart wood\nfraction", xlab="Year", type="l", col=c("yellowgreen", "brown4"), lty=1, lwd=1)
  
  # plot(I(dat$ppfd)~dat$i, ylab="PPFD", xlab="Year", type="l", col="yellowgreen")
  
  matplot(y=(cbind(df$leaf_lifespan, 
                   df$fineroot_lifespan)),
          x=df$height, 
          ylab="Leaf/fine root\nlifespan", xlab="Height", type="l", col=c("yellowgreen", "brown4"), lty=1, lwd=1)
  abline(h=1, col="grey")
}

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

life_history <- function() {
  ####
  # Initialisation files
  ####
  lho <- create_lho("tests/params/p_test_boreal.ini", 
                    "tests/data/ERAS_Monthly.csv", 
                    "tests/data/ERAS_Monthly.csv", 
                    init_co2 = 365)
  
  lho_high_co2 <- create_lho("tests/params/p_test_boreal_monthly_co2_high.ini", 
                             "tests/data/PlantFATE_monthly585.csv", 
                             "tests/data/PlantFATE_monthly585.csv", 
                             "tests/data/PlantFATE_yearly585.csv")
  
  lho_medium_co2 <- create_lho("tests/params/p_test_boreal_monthly_co2_medium.ini", 
                               "tests/data/PlantFATE_monthly245.csv", 
                               "tests/data/PlantFATE_monthly245.csv", 
                               "tests/data/PlantFATE_yearly245.csv")
  
  ####
  # Actual run outputs
  ####
  dt <- 1/12
  df <- run_for_dataset(lho, 1960, 1995, dt)
  df_high_co2 <- run_for_dataset(lho_high_co2, 2015, 2100, dt)
  df_medium_co2 <- run_for_dataset(lho_medium_co2, 2015, 2100, dt)
  
  # Plot results
  # TODO: boreal calibration - with the biomass data
  plot_plant_trajectory(df, df_high_co2, df_medium_co2, 1960, 1995)
  
  # Future scenarios
  
}


future_scenarios <- function(df, df_medium_co2, df_high_co2) {
  crossover_days = seq(as.Date("2015-01-01"), as.Date("2022-01-01"), by = "month")
 
  ###
  # Checking the fluxes!
  ###
  
  par(mfrow = c(2, 3))
  ### Assimilation
  plot(df$date, df$assim_gross, type = "l", xlab = "Time", ylab = "Assimulation Gross")
  
  plot(df_high_co2$date[df_high_co2$date %in% crossover_days], 
       df_high_co2$assim_gross[df_high_co2$date %in% crossover_days], 
       col = "red", type = "l", xlab = "Time", ylab = "Assimulation Gross")
  lines(df_medium_co2$date[df_medium_co2$date %in% crossover_days],
        df_medium_co2$assim_gross[df_medium_co2$date %in% crossover_days], 
        col = "yellow", type = "l")
  lines(df$date[df$date == crossover_days], 
        df$assim_gross[df$date == crossover_days], 
        type = "l")
  
  plot(df_high_co2$date, df_high_co2$assim_gross, col = "red", type = "l", xlab = "Time", ylab = "Assimulation Gross")
  lines(df_medium_co2$date, df_medium_co2$assim_gross, col = "yellow", type = "l")
  
  ### Total Respiration
  plot(df$date, df$rl + df$rs + df$rr, type = "l", xlab = "Time", ylab = "Total Respiration")
  
  plot(df_high_co2$date[df_high_co2$date %in% crossover_days], 
       df_high_co2$rl[df_high_co2$date %in% crossover_days] +
         df_high_co2$rs[df_high_co2$date %in% crossover_days] +
         df_high_co2$rr[df_high_co2$date %in% crossover_days], 
       col = "red", type = "l", xlab = "Time", ylab = "Assimulation Gross")
  lines(df_medium_co2$date[df_medium_co2$date %in% crossover_days],
        df_medium_co2$rl[df_medium_co2$date %in% crossover_days] + 
          df_medium_co2$rs[df_medium_co2$date %in% crossover_days] +
          df_medium_co2$rr[df_medium_co2$date %in% crossover_days], 
        col = "yellow", type = "l")
  lines(df$date[df$date == crossover_days], 
        df$rl[df$date == crossover_days] + 
          df$rs[df$date == crossover_days] + 
          df$rr[df$date == crossover_days], 
        type = "l")
  
  plot(df_high_co2$date, df_high_co2$rl + df_high_co2$rs + df_high_co2$rr, col = "red", 
       type = "l", xlab = "Time", ylab = "Total Respiration")
  lines(df_medium_co2$date, df_medium_co2$rl + df_medium_co2$rs + df_medium_co2$rr, col = "yellow", type = "l")
  
  ###
  # Difference in biomass under all scenarios
  ###
  
  # Function to create individual plots
  create_plot <- function(high_df, medium_df, y_var, y_label) {
    ggplot() +
      geom_line(data = high_df, aes(x = date, y = .data[[y_var]]), color = "red") +
      geom_line(data = medium_df, aes(x = date, y = .data[[y_var]]), color = "yellow") +
      labs(x = "Date", y = y_label) +
      theme_minimal()
  }
  
  # Create individual plots
  p1 <- create_plot(df_high_co2, df_medium_co2, "height", "Height")
  p2 <- create_plot(df_high_co2, df_medium_co2, "diameter", "Diameter")
  p3 <- create_plot(df_high_co2, df_medium_co2, "total_mass", "Total Mass")
  p4 <- create_plot(df_high_co2, df_medium_co2, "crown_area", "Crown Area")
  p_nitrogen <- create_plot(df_high_co2, df_medium_co2, "nitrogen", "Leaf Nitrogen (Jamx)") +
    theme(axis.title.x = element_text(margin = margin(t = 10)))  # Add some margin to the x-axis label
  
  # Combine plots
  combined_plot <- (p1 + p2) / (p3 + p4) / p_nitrogen +
    plot_layout(heights = c(1, 1, 1))  # Ensure equal height for all rows
  
  # Display the combined plot
  print(combined_plot)
}
