###
# Future scenarios
###

future_scenarios <- function(df, df_medium_co2, df_high_co2) {
  crossover_days = seq(as.Date("2015-01-01"), as.Date("2022-01-01"), by = "month")
  
  path_test <- "/home/josimms/Documents/Austria/Plant-FATE/tests/data"
  monthy_dataset <- fread(file.path(path_test, "ERAS_Monthly.csv"))
  monthy_dataset$Date <- as.Date(paste(monthy_dataset$Year, monthy_dataset$Month, "01", sep = "-"), format = "%Y-%m-%d")
  daily_dataset <- fread(file.path(path_test, "ERAS_dataset.csv"))
  daily_dataset$Date <- as.Date(paste(daily_dataset$Year, daily_dataset$Month, "01", sep = "-"), format = "%Y-%m-%d")
  
  ###
  # Checking the fluxes!
  ###
  
  par(mfrow = c(2, 3))
  ### Assimilation
  plot(df$date, df$assim_gross, type = "l", xlab = "Time", ylab = "Assimulation Gross, (kgC m-2 s-1)")
  points(monthy_dataset$Date, 1e-6 * monthy_dataset$GPP, pch = "x", col = "blue") # mean µmolC m⁻² s⁻¹ per month to kgC m-2 s-1
  
  plot(df_high_co2$date[df_high_co2$date %in% crossover_days], 
       df_high_co2$assim_gross[df_high_co2$date %in% crossover_days], 
       col = "red", type = "l", xlab = "Time", ylab = "Assimulation Gross (kgC m-2 day-1)")
  lines(df_medium_co2$date[df_medium_co2$date %in% crossover_days],
        df_medium_co2$assim_gross[df_medium_co2$date %in% crossover_days], 
        col = "yellow", type = "l")
  lines(df$date[df$date %in% crossover_days], 
        df$assim_gross[df$date %in% crossover_days], 
        type = "l")
  points(monthy_dataset$Date, 1e-6 * 0.04401 * monthy_dataset$GPP, pch = "x", col = "blue") # mean µmolC m⁻² s⁻¹ per month to kgC m-2 s-1
  
  plot(df_high_co2$date, df_high_co2$assim_gross, col = "red", type = "l", xlab = "Time", ylab = "Assimulation Gross")
  lines(df_medium_co2$date, df_medium_co2$assim_gross, col = "yellow", type = "l")
  points(monthy_dataset$Date, 1e-6 * 0.04401 * monthy_dataset$GPP, pch = "x", col = "blue") # mean µmolC m⁻² s⁻¹ per month to kgC m-2 s-1
  
  ### Total Respiration
  plot(df$date, df$rl + df$rs + df$rr, type = "l", xlab = "Time", ylab = "Total Respiration (kgC m-2 day-1)")
  
  plot(df_high_co2$date[df_high_co2$date %in% crossover_days], 
       df_high_co2$rl[df_high_co2$date %in% crossover_days] +
         df_high_co2$rs[df_high_co2$date %in% crossover_days] +
         df_high_co2$rr[df_high_co2$date %in% crossover_days], 
       col = "red", type = "l", xlab = "Time", ylab = "Total Respiration (kgC m-2 day-1)")
  lines(df_medium_co2$date[df_medium_co2$date %in% crossover_days],
        df_medium_co2$rl[df_medium_co2$date %in% crossover_days] + 
          df_medium_co2$rs[df_medium_co2$date %in% crossover_days] +
          df_medium_co2$rr[df_medium_co2$date %in% crossover_days], 
        col = "yellow", type = "l")
  lines(df$date[df$date %in% crossover_days], 
        df$rl[df$date %in% crossover_days] + 
          df$rs[df$date %in% crossover_days] + 
          df$rr[df$date %in% crossover_days], 
        type = "l")
  
  plot(df_high_co2$date, df_high_co2$rl + df_high_co2$rs + df_high_co2$rr, col = "red", 
       type = "l", xlab = "Time", ylab = "Total Respiration (kgC m-2 day-1)")
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
  p1 <- create_plot(df_high_co2, df_medium_co2, "height", "Height (m)")
  p3 <- create_plot(df_high_co2, df_medium_co2, "total_mass", "Total Mass (kgC m-2)")
  # Dependent on height, TODO: get rid of the next plots
  p_nitrogen <- create_plot(df_high_co2, df_medium_co2, "nitrogen", "Leaf Nitrogen (Jamx) (umol m-2 s-1)") +
    theme(axis.title.x = element_text(margin = margin(t = 10)))  # Add some margin to the x-axis label
  
  # Combine plots
  combined_plot <- (p1 + p3) / p_nitrogen +
    plot_layout(heights = c(1, 1))  # Ensure equal height for all rows
  
  # Display the combined plot
  print(combined_plot)
}

###
# Plotting function - for Life History with boreal data!
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
  
  ###
  # Basic output
  ###
  
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
  
  #summary(df$mortality)
  
  #plot(I(df$mortality)~df$date, ylab="Cumulative\nMortality", xlab="Year", type="l")
  
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
  title(main = "Hyytiälä Data and Model Output", outer = T)
  
  
  ####
  # Plotting Properties
  ####
  par(mfrow=c(4,2), mar=c(4,4,1,1), oma=c(1,1,1,1))
  plot(df_high_co2$height~df_high_co2$diameter, col = "red", ylab="Height", xlab="Diameter", pch = "x")
  points(0.01*boreal$smearII_data$amount[boreal$smearII_data$variable == "pine diameter BA weighted mean"],
         boreal$smearII_data$amount[boreal$smearII_data$variable == "pine height BA weighted mean"])
  points(0.01*boreal$smearII_data$amount[boreal$smearII_data$variable == "pine DBH arithmetic mean"],
         boreal$smearII_data$amount[boreal$smearII_data$variable == "pine height arithmetic mean"], pch = 2)
  points(0.01*boreal$smearII_data$amount[boreal$smearII_data$variable == "BA weighted mean DBH pine"],
         boreal$smearII_data$amount[boreal$smearII_data$variable == "BA weighted mean height pine"], pch = 3)
  points(df_medium_co2$height~df_medium_co2$diameter, col = "orange", pch = "x")
  points(df$height~df$diameter, pch = "x")
  
  plot(df$crown_area~I(df$height*df$diameter), type="l", ylab="Crown area", xlab="DH")
  plot(I(df$total_rep/df$total_prod)~df$height, type="l", ylab="Frac alloc to\nreproduction", xlab="Height")
  plot(I(df$total_rep/df$total_prod)~df$diameter, type="l", ylab="Frac alloc to\nreproduction", xlab="Diameter")
}


###
# Life history final results
###

life_history <- function() {
  ####
  # Finial Initialisation files
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
  future_scenarios(df, df_medium_co2, df_high_co2)
}
