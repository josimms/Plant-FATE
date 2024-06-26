boreal_calibration <- function() {
  ###
  # AMAZON RUN
  # Original simulation used to test if I have changed anything!
  ###
  sim_v2 = new(Patch, "tests/params/p_test_v2.ini")
  sim_v2$init(1000, 1050) # This determines the steps
  sim_v2$simulate()
  sim_v2$close()
  
  output_dir = "pspm_output_test" # NOTE: changed from "pspm_output2", as matches close()
  prefix = "test_3spp_100yr"
  solver = "" # NOTE: changed from "main_ref2", as matches close()
  dir_v2 = paste0("",output_dir,"/",prefix,"",solver)
  
  ###
  # BOREAL CALIBRATION
  ###
  sim_boreal_monthly = new(Patch, "tests/params/p_test_boreal.ini")
  sim_boreal_monthly$init(1000, 1050) # This determines the steps
  ### TODO: need to make sure I save these in a different place!
  sim_boreal_monthly$simulate()
  sim_boreal_monthly$close()
  
  output_dir = "pspm_output_test" # NOTE: changed from "pspm_output2", as matches close()
  prefix = "boreal_montly_calibration"
  solver = "" # NOTE: changed from "main_ref2", as matches close()
  dir_boreal_monthly = paste0("",output_dir,"/",prefix,"",solver)
  
  ###
  # RUNS WITH CO2 SCENARIOS
  ###
  
  ### ssp 245
  sim_boreal_monthly_CO2_high = new(Patch, "tests/params/p_test_boreal_monthly_co2_high.ini")
  sim_boreal_monthly_CO2_high$init(1000, 1050) # This determines the steps
  sim_boreal_monthly_CO2_high$simulate()
  sim_boreal_monthly_CO2_high$close()
  
  output_dir = "pspm_output_test" # NOTE: changed from "pspm_output2", as matches close()
  prefix = "boreal_montly_calibration_high_CO2" # TODO: what should this be?
  solver = "" # NOTE: changed from "main_ref2", as matches close()
  dir_boreal_monthly_CO2_high = paste0("",output_dir,"/",prefix,"",solver)
  
  ### spp 585
  
  sim_boreal_monthly_CO2_medium = new(Patch, "tests/params/p_test_boreal_monthly_co2_medium.ini")
  sim_boreal_monthly_CO2_medium$init(1000, 1050) # This determines the steps
  sim_boreal_monthly_CO2_medium$simulate()
  sim_boreal_monthly_CO2_medium$close()
  
  output_dir = "pspm_output_test" # NOTE: changed from "pspm_output2", as matches close()
  prefix = "boreal_montly_calibration_medium_CO2"
  solver = "" # NOTE: changed from "main_ref2", as matches close()
  dir_boreal_monthly_CO2_medium = paste0("",output_dir,"/",prefix,"",solver)
  
  ###
  # READ IN DATA
  ###
  dat_boreal_monthly = read.delim(paste0(dir_boreal_monthly,"/D_PFATE.csv"), sep = ",")
  dat_boreal_monthly_CO2_medium = read.delim(paste0(sim_boreal_monthly_CO2_medium,"/D_PFATE.csv"), sep = ",")
  dat_boreal_monthly_CO2_high = read.delim(paste0(sim_boreal_monthly_CO2_high,"/D_PFATE.csv"), sep = ",")
  
  dat_v2 = read.delim(paste0(dir_v2,"/D_PFATE.csv"), sep = ",")
  
  ###
  # Plots for the differences betweeen the Boreal and Original!
  ###
  
  par(mfrow=c(2,2))
  matplot(y=cbind(dat_boreal_monthly$GPP, dat_boreal_monthly$NPP)*1e-3*365, x=dat_boreal_monthly$YEAR, type="l", lty=1, col=c("cyan3", "cyan3"), lty = c(1, 2), main = "Boreal", ylab="GPP, NPP (kgC/m2/yr)", xlab="Time (years)")
  matplot(y=cbind(dat_boreal_monthly_CO2_medium$GPP, dat_boreal_monthly_CO2_medium$NPP)*1e-3*365, x=dat_boreal_monthly_CO2_medium$YEAR, type="l", col=c("orange", "orange"), pch = c(1, 2), main = "Boreal", ylab="GPP, NPP (kgC/m2/yr)", xlab="Time (years)")
  matplot(y=cbind(dat_boreal_monthly_CO2_high$GPP, dat_boreal_monthly_CO2_high$NPP)*1e-3*365, x=dat_boreal_monthly_CO2_high$YEAR, type="l", col=c("red", "red"), lty = c(1, 2), main = "Boreal", ylab="GPP, NPP (kgC/m2/yr)", xlab="Time (years)")
  matplot(y=cbind(dat_v2$GPP, dat_v2$NPP)*1e-3*365, x=dat_v2$YEAR, type="l", col=c("green4", "green3"), lty = c(1, 2), main = "Original", ylab="GPP, NPP (kgC/m2/yr)", xlab="Time (years)")
  
  matplot(y=cbind(dat_boreal_monthly$GS), x=dat_boreal_monthly$YEAR, type="l", lty=1, col=c("cyan3"), main = "Boreal", ylab="Stomatal conductance (mol/m2/s)", xlab="Time (years)")
  matplot(y=cbind(dat_boreal_monthly_CO2_medium$GS), x=dat_boreal_monthly_CO2_medium$YEAR, type="l", lty=1, col=c("orange"), main = "Boreal", ylab="Stomatal conductance (mol/m2/s)", xlab="Time (years)")
  matplot(y=cbind(dat_boreal_monthly_CO2_high$GS), x=dat_boreal_monthly_CO2_high$YEAR, type="l", lty=1, col=c("red"), main = "Boreal", ylab="Stomatal conductance (mol/m2/s)", xlab="Time (years)")
  matplot(y=cbind(dat_v2$GS), x=dat_v2$YEAR, type="l", lty=1, col=c("cyan3"), main = "Original", ylab="Stomatal conductance (mol/m2/s)", xlab="Time (years)")
  
  ###
  # dist_v2 = read.delim(paste0(dir_v2,"/size_distributions.csv"), header=F, sep = ",")
  # dist_v2 = dist_v2[,-ncol(dist_v2)]
  # x = exp(seq(log(0.01), log(10), length.out=100))
  # names(dist_v2)[1:2] = c("YEAR", "SPP")
  # dist_amb_v2 = dist_v2 %>% filter(YEAR == max(YEAR)) %>% pivot_longer(cols=-(YEAR:SPP), names_to="size_class") %>% group_by(YEAR,size_class) %>% summarize(de = sum(value, na.rm=T)) %>% pivot_wider(names_from = size_class, values_from = de) %>% colMeans(na.rm=T)
  
  # dist_boreal = read.delim(paste0(dir_boreal,"/size_distributions.csv"), header=F, sep = ",")
  # dist_boreal = dist_boreal[,-ncol(dist_boreal)]
  # x = exp(seq(log(0.01), log(10), length.out=100))
  # names(dist_boreal)[1:2] = c("YEAR", "SPP")
  # dist_amb_boreal = dist_boreal %>% filter(YEAR == max(YEAR)) %>% pivot_longer(cols=-(YEAR:SPP), names_to="size_class") %>% group_by(YEAR,size_class) %>% summarize(de = sum(value, na.rm=T)) %>% pivot_wider(names_from = size_class, values_from = de) %>% colMeans(na.rm=T)
}

###
# Plotting function - not for Life History
###

plot_plant_trajectory = function(dat){
  dat$leaf_area = dat$crown_area * dat$lai
  dat$heartwood_fraction = 1-dat$sapwood_fraction
  
  # par(mfrow=c(4,2), mar=c(4,4,1,1), oma=c(1,1,1,1))
  
  # plot(dat$height~dat$diameter, type="l", ylab="Height", xlab="Diameter")
  # plot(dat$crown_area~I(dat$height*dat$diameter), type="l", ylab="Crown area", xlab="DH")
  # plot(I(dat$total_rep/dat$total_prod)~dat$height, type="l", ylab="Frac alloc to\nreproduction", xlab="Height")
  # plot(I(dat$total_rep/dat$total_prod)~dat$diameter, type="l", ylab="Frac alloc to\nreproduction", xlab="Diameter")
  
  par(mfrow=c(4,4), mar=c(4,8,1,1), oma=c(1,1,1,1), mgp=c(4,1,0), cex.lab=1.2)
  
  plot(dat$height~dat$i, ylab="Height", xlab="Year", type="l", lwd=2)
  plot(dat$diameter~dat$i, ylab="Diameter", xlab="Year", col="brown", type="l", lwd=2)
  
  matplot(y=cbind(dat$total_mass,
                  dat$total_rep,
                  dat$litter_mass,
                  dat$total_mass+dat$total_rep+dat$litter_mass,
                  dat$total_prod), 
          x=dat$i, col=c("green4", "purple", "yellow4", "black", "red"), log="", lty=c(1,1,1,1,2), lwd=c(1,1,1,2,1), type="l",
          ylab="Biomass pools", xlab="Year")
  abline(h=0, col="grey")
  
  matplot(y=cbind(dat$total_mass,
                  dat$leaf_mass,
                  dat$root_mass,
                  dat$coarse_root_mass,
                  dat$stem_mass),
          x=dat$i, col=c("black", "green3", "purple", "purple3", "brown"), log="", lty=c(1,1,1,1,1), lwd=c(1,1,1,1,1), type="l",
          ylab="Biomass pools", xlab="Year")
  abline(h=0, col="grey")
  
  
  matplot(y=cbind(dat$fitness), 
          x=dat$i, col=c("purple2", "magenta", "pink"), log="", lty=c(1,1,1), lwd=c(1,1,2), type="l",
          ylab="Fitness", xlab="Year")
  abline(h=0, col="grey")
  
  # plot(dat$total_mass~dat$i, ylab="Total biomass", xlab="Year")
  # plot(dat$total_mass~dat$i, ylab="Total biomass", xlab="Year")
  # 
  # points(y=dat$total_prod-dat$litter_mass, x=dat$i, type="l", col="red")
  
  # plot(y=1 - (dat$total_prod-dat$litter_mass)/dat$total_mass, x=dat$i, ylab="Total biomass", xlab="Year")
  
  # plot(dat$ppfd[01:1000]~dat$i[01:1000], type="l")
  # plot(dat$assim_gross[901:1000]~dat$ppfd[901:1000], type="l")
  
  
  matplot(y=cbind(dat$assim_gross/dat$crown_area,
                  dat$assim_net/dat$crown_area,
                  dat$assim_net/dat$assim_gross * max(dat$assim_gross/dat$crown_area)), 
          x=dat$i, col=c("green3", "green4", scales::alpha("yellow3", 0.5)), log="", lty=1, type="l", lwd=c(1,1,3),
          ylab=expression(atop("GPP, NPP", "(kg m"^"-2"*"Yr"^"-1"*")")), xlab="Year")
  abline(h=0, col="grey")
  
  matplot(y=cbind(dat$rr/dat$crown_area,
                  dat$rs/dat$crown_area,
                  dat$rl/dat$crown_area), 
          x=dat$i, col=c("pink2", "pink3", "pink4"), log="", lty=1, type="l",
          ylab="Respiration", xlab="Year")
  
  matplot(y=cbind(dat$tr/dat$crown_area,
                  dat$tl/dat$crown_area), 
          x=dat$i, col=c("orange3", "orange4"), log="", lty=1, type="l",
          ylab="Turnover", xlab="Year",
          add=F)
  
  plot(I(dat$transpiration/dat$crown_area/1000*1000)~dat$i, type="l", col="blue", ylab="Transpitation\n(mm/yr)")
  
  plot(dat$dpsi~dat$i, type="l", col="cyan", ylab=expression(atop(Delta*psi, "(MPa)")), xlab="Year")
  
  plot(dat$vcmax~dat$i, type="l", col="limegreen", ylab=expression(atop(V[cmax], "("*mu*"mol m"^"-2"*"s"^"-1"*")")), xlab="Year")
  
  plot(I(dat$leaf_area/dat$crown_area)~dat$i, ylab="LAI", xlab="Year", type="l")
  
  plot(I(dat$mortality)~dat$i, ylab="Cumulative\nMortality", xlab="Year", type="l")
  
  plot(I(dat$mortality_inst[dat$diameter<0.5])~dat$diameter[dat$diameter<0.5], ylab="Instantaneous\nmortality rate", xlab="Diameter", type="l")
  
  matplot(y=cbind(dat$sapwood_fraction, 
                  dat$heartwood_fraction),
          x=dat$i, 
          ylab="Sap/Heart wood\nfraction", xlab="Year", type="l", col=c("yellowgreen", "brown4"), lty=1, lwd=1)
  
  # plot(I(dat$ppfd)~dat$i, ylab="PPFD", xlab="Year", type="l", col="yellowgreen")
  
  matplot(y=(cbind(dat$leaf_lifespan, 
                   dat$fineroot_lifespan)),
          x=dat$height, 
          ylab="Leaf/fine root\nlifespan", xlab="Height", type="l", col=c("yellowgreen", "brown4"), lty=1, lwd=1)
  abline(h=1, col="grey")
}

###
# Life history
###

lho_demo <- function() {
  params_file = "tests/params/p_test_v2.ini"
  lho = new(LifeHistoryOptimizer, params_file)
  lho$set_i_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv") # TODO: What does the i and a stand for?
  lho$set_a_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  lho$set_co2File("")
  lho$init_co2(414)
  print(c(lho$env$clim_inst$co2,
          lho$env$clim_acclim$co2))
  lho$init()
  
  params_file_boreal = "tests/params/p_test_boreal.ini"
  lho_2 = new(LifeHistoryOptimizer, params_file_boreal)
  lho_2$set_i_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv") # TODO: What does the i and a stand for?
  lho_2$set_a_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  lho_2$set_co2File("")
  lho_2$init_co2(414)
  print(c(lho_2$env$clim_inst$co2,
          lho_2$env$clim_acclim$co2))
  lho_2$init()
  
  dt = 0.1
  df = read.csv(text="", col.names = lho$get_header())
  for (t in seq(2000,2500,dt)){
    # growth data for each of the timesteps
    lho$grow_for_dt(t,0.1)
    df[nrow(df)+1,] = lho$get_state(t+dt)
  }
  
  df_2 = read.csv(text="", col.names = lho_2$get_header())
  for (t in seq(2000,2500,dt)){
    # growth data for each of the timesteps
    lho_2$grow_for_dt(t,0.1)
    df_2[nrow(df_2)+1,] = lho_2$get_state(t+dt)
  }
  
  plot_plant_trajectory(df)
  plot_plant_trajectory(df_2)
  
  
}

