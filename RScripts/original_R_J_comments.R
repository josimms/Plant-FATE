original_demo <- function() { # ANNOTATED VERSION
  # new is a function that calls a class object from the initial file
  # TODO: change the values here!
  # Reads csv files in the test data folder for species traits and meteorology
  # TODO: CO2 file is a different file
  # The variables and units are defined / explained here as well!
  sim = new(Patch, "tests/params/p_test_v2.ini")
  # Here the step size is being chosen
  # TODO: steps are monthly in this case, what exactly do the 1000 / 1500 refer to?
  sim$init(1000, 1050) # This determines the steps
  # Now the model is simulated
  sim$simulate()
  # Close the files that simulate opens
  # These are defined in a location in the package
  # TODO: maybe change this location when simulating so I can retain different simulations
  sim$close()
  # The functions here are defined in the plantfate_patch.cpp
  
  output_dir = "pspm_output_test" # NOTE: changed from "pspm_output2", as matches close()
  prefix = "test_3spp_100yr"
  solver = "" # NOTE: changed from "main_ref2", as matches close()
  dir = paste0("",output_dir,"/",prefix,"",solver)
  
  # NOTE: changed to csv not txt
  Zp = read.delim(paste0(dir,"/z_star.csv"), header=F, col.names = paste0("V", 1:50), sep = ",")
  lai_v = read.delim(paste0(dir,"/lai_profile.csv"), header=F, col.names = paste0("V", 1:27), sep = ",")
  
  par(mfrow=c(1,3), mar=c(4,4,1,1))
  matplot(Zp$V1, Zp[,-1], lty=1, col=rainbow(n = 10, start = 0, end = 0.85), type="l", las=1, xlab="Time (years)", ylab="Z*")
  
  
  n_year = nrow(Zp)
  
  matplot(y=1:24, x=t(-lai_v[,3:26]+lai_v[,2:25]), lty=1, col=rainbow(n = n_year, start = 0, end = 0.85, alpha=20/n_year), type="l",
          las=1, xlab="Leaf area density", ylab="Height")
  matplot(y=1:25, x=t(lai_v[,2:26]), lty=1, col=rainbow(n = n_year, start = 0, end = 0.85, alpha=20/n_year), type="l",
          las=1, xlab="Cumulative LAI", ylab="Height")
  
  
  # dat = read.delim(paste0(dir,"/AmzFACE_D_PFATE_ELE_HD.csv"), sep = ",")
  # TODO: Has this been seperated into many files - or is this validation data?
  dat = read.delim(paste0(dir,"/D_PFATE.csv"), sep = ",")
  
  par(mfrow=c(1,3), mar=c(4,4,1,1))
  
  plot(dat$LAI~dat$YEAR, type="l", col="red3", ylim=c(0,max(dat$LAI,6.5)), xlab="Time (years)", ylab="Total LAI")
  
  matplot(y=cbind(dat$GPP, dat$NPP)*1e-3*365, x=dat$YEAR, type="l", lty=1, col=c("green4", "green3"), ylab="GPP, NPP (kgC/m2/yr)", xlab="Time (years)")
  
  matplot(y=cbind(dat$GS), x=dat$YEAR, type="l", lty=1, col=c("cyan3"), ylab="Stomatal conductance (mol/m2/s)", xlab="Time (years)")
  
  dist = read.delim(paste0(dir,"/size_distributions.csv"), header=F, sep = ",")
  dist = dist[,-ncol(dist)]
  x = exp(seq(log(0.01), log(10), length.out=100))
  
  names(dist)[1:2] = c("YEAR", "SPP")
  dist_amb = dist %>% filter(YEAR == max(YEAR)) %>% pivot_longer(cols=-(YEAR:SPP), names_to="size_class") %>% group_by(YEAR,size_class) %>% summarize(de = sum(value, na.rm=T)) %>% pivot_wider(names_from = size_class, values_from = de) %>% colMeans(na.rm=T)
  
  matplot(y=cbind(as.numeric(dist_amb[gtools::mixedsort(names(dist_amb))][-1])
  )*1e-2*1e4, # Convert stems m-1 m-2 --> stems cm-1 ha-1
  x=x, type="l", log="xy", lty=1, col=c("black", "yellow3"), 
  xlim=c(0.01, 1.2), ylim=c(1e-4, 1000), ylab="Density (stems/cm/ha)", xlab="Diameter (m)", las=0)
  
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

