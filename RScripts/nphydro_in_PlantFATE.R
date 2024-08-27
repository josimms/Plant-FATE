###
# Extra Functions
###

create_lho_params <- function(params_file, i_metFile, a_metFile, co2File = "", init_co2 = NULL, param) {
  lho <- new(LifeHistoryOptimizer, params_file)
  lho$set_i_metFile(i_metFile)
  lho$set_a_metFile(a_metFile)
  lho$set_co2File(co2File)
  if (!is.null(init_co2)) lho$init_co2(init_co2)
  print(c(lho$env$clim_inst$co2, lho$env$clim_acclim$co2))
  #lho$traits0$inv
  lho$init()
  lho$parameter_x <- param
  #cbind .... with the parameter value
  return(lho)
}

lapply()

parallel::mclapply()

###
# Testing the Ib response
###

### Use the standard version of the model!

already_run = T
if (already_run) {
  load("./RScripts/df_original.RData")
} else {
  # devtools::install_github("jaideep777/Plant-FATE@develop")
  
  # Run the model to get standard outputs
  lho_original <- create_lho("tests/params/p_test_v2.ini", 
                             "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                             "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                             init_co2 = 365)
  df_original <- run_for_dataset(lho, 2000, 2015, dt)
  save(df_original, file = "./RScripts/df_original.RData")
}

### Run with the changing phydro values

other_package_loaded = F
if (other_package_loaded) {
  devtools::install_github("josimms/Plant-FATE@develop_calibration")
}

lho_Ib <- create_lho("tests/params/p_test_v2.ini", 
                     "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                     "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                     init_co2 = 365)
df_Ib <- run_for_dataset(lho, 2000, 2015, dt)

### Plot

translation = 0.01:2
cols = rainbow(length(translation))

par(mfrow = c(2, 2))
plot(df_original$date, df_original$height, ylab = "Height", xlab = "Date", type = "l")
lines(df_Ib$date, df_Ib$height, lty = 2)
plot(df_original$date, df_original$root_mass + df_original$coarse_root_mass, ylab = "Root Mass", xlab = "Date", type = "l")
count = 1
for (i in translation) {
  line(df_original$date, i*(df_original$root_mass + df_original$coarse_root_mass), col = cols[count], lty = 2)
  count = count + 1
}
legend("topleft", c("Root Mass", "Translation = 2", "Translation = 0.01"), col = c("black", cols[1], cols[length(translation)]), lty = 1, bty = "n")
lines(df_original$date, df_original$root_mass + df_original$coarse_root_mass)
lines(df_Ib$date, df_Ib$root_mass, lty = 2)
plot(df_original$date, df_original$lai, ylab = "LAI", xlab = "Date", type = "l")
lines(df_Ib$date, df_Ib$lai, lty = 2)
# TODO: GPP isn't an output of Life History!
plot(df_original$date, ylab = "LAI", xlab = "Date", type = "l")
lines(df_Ib$date, lty = 2)

###
# Plant-FATE aggregation
###

amazon_sim <- run_simulation("tests/params/p_test_v2.ini", 1000, 1050, "test_3spp_100yr")
amazon_sim_data <- read_sim_data(amazon_sim$dir_path)

plot(amazon_sim_data$D$YEAR, amazon_sim_data$D$GPP)

# TODO: calibration files updated with the infrastructure parameter

# TODO: plot the amazon with different levels of underground investment
  # TODO: how should this relate to the root parameters?




