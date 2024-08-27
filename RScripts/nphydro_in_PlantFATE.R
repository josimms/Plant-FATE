
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
  df_original <- run_for_dataset(lho_original, 2000, 2015, dt)
  save(df_original, file = "./RScripts/df_original.RData")
}

### Run with the changing phydro values

other_package_loaded = F
if (other_package_loaded) {
  devtools::install_github("josimms/Plant-FATE@develop_calibration", force = TRUE)
}

lho_Ib <- create_lho("tests/params/p_test_v2.ini", 
                     "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                     "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                     init_co2 = 365)
df_Ib <- run_for_dataset(lho_Ib, 2000, 2015, dt)

### Plot

translation = seq(0.6, 2, length.out = 20)
cols = rainbow(length(translation))

par(mfrow = c(2, 2))
plot(df_original$date, df_original$height, ylab = "Height", xlab = "Date", type = "l")
lines(df_Ib$date, df_Ib$height, lty = 2)
plot(df_original$date, df_original$root_mass + df_original$coarse_root_mass, ylab = "Root Mass", xlab = "Date", type = "l")
lines(df_original$date, df_Ib$root_mass + df_Ib$coarse_root_mass, lty = 2)
par(new = T)
plot(NULL,
     xlim = c(df_original$date[1], df_original$date[nrow(df_original)]),
     ylim = 0.5 + c(0, 0.47),
     axes = FALSE,
     xlab = "", ylab = "")
axis(4)
mtext("Ib", side = 4, line = 3, cex = 0.8)
count = 1
for (i in translation) {
  lines(df_original$date, 0.5 + i*(df_original$root_mass + df_original$coarse_root_mass), col = cols[count], lty = 2)
  count = count + 1
}
legend("topleft", c("Root Mass", "Translation = 2", "Translation = 0.01"), 
       col = c("black", cols[1], cols[length(translation)]), 
       lty = 1, bty = "n", cex = 0.75)
lines(df_Ib$date, df_Ib$root_mass, lty = 2)
plot(df_original$date, df_original$lai, ylab = "LAI", xlab = "Date", type = "l")
lines(df_Ib$date, df_Ib$lai, lty = 2)
plot(df_original$date, df_original$assim_gross, ylab = "Assim gross", xlab = "Date", type = "l", ylim = c(0, max(df_original$assim_gross)))
lines(df_Ib$date, df_Ib$assim_gross, lty = 2)

###
# Fit the Ib parameter
### 

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
  print(lho$) # $infra_translation <- param
  lho$init()
  return(lho)
}

sa_ib <- list()
potential_parameters = 1:2
for (i in potential_parameters) {
  lho_temp <- create_lho_params("tests/params/p_test_v2.ini", 
                         "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                         "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                         co2File = "",
                         init_co2 = 365,
                         i)
  sa_ib[[which(potential_parameters == i)]] <- run_for_dataset(lho_temp, 2000, 2015, dt)
  sa_ib[[which(potential_parameters == i)]]$Ib_value <- i
}

###
# Plant-FATE aggregation
###

amazon_sim <- run_simulation("tests/params/p_test_v2.ini", 1000, 1050, "test_3spp_100yr")
amazon_sim_data <- read_sim_data(amazon_sim$dir_path)

plot(amazon_sim_data$D$YEAR, amazon_sim_data$D$GPP)

# TODO: calibration files updated with the infrastructure parameter

# TODO: plot the amazon with different levels of underground investment
  # TODO: how should this relate to the root parameters?




