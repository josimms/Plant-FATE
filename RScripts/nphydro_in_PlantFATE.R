###
###
# Libraries
###

library(ggplot2)
library(patchwork)
library(parallel)

###
# Plotting function
###

# Define the function
create_combined_plot_Ib_value <- function(df_original, sa_ib_all, color_scale) {
  
  # Plot 1: Height
  p1 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = height, color = Ib_value), shape = 45, size = 3) +
    geom_line(data = df_original, aes(x = date, y = height), color = "black") +
    labs(x = "Date", y = "Height") +
    color_scale +
    theme_minimal()
  
  # Plot 2: Root Mass
  p2 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = root_mass, color = Ib_value), shape = 45, size = 3) +
    geom_line(data = df_original, aes(x = date, y = root_mass), color = "black") +
    labs(x = "Date", y = "Root Mass") +
    color_scale +
    theme_minimal()
  
  # Plot 3: Ib Value
  p3 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = Ib_min + Ib_value * (root_mass), color = Ib_value), shape = 45, size = 3) +
    labs(x = "Date", y = "Ib Value") +
    color_scale +
    theme_minimal()
  
  # Plot 4: LAI
  p4 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = lai, color = Ib_value), shape = 45, size = 3) +
    geom_line(data = df_original, aes(x = date, y = lai), color = "black") +
    labs(x = "Date", y = "LAI") +
    color_scale +
    theme_minimal()
  
  # Plot 5: Assim gross
  p5 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = assim_gross, color = Ib_value), shape = 45, size = 3) +
    geom_line(data = df_original, aes(x = date, y = assim_gross), color = "black") +
    labs(x = "Date", y = "Assim gross") +
    color_scale +
    theme_minimal()
  
  # Combine plots
  combined_plot <- (p1 + p2) / (p3 + p4) / p5 +
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  
  # Return the combined plot
  return(combined_plot)
}

# Define the function
create_combined_plot_Ib_min <- function(df_original, sa_ib_all, color_scale) {
  
  # Plot 1: Height
  p1 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = height, color = Ib_min), shape = 45, size = 3) +
    geom_line(data = df_original, aes(x = date, y = height), color = "black") +
    labs(x = "Date", y = "Height") +
    color_scale +
    theme_minimal()
  
  # Plot 2: Root Mass
  p2 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = root_mass, color = Ib_min), shape = 45, size = 3) +
    geom_line(data = df_original, aes(x = date, y = root_mass), color = "black") +
    labs(x = "Date", y = "Root Mass") +
    color_scale +
    theme_minimal()
  
  # Plot 3: Ib Min
  p3 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = Ib_min + Ib_value * (root_mass), color = Ib_min), shape = 45, size = 3) +
    labs(x = "Date", y = "Ib Min") +
    color_scale +
    theme_minimal()
  
  # Plot 4: LAI
  p4 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = lai, color = Ib_min), shape = 45, size = 3) +
    geom_line(data = df_original, aes(x = date, y = lai), color = "black") +
    labs(x = "Date", y = "LAI") +
    color_scale +
    theme_minimal()
  
  # Plot 5: Assim gross
  p5 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = assim_gross, color = Ib_min), shape = 45, size = 3) +
    geom_line(data = df_original, aes(x = date, y = assim_gross), color = "black") +
    labs(x = "Date", y = "Assim gross") +
    color_scale +
    theme_minimal()
  
  # Combine plots
  combined_plot <- (p1 + p2) / (p3 + p4) / p5 +
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  
  # Return the combined plot
  return(combined_plot)
}

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
     ylim = 0.9 + c(0, 0.47),
     axes = FALSE,
     xlab = "", ylab = "")
axis(4, col.axis = 'red', col.ticks = 'red')
mtext("Ib", side = 4, line = 3, cex = 0.8, col = "red")
lines(df_original$date, 0.7435897 + 4*(df_original$root_mass), col = "red", lty = 2)
legend("topleft", c("Original Result", "Nitrogen Result", "Ib parameter"), 
       col = c("black", "black", "red"), 
       lty = c(1, 2, 2), bty = "n")
lines(df_Ib$date, df_Ib$root_mass, lty = 2)
plot(df_original$date, df_original$lai, ylab = "LAI", xlab = "Date", type = "l")
lines(df_Ib$date, df_Ib$lai, lty = 2)
plot(df_original$date, df_original$assim_gross, ylab = "Assim gross", xlab = "Date", type = "l", ylim = c(0.02, max(df_Ib$assim_gross)))
lines(df_Ib$date, df_Ib$assim_gross, lty = 2)

plot(df_Ib$date, df_Ib$crown_area)

###
# Fit the Ib parameter Amazon
### 

### Extra Functions

create_lho_params <- function(params_file, i_metFile, a_metFile, co2File = "", init_co2 = NULL, param) {
  lho <- new(LifeHistoryOptimizer, params_file)
  lho$set_i_metFile(i_metFile)
  lho$set_a_metFile(a_metFile)
  lho$set_co2File(co2File)
  if (!is.null(init_co2)) lho$init_co2(init_co2)
  print(c(lho$env$clim_inst$co2, lho$env$clim_acclim$co2))
  lho$par0$infra_translation <- param
  lho$par0$infra_min <- 0.55
  lho$init()
  return(lho)
}

potential_parameters <- seq(2, 4, length.out = 40)

# Use mclapply for parallel processing
sa_ib <- mclapply(potential_parameters, function(i) {
  lho_temp <- create_lho_params("tests/params/p_test_v2.ini", 
                                "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                                "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                                co2File = "",
                                init_co2 = 365,
                                i)
  result <- run_for_dataset(lho_temp, 2000, 2015, dt)
  result$parameter <- i
  return(result)
}, mc.cores = detectCores()-2)

# Combine the results into a single data frame
sa_ib_all <- do.call(rbind, sa_ib)

# Plot
color_scale <- scale_color_gradientn(colours = rainbow(5), name = "Ib Value")
combined_plot <- create_combined_plot(df_original, sa_ib_all, color_scale)
print(combined_plot)

###
# Two variables
###

# Function to create LifeHistoryOptimizer objects
create_lho_params_two <- function(params_file, i_metFile, a_metFile, co2File = "", init_co2 = NULL, Ib_transfrom, Ib_min) {
  lho <- new(LifeHistoryOptimizer, params_file)
  lho$set_i_metFile(i_metFile)
  lho$set_a_metFile(a_metFile)
  lho$set_co2File(co2File)
  if (!is.null(init_co2)) lho$init_co2(init_co2)
  print(c(lho$env$clim_inst$co2, lho$env$clim_acclim$co2))
  lho$par0$infra_translation <- Ib_transfrom
  lho$par0$infra_min <- Ib_min
  lho$init()
  return(lho)
}

# Generate all combinations of potential parameters
Ib_transform_values <- seq(2, 5, length.out = 40)
Ib_min_values <- seq(0.5, 1, length.out = 40)

# Create a data frame of all parameter combinations
param_grid <- expand.grid(Ib_value = Ib_transform_values, Ib_min = Ib_min_values)

# Use mclapply for parallel processing over the parameter grid
sa_ib_both <- mclapply(1:nrow(param_grid), function(i) {
  params <- param_grid[i, ]
  
  # Initialize result as NULL, to be filled later
  result <- NULL
  
  # Error handling with tryCatch
  lho_temp <- create_lho_params_two("tests/params/p_test_v2.ini", 
                                    "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                                    "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                                    co2File = "",
                                    init_co2 = 365,
                                    Ib_transfrom = params$Ib_value,
                                    Ib_min = params$Ib_min)
  result <- run_for_dataset(lho_temp, 2000, 2015, dt)
  result$Ib_value <- params$Ib_value
  result$Ib_min <- params$Ib_min
  
  return(result)
}, mc.cores = detectCores()-2)

# Combine the results into a single data frame
sa_ib_all_both <- do.call(rbind, sa_ib_both)

# Plot
color_scale_Ib_value <- scale_color_gradientn(colours = rainbow(5), name = "Ib Value")
combined_plot <- create_combined_plot_Ib_value(df_original, sa_ib_all_both[sa_ib_all_both$Ib_min == unique(sa_ib_all_both$Ib_min)[20],], color_scale_Ib_value)
print(combined_plot)

color_scale_Ib_min <- scale_color_gradientn(colours = rainbow(5), name = "Ib Min")
combined_plot <- create_combined_plot_Ib_min(df_original, sa_ib_all_both[sa_ib_all_both$Ib_value == 4,], color_scale_Ib_min)
print(combined_plot)

###
# Rosendahl / Boreal
###


###
# Plant-FATE aggregation
###

# amazon_sim <- run_simulation("tests/params/p_test_v2.ini", 1000, 1050, "test_3spp_100yr")
# amazon_sim_data <- read_sim_data(amazon_sim$dir_path)

# plot(amazon_sim_data$D$YEAR, amazon_sim_data$D$GPP)

# TODO: calibration files updated with the infrastructure parameter

# TODO: plot the amazon with different levels of underground investment
  # TODO: how should this relate to the root parameters?




