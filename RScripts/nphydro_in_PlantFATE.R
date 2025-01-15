###
###
# Libraries
###

library(ggplot2)
library(patchwork)
library(parallel)
library(dplyr)

###
# plot the marginal behaviour
###

dF_dzeta = function(zeta, N) {
  alpha = 0.1
  ajmax = 800
  a = 1
  out = (alpha * ajmax * N)/(a * zeta)^2 - a * 0.1
}

# Define ranges for N and Ib
N_range <- seq(0.005, 0.1, length.out = 10)
zeta_range <- seq(0.5, 1.5, length.out = 40)

# Create a grid of all combinations of N and Ib
grid <- expand.grid(N = N_range, zeta = zeta_range)
grid$dF_dzeta <- mapply(dF_dzeta, grid$zeta, grid$N)

p <- ggplot(grid, aes(x = zeta, y = dF_dzeta, color = factor(N))) +
  geom_line() +
  scale_color_viridis_d() +  # Use viridis color palette for better distinction
  labs(x = expression(a),
       y = expression(frac(dF, dzeta)),
       color = "N (g g-1)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold"),  # Larger, bold title
    axis.title = element_text(size = 18, face = "bold"),  # Larger axis titles
    axis.text = element_text(size = 14),  # Larger axis tick labels
    legend.title = element_text(size = 16, face = "bold"),  # Larger legend title
    legend.text = element_text(size = 14),  # Larger legend text
    legend.position = "right",
    axis.title.y = element_text(angle = 0, vjust = 0.5)  # Keep y-axis label vertical
  )

print(p)

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
  
  # Plot 4: LAI
  p4 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = crown_area, color = Ib_value), shape = 45, size = 3) +
    geom_line(data = df_original, aes(x = date, y = crown_area), color = "black") +
    labs(x = "Date", y = "Crown Area") +
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
  combined_plot <- (p1 + p2) / (p4 + p3) / p5 +
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  
  # Return the combined plot
  return(combined_plot)
}

# Define the function
create_combined_plot_zeta <- function(df_original, sa_ib_all, color_scale) {
  
  # Plot 1: Height
  p1 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = height, color = zeta), shape = 45, size = 3) +
    geom_line(data = df_original, aes(x = date, y = height), color = "black") +
    labs(x = "Date", y = "Height") +
    color_scale +
    theme_minimal()
  
  # Plot 2: Root Mass
  p2 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = root_mass, color = zeta), shape = 45, size = 3) +
    geom_line(data = df_original, aes(x = date, y = root_mass), color = "black") +
    labs(x = "Date", y = "Root Mass") +
    color_scale +
    theme_minimal()
  
  # Plot 3: Ib Min
  p3 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = Ib_value * zeta, color = zeta), shape = 45, size = 3) +
    labs(x = "Date", y = expression(zeta)) +
    color_scale +
    theme_minimal()
  
  # Plot 4: LAI
  p4 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = crown_area, color = zeta), shape = 45, size = 3) +
    geom_line(data = df_original, aes(x = date, y = crown_area), color = "black") +
    labs(x = "Date", y = "Crown Area") +
    color_scale +
    theme_minimal()
  
  # Plot 5: Assim gross
  p5 <- ggplot() +
    geom_point(data = sa_ib_all, aes(x = date, y = assim_gross, color = zeta), shape = 45, size = 3) +
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
  devtools::install_github("jaideep777/Plant-FATE@develop", force = T)
  
  # Run the model to get standard outputs
  lho_original <- create_lho("tests/params/p_test_v2.ini", 
                             "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                             "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                             init_co2 = 365)
  df_original <- run_for_dataset(lho_original, 2000, 2015, dt)
  save(df_original, file = "./RScripts/df_original.RData")
}

### Run with the changing phydro values

create_lho_a <- function(params_file, i_metFile, a_metFile, co2File = "", init_co2 = NULL, a) {
  lho <- new(LifeHistoryOptimizer, params_file)
  lho$set_i_metFile(i_metFile)
  lho$set_a_metFile(a_metFile)
  lho$set_co2File(co2File)
  if (!is.null(init_co2)) lho$init_co2(init_co2)
  print(c(lho$env$clim_inst$co2, lho$env$clim_acclim$co2))
  lho$par0$infra_translation <- a
  lho$init()
  return(lho)
}

other_package_loaded = F
if (other_package_loaded) {
  devtools::install_github("jaideep777/Plant-FATE@develop", force = TRUE)
}

dt = 1/12
lho_Ib_high <- create_lho_a("tests/params/p_test_v2.ini", 
                         "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                         "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                         init_co2 = 365,
                         a = 1.2)
df_Ib_high <- run_for_dataset(lho_Ib_high, 2000, 2015, dt)

lho_Ib_medium <- create_lho_a("tests/params/p_test_v2.ini", 
                           "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                           "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                           init_co2 = 365,
                           a = 1)
df_Ib_medium <- run_for_dataset(lho_Ib_medium, 2000, 2015, dt)

lho_Ib_medium_low <- create_lho_a("tests/params/p_test_v2.ini", 
                            "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                            "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                            init_co2 = 365,
                            a = 0.75)
df_Ib_medium_low <- run_for_dataset(lho_Ib_medium_low, 2000, 2015, dt)

df_Ib_low <- create_lho_a("tests/params/p_test_v2.ini", 
                           "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                           "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                           init_co2 = 365,
                           a = 0.5)
df_Ib_low <- run_for_dataset(df_Ib_low, 2000, 2015, dt)

### Plot

# Set up the layout for 3 plots on top, 2 on bottom
par(mfrow = c(2, 3))

# Plot 1: Height
plot(df_original$date, df_original$height, ylab = "Height, m", xlab = "Date", type = "l", 
     ylim = c(df_original$height[1], max(df_Ib$height)), cex = 1.25)
lines(df_Ib_high$date, df_Ib_high$height, lty = 3, col = "red")
lines(df_Ib_low$date, df_Ib_low$height, col = "blue")
lines(df_Ib_medium$date, df_Ib_medium$height, col = "orange")
lines(df_Ib_medium_low$date, df_Ib_medium_low$height, col = "green")
legend("bottomright", legend = c(0.5, 0.75, 1, 1.2), title = "a",
       col = c("red", "orange", "green", "blue"), 
       lty = 1, bty = "n", cex = 1.25)

# Plot 2: Crown Area
plot(df_original$date, df_original$crown_area, ylab = "Crown Area, m2", xlab = "Date", 
     type = "l", ylim = c(df_original$crown_area[1], max(df_original$crown_area)), cex = 1.25)
lines(df_Ib_high$date, df_Ib_high$crown_area, lty = 3, col = "red")
lines(df_Ib_low$date, df_Ib_low$crown_area, col = "blue")
lines(df_Ib_medium$date, df_Ib_medium$crown_area, col = "orange")
lines(df_Ib_medium_low$date, df_Ib_medium_low$crown_area, col = "green")

# Plot 4: Nitrogen
plot(df_Ib_high$date, df_Ib_high$nitrogen, ylab = "Nitrogen, g g-1 dry mass", xlab = "Date", 
     type = "l", ylim = c(0, max(df_Ib_low$nitrogen, df_Ib_medium$nitrogen, df_Ib_medium_low$nitrogen, df_Ib_high$nitrogen)), col = "red", cex = 1.25)
lines(df_Ib_low$date, df_Ib_low$nitrogen, col = "blue")
lines(df_Ib_medium$date, df_Ib_medium$nitrogen, col = "orange")
lines(df_Ib_medium_low$date, df_Ib_medium_low$nitrogen, col = "green")

# Plot 5: Assim gross
plot(df_Ib_low$date, df_Ib_low$assim_gross, col = "blue", ylab = "Assim gross, umol m-2 s-1", xlab = "Date", 
     type = "l", cex = 1.25)
lines(df_Ib_1$date, df_Ib_1$assim_gross, col = "orange")
lines(df_Ib_medium$date, df_Ib_medium$assim_gross, col = "green")
lines(df_Ib_medium_low$date, df_Ib_medium_low$assim_gross, col = "red")

# Plot 6: zeta
plot(df_Ib_high$date, df_Ib_high$zeta, col = "red", ylab = "zeta, ratio", xlab = "Date", 
     type = "l", cex = 1.25, ylim = c(0, max(df_Ib_high$zeta, df_Ib_low$zeta, df_Ib_medium$zeta, df_Ib_medium_low$zeta)))
lines(df_Ib_low$date, df_Ib_low$zeta, col = "blue")
lines(df_Ib_medium$date, df_Ib_medium$zeta, col = "orange")
lines(df_Ib_medium_low$date, df_Ib_medium_low$zeta, col = "green")

# Assuming df_original, df_Ib, df_Ib_2, df_Ib_medium, and df_Ib_medium_low are your dataframes

# Function to create a base plot
create_base_plot <- function(data, x, y, ylabel, ylim) {
  ggplot(data, aes(x = {{x}}, y = {{y}})) +
    geom_line(aes(color = "Original", linetype = "Original")) +
    labs(x = "Date", y = ylabel) +
    ylim(ylim) +
    theme_minimal(base_size = 12)
}

# Plot 1: Height
p1 <- create_base_plot(df_original, date, height, "Height, m", c(df_original$height[1], max(df_Ib$height))) +
  geom_line(data = df_Ib_high, aes(color = "0.2", linetype = "0.2")) +
  geom_line(data = df_Ib_low, aes(color = "0.4", linetype = "0.4")) +
  geom_line(data = df_Ib_medium, aes(color = "0.3", linetype = "0.3")) +
  geom_line(data = df_Ib_medium_low, aes(color = "0.15", linetype = "0.15")) +
  scale_color_manual(values = c("Original" = "black", "0.15" = "red", "0.2" = "orange", "0.3" = "green", "0.4" = "blue"),
                     name = expression(a)) +
  scale_linetype_manual(values = c("Original" = "solid", "0.15" = "solid", "0.2" = "dashed", "0.3" = "solid", "0.4" = "solid"),
                        name = expression(zeta)) +
  theme(legend.position = "bottom")

# Plot 2: Crown Area
p2 <- create_base_plot(df_original, date, crown_area, "Crown Area, m2", 
                       c(df_original$crown_area[1], max(df_original$crown_area))) +
  geom_line(data = df_Ib_high, aes(color = "0.2", linetype = "0.2")) +
  geom_line(data = df_Ib_low, aes(color = "0.4", linetype = "0.4")) +
  geom_line(data = df_Ib_medium, aes(color = "0.3", linetype = "0.3")) +
  geom_line(data = df_Ib_medium_low, aes(color = "0.15", linetype = "0.15")) +
  scale_color_manual(values = c("Original" = "black", "0.15" = "red", "0.2" = "orange", "0.3" = "green", "0.4" = "blue"),
                     guide = "none") +
  scale_linetype_manual(values = c("Original" = "solid", "0.15" = "solid", "0.2" = "dashed", "0.3" = "solid", "0.4" = "solid"),
                        guide = "none")

# Plot 4: Nitrogen
p4 <- ggplot(df_Ib_high, aes(x = date, y = nitrogen)) +
  geom_line(aes(color = "0.2", linetype = "0.2")) +
  geom_line(data = df_Ib_low, aes(color = "0.4", linetype = "0.4")) +
  geom_line(data = df_Ib_medium, aes(color = "0.3", linetype = "0.3")) +
  geom_line(data = df_Ib_medium_low, aes(color = "0.15", linetype = "0.15")) +
  labs(x = "Date", y = "Nitrogen, g g-1 dry mass") +
  ylim(0, 0.35) +
  scale_color_manual(values = c("0.15" = "red", "0.2" = "orange", "0.3" = "green", "0.4" = "blue"),
                     guide = "none") +
  scale_linetype_manual(values = c("0.15" = "solid", "0.2" = "dashed", "0.3" = "solid", "0.4" = "solid"),
                        guide = "none") +
  theme_minimal(base_size = 12)

# Plot 5: Assim gross
p5 <- create_base_plot(df_original, date, assim_gross, "Assim gross, umol m-2 s-1", c(0, 0.15)) +
  geom_line(data = df_Ib_low, aes(color = "0.4", linetype = "0.4")) +
  geom_line(data = df_Ib_medium, aes(color = "0.3", linetype = "0.3")) +
  geom_line(data = df_Ib_medium_low, aes(color = "0.15", linetype = "0.15")) +
  scale_color_manual(values = c("Original" = "black", "0.15" = "red", "0.2" = "orange", "0.3" = "green", "0.4" = "blue"),
                     guide = "none") +
  scale_linetype_manual(values = c("Original" = "solid", "0.15" = "solid", "0.2" = "dashed", "0.3" = "solid", "0.4" = "solid"),
                        guide = "none")

# Plot 6: Assim gross (with df_Ib)
p6 <- create_base_plot(df_original, date, assim_gross, "Assim gross, umol m-2 s-1", c(0, 0.15)) +
  geom_line(data = df_Ib_high, aes(color = "0.2", linetype = "0.2")) +
  scale_color_manual(values = c("Original" = "black", "0.2" = "orange"),
                     guide = "none") +
  scale_linetype_manual(values = c("Original" = "solid", "0.2" = "dashed"),
                        guide = "none")

# Plot 7
p7 <- ggplot(df_Ib_high, aes(x = date, y = zeta)) +
  geom_line(aes(color = "0.2", linetype = "0.2")) +
  geom_line(data = df_Ib_low, aes(color = "0.4", linetype = "0.4")) +
  geom_line(data = df_Ib_medium, aes(color = "0.3", linetype = "0.3")) +
  geom_line(data = df_Ib_medium_low, aes(color = "0.15", linetype = "0.15")) +
  labs(x = "Date", y = "zeta, ratio") +
  ylim(0, 0.35) +
  scale_color_manual(values = c("0.15" = "red", "0.2" = "orange", "0.3" = "green", "0.4" = "blue"),
                     guide = "none") +
  scale_linetype_manual(values = c("0.15" = "solid", "0.2" = "dashed", "0.3" = "solid", "0.4" = "solid"),
                        guide = "none") +
  theme_minimal(base_size = 12)

# Combine plots
combined_plot <- (p1 + p2 + p4) / (p5 + p6 + p7) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Display the combined plot
combined_plot

###
# Two variables
###

# Function to create LifeHistoryOptimizer objects
create_lho_params_two <- function(params_file, i_metFile, a_metFile, co2File = "", init_co2 = NULL, Ib_transfrom, zeta) {
  lho <- new(LifeHistoryOptimizer, params_file)
  lho$set_i_metFile(i_metFile)
  lho$set_a_metFile(a_metFile)
  lho$set_co2File(co2File)
  if (!is.null(init_co2)) lho$init_co2(init_co2)
  print(c(lho$env$clim_inst$co2, lho$env$clim_acclim$co2))
  lho$par0$infra_translation <- Ib_transfrom
  lho$traits0$zeta <- zeta
  lho$init()
  return(lho)
}

# Generate all combinations of potential parameters
Ib_transform_values <- seq(4, 6, length.out = 40)
zeta_values <- seq(0.15, 0.4, length.out = 40)

# Create a data frame of all parameter combinations
param_grid <- expand.grid(Ib_value = Ib_transform_values, zeta = zeta_values)

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
                                    zeta = params$zeta)
  result <- run_for_dataset(lho_temp, 2000, 2015, dt)
  result$Ib_value <- params$Ib_value
  result$zeta <- params$zeta
  
  return(result)
}, mc.cores = detectCores()-2)

sa_ib_both_no_error <- sa_ib_both[sapply(sa_ib_both, is.data.frame)]

# Combine the results into a single data frame
sa_ib_all_both <- do.call(rbind, sa_ib_both_no_error)

# Plot
color_scale_Ib_value <- scale_color_gradientn(colours = rainbow(5), name = "a")
combined_plot <- create_combined_plot_Ib_value(df_original, sa_ib_all_both[sa_ib_all_both$zeta == unique(sa_ib_all_both$zeta)[9],], color_scale_Ib_value)
print(combined_plot)

color_scale_zeta <- scale_color_gradientn(colours = rainbow(5), name = expression(zeta))
combined_plot <- create_combined_plot_zeta(df_original, sa_ib_all_both[sa_ib_all_both$Ib_value == unique(sa_ib_all_both$Ib_value)[36],], color_scale_zeta)
print(combined_plot)

###
# Raised CO2
###

raied_co2 <- function(init_co2, a = 5) {
  lho_Ib <- create_lho_params_two("tests/params/p_test_v2.ini", 
                                   "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                                   "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                                   init_co2 = init_co2,
                                  zeta = 0.2,
                                  Ib_transfrom = a)
  df_Ib <- run_for_dataset(lho_Ib, 2000, 2015, dt)
  
  
  lho_Ib_2 <- create_lho_params_two("tests/params/p_test_v2.ini", 
                              "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                              "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                              init_co2 = init_co2,
                              Ib_transfrom = a,
                              zeta = 0.4)
  df_Ib_2 <- run_for_dataset(lho_Ib_2, 2000, 2015, dt)
  
  lho_Ib_medium <- create_lho_params_two("tests/params/p_test_v2.ini", 
                                "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                                "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                                init_co2 = init_co2,
                                Ib_transfrom = a,
                                zeta = 0.3)
  df_Ib_medium <- run_for_dataset(lho_Ib_medium, 2000, 2015, dt)
  
  lho_Ib_medium_low <- create_lho_params_two("tests/params/p_test_v2.ini", 
                                 "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                                 "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv", 
                                 init_co2 = init_co2,
                                 Ib_transfrom = a,
                                 zeta = 0.15)
  df_Ib_medium_low <- run_for_dataset(lho_Ib_medium_low, 2000, 2015, dt)
  
  out = list(df_Ib = df_Ib,
             lho_Ib = lho_Ib$traits0$zeta,
             df_Ib_2 = df_Ib_2,
             lho_Ib_2 = lho_Ib_2$traits0$zeta,
             df_Ib_medium = df_Ib_medium,
             lho_Ib_medium = lho_Ib_medium$traits0$zeta,
             df_Ib_medium_low = df_Ib_medium_low,
             lho_Ib_medium_low = lho_Ib_medium_low$traits0$zeta)
  
  return(out)
}


###
# Plot
###

plot_co2 <- function(init_co2, init_co2_2, a) {
  ### 
  # Plot
  ###
  co2_init_1 <- raied_co2(init_co2, a = a)
  co2_init_2 <- raied_co2(init_co2_2, a = a)
  
  all_dataframes = c(co2_init_1[c(1, 3, 5, 7)], co2_init_2[c(1, 3, 5, 7)])
  
  create_base_plot <- function(data, x, y, ylabel, ylim) {
    ggplot(data, aes(x = {{x}}, y = {{y}})) +
      geom_line(aes(color = "0.2", linetype = "low")) +
      labs(x = "Date", y = ylabel) +
      ylim(ylim) +
      theme_minimal(base_size = 12)
  }
  
  overall_min <- purrr::map_dbl(all_dataframes, ~ min(.x$height)) %>% min()
  overall_max <- purrr::map_dbl(all_dataframes, ~ max(.x$height)) %>% max()
  
  # Plot 1: Height (already provided, included for completeness)
  p1 <- create_base_plot(df_Ib, date, height, "Height, m", c(overall_min, overall_max)) +
    geom_line(data = co2_init_1$df_Ib_2, aes(color = "0.4", linetype = "low")) +
    geom_line(data = co2_init_1$df_Ib_medium, aes(color = "0.3", linetype = "low")) +
    geom_line(data = co2_init_1$df_Ib_medium_low, aes(color = "0.15", linetype = "low")) +
    geom_line(data = co2_init_2$df_Ib, aes(color = "0.2", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_2, aes(color = "0.4", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_medium, aes(color = "0.3", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_medium_low, aes(color = "0.15", linetype = "high")) +
    scale_color_manual(values = c("0.15" = "red", "0.2" = "orange", "0.3" = "green", "0.4" = "blue"),
                       name = expression(a)) +
    scale_linetype_manual(values = c("low" = "solid", "high" = "dashed"),
                          name = "CO2 Values") +
    theme(legend.position = "right",
          legend.direction = "vertical")
  
  # Plot 2: Crown Area
  overall_min <- purrr::map_dbl(all_dataframes, ~ min(.x$crown_area)) %>% min()
  overall_max <- purrr::map_dbl(all_dataframes, ~ max(.x$crown_area)) %>% max()
  
  p2 <- create_base_plot(df_Ib, date, crown_area, "Crown Area, m2", c(overall_min, overall_max)) +
    geom_line(data = co2_init_1$df_Ib_2, aes(color = "0.4", linetype = "low")) +
    geom_line(data = co2_init_1$df_Ib_medium, aes(color = "0.3", linetype = "low")) +
    geom_line(data = co2_init_1$df_Ib_medium_low, aes(color = "0.15", linetype = "low")) +
    geom_line(data = co2_init_2$df_Ib, aes(color = "0.2", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_2, aes(color = "0.4", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_medium, aes(color = "0.3", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_medium_low, aes(color = "0.15", linetype = "high")) +
    scale_color_manual(values = c("0.15" = "red", "0.2" = "orange", "0.3" = "green", "0.4" = "blue"),
                       name = expression(zeta)) +
    scale_linetype_manual(values = c("low" = "solid", "high" = "dashed"),
                          name = "CO2 Values") +
    theme(legend.position = "none")
  
  # Plot 3: Belowground Infrastructure
  p3 <- create_base_plot(co2_init_1$df_Ib, date, a * co2_init_1$lho_Ib, "", c(0.6, 2.0)) +
    geom_line(data = co2_init_1$df_Ib_2, aes(y = a * co2_init_1$lho_Ib_2, color = "0.4", linetype = "low")) +
    geom_line(data = co2_init_1$df_Ib_medium, aes(y = a * co2_init_1$lho_Ib_medium, color = "0.3", linetype = "low")) +
    geom_line(data = co2_init_1$df_Ib_medium_low, aes(y = a * co2_init_1$lho_Ib_medium_low, color = "0.15", linetype = "low")) +
    geom_line(data = co2_init_2$df_Ib, aes(y = a * co2_init_2$lho_Ib, color = "0.2", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_2, aes(y = a * co2_init_2$lho_Ib_2, color = "0.4", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_medium, aes(y = a * co2_init_2$lho_Ib_medium, color = "0.3", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_medium_low, aes(y = a * co2_init_2$lho_Ib_medium_low, color = "0.15", linetype = "high")) +
    labs(x = "Date", y = expression(paste("Belowground Infrastructure", (a * zeta), sep = " "))) +
    ylim(0.5, 2) +
    scale_color_manual(values = c("0.15" = "red", "0.2" = "orange", "0.3" = "green", "0.4" = "blue"),
                       name = expression(zeta)) +
    scale_linetype_manual(values = c("low" = "solid", "high" = "dashed"),
                          name = "CO2 Values") +
    theme(legend.position = "none")
  
  # Plot 4: Nitrogen
  overall_min <- purrr::map_dbl(all_dataframes, ~ min(.x$crown_area)) %>% min()
  overall_max <- purrr::map_dbl(all_dataframes, ~ max(.x$crown_area)) %>% max()
  
  p4 <- create_base_plot(co2_init_1$df_Ib, date, nitrogen, "Nitrogen, g g-1 dry mass", c(overall_min, overall_max)) +
    geom_line(data = co2_init_1$df_Ib_2, aes(color = "0.4", linetype = "low")) +
    geom_line(data = co2_init_1$df_Ib_medium, aes(color = "0.3", linetype = "low")) +
    geom_line(data = co2_init_1$df_Ib_medium_low, aes(color = "0.15", linetype = "low")) +
    geom_line(data = co2_init_2$df_Ib, aes(color = "0.2", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_2, aes(color = "0.4", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_medium, aes(color = "0.3", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_medium_low, aes(color = "0.15", linetype = "high")) +
    ylim(0, 0.35) +
    scale_color_manual(values = c("0.15" = "red", "0.2" = "orange", "0.3" = "green", "0.4" = "blue"),
                       name = expression(zeta)) +
    scale_linetype_manual(values = c("low" = "solid", "high" = "dashed"),
                          name = "CO2 Values") +
    theme(legend.position = "none")
  
  # Plot 5: Assim gross
  overall_min <- purrr::map_dbl(all_dataframes, ~ min(.x$assim_gross)) %>% min()
  overall_max <- purrr::map_dbl(all_dataframes, ~ max(.x$assim_gross)) %>% max()
  
  p5 <- create_base_plot(co2_init_1$df_Ib, date, assim_gross, "Assim gross, umol m-2 s-1", 
                         c(overall_min, overall_max)) +
    geom_line(data = co2_init_1$df_Ib_2, aes(color = "0.4", linetype = "low")) +
    geom_line(data = co2_init_1$df_Ib_medium, aes(color = "0.3", linetype = "low")) +
    geom_line(data = co2_init_1$df_Ib_medium_low, aes(color = "0.15", linetype = "low")) +
    geom_line(data = co2_init_2$df_Ib, aes(color = "0.2", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_2, aes(color = "0.4", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_medium, aes(color = "0.3", linetype = "high")) +
    geom_line(data = co2_init_2$df_Ib_medium_low, aes(color = "0.15", linetype = "high")) +
    scale_color_manual(values = c("0.15" = "red", "0.2" = "orange", "0.3" = "green", "0.4" = "blue"),
                       name = expression(zeta)) +
    scale_linetype_manual(values = c("low" = "solid", "high" = "dashed"),
                          name = "CO2 Values") +
    theme(legend.position = "none")
  
  # Combine plots
  combined_plot <- (p1 + p2 + p3) / (p4 + p5) +
    plot_layout(guides = "collect") &
    plot_annotation(
      title = paste("CO2 Analysis (CO2: low", init_co2, "ppm, high", init_co2_2, "ppm, a:", a, ")"),
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 0, size = 10, margin = margin(t = 20)),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
      )
    )
  
  # Display the combined plot
  print(combined_plot)
}


#####
# Results
#####

plot_co2(365, 1200, a = 1)
plot_co2(365, 1200, a = 0.75)
