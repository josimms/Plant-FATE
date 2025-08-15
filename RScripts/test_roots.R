blank <- function() {
  ###
  # Simulation!
  ###
  
  lho <- new(LifeHistoryOptimizer, "tests/params/p_test_v3.ini")
  lho$set_i_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  lho$set_a_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  lho$set_co2File("")
  lho$set_soil_nitrogen(0.5)
  lho$init() # Note the diameter and the height are correct here
  
  # Now not even the initialisation works!
  dt <- 1/12
  
  df <- data.frame(matrix(ncol = length(lho$get_header()), nrow = 0))
  col_names <- lho$get_header()
  
  start_year <- 2000
  end_year <- 2015

  results <- vector("list", length(seq(start_year, end_year, dt)))
  i <- 1
  
  for (t in seq(start_year, end_year, dt)) {
    results[[i]] <- tryCatch({
      lho$grow_for_dt(t, dt)
      state <- lho$get_state(t + dt)
      df_row <- as.data.frame(t(state), stringsAsFactors = FALSE)
      names(df_row) <- col_names   # force the names here
      df_row
    },
    error = function(e) {
      message("Error at t = ", t, ": ", conditionMessage(e))
      na_row <- as.data.frame(as.list(rep(NA, length(col_names))), stringsAsFactors = FALSE)
      names(na_row) <- col_names
      na_row
    })
    i <- i + 1
  }
  
  df <- do.call(rbind, results)
  names(df) <- col_names
  
  
  df$date <- seq(
    as.Date(paste0(start_year, "-01-01")),
    as.Date(paste0(end_year, "-01-01")),
    by = "month"
  )[1:nrow(df)]
  
  ###
  # Plots
  ###
  
  par(mfrow = c(2, 2))
  plot(df$assim_net, main = "Net Assimilation", xlab = "timestep", ylab = "assim_net")
  plot(df$tree_nitrogen, main = "Tree Nitrogen", xlab = "timestep", ylab = "gN")
  plot(df$potential_leaf_nitrogen, main = "Leaf Nitrogen", xlab = "timestep", ylab = "gN", ylim = c(min(df$optimal_leaf_nitrogen), max(df$potential_leaf_nitrogen)))
  points(df$optimal_leaf_nitrogen, xlab = "timestep", pch = "x", col = "red")
  legend("left", c("Optimisation Maximum", "Optimal Value"), pch = c("o", "x"), bty = "n", col = c("black", "red"))
  plot(df$nitrogen_uptake, main = "Nitrogen Uptake", xlab = "timestep", ylab = "gN per biomass per timestep")
  
  plot(df$ectomycorrhiza_mass, main = "Ectomycorrhiza Mass", xlab = "timestep", ylab = "kg")
  plot(df$root_mass, main = "Root Mass", xlab = "timestep", ylab = "tonne")
  plot(df$root_length, main = "Root Length", xlab = "timestep", ylab = "mm")
  plot(df$root_no, main = "Root No", xlab = "timestep", ylab = "no")
  
  plot(df$height, main = "Height", xlab = "timestep", ylab = "m")
  plot(df$diameter, main = "Diameter", xlab = "timestep", ylab = "m")
  plot(df$leaf_mass, main = "Leaf Mass", xlab = "timestep", ylab = "tonne")
  plot(df$coarse_root_mass, main = "Coarse Root Mass", xlab = "timestep", ylab = "tonne")
}


