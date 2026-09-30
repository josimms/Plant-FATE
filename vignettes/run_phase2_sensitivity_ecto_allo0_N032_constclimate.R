# Starting-ecto_allo sensitivity at N=0.32 (Korhonen whole-soil-profile value),
# same const-climate diagnostic as run_phase2_sensitivity_ecto_allo0_constclimate.R
# (which only covered N=0.20/2.00) -- checking whether the low ecto_allo seen in
# the full N=0.32 dynamic run (relaxed_local_N_0.32_refixed.csv, ends ~0.056) is
# local-optimum trapping (settles at a different plateau per starting point) or
# a robust conclusion (converges to the same plateau regardless of start).
# See notes_root_no_belowground_economy.md section 24 for the N=0.20/2.00 result.

suppressMessages(devtools::load_all("/home/josimms/Documents/Austria/Plant-FATE", quiet = TRUE))

param_file   <- "/home/josimms/Documents/Austria/Plant-FATE/tests/params/p_test_boreal.ini"
weather_file <- "/home/josimms/Documents/Austria/Plant-FATE/data/ERAS_Monthly_constant1960.csv"
reopt_dt <- 1/12
trial_horizon_years <- 3
start_year <- 1960
end_year   <- 1979.25

cache_dir <- "plots/dynamic/phase2_relaxed_refixed"
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

run_sens <- function(n_val, ecto0_val, cache_file) {
  cache_path <- file.path(cache_dir, cache_file)
  if (file.exists(cache_path)) {
    cat("cached:", cache_file, "\n")
    return(invisible(NULL))
  }

  lho <- new(LifeHistoryOptimizer, param_file)
  lho$set_i_metFile(weather_file)
  lho$set_a_metFile(weather_file)
  lho$set_co2File("")
  lho$set_soil_nitrogen(n_val)
  lho$traits0$investment_from_tree <- ecto0_val
  lho$init()

  t0 <- Sys.time()
  log <- lho$run_with_relaxed_local_reopt_trajectory(
    start_year, end_year, reopt_dt, trial_horizon_years,
    3.0,   1e3,  5e7,
    1.5,   0.05, 6.0,
    0.05,  0.0,  1.0,
    0.05,  0.0,  1.0
  )
  df <- as.data.frame(do.call(rbind, log))
  names(df) <- c(lho$get_header(), "ecto_allo", "mycorrhized", "root_no_target", "root_length_target")

  df$date  <- as.Date(df$i - 2440588, origin = "1970-01-01")
  df$year  <- as.integer(format(df$date, "%Y"))
  df$month <- as.integer(format(df$date, "%m"))
  df$soil_nitrogen <- n_val
  df$starting_ecto_allo0 <- ecto0_val

  write.csv(df, cache_path, row.names = FALSE)
  cat("done:", cache_file, "in", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "min\n")
}

n_val      <- 0.32
ecto0_vals <- c(0.05, 0.20, 0.60)

for (e0 in ecto0_vals) {
  run_sens(n_val, e0, sprintf("sens_constclimate_N_%.2f_ecto0_%.2f.csv", n_val, e0))
}

cat("N=0.32 ECTO_ALLO0 SENSITIVITY RUNS COMPLETE\n")
