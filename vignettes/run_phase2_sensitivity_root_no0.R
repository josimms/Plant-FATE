# Sensitivity of Phase 2 (run_with_relaxed_local_reopt_trajectory) to the
# starting root_no0, redone post the 2026-09-23 u_transfer fix (see
# notes_root_no_belowground_economy.md section 11/12/13). Matches the
# duration of the original phase2_relaxed/sens_N_*.csv files (~1960-1979,
# 19 years -- an establishment-phase-only check, not a full 62yr run).

suppressMessages(devtools::load_all("/home/josimms/Documents/Austria/Plant-FATE", quiet = TRUE))

param_file   <- "/home/josimms/Documents/Austria/Plant-FATE/tests/params/p_test_boreal.ini"
weather_file <- "/home/josimms/Documents/Austria/Plant-FATE/data/ERAS_Monthly.csv"
reopt_dt <- 1/12
trial_horizon_years <- 3  # receding-horizon lookahead -- see notes section 19
start_year <- 1960
end_year   <- 1979.25

cache_dir <- "plots/dynamic/phase2_relaxed_refixed"
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

run_sens <- function(n_val, root_no0_val, cache_file) {
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
  lho$init()
  lho$root_override(root_no0_val, 1.24)

  t0 <- Sys.time()
  log <- lho$run_with_relaxed_local_reopt_trajectory(
    start_year, end_year, reopt_dt, trial_horizon_years,
    3.0,   1e3,  5e7,
    1.5,   0.05, 6.0,
    0.05,  0.0,  1.0,   # ecto_allo max raised from 0.3 -- see notes section 17/18
    0.05,  0.0,  1.0    # mycorrhized max raised from 0.95 -- see notes
  )
  df <- as.data.frame(do.call(rbind, log))
  names(df) <- c(lho$get_header(), "ecto_allo", "mycorrhized", "root_no_target", "root_length_target")

  df$date  <- as.Date(df$i - 2440588, origin = "1970-01-01")
  df$year  <- as.integer(format(df$date, "%Y"))
  df$month <- as.integer(format(df$date, "%m"))
  df$soil_nitrogen <- n_val
  df$starting_root_no0 <- root_no0_val

  write.csv(df, cache_path, row.names = FALSE)
  cat("done:", cache_file, "in", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "min\n")
}

n_levels       <- c(0.15, 0.32, 2.00)
root_no0_vals  <- c(1e5, 3e5, 1e6)

for (n in n_levels) {
  for (rn0 in root_no0_vals) {
    run_sens(n, rn0, sprintf("sens_N_%.2f_root_no0_%.0e.csv", n, rn0))
  }
}

cat("ALL SENSITIVITY RUNS COMPLETE\n")
