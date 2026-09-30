# Static-calibration run at an arbitrary N_s, mirroring
# run_static_calibration_current.R exactly (no root_override -- uses
# p_test_boreal.ini defaults: root_no0=3e5, root_length0=2.0mm,
# investment_from_tree=0.2, mycorrhized=0.9) except for set_soil_nitrogen
# and the output path. Used to build a static-calibration counterpart for
# each already-generated dynamic reopt run (relaxed_local_N_*_refixed.csv),
# for direct comparison.
#
# Usage: Rscript run_static_calibration_generic.R <N_value>

suppressMessages(devtools::load_all("/home/josimms/Documents/Austria/Plant-FATE", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
n_val <- as.numeric(args[1])
if (is.na(n_val)) stop("Usage: Rscript run_static_calibration_generic.R <N_value>")

param_file   <- "/home/josimms/Documents/Austria/Plant-FATE/tests/params/p_test_boreal.ini"
weather_file <- "/home/josimms/Documents/Austria/Plant-FATE/data/ERAS_Monthly.csv"
dt <- 1/12
start_year <- 1960
end_year   <- 2022
years_seq  <- seq(start_year, end_year, dt)

lho <- new(LifeHistoryOptimizer, param_file)
lho$set_i_metFile(weather_file)
lho$set_a_metFile(weather_file)
lho$set_co2File("")
lho$set_soil_nitrogen(n_val)
lho$init()
col_names <- lho$get_header()

t0 <- Sys.time()
results <- lapply(years_seq, function(t) {
  tryCatch({
    lho$grow_for_dt(t, dt)
    state <- lho$get_state(t + dt)
    df_row <- as.data.frame(t(state))
    names(df_row) <- col_names
    df_row
  }, error = function(e) {
    setNames(as.data.frame(as.list(rep(NA, length(col_names)))), col_names)
  })
})
df <- do.call(rbind, results)
names(df) <- col_names
df$date  <- seq(as.Date(paste0(start_year, "-01-01")),
                as.Date(paste0(end_year, "-01-01")), by = "month")[1:nrow(df)]
df$year  <- as.integer(format(df$date, "%Y"))
df$month <- as.integer(format(df$date, "%m"))
df$source <- "Static (calibration)"
df$soil_nitrogen <- n_val

n_tag <- sprintf("%.2f", n_val)
out_path <- sprintf(
  "/home/josimms/Documents/Austria/Plant-FATE/vignettes/plots/dynamic/static_df_N%s_calibrated_current.rds",
  gsub("\\.", "", n_tag)
)
saveRDS(df, out_path)
cat("N=", n_val, " done in ", round(as.numeric(Sys.time()-t0, units="mins"),1),
    " min. Saved: ", out_path, "\n", sep = "")
