# Compare each already-generated dynamic reopt run (relaxed_local_N_*_refixed.csv)
# against its static-calibration counterpart (static_df_N*_calibrated_current.rds,
# built by run_static_calibration_generic.R) to see whether any N level gives
# similar static vs. dynamic outcomes.
#
# NB: the dynamic reopt CSVs' year/month columns are mislabeled (naive
# by="month" sequence vs. the trajectory's actual adaptive step sizes) --
# real calendar dates are recovered here from the `i` (Julian day) column,
# as established for the N=0.32 case.

suppressMessages(library(dplyr))

dyn_dir <- "plots/dynamic"
dyn_files <- list.files(dyn_dir, pattern = "^relaxed_local_N_[0-9.]+_refixed\\.csv$", full.names = TRUE)
n_from_dyn <- as.numeric(sub("^relaxed_local_N_([0-9.]+)_refixed\\.csv$", "\\1", basename(dyn_files)))

mkderived <- function(df) {
  df %>% mutate(
    gpp_per_ca = assim_gross / crown_area,
    CN = ifelse(ectomycorrhiza_mass > 1e-6,
                (ectomycorrhiza_mass * 0.44 + ectomycorrhiza_C_free) /
                  pmax(ectomycorrhiza_N_biomass + ectomycorrhiza_N_free, 1e-12), NA_real_),
    accessible_N = N_bar_roots + N_bar_static
  )
}

results <- lapply(seq_along(dyn_files), function(k) {
  n_val <- n_from_dyn[k]
  n_tag <- sprintf("%.2f", n_val)
  stat_path <- file.path(dyn_dir, sprintf("static_df_N%s_calibrated_current.rds", gsub("\\.", "", n_tag)))
  if (!file.exists(stat_path)) {
    message("No static counterpart for N=", n_tag, " -- skipping")
    return(NULL)
  }

  dyn  <- read.csv(dyn_files[k])
  stat <- readRDS(stat_path)

  dyn$date_real  <- as.Date(dyn$i - 2440588, origin = "1970-01-01")
  dyn$year_real  <- as.integer(format(dyn$date_real, "%Y"))
  dyn$month_real <- as.integer(format(dyn$date_real, "%m"))

  dyn  <- mkderived(dyn)
  stat <- mkderived(stat)

  common_max_year <- min(max(dyn$year_real, na.rm = TRUE), max(stat$year, na.rm = TRUE))

  gs_dyn <- dyn %>% filter(month_real %in% 6:8, year_real <= common_max_year) %>%
    group_by(year = year_real) %>%
    summarise(height = mean(height, na.rm = TRUE), gpp_per_ca = mean(gpp_per_ca, na.rm = TRUE),
              ecm_ca = mean(ectomycorrhiza_mass / crown_area, na.rm = TRUE),
              CN = mean(CN, na.rm = TRUE), accessible_N = mean(accessible_N, na.rm = TRUE),
              .groups = "drop")

  gs_stat <- stat %>% filter(month %in% 6:8, year <= common_max_year) %>%
    group_by(year) %>%
    summarise(height = mean(height, na.rm = TRUE), gpp_per_ca = mean(gpp_per_ca, na.rm = TRUE),
              ecm_ca = mean(ectomycorrhiza_mass / crown_area, na.rm = TRUE),
              CN = mean(CN, na.rm = TRUE), accessible_N = mean(accessible_N, na.rm = TRUE),
              .groups = "drop")

  cmp <- inner_join(gs_dyn, gs_stat, by = "year", suffix = c("_dyn", "_stat"))
  if (nrow(cmp) < 5) {
    message("N=", n_tag, ": too few overlapping GS-years (", nrow(cmp), ") -- skipping")
    return(NULL)
  }

  pct <- function(a, b) mean(abs((a - b) / b) * 100, na.rm = TRUE)
  last_year <- max(cmp$year)
  last_row  <- cmp %>% filter(year == last_year)

  data.frame(
    N = n_val,
    n_overlap_years = nrow(cmp),
    last_year = last_year,
    pct_diff_height       = pct(cmp$height_dyn,       cmp$height_stat),
    pct_diff_gpp_per_ca   = pct(cmp$gpp_per_ca_dyn,    cmp$gpp_per_ca_stat),
    pct_diff_ecm_ca       = pct(cmp$ecm_ca_dyn,        cmp$ecm_ca_stat),
    pct_diff_CN           = pct(cmp$CN_dyn,            cmp$CN_stat),
    pct_diff_accessible_N = pct(cmp$accessible_N_dyn,  cmp$accessible_N_stat),
    height_dyn_final  = last_row$height_dyn,  height_stat_final  = last_row$height_stat,
    gpp_dyn_final     = last_row$gpp_per_ca_dyn, gpp_stat_final  = last_row$gpp_per_ca_stat
  )
})

summary_df <- do.call(rbind, results) %>% arrange(N)

# Overall dissimilarity score = mean of the 5 abs-%-diff metrics (excluding
# accessible_N, which is on a very different scale and dominated by near-zero
# static-run values -- keep it visible in the table but not in the ranking).
summary_df$overall_score <- rowMeans(
  summary_df[, c("pct_diff_height", "pct_diff_gpp_per_ca", "pct_diff_ecm_ca", "pct_diff_CN")],
  na.rm = TRUE
)

cat("=== Static vs. dynamic similarity across all completed N values ===\n")
cat("(mean abs %% diff over overlapping growing-season years; lower = more similar)\n\n")
print(summary_df %>% arrange(overall_score), digits = 3)

cat("\n=== Most similar N (lowest overall_score) ===\n")
print(summary_df %>% arrange(overall_score) %>% head(3), digits = 3)

write.csv(summary_df, "plots/dynamic/static_vs_dynamic_all_N_summary.csv", row.names = FALSE)
cat("\nSaved: plots/dynamic/static_vs_dynamic_all_N_summary.csv\n")
