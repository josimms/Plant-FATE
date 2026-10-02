# Variant of regenerate_lh09_lh10_and_fig1.R at N_s = 0.32 (Korhonen 2013
# whole-soil-profile-to-bedrock reading) instead of the manuscript's current
# N_s = 0.20 boreal-calibration value. Does NOT touch the original
# all_lh_09/all_lh_10/fig1_calibration_to_dynamic.png files -- everything
# here is written under a "_N032" suffix so the two can be compared side by
# side.
#
# Also corrects a real bug found while investigating this: the dynamic reopt
# CSVs' `date` column is a naive seq(..., by="month") over output rows, but
# the trajectory's actual step sizes are adaptive/variable (receding trial
# horizon + step-size shrinking), so the labelled dates drift from the real
# simulated calendar time -- by ~3.5 years at the end of a 62-year run. Real
# dates are recovered here from the `i` (Julian day) column for both dynamic
# series before plotting.

suppressMessages({
  library(tidyverse)
  library(data.table)
})

pub_fig_dir <- here::here("manuscript/oup-authoring-template/Figures")
dir.create(pub_fig_dir, showWarnings = FALSE, recursive = TRUE)
dyn_out_dir <- "plots/dynamic"
param_file  <- "/home/josimms/Documents/Austria/Plant-FATE/tests/params/p_test_boreal.ini"

# ------------------------------------------------------------------
# Observational data (verbatim from Publication Plots.Rmd)
# ------------------------------------------------------------------
hyde_all <- "~/Documents/Austria/Hyytiala_all_data/"
data <- lapply(paste0(hyde_all, list.files(hyde_all))[1:9], read.csv)
names(data) <- list.files(hyde_all)[1:9]
useful_data <- merge(data[[2]], data[[5]], c('plotID', 'eventID', 'eventYear'), all=TRUE)

halme_et_al_2022 <- data.frame(matrix(nrow = 9, ncol = 2))
halme_et_al_2022$height         <- c(17.1, 21.0, 20.5, 18.3, 13.2, 20.7, 16.8, 23.6, 13.6)
halme_et_al_2022$crown_diameter <- c(3.4,  4.0,  4.1,  3.3,  2.9,  3.7,  3.4,  4.2,  2.8)

data_directory <- "~/Documents/CASSIA_Calibration/Processed_Data/"
loaded_data <- CASSIA::load_data(data_directory)

raw.directory <- "/home/josimms/Documents/CASSIA_Calibration/Raw_Data/hyytiala_weather/"
environmental.variable.list <- list()
count <- 1
for (variable in c("GPP", "NEE", "F_CO2_leaf", "F_H2O_leaf", "ET_gapf")) {
  environmental.variable.list[[count]] <- data.table::rbindlist(
    lapply(paste0(raw.directory, list.files(raw.directory, variable)), data.table::fread))
  environmental.variable.list[[count]]$Date <- paste(
    environmental.variable.list[[count]]$Year,
    environmental.variable.list[[count]]$Month,
    environmental.variable.list[[count]]$Day, sep = "-")
  environmental.variable.list[[count]]$Monthly <- paste(
    environmental.variable.list[[count]]$Year,
    environmental.variable.list[[count]]$Month, sep = "-")
  count <- count + 1
}
names(environmental.variable.list) <- c("GPP", "NEE", "F_CO2_leaf", "F_H2O_leaf", "ET")

DT  <- copy(environmental.variable.list[["GPP"]])
DT2 <- copy(environmental.variable.list[["NEE"]])
DT5 <- copy(environmental.variable.list[["ET"]])

GPP_out <- DT[,  .(GPP_mean = mean(HYY_EDDY233.GPP,    na.rm = TRUE)), by = Monthly]
NEE_out <- DT2[, .(NEE_mean = mean(HYY_EDDY233.NEE,    na.rm = TRUE)), by = Monthly]
ET_out  <- DT5[, .(ET_mean  = mean(HYY_EDDY233.ET_gapf, na.rm = TRUE)), by = Monthly]

GPP_out <- GPP_out[, Monthly := zoo::as.yearmon(Monthly, "%Y-%m")]
NEE_out <- NEE_out[, Monthly := zoo::as.yearmon(Monthly, "%Y-%m")]
ET_out  <- ET_out[,  Monthly := zoo::as.yearmon(Monthly, "%Y-%m")]
setorder(GPP_out, Monthly); setorder(NEE_out, Monthly); setorder(ET_out, Monthly)

area_per_tree        <- 1
GPP_out$GPP_mean_kg  <- GPP_out$GPP_mean * 12.011 * 1e-9 * 60 * 60 * 24 * 365.25 * area_per_tree
NEE_out$NEE_mean_kg  <- NEE_out$NEE_mean * 12.011 * 1e-9 * 60 * 60 * 24 * 365.25 * area_per_tree
GPP_out$NEE_mean     <- NEE_out$NEE_mean
GPP_out$NEE_mean_kg  <- NEE_out$NEE_mean_kg
GPP_out$ET           <- ET_out$ET_mean
Eddy_covariance      <- GPP_out
Eddy_covariance[, MonthlyDate := as.Date(paste0("01 ", Monthly), format = "%d %b %Y")]
Eddy_covariance[, Month       := as.integer(format(MonthlyDate, "%m"))]

ini_lines <- readLines(param_file)
read_ini_val <- function(key) {
  line <- grep(paste0("^", key, "\\s*="), ini_lines, value = TRUE)[1]
  as.numeric(sub(paste0("^", key, "\\s*=\\s*([0-9.e+\\-]+).*"), "\\1", line))
}
mycorrhized_val <- read_ini_val("mycorrhized")

# ------------------------------------------------------------------
# The static-calibration run at N=0.32 (run_static_calibration_generic.R 0.32)
# ------------------------------------------------------------------
df <- readRDS(file.path(dyn_out_dir, "static_df_N032_calibrated_current.rds"))

lh_cols       <- c("black", "#E69F00", "#D55E00")
col_obs       <- "lightblue"
col_range     <- "lightblue"
lh_cex_axis   <- 2.0
lh_cex_lab    <- 1.7
lh_cex_main   <- 2.4
lh_cex_legend <- 1.8
lh_lwd_model  <- 3
lh_lwd_obs    <- 2
lh_pch_obs    <- 16

add_range <- function(x, y_min, y_max,
                      col = adjustcolor(col_range, alpha.f = 0.5),
                      lwd = lh_lwd_obs, cap = 0.015) {
  arrows(x, y_min, x, y_max, code = 3, angle = 90, length = cap, col = col, lwd = lwd)
}

# ================================================================
# Figure 9 (N=0.32): main publication panel
# ================================================================
png(file.path(pub_fig_dir, "all_lh_09_publication_main_N032.png"),
    width = 12, height = 10, units = "in", res = 300)
par(mfrow = c(4, 2), mar = c(4, 9, 3, 1), oma = c(4, 0, 2, 0),
    family = "serif", las = 1, tcl = -0.4, mgp = c(5, 1, 0))

ylim_gpp <- range(df$assim_gross/df$crown_area/2.04, Eddy_covariance$GPP_mean_kg, na.rm = TRUE)
plot(df$date, df$assim_gross/df$crown_area/2.04, type = "l", col = lh_cols[1], lwd = lh_lwd_model,
     ylim = ylim_gpp, xlab = "",
     ylab = expression(atop("GPP", "(kg C m"^{-2}*" year"^{-1}*")")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
points(Eddy_covariance$MonthlyDate, Eddy_covariance$GPP_mean_kg, col = col_obs, pch = lh_pch_obs, cex = 0.8)
mtext("(a)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

ylim_npp <- range(df$assim_net/df$crown_area/2.04, -Eddy_covariance$NEE_mean_kg, na.rm = TRUE)
plot(df$date, df$assim_net/df$crown_area/2.04, type = "l", col = lh_cols[1], lwd = lh_lwd_model,
     ylim = ylim_npp, xlab = "",
     ylab = expression(atop("NPP", "(kg C m"^{-2}*" year"^{-1}*")")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
points(Eddy_covariance$MonthlyDate, -Eddy_covariance$NEE_mean_kg, col = col_obs, pch = lh_pch_obs, cex = 0.8)
mtext("(b)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

smear_h_idx  <- loaded_data$smearII_data$variable == "pine height BA weighted mean"
smear_h_val  <- loaded_data$smearII_data$amount[smear_h_idx]
smear_h_date <- as.Date(paste0(loaded_data$smearII_data$date[smear_h_idx], "-01-01"))
ylim_h <- range(df$height, smear_h_val, halme_et_al_2022$height, useful_data$averageTreeHeight, na.rm = TRUE)
plot(df$date, df$height, type = "l", col = lh_cols[1], lwd = lh_lwd_model,
     ylim = ylim_h, xlab = "", ylab = "Height\n(m)",
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
points(smear_h_date, smear_h_val, col = adjustcolor(col_range, alpha.f = 0.15), pch = 4, cex = 1.5, lwd = lh_lwd_obs)
add_range(as.Date("2017-06-01"), min(halme_et_al_2022$height, na.rm = TRUE), max(halme_et_al_2022$height, na.rm = TRUE))
for (id in unique(useful_data$plotID)) {
  sub <- useful_data[useful_data$plotID == id & !is.na(useful_data$averageTreeHeight), ]
  sub <- sub[order(sub$eventYear), ]
  if (nrow(sub) < 1) next
  x <- as.Date(as.character(sub$eventYear), format = "%Y")
  y <- sub$averageTreeHeight
  x_plot <- x[1]; y_plot <- y[1]
  for (j in seq_len(nrow(sub) - 1)) {
    if (y[j + 1] < y[j]) { x_plot <- c(x_plot, NA, x[j + 1]); y_plot <- c(y_plot, NA, y[j + 1]) }
    else { x_plot <- c(x_plot, x[j + 1]); y_plot <- c(y_plot, y[j + 1]) }
  }
  points(x, y, col = adjustcolor(col_range, alpha.f = 0.15), pch = 16, cex = 0.4)
  lines(x_plot, y_plot, col = adjustcolor(col_range, alpha.f = 0.15), lwd = lh_lwd_obs)
}
mtext("(c)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

smear_d_idx  <- loaded_data$smearII_data$variable == "pine diameter BA weighted mean"
smear_d_val  <- 0.01 * loaded_data$smearII_data$amount[smear_d_idx]
smear_d_date <- as.Date(paste0(loaded_data$smearII_data$date[smear_d_idx], "-01-01"))
ylim_d <- range(df$diameter, smear_d_val, 0.01 * useful_data$averageTreeDiameter, na.rm = TRUE)
plot(df$date, df$diameter, type = "l", col = lh_cols[1], lwd = lh_lwd_model,
     ylim = ylim_d, xlab = "", ylab = "Diameter\n(m)",
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
points(smear_d_date, smear_d_val, col = adjustcolor(col_range, alpha.f = 0.15), pch = 4, cex = 1.5, lwd = lh_lwd_obs)
for (id in unique(useful_data$plotID)) {
  sub <- useful_data[useful_data$plotID == id & !is.na(useful_data$averageTreeDiameter), ]
  sub <- sub[order(sub$eventYear), ]
  if (nrow(sub) < 1) next
  x <- as.Date(as.character(sub$eventYear), format = "%Y")
  y <- 0.01 * sub$averageTreeDiameter
  x_plot <- x[1]; y_plot <- y[1]
  for (j in seq_len(nrow(sub) - 1)) {
    if (y[j + 1] < y[j]) { x_plot <- c(x_plot, NA, x[j + 1]); y_plot <- c(y_plot, NA, y[j + 1]) }
    else { x_plot <- c(x_plot, x[j + 1]); y_plot <- c(y_plot, y[j + 1]) }
  }
  points(x, y, col = adjustcolor(col_range, alpha.f = 0.15), pch = 16, cex = 0.4)
  lines(x_plot, y_plot, col = adjustcolor(col_range, alpha.f = 0.15), lwd = lh_lwd_obs)
}
mtext("(d)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

ylim_vc <- range(df$vcmax/df$lai, 30, na.rm = TRUE)
plot(df$date, df$vcmax/df$lai, type = "n", ylim = ylim_vc, xlab = "",
     ylab = expression(atop("V"[cmax], "("*mu*"mol m"^{-2}*" s"^{-1}*")")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
rect(par("usr")[1], 0, par("usr")[2], 30, col = adjustcolor("lightblue", alpha.f = 0.5), border = NA)
lines(df$date, df$vcmax/df$lai, col = lh_cols[1], lwd = lh_lwd_model)
mtext("(e)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

korhonen_needle_n_lo <- 0.0121
korhonen_needle_n_hi <- 0.0151
ylim_oln <- range(0, df$optimal_leaf_nitrogen * 1.1, korhonen_needle_n_hi, na.rm = TRUE)
plot(df$date, df$optimal_leaf_nitrogen, type = "n", ylim = ylim_oln, xlab = "",
     ylab = expression(atop("Optimal leaf N", "(g g"^{-1}*")")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
rect(as.numeric(as.Date("2007-01-01")), korhonen_needle_n_lo, par("usr")[2], korhonen_needle_n_hi,
     col = adjustcolor("lightblue", alpha.f = 0.5), border = NA)
idx <- !is.na(df$leaf_nitrogen_concentration)
polygon(c(df$date[idx], rev(df$date[idx])), c(df$leaf_nitrogen_concentration[idx], rep(0, sum(idx))),
        col = adjustcolor(lh_cols[1], alpha.f = 0.5), border = NA)
points(df$date, df$optimal_leaf_nitrogen, col = lh_cols[1], pch = 20, cex = 1)
mtext("(f)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

uptake_1 <- df$mycorrhizal_export_to_tree + df$root_uptake_actual
nuptake_obs_val  <- 0.013
nbar_obs_val     <- 1/13
nbar_obs_date    <- as.Date("2008-08-01")
ylim_nu <- range(uptake_1, nuptake_obs_val, na.rm = TRUE)
plot(df$date, uptake_1, type = "l", col = lh_cols[1], lwd = lh_lwd_model, ylim = ylim_nu, xlab = "",
     ylab = expression(atop("N uptake", "(kg N tree"^{-1}*" year"^{-1}*")")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
points(nbar_obs_date, nbar_obs_val, col = col_range, pch = lh_pch_obs, cex = 2.5)
mtext("(g)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

ratio_1 <- df$ectomycorrhiza_mass / df$root_mass
ylim_nb <- range(ratio_1, 1, 2, na.rm = TRUE)
plot(df$date, ratio_1, type = "l", col = lh_cols[1], lwd = lh_lwd_model, ylim = ylim_nb, xlab = "",
     ylab = expression(atop("ECM / fine root biomass", "(kg kg"^{-1}*")")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
rect(as.numeric(as.Date("2007-01-01")), 1, par("usr")[2], 2, col = adjustcolor("lightblue", alpha.f = 0.5), border = NA)
lines(df$date, ratio_1, col = lh_cols[1], lwd = lh_lwd_model)
mtext("(h)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", horiz = FALSE, bty = "n", cex = lh_cex_legend, ncol = 2,
       legend = c("Eddy covariance", "Range of realistic boreal observations"),
       col = c(col_obs, col_range), lty = c(NA, 1), pch = c(lh_pch_obs, NA), lwd = c(NA, lh_lwd_obs))
dev.off()
cat("saved all_lh_09_publication_main_N032.png\n")

# ================================================================
# Figure 10 (N=0.32): belowground panel
# ================================================================
nbar_lower   <- 4e-6
nbar_upper   <- 8e-5
ntrans_lower <- 0.08
ntrans_upper <- 0.14

uptake_1  <- df$mycorrhizal_export_to_tree + df$root_uptake_actual
bg_mass_1 <- df$root_mass + df$ectomycorrhiza_mass
eff_1     <- uptake_1 / bg_mass_1

k_0_par         <- read_ini_val("k_0")
k_1_par         <- read_ini_val("k_1")
k_13_par        <- read_ini_val("k_13")
depth_par       <- read_ini_val("depth")
rho_myco_par    <- read_ini_val("rho_myco")
myco_d_par      <- read_ini_val("myco_diameter")
mycorrhized_par <- mycorrhized_val
compute_sa_density <- function(d) {
  root_d_mm <- k_1_par / d$root_length^k_0_par
  SA_fr     <- pi * (root_d_mm/1000) * (d$root_length/1000) * d$root_no * d$crown_area * d$lai
  SA_m      <- 4 * d$ectomycorrhiza_mass / (rho_myco_par * myco_d_par)
  SA_active <- (1 - mycorrhized_par) * SA_fr + SA_m
  crown_r   <- sqrt(d$crown_area / pi)
  R         <- k_13_par * crown_r
  V_zone    <- (2/3) * pi * R^2 * depth_par
  list(SA_active = SA_active, V_zone = V_zone, density = SA_active / V_zone, SA_fr = SA_fr, SA_m = SA_m)
}
sa1 <- compute_sa_density(df)

png(file.path(pub_fig_dir, "all_lh_10_publication_belowground_N032.png"),
    width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(3, 2), mar = c(4, 9, 3, 1), oma = c(4, 0, 2, 0),
    family = "serif", las = 1, tcl = -0.4, mgp = c(5, 1, 0))

N_avail_1 <- df$N_bar_roots + ifelse(df$N_bar_static > 0, df$N_bar_static, 0)
ylim_nb <- range(N_avail_1, nbar_lower, nbar_upper, na.rm = TRUE)
plot(df$date, N_avail_1, type = "n", ylim = ylim_nb, log = "y", xlab = "",
     ylab = expression(atop("Available N", "(kg N m"^{-3}*")")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
rect(as.numeric(as.Date("2007-01-01")), nbar_lower, par("usr")[2], nbar_upper,
     col = adjustcolor("lightblue", alpha.f = 0.5), border = NA)
lines(df$date, N_avail_1, col = lh_cols[1], lwd = lh_lwd_model)
mtext("(a)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
title(sub = "Shading: Korhonen soil-solution mineral N range", col.sub = "grey40")
box(bty = "l")

total_mass_1  <- df$stem_mass + df$leaf_mass + df$root_mass + df$coarse_root_mass
tn_per_mass_1 <- df$tree_nitrogen / total_mass_1
plot(df$date, tn_per_mass_1, type = "l", col = lh_cols[1], lwd = lh_lwd_model, log = "y", xlab = "",
     ylab = expression(atop("Internal tree N / biomass", "(kg N kg"^{-1}*" C)")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
mtext("(b)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

ylim_eff <- range(eff_1, na.rm = TRUE)
plot(df$date, eff_1, type = "l", col = lh_cols[1], lwd = lh_lwd_model, ylim = ylim_eff, xlab = "",
     ylab = expression(atop("N uptake efficiency", "(kg N kg"^{-1}*" C year"^{-1}*")")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
mtext("(c)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

ylim_nt <- range(df$mycorrhizal_export_to_tree, ntrans_lower, ntrans_upper, na.rm = TRUE)
plot(df$date, df$mycorrhizal_export_to_tree, type = "l", col = lh_cols[1], lwd = lh_lwd_model, ylim = ylim_nt, xlab = "",
     ylab = expression(atop("ECM N transfer", "(kg N tree"^{-1}*" year"^{-1}*")")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
rect(as.numeric(as.Date("2007-01-01")), ntrans_lower, par("usr")[2], ntrans_upper,
     col = adjustcolor("lightblue", alpha.f = 0.5), border = NA)
lines(df$date, df$mycorrhizal_export_to_tree, col = lh_cols[1], lwd = lh_lwd_model)
mtext("(d)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

ylim_sa <- range(sa1$density, 10, 80, na.rm = TRUE)
plot(df$date, sa1$density, type = "l", col = lh_cols[1], lwd = lh_lwd_model, ylim = ylim_sa, xlab = "",
     ylab = expression(atop("Active SA density", "(m"^{2}*" m"^{-3}*")")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
rect(par("usr")[1], 10, par("usr")[2], 80, col = adjustcolor("lightblue", alpha.f = 0.5), border = NA)
lines(df$date, sa1$density, col = lh_cols[1], lwd = lh_lwd_model)
mtext("(e)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

CN_1 <- (df$ectomycorrhiza_mass * 0.44 + df$ectomycorrhiza_C_free) /
         pmax(df$ectomycorrhiza_N_biomass + df$ectomycorrhiza_N_free, 1e-12)
plot(df$date, CN_1, type = "l", col = lh_cols[1], lwd = lh_lwd_model, xlab = "",
     ylab = expression(atop("Mycorrhizal C:N", "(kg C kg"^{-1}*" N)")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
mtext("(f)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", horiz = FALSE, bty = "n", cex = lh_cex_legend, ncol = 1,
       legend = "Range of realistic boreal observations",
       col = col_range, lty = 1, lwd = lh_lwd_obs)
dev.off()
cat("saved all_lh_10_publication_belowground_N032.png\n")

# ================================================================
# Figure 1 (N=0.32 variant): static calibration vs. dynamic
# re-optimisation at N=0.32 (Korhonen whole-profile) and N=2.00 (High N),
# in the same base-R serif style as lh_09/lh_10. Dynamic series' dates are
# recovered from their `i` (Julian day) column -- see header comment.
# ================================================================
fix_dates <- function(d) {
  d$date <- as.Date(d$i - 2440588, origin = "1970-01-01")
  d
}
dyn_boreal <- read_csv(file.path(dyn_out_dir, "relaxed_local_N_0.32_refixed.csv"), show_col_types = FALSE) %>%
  fix_dates()
dyn_highN  <- read_csv(file.path(dyn_out_dir, "relaxed_local_N_2.00_refixed.csv"), show_col_types = FALSE) %>%
  fix_dates()

f1_cols <- c("black", "#0072B2", "#D55E00")   # Okabe-Ito: static / N=0.32 / High N=2.00
f1_lty_param <- c(2, 3, 3)                     # dashed (prescribed) / dotted (optimised) x2
f1_lwd       <- 2.5

lh_cex_axis   <- 4.0
lh_cex_lab    <- 3.6
lh_cex_main   <- 3.8
lh_cex_legend <- 3.8

f1_gpp_per_ca  <- function(d) d$assim_gross / d$crown_area
f1_ecto_per_ca <- function(d) d$ectomycorrhiza_mass / d$crown_area
f1_CN <- function(d) (d$ectomycorrhiza_mass * 0.44 + d$ectomycorrhiza_C_free) /
                       pmax(d$ectomycorrhiza_N_biomass + d$ectomycorrhiza_N_free, 1e-12)
f1_accessible_N <- function(d) d$N_bar_roots + d$N_bar_static

series <- list(df, dyn_boreal, dyn_highN)
is_static <- c(TRUE, FALSE, FALSE)

png(file.path(pub_fig_dir, "fig1_calibration_to_dynamic_N032.png"),
    width = 27, height = 22, units = "in", res = 300)
par(mfrow = c(3, 3), mar = c(6, 24, 4, 1), oma = c(11, 0, 2, 0),
    family = "serif", las = 1, tcl = -0.5, mgp = c(15, 2.4, 0))

param_bg <- "grey92"

plot_panel <- function(get_y, ylab, tag, log = "", lty_set = rep(1, 3), ylim = NULL, bg_shade = NULL) {
  if (is.null(ylim)) {
    ally <- unlist(lapply(series, get_y))
    ylim <- range(ally, na.rm = TRUE)
  }
  plot(series[[1]]$date, get_y(series[[1]]), type = "n", log = log,
       ylim = ylim, xlab = "", ylab = ylab,
       cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
  if (!is.null(bg_shade)) {
    rect(par("usr")[1], ylim[1], par("usr")[2], ylim[2], col = bg_shade, border = NA)
  }
  for (i in seq_along(series)) {
    lines(series[[i]]$date, get_y(series[[i]]), col = f1_cols[i], lwd = f1_lwd, lty = lty_set[i])
  }
  mtext(tag, side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
  box(bty = "l")
}

# (a) ECM allocation (parameter: dashed=static, dotted=dynamic)
plot_panel(function(d) if (identical(d, df)) rep(0.2, nrow(d)) else d$ecto_allo,
           "ECM allocation\n(% NPP)", "(a)", lty_set = f1_lty_param, ylim = c(0, 1), bg_shade = param_bg)

# (b) Root tip density (parameter, log scale)
plot_panel(function(d) d$root_no, expression(atop("Root tip density", "(m"^{-2}*")")), "(b)",
           log = "y", lty_set = f1_lty_param, ylim = c(1e3, 5e7), bg_shade = param_bg)

# (c) Root tip length (parameter)
plot_panel(function(d) d$root_length, "Root tip length\n(mm)", "(c)", lty_set = f1_lty_param, ylim = c(0.05, 6.0), bg_shade = param_bg)

# (d) ECM colonisation (parameter)
plot_panel(function(d) if (identical(d, df)) rep(0.9, nrow(d)) else d$mycorrhized,
           "ECM colonisation\n(% roots covered)", "(d)", lty_set = f1_lty_param, ylim = c(0, 1), bg_shade = param_bg)

# (e) Height + boreal observations
ylim_h <- range(df$height, dyn_boreal$height, dyn_highN$height,
                smear_h_val, halme_et_al_2022$height, useful_data$averageTreeHeight, na.rm = TRUE)
plot(df$date, df$height, type = "n", ylim = ylim_h, xlab = "", ylab = "Height\n(m)",
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
points(smear_h_date, smear_h_val, col = adjustcolor(col_range, alpha.f = 0.15), pch = 4, cex = 1.5, lwd = lh_lwd_obs)
add_range(as.Date("2017-06-01"), min(halme_et_al_2022$height, na.rm = TRUE), max(halme_et_al_2022$height, na.rm = TRUE))
for (id in unique(useful_data$plotID)) {
  sub <- useful_data[useful_data$plotID == id & !is.na(useful_data$averageTreeHeight), ]
  sub <- sub[order(sub$eventYear), ]
  if (nrow(sub) < 1) next
  x <- as.Date(as.character(sub$eventYear), format = "%Y"); y <- sub$averageTreeHeight
  x_plot <- x[1]; y_plot <- y[1]
  for (j in seq_len(nrow(sub) - 1)) {
    if (y[j + 1] < y[j]) { x_plot <- c(x_plot, NA, x[j + 1]); y_plot <- c(y_plot, NA, y[j + 1]) }
    else { x_plot <- c(x_plot, x[j + 1]); y_plot <- c(y_plot, y[j + 1]) }
  }
  points(x, y, col = adjustcolor(col_range, alpha.f = 0.15), pch = 16, cex = 0.4)
  lines(x_plot, y_plot, col = adjustcolor(col_range, alpha.f = 0.15), lwd = lh_lwd_obs)
}
for (i in seq_along(series)) lines(series[[i]]$date, series[[i]]$height, col = f1_cols[i], lwd = f1_lwd)
mtext("(e)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

# (f) GPP per crown area + eddy covariance
gpp_vals <- lapply(series, f1_gpp_per_ca)
ylim_g <- range(unlist(gpp_vals), Eddy_covariance$GPP_mean_kg, na.rm = TRUE)
plot(df$date, f1_gpp_per_ca(df), type = "n", ylim = ylim_g, xlab = "",
     ylab = expression(atop("GPP per crown area", "(kg C m"^{-2}*" year"^{-1}*")")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
points(Eddy_covariance$MonthlyDate, Eddy_covariance$GPP_mean_kg, col = col_obs, pch = lh_pch_obs, cex = 0.8)
for (i in c(3, 1, 2)) lines(series[[i]]$date, gpp_vals[[i]], col = f1_cols[i], lwd = f1_lwd)
mtext("(f)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

# (g) ECM biomass per crown area
plot_panel(f1_ecto_per_ca, expression(atop("ECM biomass / crown area", "(kg m"^{-2}*")")), "(g)")

# (h) Mycorrhizal C:N ratio
plot_panel(f1_CN, expression(atop("Mycorrhizal C:N", "(kg C kg"^{-1}*" N)")), "(h)")

# (i) Accessible N (log scale) + Korhonen soil-solution mineral N range
acc_vals <- lapply(series, f1_accessible_N)
ylim_i <- range(unlist(acc_vals), nbar_lower, nbar_upper, na.rm = TRUE)
plot(series[[1]]$date, f1_accessible_N(series[[1]]), type = "n", log = "y", ylim = ylim_i,
     xlab = "", ylab = expression(atop("Accessible N", "(kg N m"^{-3}*")")),
     cex.axis = lh_cex_axis, cex.lab = lh_cex_lab)
rect(par("usr")[1], nbar_lower, par("usr")[2], nbar_upper,
     col = adjustcolor("lightblue", alpha.f = 0.5), border = NA)
for (i in seq_along(series)) lines(series[[i]]$date, acc_vals[[i]], col = f1_cols[i], lwd = f1_lwd)
mtext("(i)", side = 3, adj = 0, line = 0.2, font = 2, cex = lh_cex_main)
box(bty = "l")

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", horiz = FALSE, bty = "n", cex = lh_cex_legend, ncol = 3,
       legend = c("Static (calibration)", "Dynamic, N=0.32 (Korhonen)", "Dynamic, N=2.00 (High N)",
                  "Eddy covariance / SMEAR II obs.", "Range of realistic boreal observations"),
       col = c(f1_cols, col_obs, col_range),
       lty = c(1, 1, 1, NA, 1), pch = c(NA, NA, NA, lh_pch_obs, NA), lwd = c(f1_lwd, f1_lwd, f1_lwd, NA, lh_lwd_obs))
dev.off()
cat("saved fig1_calibration_to_dynamic_N032.png\n")
