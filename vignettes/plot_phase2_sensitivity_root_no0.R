# Redo the "Sensitivity to starting root_no0" figure (originally
# vignettes/plots/dynamic/phase2_relaxed/sensitivity_starting_root_no0.png)
# with the post-u_transfer-fix data from run_phase2_sensitivity_root_no0.R.
# Now covers all the variables that actually change across the sensitivity
# runs (not just root_no/height), one grid: rows = soil N, columns = variable.

suppressMessages({
  library(tidyverse)
  library(ggh4x)
})

cache_dir <- "plots/dynamic/phase2_relaxed_refixed"

pub_theme <- theme_classic(base_size = 12) +
  theme(
    axis.title       = element_text(size = 12, face = "bold"),
    axis.text        = element_text(size = 9, colour = "black"),
    legend.text      = element_text(size = 11),
    strip.text       = element_text(size = 10, face = "bold"),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3)
  )

n_levels      <- c(0.15, 0.32, 2.00)
n_labels      <- c("N=0.15 (Low)", "N=0.32 (boreal calib.)", "N=2.00 (High)")
root_no0_vals <- c(1e5, 3e5, 1e6)

grid <- expand.grid(n = n_levels, rn0 = root_no0_vals)
raw <- pmap_dfr(grid, function(n, rn0) {
  f <- file.path(cache_dir, sprintf("sens_N_%.2f_root_no0_%.0e.csv", n, rn0))
  read_csv(f, show_col_types = FALSE) %>%
    mutate(N_label = factor(sprintf("N=%.2f (%s)", n,
                                     c("0.15"="Low","0.32"="boreal calib.","2.00"="High")[sprintf("%.2f", n)]),
                             levels = n_labels),
           root_no0_label = sprintf("root_no0=%.0e", rn0))
})
raw$date <- as.Date(raw$date)
raw$year_num <- as.numeric(format(raw$date, "%Y")) + (as.numeric(format(raw$date, "%m")) - 1) / 12

raw <- raw %>%
  arrange(N_label, root_no0_label, date) %>%
  group_by(N_label, root_no0_label) %>%
  mutate(height_increment = pmax(height - lag(height, 12), 0)) %>%
  ungroup() %>%
  mutate(
    gpp_per_ca   = assim_gross / crown_area,
    ecto_per_ca  = ectomycorrhiza_mass / crown_area,
    accessible_N = N_bar_roots + N_bar_static,
    CN           = (ectomycorrhiza_mass * 0.44 + ectomycorrhiza_C_free) /
                     pmax(ectomycorrhiza_N_biomass + ectomycorrhiza_N_free, 1e-12),
    log10_root_no       = log10(root_no),
    log10_accessible_N  = log10(pmax(accessible_N, 1e-12)),
    ecto_allo_pct        = ecto_allo * 100,
    mycorrhized_pct      = mycorrhized * 100
  )

var_spec <- tribble(
  ~var,                  ~label,
  "log10_root_no",       "Root tip density\n(log10 m⁻²)",
  "root_length",         "Root tip length\n(mm)",
  "ecto_allo_pct",       "ECM allocation\n(% NPP)",
  "mycorrhized_pct",     "ECM colonisation\n(% roots covered)",
  "height",              "Height\n(m)",
  "height_increment",    "Height increment\n(m yr⁻¹)",
  "gpp_per_ca",          "GPP per crown area\n(kg C m⁻² yr⁻¹)",
  "ecto_per_ca",         "ECM biomass per\ncrown area (kg m⁻²)",
  "CN",                  "Mycorrhizal C:N\nratio (kg C kg⁻¹ N)",
  "log10_accessible_N",  "Accessible N\n(log10 kg N m⁻³)"
)

long <- raw %>%
  select(year_num, N_label, root_no0_label, all_of(var_spec$var)) %>%
  pivot_longer(cols = all_of(var_spec$var), names_to = "var", values_to = "value") %>%
  mutate(var_label = factor(var_spec$label[match(var, var_spec$var)], levels = var_spec$label))

col3 <- scale_colour_manual(name = "Starting root_no0",
                             values = c("root_no0=1e+05" = "#1b9e77",
                                        "root_no0=3e+05" = "#7570b3",
                                        "root_no0=1e+06" = "#d95f02"))

# Pin the ECM colonisation panel's y-axis to the true 0-100% domain (rather
# than free-scaling to whatever narrow range the data happens to occupy) by
# adding invisible anchor points at 0 and 100 -- geom_blank in that facet
# only, so every other panel's free_y scale is untouched.
mycorrhized_label <- var_spec$label[var_spec$var == "mycorrhized_pct"]
anchor <- expand.grid(N_label = unique(long$N_label), value = c(0, 100)) %>%
  mutate(var_label = factor(mycorrhized_label, levels = levels(long$var_label)),
         year_num = min(long$year_num, na.rm = TRUE))

p <- ggplot(long, aes(year_num, value, colour = root_no0_label)) +
  geom_line(linewidth = 0.7) +
  geom_blank(data = anchor, inherit.aes = FALSE, aes(x = year_num, y = value)) +
  col3 +
  facet_grid2(N_label ~ var_label, scales = "free_y", independent = "y", switch = "y") +
  labs(title = "Sensitivity to starting root_no0 (real climate, 1960-2022 ERA5)",
       x = "Year", y = NULL) +
  pub_theme +
  theme(legend.position = "bottom", strip.placement = "outside")

ggsave(file.path(cache_dir, "sensitivity_starting_root_no0.png"), p, width = 30, height = 9, dpi = 300, limitsize = FALSE)
cat("saved sensitivity_starting_root_no0.png\n")
