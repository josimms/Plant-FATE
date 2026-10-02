# Starting-root_length sensitivity under constant (single year, 1960,
# repeated) climate -- isolates search convergence from real inter-annual
# climate variability. x-axis is elapsed simulation time, not calendar year.
# Companion to plot_fig_sensitivity_root_no0_constclimate.R.

suppressMessages({
  library(tidyverse)
  library(ggh4x)
})

cache_dir   <- "plots/dynamic/phase2_relaxed_refixed"
data_dir    <- cache_dir
pub_fig_dir <- here::here("manuscript/oup-authoring-template/Figures")

pub_theme <- theme_classic(base_size = 17) +
  theme(
    axis.title       = element_text(size = 17, face = "bold"),
    axis.text        = element_text(size = 13, colour = "black"),
    legend.text      = element_text(size = 22),
    legend.title     = element_text(size = 24, face = "bold"),
    strip.text       = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3)
  )

n_levels  <- c(0.32, 2.00)
n_labels  <- c("N=0.32 (boreal calib.)", "N=2.00 (High)")
rl0_vals  <- c(0.5, 2.0, 5.0)

grid <- expand.grid(n = n_levels, rl0 = rl0_vals)
raw <- pmap_dfr(grid, function(n, rl0) {
  f <- file.path(data_dir, sprintf("sens_constclimate_N_%.2f_root_length0_%.1f.csv", n, rl0))
  read_csv(f, show_col_types = FALSE) %>%
    mutate(N_label = factor(sprintf("N=%.2f (%s)", n,
                                     c("0.32"="boreal calib.","2.00"="High")[sprintf("%.2f", n)]),
                             levels = n_labels),
           rl0_label = sprintf("%.1f", rl0))
})
raw$date <- as.Date(raw$date)

raw <- raw %>%
  group_by(N_label, rl0_label) %>%
  mutate(sim_year = as.numeric(date - min(date)) / 365.25) %>%
  ungroup()

raw <- raw %>%
  arrange(N_label, rl0_label, date) %>%
  mutate(
    gpp_per_ca   = assim_gross / crown_area,
    ecto_per_ca  = ectomycorrhiza_mass / crown_area,
    accessible_N = N_bar_roots + N_bar_static,
    log10_root_no       = log10(root_no),
    log10_accessible_N  = log10(pmax(accessible_N, 1e-12)),
    ecto_allo_pct        = ecto_allo * 100,
    mycorrhized_pct      = mycorrhized * 100
  )

# ------------------------------------------------------------------
# Divergence statistics: how different are the three starting-root_length
# trajectories from each other, at the end of the run, per variable and
# per N level? Reported as range / median (%).
# ------------------------------------------------------------------
stat_vars <- c("root_no", "root_length", "ecto_allo", "mycorrhized",
               "height", "gpp_per_ca", "ecto_per_ca", "accessible_N")
final_vals <- raw %>%
  group_by(N_label, rl0_label) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(N_label, rl0_label, all_of(stat_vars))

divergence <- final_vals %>%
  pivot_longer(all_of(stat_vars), names_to = "var", values_to = "value") %>%
  group_by(N_label, var) %>%
  summarise(range_pct_of_median = 100 * (max(value) - min(value)) / abs(median(value)), .groups = "drop") %>%
  pivot_wider(names_from = var, values_from = range_pct_of_median)

cat("\n== Divergence across starting root_length at final simulated year (constant climate) ==\n")
cat("(range as % of median, across the 3 starting points)\n")
print(divergence, width = Inf)
write_csv(divergence, file.path(pub_fig_dir, "..", "divergence_stats_root_length0_constclimate.csv"))

var_spec <- tribble(
  ~var,                  ~label,
  "log10_root_no",       "Root tip density\n(log10 m⁻²)",
  "root_length",         "Root tip length\n(mm)",
  "ecto_allo_pct",       "ECM allocation\n(% NPP)",
  "mycorrhized_pct",     "ECM colonisation\n(% roots covered)",
  "height",              "Height\n(m)",
  "gpp_per_ca",          "GPP per crown area\n(kg C m⁻² yr⁻¹)",
  "ecto_per_ca",         "ECM biomass per\ncrown area (kg m⁻²)",
  "log10_accessible_N",  "log10 Accessible N\n(kg N m⁻³)"
)

long <- raw %>%
  select(sim_year, N_label, rl0_label, all_of(var_spec$var)) %>%
  pivot_longer(cols = all_of(var_spec$var), names_to = "var", values_to = "value") %>%
  mutate(var_label = factor(var_spec$label[match(var, var_spec$var)], levels = var_spec$label))

col3 <- scale_colour_manual(name = "Initial root tip length (mm)",
                             values = c("0.5" = "#1b9e77",
                                        "2.0" = "#7570b3",
                                        "5.0" = "#d95f02"))

full_range_anchors <- tribble(
  ~var,             ~lo,           ~hi,
  "log10_root_no",  log10(1e3),    log10(5e7),
  "root_length",    0.05,          6.0,
  "ecto_allo_pct",  0,             100,
  "mycorrhized_pct",0,             100
) %>%
  mutate(var_label = factor(var_spec$label[match(var, var_spec$var)], levels = levels(long$var_label)))

anchor <- expand.grid(N_label = unique(long$N_label), var = full_range_anchors$var) %>%
  left_join(full_range_anchors, by = "var") %>%
  pivot_longer(c(lo, hi), values_to = "value") %>%
  mutate(sim_year = min(long$sim_year, na.rm = TRUE))

p <- ggplot(long, aes(sim_year, value, colour = rl0_label)) +
  geom_line(linewidth = 0.9) +
  geom_blank(data = anchor, inherit.aes = FALSE, aes(x = sim_year, y = value)) +
  col3 +
  facet_grid2(N_label ~ var_label, scales = "free_y", independent = "y", switch = "y") +
  labs(x = "Simulation year (elapsed, weather held constant)", y = NULL) +
  pub_theme +
  theme(legend.position = "bottom", strip.placement = "outside")

ggsave(file.path(pub_fig_dir, "fig_sensitivity_root_length0_constclimate.png"), p, width = 27, height = 7, dpi = 300, limitsize = FALSE)
cat("saved fig_sensitivity_root_length0_constclimate.png (data_dir =", data_dir, ")\n")
