# Starting-root_no0 sensitivity ("effect of growth stage" -- does the
# search's converged trajectory depend on the tree's assumed initial
# belowground investment?), under real ERA5 climate. Companion to
# plot_fig_sensitivity_root_no0_constclimate.R, which repeats the same test
# under constant (repeated-year) climate to isolate search convergence from
# real inter-annual climate variability.
#
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

n_levels      <- c(0.32, 2.00)
n_labels      <- c("N=0.32 (boreal calib.)", "N=2.00 (High)")
root_no0_vals <- c(1e5, 3e5, 1e6)

grid <- expand.grid(n = n_levels, rn0 = root_no0_vals)
raw <- pmap_dfr(grid, function(n, rn0) {
  f <- file.path(data_dir, sprintf("sens_N_%.2f_root_no0_%.0e.csv", n, rn0))
  read_csv(f, show_col_types = FALSE) %>%
    mutate(N_label = factor(sprintf("N=%.2f (%s)", n,
                                     c("0.15"="Low","0.32"="boreal calib.","2.00"="High")[sprintf("%.2f", n)]),
                             levels = n_labels),
           root_no0_label = c("1e+05"="1×10⁵","3e+05"="3×10⁵","1e+06"="1×10⁶")[sprintf("%.0e", rn0)])
})
raw$date <- as.Date(raw$date)
raw$year_num <- as.numeric(format(raw$date, "%Y")) + (as.numeric(format(raw$date, "%m")) - 1) / 12

raw <- raw %>%
  arrange(N_label, root_no0_label, date) %>%
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
# Divergence statistics: how different are the three starting-root_no0
# trajectories from each other, at the end of the (establishment-phase)
# run, per variable and per N level? Reported as range / median (%).
# ------------------------------------------------------------------
stat_vars <- c("root_no", "root_length", "ecto_allo", "mycorrhized",
               "height", "gpp_per_ca", "ecto_per_ca", "accessible_N")
final_vals <- raw %>%
  group_by(N_label, root_no0_label) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(N_label, root_no0_label, all_of(stat_vars))

divergence <- final_vals %>%
  pivot_longer(all_of(stat_vars), names_to = "var", values_to = "value") %>%
  group_by(N_label, var) %>%
  summarise(range_pct_of_median = 100 * (max(value) - min(value)) / abs(median(value)), .groups = "drop") %>%
  pivot_wider(names_from = var, values_from = range_pct_of_median)

cat("\n== Divergence across starting root_no0 at final simulated year (real climate) ==\n")
cat("(range as % of median, across the 3 starting points)\n")
print(divergence, width = Inf)
write_csv(divergence, file.path(pub_fig_dir, "..", "divergence_stats_root_no0_realclimate.csv"))

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
  select(year_num, N_label, root_no0_label, all_of(var_spec$var)) %>%
  pivot_longer(cols = all_of(var_spec$var), names_to = "var", values_to = "value") %>%
  mutate(var_label = factor(var_spec$label[match(var, var_spec$var)], levels = var_spec$label))

col3 <- scale_colour_manual(name = "Initial Root Number (m⁻²)",
                             values = c("1×10⁵" = "#1b9e77",
                                        "3×10⁵" = "#7570b3",
                                        "1×10⁶" = "#d95f02"))

# Anchor the belowground TRAIT panels (not the emergent-output panels) to
# the full range trialled by the local search (run_phase2_relaxed_reopt.R),
# via invisible geom_blank points -- same trick as fig1/fig_N_gradient.
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
  mutate(year_num = min(long$year_num, na.rm = TRUE))

p <- ggplot(long, aes(year_num, value, colour = root_no0_label)) +
  geom_line(linewidth = 0.9) +
  geom_blank(data = anchor, inherit.aes = FALSE, aes(x = year_num, y = value)) +
  col3 +
  facet_grid2(N_label ~ var_label, scales = "free_y", independent = "y", switch = "y") +
  labs(x = "Year", y = NULL) +
  pub_theme +
  theme(legend.position = "bottom", strip.placement = "outside")

ggsave(file.path(pub_fig_dir, "fig_sensitivity_root_no0_realclimate.png"), p, width = 27, height = 7, dpi = 300, limitsize = FALSE)
cat("saved fig_sensitivity_root_no0_realclimate.png (data_dir =", data_dir, ")\n")
