# N=0.32 equivalent of plot_ecto_allo0_raw_trajectory.R (which only covered
# N=0.20/2.00) -- does ecto_allo converge to the same plateau regardless of
# starting point at N=0.32 (the Korhonen whole-soil-profile value), or does
# it show the same local-optimum trapping documented in
# notes_root_no_belowground_economy.md section 24?

suppressMessages({
  library(tidyverse)
})

cache_dir  <- "plots/dynamic/phase2_relaxed_refixed"
ecto0_vals <- c(0.05, 0.20, 0.60)

raw <- map_dfr(ecto0_vals, function(e0) {
  f <- file.path(cache_dir, sprintf("sens_constclimate_N_0.32_ecto0_%.2f.csv", e0))
  read_csv(f, show_col_types = FALSE) %>%
    mutate(ecto0_label = sprintf("start=%.2f", e0))
})
raw$date <- as.Date(raw$date)
raw <- raw %>%
  group_by(ecto0_label) %>%
  mutate(sim_year = as.numeric(date - min(date)) / 365.25) %>%
  ungroup() %>%
  arrange(ecto0_label, date)

diag <- raw %>%
  group_by(ecto0_label) %>%
  summarise(
    ecto_allo_yr1_mean  = mean(ecto_allo[sim_year < 1]),
    ecto_allo_end_mean  = mean(ecto_allo[sim_year > max(sim_year) - 3]),
    ecto_allo_end_sd    = sd(ecto_allo[sim_year > max(sim_year) - 3]),
    ecto_allo_end_min   = min(ecto_allo[sim_year > max(sim_year) - 3]),
    ecto_allo_end_max   = max(ecto_allo[sim_year > max(sim_year) - 3]),
    .groups = "drop"
  )
cat("== N=0.32: ecto_allo start-of-run vs last-3-simulated-years (mean/sd/range) ==\n")
print(diag, width = Inf, n = Inf)

p <- ggplot(raw, aes(sim_year, ecto_allo, colour = ecto0_label)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0.2, linetype = "dashed", colour = "grey40") +
  annotate("text", x = 1, y = 0.2, label = "static calibration (0.2)", vjust = -0.6,
           hjust = 0, size = 3.5, colour = "grey40") +
  scale_colour_manual(name = "Starting ecto_allo",
                       values = c("start=0.05" = "#1b9e77", "start=0.20" = "#7570b3", "start=0.60" = "#d95f02")) +
  labs(x = "Simulation year (elapsed, weather held constant)",
       y = "ecto_allo (fraction of NPP committed to ECM)",
       title = "Raw ecto_allo trajectory by starting point, N=0.32 (Korhonen)") +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom")

out_path <- file.path(cache_dir, "ecto_allo0_raw_trajectory_diagnostic_N032.png")
ggsave(out_path, p, width = 9, height = 6, dpi = 200)
cat("\nsaved", out_path, "\n")
