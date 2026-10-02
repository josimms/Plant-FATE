# Root-traits diagnostic: WITH vs WITHOUT the exponential relaxation delay,
# not the trial-horizon lookahead. root_no/root_length are not adopted
# instantaneously when the search commits to a new target each year;
# instead they relax exponentially toward that target with time constant
# tau = root_lifespan(traits) (inst/include/life_history.h). "Without delay"
# here means the committed target itself (root_no_target/root_length_target,
# i.e. what the search decided, before relaxation smooths it into effect);
# "with delay" is the actual (relaxed) root_no/root_length that the tree
# physically realises. Both come from the same current (horizon=3,
# adaptive-step) runs -- see diagnostic_root_no_actual_vs_target.png
# (plot_phase2_relaxed_full_set.R) for the original single-panel version
# this is distilled from.

suppressMessages({
  library(tidyverse)
  library(patchwork)
})

pub_theme <- theme_classic(base_size = 13) +
  theme(
    axis.title       = element_text(size = 13, face = "bold"),
    axis.text        = element_text(size = 11, colour = "black"),
    legend.text      = element_text(size = 12),
    strip.text       = element_text(size = 12, face = "bold"),
    plot.tag         = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3)
  )

dyn_dir     <- "plots/dynamic"
pub_fig_dir <- here::here("manuscript/oup-authoring-template/Figures")

raw <- read_csv(file.path(dyn_dir, "relaxed_local_N_0.32_refixed.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

col2 <- scale_colour_manual(name = NULL, values = c(
  "Without delay (committed target)" = "#d6604d", "With delay (exponential relaxation)" = "black"))
lty2 <- scale_linetype_manual(name = NULL, values = c(
  "Without delay (committed target)" = "22", "With delay (exponential relaxation)" = "solid"))

p_no <- ggplot(raw) +
  geom_line(aes(date, root_no_target, colour = "Without delay (committed target)",
                linetype = "Without delay (committed target)"), linewidth = 0.6) +
  geom_line(aes(date, root_no, colour = "With delay (exponential relaxation)",
                linetype = "With delay (exponential relaxation)"), linewidth = 0.9) +
  col2 + lty2 +
  scale_y_log10(labels = scales::label_scientific()) +
  labs(x = "Year", y = "Root tip density\n(m⁻²)", tag = "(a)") + pub_theme

p_len <- ggplot(raw) +
  geom_line(aes(date, root_length_target, colour = "Without delay (committed target)",
                linetype = "Without delay (committed target)"), linewidth = 0.6) +
  geom_line(aes(date, root_length, colour = "With delay (exponential relaxation)",
                linetype = "With delay (exponential relaxation)"), linewidth = 0.9) +
  col2 + lty2 +
  labs(x = "Year", y = "Root tip length\n(mm)", tag = "(b)") + pub_theme

fig <- (p_no | p_len) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(pub_fig_dir, "fig_horizon_root_traits_diagnostic.png"), fig, width = 12, height = 5, dpi = 300)
cat("saved fig_horizon_root_traits_diagnostic.png\n")
