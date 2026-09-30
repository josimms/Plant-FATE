# Figure 2 panel (height, GPP/crown area, ECM biomass/crown area, C:N ratio,
# accessible N), but with one trajectory per soil-N value the joint grid
# actually completed, instead of just the Low/High pair used in the Rmd.

library(tidyverse)
library(patchwork)

pub_theme <- theme_classic(base_size = 24) +
  theme(
    axis.title       = element_text(size = 24, face = "bold"),
    axis.text        = element_text(size = 22, colour = "black"),
    axis.line        = element_line(colour = "black", linewidth = 0.6),
    axis.ticks       = element_line(colour = "black", linewidth = 0.5),
    legend.text      = element_text(size = 22),
    legend.key.size  = unit(0.9, "lines"),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin           = margin(8, 12, 8, 8),
    strip.text            = element_text(size = 24, face = "bold"),
    plot.tag              = element_text(size = 20, face = "bold"),
    legend.title.position = "top",
    legend.title          = element_text(size = 24, face = "bold", hjust = 0.5)
  )

pub_cache <- "plots/pub_plots_all_joint_annual.csv"
all_joint_df <- read_csv(pub_cache, show_col_types = FALSE)
cat("Loaded", nrow(all_joint_df), "rows from cache:", pub_cache, "\n")

# -- Same derived columns as `annual_gpp_wrangling` in Publication Plots.Rmd --
annual_gs_all_full <- all_joint_df %>%
  mutate(
    net_gpp_ca    = (assim_gross - C_export_to_myco - rr * 0.44 - tr * 0.44) / (crown_area * lai),
    ecm_frac_gpp  = C_export_to_myco / assim_gross,
    accessible_N  = N_bar_roots + N_bar_static,
    mycorrhized   = 0.9
  ) %>%
  arrange(soil_nitrogen, ecto_allo, root_no, root_length, year) %>%
  group_by(soil_nitrogen, ecto_allo, root_no, root_length) %>%
  mutate(height_increment = height - lag(height)) %>%
  ungroup()

annual_gs_all <- annual_gs_all_full %>%
  filter(year >= min(year, na.rm = TRUE) + 5)

soil_N_unique <- sort(unique(annual_gs_all$soil_nitrogen))
cat("N values simulation completed for (", length(soil_N_unique), "):\n", sep = "")
print(soil_N_unique)

# -- Same logic as `best_combo_lifetime`/`traj_gpp`, but for every N --
best_combo_lifetime_all <- annual_gs_all_full %>%
  group_by(soil_nitrogen, ecto_allo, root_no, root_length) %>%
  summarise(mean_gpp_ca = mean(net_gpp_ca, na.rm = TRUE), .groups = "drop") %>%
  group_by(soil_nitrogen) %>%
  slice_max(mean_gpp_ca, n = 1, with_ties = FALSE) %>%
  select(soil_nitrogen, ecto_allo, root_no, root_length)

traj_gpp_all <- annual_gs_all_full %>%
  semi_join(best_combo_lifetime_all,
            by = c("soil_nitrogen", "ecto_allo", "root_no", "root_length")) %>%
  mutate(
    CN         = ifelse(ectomycorrhiza_mass > 1e-6,
                         (ectomycorrhiza_mass * 0.44 + ectomycorrhiza_C_free) /
                           pmax(ectomycorrhiza_N_biomass + ectomycorrhiza_N_free, 1e-12),
                         NA_real_),
    gpp_per_ca = assim_gross / crown_area
  )

# Sequential colour scale (soil N is a magnitude quantity, 15 levels) --
# matches the viridis-continuous convention used elsewhere in the doc.
traj_colour_all <- scale_colour_viridis_c(
  name   = "Soil N\n(kg N m⁻³)",
  option = "viridis"
)

p_a <- ggplot(traj_gpp_all, aes(x = year, y = height, colour = soil_nitrogen, group = soil_nitrogen)) +
  geom_line(linewidth = 0.8) +
  traj_colour_all +
  labs(x = NULL, y = "Height (m)", tag = "(a)") + pub_theme

p_b <- ggplot(traj_gpp_all, aes(x = year, y = gpp_per_ca, colour = soil_nitrogen, group = soil_nitrogen)) +
  geom_line(linewidth = 0.8) +
  traj_colour_all +
  labs(x = NULL, y = "GPP per crown area\n(kg C m⁻² yr⁻¹)", tag = "(b)") + pub_theme

p_c <- ggplot(traj_gpp_all,
    aes(x = year, y = ectomycorrhiza_mass / crown_area, colour = soil_nitrogen, group = soil_nitrogen)) +
  geom_line(linewidth = 0.8) +
  traj_colour_all + scale_y_continuous(labels = scales::label_scientific()) +
  labs(x = NULL, y = "Ectomycorrhizal biomass\nper crown area (kg m⁻²)", tag = "(c)") + pub_theme

p_d <- ggplot(traj_gpp_all, aes(x = year, y = CN, colour = soil_nitrogen, group = soil_nitrogen)) +
  geom_line(linewidth = 0.8) +
  traj_colour_all +
  labs(x = NULL, y = "Mycorrhizal C:N ratio\n(kg C kg⁻¹ N)", tag = "(d)") + pub_theme

p_e <- ggplot(traj_gpp_all, aes(x = year, y = accessible_N, colour = soil_nitrogen, group = soil_nitrogen)) +
  geom_line(linewidth = 0.8) +
  scale_y_log10() +
  traj_colour_all +
  labs(x = "Year", y = "Accessible N\n(kg N m⁻³)", tag = "(e)") + pub_theme

fig2_all_N <- (p_a | p_b) / (p_c | p_d) / (p_e | plot_spacer()) +
  plot_layout(guides = "collect") &
  theme(legend.position  = "right",
        legend.text      = element_text(size = 14),
        legend.key.size  = unit(1.0, "lines"),
        plot.tag         = element_text(size = 22, face = "bold"),
        axis.text.x      = element_text(angle = 45, hjust = 1))

dir.create("plots", showWarnings = FALSE, recursive = TRUE)
out_path <- "plots/fig2_all_N_trajectories.png"
ggsave(out_path, plot = fig2_all_N, width = 14, height = 16, dpi = 300)
cat("Saved", out_path, "\n")
