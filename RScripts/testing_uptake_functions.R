# ==========================================================
# Combined R model for plant nitrogen uptake
# (includes all PlantArchitecture & Trait helper functions)
# ==========================================================

# -------------------------------
# Constants and parameters
# -------------------------------
u_c_B   <- 1500.0      # Michaelis-Menten constant
e_u_myco <- 85       # uptake efficiency for mycorrhiza
e_u_root <- 250        # uptake efficiency for roots
k_12 <- 0.02            # half-saturation for mycorrhiza biomass limitation
k_13 <- 0.02            # half-saturation for roots
k_9  <- 10            # for nitrogen gate
mycorrhized <- 0.9     # fraction of roots with mycorrhiza
investment_from_myco <- 0.2  # fraction of C allocated to mycorrhiza

# -------------------------------
# Trait parameters
# -------------------------------
PlantTraits <- list(
  # structural constants (examples)
  k_1                 = 0.35,             # Fitting parameter for root length [mm3/2] (Ding), Diameter-length allometry constant
  k_2                 = 0.2,              # Fitted parameter for woody plants [unitless] (Kong, 2019), Fitting parameter in PRD equation
  k_3                 = 0.2014,           # Fitted parameter for woody plants [mm] (Kong, 2019), Coefficient in root density equation
  k_4                 = 11632.91276,      # Fitted parameter root density [kg m-3] (Ding), Intercept in root density equation
  k_5                 = -334.3626755,     # Fitted parameter root density [kg m-3] (Ding), Alternate intercept (not used)
  k_6                 = 3,                # Fitted parameter root lifespan [mm-1] (Ding), Coefficient in age equation
  k_7                 = 3,                # Fitted parameter root lifespan [years] (Ding), Max root age
  
  # leaf & mycorrhizal traits
  lma = 0.13,    # kg C per m2
  investment_from_tree = 0.2,
  mycorrhizal_turnover = 0.2,
  mycorrhizal_biomass_conversion = 0.8,
  
  # misc
  hmat = 20
)

# -------------------------------
# Plant architecture
# -------------------------------
PlantArchitecture <- list(
  ectomycorrhiza_mass = 1.3,  # kg C
  root_length = 1.5,          # mm
  root_no = 1e6,              # number of roots
  crown_area = 0.12,           # m2
  lai = 4.5                   # leaf area index
)

# -------------------------------
# Helper functions (from C++)
# -------------------------------
root_diameter <- function(G, T) {
  T$k_1 / sqrt(G$root_length)
}

root_density <- function(G, T) {
  prc <- (T$k_2 + T$k_3 / root_diameter(G, T))^2
  T$k_4 * prc + T$k_5
}

root_lifespan <- function(G, T) {
  T$k_6 * (1 - exp(-T$k_7 * root_diameter(G, T)))
}

leaf_mass <- function(G, T) {
  G$crown_area * G$lai * T$lma
}

root_mass_calc <- function(G, T) {
  d <- root_diameter(G, T)
  rho <- root_density(G, T)
  rho * (d/2)^2 * G$root_length * pi * G$root_no * 1e-9 * G$crown_area
}

get_ectomycorrhiza_mass <- function(G, exudates, T) {
  input <- exudates * T$investment_from_tree
  G$ectomycorrhiza_mass <- (1 - T$mycorrhizal_turnover) * G$ectomycorrhiza_mass +
    input * T$mycorrhizal_biomass_conversion
  G
}

# -------------------------------
# Uptake equations
# -------------------------------
uptake_myco <- function(N, G) {
  biomass_limitation <- G$ectomycorrhiza_mass / (G$ectomycorrhiza_mass + 2.0 * k_12)
  nitrogen_uptake <- (u_c_B * N * e_u_myco) / (u_c_B + N * e_u_myco)
  nitrogen_capacity <- G$ectomycorrhiza_mass * biomass_limitation * nitrogen_uptake
  nitrogen_capacity * N / (nitrogen_capacity + N)
}

uptake_roots <- function(N, G, T) {
  root_length <- G$root_length
  root_no <- G$root_no
  rho <- root_density(G, T)
  d <- root_diameter(G, T)
  
  one_root_mass <- rho * (pi * (d/2)^2) * root_length * 1e-9
  surface_area <- root_no * pi * (root_length * 1e-3) * (d * 1e-3)
  biomass_conversion <- 4.0 * one_root_mass / (rho * d * 1e-3)
  biomass_limitation <- surface_area / (surface_area + 2.0 * k_13)
  
  nitrogen_uptake <- (u_c_B * N * e_u_root) / (u_c_B + N * e_u_root)
  nitrogen_capacity <- biomass_conversion * biomass_limitation * nitrogen_uptake
  nitrogen_capacity * N / (nitrogen_capacity + N)
}

nitrogen_gate <- function(G, T) {
  surface_area <- G$root_no * pi * G$root_length * root_diameter(G, T)
  surface_area / (surface_area + k_9)
}

nitrogen_plant <- function(N, G, T) {
  uptake_roots_term <- (1 - mycorrhized) * uptake_roots(N, G, T)
  uptake_mycorrhiza_term <- mycorrhized * nitrogen_gate(G, T) *
    investment_from_myco * uptake_myco(N, G)
  total_uptake <- uptake_roots_term + uptake_mycorrhiza_term
  
  list(
    uptake_total = total_uptake,
    roots = uptake_roots_term,
    myco = uptake_mycorrhiza_term
  )
}

# -------------------------------
# Run simulation and plot
# -------------------------------
test_N <- seq(0.01, 2, length.out = 100)
results_total <- sapply(test_N, function(N) nitrogen_plant(N, PlantArchitecture, PlantTraits)$uptake_total)
results_roots <- sapply(test_N, function(N) nitrogen_plant(N, PlantArchitecture, PlantTraits)$roots)
results_myco  <- sapply(test_N, function(N) nitrogen_plant(N, PlantArchitecture, PlantTraits)$myco)

plot(test_N, results_total, type = "l", lwd = 3, col = "black",
     xlab = "Soil N concentration (arbitrary units)",
     ylab = "Nitrogen uptake (g N per kg C)",
     main = "Nitrogen Uptake Response Curve")
lines(test_N, results_roots, col = "darkgreen", lwd = 2, lty = 2)
lines(test_N, results_myco,  col = "orange", lwd = 2, lty = 3)
legend("bottomright",
       legend = c("Total", "Roots", "Mycorrhiza"),
       col = c("black", "darkgreen", "orange"), lwd = 2, lty = c(1,2,3))

