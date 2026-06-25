#ifndef PLANT_UPTAKE_TPP
#define PLANT_UPTAKE_TPP

#include <temperature_dependencies_photosynthesis.h>

namespace plant{

template<class _Climate>
void Uptake::nitrogen_plant(_Climate C, PlantArchitecture& G, PlantParameters& par, PlantTraits& T) {

  // PARAMETERS
  // Use dynamic climate nitrogen as the soil concentration (N_s is the ini fallback)
  N_s = C.clim_acclim.nitrogen;

  // AGE EFFECT
  double age_factor = uptake_age(G, T);

  // TEMPERATURE EFFECT ON UPTAKE
  // Arrhenius response for root N transporter kinetics (u_max temperature sensitivity).
  // Ea = 50 kJ/mol gives Q10 ≈ 2 in the boreal temperature range, consistent with
  // measured root ion uptake kinetics (Kronzucker et al.; Bassirirad 2000).
  // Normalised to 1 at 25 °C (tkref = 298.15 K).
  // Hard-zero below 0 °C: frozen soil shuts off uptake.
  static constexpr double Ea_uptake = 50000.0; // J/mol, activation energy for N uptake
  double f_temp = (C.clim_inst.tc > 0.0)
      ? phydro::calc_ftemp_arrhenius(C.clim_inst.tc + 273.15, Ea_uptake)
      : 0.0;

  // CORE UPTAKE
  uptake_core(G, T, par);

  // Apply temperature scaling to both uptake pathways
  U_root *= f_temp;
  U_myco *= f_temp;

  // Maximum N transfer rate across the root-fungus membrane interface.
  // Caps how much mycorrhizal N the root can actually absorb, regardless of
  // how much the fungus takes up. Analogous to u_max for direct root uptake.
  max_N_transfer = u_transfer * mycorrhized * G.root_surface_area(T) * par.years_per_tunit_avg;
  max_C_transfer = c_transfer * mycorrhized * G.root_surface_area(T) * par.years_per_tunit_avg;

  // I_b: use the N actually delivered by the fungal network on the previous
  // timestep (G.N_export) rather than the current-step soil uptake estimate.
  // G.N_export already carries age_factor * m * f_temp via mycorrhizal_root_reduction,
  // so only the direct-root term needs age_factor applied here.
  {
    double N_eff = age_factor * (1.0 - mycorrhized) * U_root + G.N_export;
    I_b = std::max(N_eff / G.crown_area, 1e-10);
  }

  G.I_b = using_Ib ? I_b : 1.0;
  G.using_Ib = using_Ib;

  // UPTAKE
  double uptake_roots_term = (1.0 - mycorrhized) * U_root;

  // CORRECTED FOR TIMESTEP AND TEMPERATURE
  G.nitrogen_uptake_roots = age_factor * uptake_roots_term;

  // Mycorrhizal reduction factor (age, colonisation fraction, temperature).
  // The surface-area cap is applied inside dmyco_dt where N available is known.
  mycorrhizal_root_reduction = age_factor * mycorrhized * f_temp;
}

} // namespace plant

#endif // PLANT_UPTAKE_TPP