#ifndef PLANT_UPTAKE_TPP
#define PLANT_UPTAKE_TPP

#include <temperature_dependencies_photosynthesis.h>

namespace plant{

template<class _Climate>
void Uptake::nitrogen_plant(_Climate C, PlantArchitecture& G, PlantParameters& par, PlantTraits& T) {

  // Efficiency of N transfer from mycorrhiza to tree (lagged one timestep)
  // eta = N actually exported to tree / N taken up by mycorrhiza
  transfer_efficiency_photosynthesis = (U_myco > 1e-10)
      ? std::min(1.0, G.N_export / U_myco)
      : 0.0;

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

  // Recompute I_b with temperature and age corrections so it matches the
  // nitrogen that actually enters dnitrogen_dt_free.
  {
    double N_eff = age_factor * ((1.0 - mycorrhized) * U_root
        + transfer_efficiency_photosynthesis * mycorrhized * nitrogen_gate(G, T) * U_myco);
    I_b = std::max(N_eff / G.crown_area, 1e-10);
  }

  G.I_b = using_Ib ? I_b : 1.0;
  G.using_Ib = using_Ib;

  // UPTAKE
  double uptake_roots_term = (1.0 - mycorrhized) * U_root;

  // CORRECTED FOR TIMESTEP AND TEMPERATURE
  G.nitrogen_uptake_roots = age_factor * uptake_roots_term;

  // Mycorrhizal reduction due to root structure
  mycorrhizal_root_reduction = age_factor * nitrogen_gate(G, T) * mycorrhized * f_temp;
}

} // namespace plant

#endif // PLANT_UPTAKE_TPP