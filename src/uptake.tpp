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

  // Downregulate ECM uptake when fungal N pool is replete.
  // Mirrors the transfer ramp: same two thresholds (nc_ecm_min, nc_myco) control both
  // input (uptake) and output (transfer), so only one new parameter is needed.
  // f_uptake = 1 when N-poor (nc_ecm <= nc_ecm_min, C:N >= 60): full uptake rate.
  // f_uptake = 0 when N-replete (nc_ecm >= nc_myco, C:N <= 10): uptake shuts off.
  {
    // Both numerator and denominator use free (labile) pools only.
    // When both are zero: nc_ecm = 0 → f_uptake = 1 (allow uptake to start).
    // When N_free > 0 but C_free ≈ 0: ratio >> nc_myco → f_uptake = 0
    // (fungus has excess N relative to C, don't acquire more).
    double nc_ecm   = G.ectomycorrhiza_N_free / std::max(G.ectomycorrhiza_C_free, 1e-12);
    double f_uptake = 1.0 - std::max(0.0, std::min(1.0,
        (nc_ecm - nc_ecm_min) / (T.nc_myco - nc_ecm_min)));
    U_myco *= f_uptake;
  }

  // Maximum N transfer capacity at the root-fungus interface.
  // root_interface [m² yr tunit⁻¹]: colonised root surface area × time scaling.
  // u_transfer is flux per unit surface area [kg m⁻² yr⁻¹],
  // so max_N_transfer has units of [kg tunit⁻¹].
  double root_interface = age_factor * mycorrhized * G.root_surface_area(T) * par.years_per_tunit_avg;
  max_N_transfer = u_transfer * root_interface;
  
  // UPTAKE ROOTS
  G.nitrogen_uptake_roots = age_factor * (1.0 - mycorrhized) * U_root;

  // I_b: current-timestep N flux from roots and mycorrhiza only (no N store).
  // Excluding the N store lets I_b reflect actual soil N supply, which varies
  // meaningfully between low-N and high-N conditions.
  {
    double N_eff = G.nitrogen_uptake_roots + G.N_export;
    I_b = std::max(N_eff / G.crown_area, 1e-10);
  }

  G.I_b = using_Ib ? I_b : 1.0;
  G.using_Ib = using_Ib;

  // Mycorrhizal reduction factor (age, colonisation fraction, temperature).
  mycorrhizal_root_reduction = age_factor * mycorrhized * f_temp;
}

} // namespace plant

#endif // PLANT_UPTAKE_TPP