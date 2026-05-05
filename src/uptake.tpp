#ifndef PLANT_UPTAKE_TPP
#define PLANT_UPTAKE_TPP

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

  // CORE UPTAKE
  uptake_core(G, T, par);

  G.I_b = using_Ib ? I_b : 1.0;

  // UPTAKE
  double uptake_roots_term = (1.0 - mycorrhized) * U_root;
  
  // CORRECTED FOR TIMESTEP
  G.nitrogen_uptake_roots = age_factor * uptake_roots_term;
  
  // Mycorrhizal reduction due to root structure
  mycorrhizal_root_reduction = age_factor * nitrogen_gate(G, T) * mycorrhized;
}

} // namespace plant

#endif // PLANT_UPTAKE_TPP