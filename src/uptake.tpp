#ifndef PLANT_UPTAKE_TPP
#define PLANT_UPTAKE_TPP

namespace plant{

template<class _Climate>
void Uptake::nitrogen_plant(_Climate C, PlantArchitecture& G, PlantParameters& par, PlantTraits& T) {
  
  // PARAMETERS
  double N = C.clim_acclim.nitrogen;
  
  // AGE EFFECT
  double age_factor = uptake_age(G, T);
  
  // CORE UPTAKE
  uptake_core(G, T, par);
  
  // UPTAKE
  double uptake_roots_term = (1.0 - mycorrhized) * U_root;
  
  // CORRECTED FOR TIMESTEP
  G.nitrogen_uptake_roots = age_factor * uptake_roots_term; // kg per year
  
  // Mycorrhizal reduction due to root sturcture
  mycorrhizal_root_reduction = age_factor * nitrogen_gate(G, T) * mycorrhized;
}

} // namespace plant

#endif // PLANT_UPTAKE_TPP