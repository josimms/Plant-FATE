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
  uptake_core(G, T);
  
  // UPTAKE
  double uptake_roots_term = (1.0 - mycorrhized) * U_root;
  double uptake_mycorrhiza_term = mycorrhized * investment_from_myco * nitrogen_gate(G, T) * U_myco;
  
  // CORRECTED FOR TIMESTEP
  G.nitrogen_uptake = age_factor * (uptake_roots_term + uptake_mycorrhiza_term) * par.years_per_tunit_avg; // kg per year

}

} // namespace plant

#endif // PLANT_UPTAKE_TPP