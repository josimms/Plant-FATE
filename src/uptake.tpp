#ifndef PLANT_UPTAKE_TPP
#define PLANT_UPTAKE_TPP

namespace plant{

// Plant uptake
  // const PlantArchitecture& G, PlantTraits& T
  double Uptake::plant_uptake(double crown_area, double root_length, double root_no, 
                              traits, double mycorrhized, double investment_from_tree, bool myco_true = TRUE) {
    
    // --- Root functions ----
    root_d_mm = root_diameter(root_length, traits);
    rho_root = root_density(root_length, traits);
    age = root_lifespan(root_length, traits);
      
    B_root = root_mass(root_length, root_no, crown_area, traits);
      
    // --- Core uptake calculation ---
    core <- uptake_core(
        B_root = B_root,
        B_myco = ectomycorrhiza_mass,
        root_d_mm = root_d_mm
    );
          
  }

template<class _Climate>
void Uptake::nitrogen_plant(_Climate C, PlantArchitecture& G, PlantParameters& par, PlantTraits& T) {
  
  // PARAMETERS
  double N = C.clim_acclim.nitrogen;
  
  // AGE EFFECT
  double age_factor = uptake_age(G, T);
      
  // CORE UPTAKE
  UptakeResult core = uptake_core(G, T);
  
  // UPTAKE
  double uptake_roots_term = (1.0 - mycorrhized) * core.U_root;
  double uptake_mycorrhiza_term = mycorrhized * nitrogen_gate(G, T) * investment_from_myco * core.U_myco;
  
  // CORRECTED FOR TIMESTEP
  G.nitrogen_uptake = age_factor * (uptake_roots_term + uptake_mycorrhiza_term) * par.years_per_tunit_avg; // kg per year
}

} // namespace plant

#endif // PLANT_UPTAKE_TPP