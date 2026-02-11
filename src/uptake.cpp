#include "plant.h"
#include "traits_params.h"
#include "plant_architecture.h"
#include "uptake.h" 
using namespace std;

namespace plant {

  // Uptake function
  void Uptake::init(io::Initializer& I){
    
    // Nitrogen parameters
    myco_diameter     = I.get<double>("myco_diameter");
    rho_myco          = I.get<double>("rho_myco");
    D                 = I.get<double>("D");
    N_s               = I.get<double>("N_s");
    u_max             = I.get<double>("u_max");
    u_max_kg          = I.get<double>("u_max_kg");
    C_r               = I.get<double>("C_r");
    depth             = I.get<double>("depth");
    k8                = I.get<double>("k8");
    k20               = I.get<double>("k20");
    k21               = I.get<double>("k21");
    k23               = I.get<double>("k23");
    
  }

  // Uptake with age reduction
  double Uptake::uptake_age(const PlantArchitecture& G, PlantTraits& T) {
    return 1/(1 + exp(G.root_lifespan(T)) - k_8);
  }
  
  // Nitrogen uptake gate
  double Uptake::nitrogen_gate(const PlantArchitecture& G, PlantTraits& T) {
    double surface_area = G.root_no * M_PI * G.root_length * G.root_diameter(T);
    
    return surface_area/(surface_area + k_9);
  }

  // Depletion radius calculation
  double Uptake::rd() {
    return D * N_flux / u_max;
  }

  // Surface area explored per root biomass
  double Uptake::e_u_root(double r_d, double root_diameter_mm, double root_density) {
    double d_m <- root_diameter_mm / 1000;
    return 4.0 * pow(r_d, 2.0) / (root_density * pow(d_m, 2.0));
  }

  // Surface area explored per mycorrhizal biomass
  double Uptake::e_u_myco(double r_d, double myco_diameter_m, double myco_density) {
    return 4.0 * pow(r_d, 2.0) / (myco_density * pow(myco_diameter_m, 2.0));
  }

  // Crowding function
  double Uptake::S_crowding(double B_root, double B_myco, double rho_root, double e_root, double e_myco) {

    // --- Ellipsoid soil volume ---
    V_soil = (4.0/3.0) * pi * pow(k20 * C_r, 2.0) * depth;
      
    // --- Free soil volume corrected for porosity and existing biomass ---
    V_free = k21 * V_soil - B_root / rho_root - B_myco / rho_myco;
      
    // --- Total explored volume --- TODO: coarse roots
    V_explored = B_root * e_root + B_myco * e_myco;
      
    // --- Saturation function ---
    S <- if (V_explored > V_free) {
      V_free/V_explored;
    } else 1;
      
    return(S)
  }

  // Core uptake function
  UptakeResult Uptake::uptake_core(const PlantArchitecture& G, PlantTraits& traits) {
    
    // --- Generate biomass ---
    B_root = root_mass(traits);
    d_root_m = root_diameter(traits); // TODO check units
    rho_root = root_density(traits);
    
    // --- Depletion ratio ---
    deplition_radius = r_d(D, N_s, u_max);
    
    // Soil surface per root biomass
    e_root = e_u_root(deplition_radius, d_root_m, rho_root);
    e_myco = e_u_myco(deplition_radius, myco_diameter, rho_myco);
    
    // --- Compute saturation factor ---
    Sval = S_crowding(B_root, G.ectomycorrhiza_mass, e_root, e_myco);
        
    // --- Final uptake ---
    U_root_c = B_root * u_max_kg * Sval;
    U_myco_c = G.ectomycorrhiza_mass * u_max_kg * Sval;
        
    U_s_root = N_s * e_root / (N_s * e_root + k23);
    U_s_myco = N_s * e_myco / (N_s * e_myco + k23);
          
    U_root = U_root_c * U_s_root;
    U_myco = U_myco_c * U_s_myco;
        
    // --- Return all relevant info ---
    return {U_root, U_myco, Sval, deplition_radius, e_root, e_myco};
  }

} // End namespace