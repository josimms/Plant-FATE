#include "plant.h"
#include "traits_params.h"
#include "plant_architecture.h"
#include "uptake.h" 
using namespace std;

namespace plant {

  // Uptake function
  void Uptake::init(io::Initializer& I){
    
    // Nitrogen parameters
    mycorrhized           = I.get<double>("mycorrhized");
    u_max                 = I.get<double>("u_max");
    u_max_kg              = I.get<double>("u_max_kg");
    myco_diameter         = I.get<double>("myco_diameter");
    rho_myco              = I.get<double>("rho_myco");
    D                     = I.get<double>("D");
    N_s                   = I.get<double>("N_s");
    depth                 = I.get<double>("depth");
    k_8                   = I.get<double>("k_8");
    k_9                   = I.get<double>("k_9");
    k_20                  = I.get<double>("k_20");
    k_21                  = I.get<double>("k_21");
    k_23                  = I.get<double>("k_23");
    
  }

  // Uptake with age reduction
  double Uptake::uptake_age(const PlantArchitecture& G, PlantTraits& T) {
    return 1.0 / (1.0 + exp(G.root_lifespan(T)) - k_8);
  }
  
  // Nitrogen uptake gate
  double Uptake::nitrogen_gate(const PlantArchitecture& G, PlantTraits& T) {
    double surface_area = G.root_no * M_PI * G.root_length * G.root_diameter(T);
    
    return surface_area/(surface_area + k_9);
  }

  // Depletion radius calculation
  double Uptake::deplition_radius() {
    return D * N_s / u_max;
  }

  // Surface area explored per root biomass
  double Uptake::e_u_root(double rd, double root_diameter_mm, double root_density) {
    double d_m = root_diameter_mm / 1000;
    return 4.0 * pow(rd, 2.0) / (root_density * pow(d_m, 2.0));
  }

  // Surface area explored per mycorrhizal biomass
  double Uptake::e_u_myco(double rd, double myco_diameter_m, double myco_density) {
    return 4.0 * pow(rd, 2.0) / (myco_density * pow(myco_diameter_m, 2.0));
  }

  // Crowding function
  double Uptake::S_crowding(double B_root, double B_myco, double rho_root, double e_root, double e_myco, double crown_area) {
    
    // --- Ellipsoid soil volume ---
    double V_soil = (4.0/3.0) * M_PI * pow(k_20 * crown_area, 2.0) * depth;
      
    // --- Free soil volume corrected for porosity and existing biomass ---
    double V_free = k_21 * V_soil - B_root / rho_root - B_myco / rho_myco;
      
    // --- Total explored volume --- TODO: coarse roots
    double V_explored = B_root * e_root + B_myco * e_myco;
      
    // --- Saturation function ---
    double S;
    if (V_explored > V_free) {
      S = V_free / V_explored;
    } else {
      S = 1.0;
    }
      
    return S;
  }

  // Core uptake function
  void Uptake::uptake_core(const PlantArchitecture& G, PlantTraits& traits, PlantParameters& par) {
    
    // --- Generate biomass ---
    double B_root = G.root_mass(traits);
    double d_root_m = G.root_diameter(traits); // TODO check units
    double rho_root = G.root_density(traits);
    
    // --- Depletion ratio ---
    rd = deplition_radius();
    
    // Soil surface per root biomass
    e_root = e_u_root(rd, d_root_m, rho_root);
    e_myco = e_u_myco(rd, myco_diameter, rho_myco);
    
    // --- Compute saturation factor ---
    Sval = S_crowding(B_root, G.ectomycorrhiza_mass, rho_root, e_root, e_myco, G.crown_area);
    
    // --- Final uptake ---
    double U_root_c = B_root * u_max_kg * Sval;
    double U_myco_c = G.ectomycorrhiza_mass * u_max_kg * Sval;
        
    double U_s_root = N_s * e_root / (N_s * e_root + k_23);
    double U_s_myco = N_s * e_myco / (N_s * e_myco + k_23);
          
    U_root = U_root_c * U_s_root * par.years_per_tunit_avg;
    U_myco = U_myco_c * U_s_myco * par.years_per_tunit_avg;
    
  }

} // End namespace