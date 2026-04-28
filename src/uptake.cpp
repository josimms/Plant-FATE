#include "plant.h"
#include "traits_params.h"
#include "plant_architecture.h"
#include "uptake.h" 
using namespace std;

namespace plant {

  // Uptake function
  void Uptake::init(io::Initializer& I){
    
    // Nitrogen parameters
    using_Ib              = (I.get<std::string>("using_Ib") == "true");
    mycorrhized           = I.get<double>("mycorrhized");
    u_max                 = I.get<double>("u_max");
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
    return 1.0 / (1.0 + exp(G.root_lifespan(T) - k_8));
  }
  
  // Nitrogen uptake gate
  double Uptake::nitrogen_gate(const PlantArchitecture& G, PlantTraits& T) {
    double surface_area = G.root_no * M_PI * G.root_length * G.root_diameter(T);
    
    return surface_area/(surface_area + k_9);
  }

  // Depletion radius calculation
  double Uptake::deplition_radius() {
    return D * (N_s + k_23) / u_max;
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
  double Uptake::S_crowding(double B_root, double B_myco, double rho_root, double e_root, double e_myco, double crown_radius) {
    
    // --- Ellipsoid soil volume ---
    double V_soil = (2.0/3.0) * M_PI * pow(k_20 * crown_radius, 2.0) * depth;
      
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
    double d_root_mm = G.root_diameter(traits);
    double rho_root = G.root_density(traits);
    double u_Bmax_fr  = 4.0 * u_max / (rho_root * d_root_mm/1000.0);
    double u_Bmax_m   = 4.0 * u_max / (rho_myco * myco_diameter);
    
    // --- Depletion ratio ---
    rd = deplition_radius();
    
    // Soil surface per root biomass
    e_root = e_u_root(rd, d_root_mm, rho_root);
    e_myco = e_u_myco(rd, myco_diameter, rho_myco);

    // Belowground infrastructure index (Eq. belowground_infra)
    // I_b = (1-m)*c(n,l)*e_fr + η*m*g(n,l)*(B_m/A_cw)*e_m
    double c_nl = B_root / (2.0 * G.crown_area);
    I_b = (1.0 - mycorrhized) * c_nl * e_root
        + transfer_efficiency_photosynthesis * mycorrhized * nitrogen_gate(G, traits) * (G.ectomycorrhiza_mass / G.crown_area) * e_myco;

    // --- Compute saturation factor ---
    double crown_radius = std::sqrt(G.crown_area / M_PI);
    Sval = S_crowding(B_root, G.ectomycorrhiza_mass, rho_root, e_root, e_myco, crown_radius);
    
    double U_s = N_s / (N_s + k_23);
    
    U_root = B_root * u_Bmax_fr * Sval * U_s * par.years_per_tunit_avg;
    U_myco = G.ectomycorrhiza_mass * u_Bmax_m * Sval * U_s * par.years_per_tunit_avg;
          
  }

} // End namespace