#include "plant.h"
using namespace std;

namespace plant {

  double Uptake::uptake_myco(double N) {
    // TODO: can I pull this from somewhere?
    double myco_biomass;
    
    double biomass_limitation = myco_biomass / (myco_biomass + 2.0 * k_12);
    double nitrogen_uptake = (u_c_B * N * e_u_myco) / (u_c_B + N * e_u_myco);
    
    double nitrogen_capacity = myco_biomass/2.0 * biomass_limitation * nitrogen_uptake; 
    
    return nitrogen_capacity * N / (nitrogen_capacity + N);
  }
  
  double Uptake::uptake_roots(double N, PlantArchitecture& G, PlantTraits& T) {
    double root_length = G.root_length;
    double root_no = G.root_no;
    
    double root_mass_calc = G.root_mass(T);
    
    double surface_area = root_no * M_PI * root_length * G.root_diameter(T);
      
    double biomass_conversion = 4 * root_mass_calc / (G.root_density(T) * G.root_diameter(T));
    double biomass_limitation = surface_area / (surface_area + 2.0 * k_13);
    double nitrogen_uptake = (u_c_B * N * e_u_root) / (u_c_B + N * e_u_root);
    
    double nitrogen_capacity = biomass_conversion * biomass_limitation * nitrogen_uptake;
    
    return nitrogen_capacity * N / (nitrogen_capacity + N);
  }
  
  double Uptake::uptake_age(const PlantArchitecture& G, PlantTraits& T) {
    return 1/(1 + exp(G.root_lifespan(T)) - k_8);
  }
  
  double Uptake::nitrogen_gate(const PlantArchitecture& G, PlantTraits& T) {
    double root_length = G.root_length;
    double root_no = G.root_no;
    
    double surface_area = root_no * M_PI * root_length * G.root_diameter(T);
    
    return surface_area/(surface_area + k_9);
  }
  
  double Uptake::nitrogen_plant(double N, PlantArchitecture& G, PlantTraits& T) {
    double root_length = G.root_length;
    double root_no = G.root_no;
    
    double age_effect = uptake_age(G, T);
    
    double uptake_roots_term = (1 - mycorrhized) * uptake_roots(N, G, T); 
    double uptake_mycorrhiza_term = mycorrhized * nitrogen_gate(G, T) * investment_myco * uptake_myco(N);
    
    return age_effect * (uptake_roots_term + uptake_mycorrhiza_term);
  }

  void Uptake::init(io::Initializer& I){
    
    // Nitrogen parameters
    k_8                 = I.get<double>("k_8");
    k_9                 = I.get<double>("k_9");
    k_10                = I.get<double>("k_10");
    k_11                = I.get<double>("k_11");
    k_12                = I.get<double>("k_12");
    k_13                = I.get<double>("k_13");
    mycorrhized         = I.get<double>("mycorrhized");
    investment_myco     = I.get<double>("investment_myco");
    u_c_B               = I.get<double>("u_c_B");
    e_u_root            = I.get<double>("e_u_root");
    e_u_myco            = I.get<double>("e_u_myco");
    q                   = I.get<double>("q");
    
  }

} // End namespace