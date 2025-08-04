#include "plant.h"
using namespace std;

namespace plant {

  double nitrogen::uptake_myco(double N) {
    // TODO: can I pull this from somewhere?
    double myco_biomass;
    
    // Parameters
    double k_12;
    double u_c_B;
    double e_u_myco;
    
    double biomass_limitation = myco_biomass / (myco_biomass + 2.0*k_12);
    double nitrogen_uptake = (u_c_B * N * e_u_myco) / (u_c_B + N * e_u_myco);
    
    double nitrogen_capacity = myco_biomass/2.0 * biomass_limitation * nitrogen_uptake; 
    
    return nitrogen_capacity * N / (nitrogen_capapcity + N);
  }
  
  double nitrogen::uptake_roots(double N, double _rl, double _rn) {
    double root_length = _rl;
    double root_no = _rn;
    
    // TODO: can I pull this from somewhere?
    double root_biomass;
    
    // Parameters
    double k_13;
    double u_c_B;
    double e_u_root;
    
    double surface_area = root_no * M_PI * root_length * root_diameter(root_length);
    double biomass = root_no * M_PI * pow(root_length/2.0, 2.0) * 
      
      double biomass_conversion = 4 * biomass / (root_denisty(root_length) * root_diameter(root_length));
    double biomass_limitation = surface_area / (surface_area + 2.0*k_13);
    double nitrogen_uptake = (u_c_B * N * e_u_root) / (u_c_B + N * e_u_root);
    
    double nitrogen_capacity = biomass_conversion * biomass_limitation * nitrogen_uptake
    
    return nitrogen_capacity * N / (nitrogen_capacity + N);
  }
  
  double nitrogen::uptake_age(double _rl) {
    double root_length = _rl;
    // TODO: this should join some sort of structure
    double k_8;
    
    return 1/(1 + exp(root_lifespan(root_length)) - k_8);
  }
  
  double nitrogen::nitrogen_gate(double _rl, double _rn) {
    double root_length = _rl;
    double root_no = _rn;
    
    double surface_area = root_no * M_PI * root_length * root_diameter(root_length);
    
    return surface_area/(surface_area + k_9);
  }
  
  double nitrogen::nitrogen_plant(double N, double _rl, double _rn) {
    double root_length = _rl;
    double root_no = _rn;
    
    // Parameters
    double mycorrhized;
    double investment_myco;
    
    double age = uptake_age(root_length);
    
    uptake_roots_term = (1 - mycorrhized) * uptake_roots(N, root_length, root_no); 
    uptake_mycorrhiza_term = mycorrhized * nitrogen_gate(root_length, root_no) * investment_myco * uptake_mycorrhiza(N);
    
    return age * (uptake_roots_term + uptake_mycorrhiza_term);
  }

}