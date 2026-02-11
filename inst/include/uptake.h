#ifndef PLANT_UPTAKE_H
#define PLANT_UPTAKE_H

#include "plant_architecture.h"
#include "traits_params.h"
#include "utils/initializer_v2.h"

namespace plant {

class Uptake {
  struct UptakeResult {
    double U_root;
    double U_myco;
    double Sval;
    double rd;
    double e_root;
    double e_myco;
  };
  
  
public:
  double myco_diameter;
  double rho_myco;
  double D;
  double N_s;
  double u_max; 
  double u_max_kg; 
  double C_r;
  double depth;
  double k8;
  double k20;
  double k21;
  double k23;
  
  // Uptake equations
  /// @{brief Uptake Functions
  double uptake_age(const PlantArchitecture& G, PlantTraits& T);
  double nitrogen_gate(const PlantArchitecture& G, PlantTraits& T);
  
  double rd();
  double e_u_root(double r_d, double root_diameter_mm, double root_density);
  double e_u_myco(double r_d, double myco_diameter_m, double myco_density);
  double S_crowding(double B_root, double B_myco, double rho_root, double e_root, double e_myco);
  UptakeCoreResult Uptake::uptake_core(const PlantArchitecture& G, PlantTraits& traits);
    
  
  template<class _Climate>
  void nitrogen_plant(_Climate C, PlantArchitecture& G, PlantParameters& par, PlantTraits& T);
  /// @}
  
  // Initialisation function
  void init(io::Initializer& I);
};

} // end of namespace

#include "uptake.tpp"

#endif // PLANT_UPTAKE_H
