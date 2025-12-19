#ifndef PLANT_UPTAKE_H
#define PLANT_UPTAKE_H

#include "plant_architecture.h"
#include "traits_params.h"
#include "utils/initializer_v2.h"

namespace plant {

class Uptake {
public:
  // Parameters
  double k_8;
  double k_9;
  double k_11;
  double k_12;
  double k_13;
  double mycorrhized;
  double investment_from_myco;
  double u_c_B;
  double e_u_root;
  double e_u_myco;
  double q;
  
  // Uptake equations
  /// @{brief Uptake Functions
  double uptake_myco(double N, PlantArchitecture& G);
  double uptake_roots(double N, PlantArchitecture& G, PlantTraits& T);
  double uptake_age(const PlantArchitecture& G, PlantTraits& T);
  double nitrogen_gate(const PlantArchitecture& G, PlantTraits& T);
  
  template<class _Climate>
  void nitrogen_plant(_Climate C, PlantArchitecture& G, PlantTraits& T);
  /// @}
  
  // Initialisation function
  void init(io::Initializer& I);
  
  // Process function
  void nitrogen_based_root_optimisation(PlantArchitecture& G, PlantTraits& T, Uptake& U);
};

} // end of namespace

#include "uptake.tpp"

#endif // PLANT_UPTAKE_H
