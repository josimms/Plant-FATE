#include "uptake.h"

namespace plant {

namespace uptake {

template<class Env>
void nitrogen_based_root_optimisation(PlantArchitecture& G, PlantTraits& T, Uptake& U) {

  double cost = 0.0;
  double assimilation = 0.0;

  // Optimisation
  double optimisation = assimilation - U.q * cost;

  // TODO: calculate the zeta term and feed that to the outputs
  
  G.root_length = 0.0;
  G.root_no = 0.0;
  // G.zeta = 0.0; // TODO: is this from the assimilation file?
}

} // End namespace plant

} // End namespace upatake