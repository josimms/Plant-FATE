template<class Env>

namespace nitrogen {

nitrogen_process(clim C, PlantArchitecture& G, PlantTraits& T) {

  // STEP 1: Uptake the nitrogen
  
  // STEP 2: Calculate the internal nitrogen balance
  // TODO: ask and add the nirogen used in the growth / photosynthesis
  // TODO: Where should the nitrogen affect the assimilation?
  
  // STEP 3: Cost calculated in the assimilation class!
  
  // STEP 4: Optimisation
  double assimilation = 0; // TODO: dependent on the photosynthesis
  double optimisation = assimilation - q * cost;

  // TODO: calculate the zeta term and feed that to the outputs

}

};