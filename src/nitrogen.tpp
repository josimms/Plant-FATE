template<class Env>
nitrogen_process(clim C, const PlantArchitecture& G) {

  // TODO: climate should have nitrogen in it as an input, maybe "if no N constant N" could be added
  
  // TODO: where should the ectomycorrhiza be in the code?
  uptake_myco;
  
  // TODO: need to initiate U in the init part
  nitrogen::nitrogen_plant(C.nitrogen, const PlantArchitecture& G, uptake U);
  
  // Optimisation should be added here
  
  // Cost - nitrogen

}