namespace plant {

  class nitrogen {
  
    public:
      
    struct {
      // Parameters
      k_8;
      k_9;
      k_10;
      k_11;
      k_12;
      k_13;
      mycorrhized;
      investment_myco;
      // Nitrogen state
      tree_nitrogen;
    } uptake;
      
    public:
    // Uptake equations
    double uptake_myco(double N);
    double uptake_roots(double N, const PlantArchitecture& G);
    double uptake_age(const PlantArchitecture& G);
    double nitrogen_gate(const PlantArchitecture& G);
    double nitrogen_plant(double N, const PlantArchitecture& G, uptake U);
  
  };


}

  