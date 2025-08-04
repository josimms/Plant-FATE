namespace plant {

  class nitrogen {
  
    public:
      
    struct {
      // 
      k_8
      k_9
      k_10
      k_11
      k_12
      k_13
    } uptake;
      
    public:
    // Uptake equations
    double uptake_myco(double N);
    double uptake_roots(double N, double _rl, double _rn);
    double uptake_age(double _rl);
    double nitrogen_gate(double _rl, double _rn);
    double nitrogen_plant(double N, double _rl, double _rn);
  
  };


}

  