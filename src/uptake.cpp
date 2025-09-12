#include "plant.h"
using namespace std;

namespace plant {

  double Uptake::uptake_myco(double N, PlantArchitecture& G) {
    double biomass_limitation = G.ectomycorrhiza_mass / (G.ectomycorrhiza_mass + 2.0 * k_12);
    double nitrogen_uptake = (u_c_B * N * e_u_myco) / (u_c_B + N * e_u_myco);
    
    double nitrogen_capacity = G.ectomycorrhiza_mass * biomass_limitation * nitrogen_uptake; 
    
    // NOTE this is for all of the ectomycorrhizal biomass.
    // gN per kg C
    return nitrogen_capacity * N / (nitrogen_capacity + N);
  }
  
  double Uptake::uptake_roots(double N, PlantArchitecture& G, PlantTraits& T) {
    double root_length = G.root_length;
    double root_no = G.root_no;
    
    // kg / m3 * mm2 * mm * no * 1e-9 * no = kg C
    double one_root_mass = G.root_density(T) * pow(G.root_diameter(T)/2.0, 2.0) * root_length * M_PI * root_no * 1e-9;
    double surface_area = root_no * M_PI * (root_length * 1e-3) * (G.root_diameter(T) * 1e-3); // m
      
    // TODO: I think that this doesn't make sense with the units as the G.root_mass is for the whole root system, but the surface area transformation is for the surface area
    double biomass_conversion = 4 * one_root_mass / (G.root_density(T) * G.root_diameter(T) * 1e-3); // kg / (kg-1/m3 * mm * 1e-3) = m2
    double biomass_limitation = surface_area / (surface_area + 2.0 * k_13);
    double nitrogen_uptake = (u_c_B * N * e_u_root) / (u_c_B + N * e_u_root);
    
    double nitrogen_capacity = biomass_conversion * biomass_limitation * nitrogen_uptake;
    
    // gN per kg C
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
    double uptake_mycorrhiza_term = mycorrhized * nitrogen_gate(G, T) * investment_from_myco * uptake_myco(N, G);
    
    return 10; age_effect * (uptake_roots_term + uptake_mycorrhiza_term);
  }

  void Uptake::init(io::Initializer& I){
    
    // Nitrogen parameters
    k_8                   = I.get<double>("k_8");
    k_9                   = I.get<double>("k_9");
    k_11                  = I.get<double>("k_11");
    k_12                  = I.get<double>("k_12");
    k_13                  = I.get<double>("k_13");
    mycorrhized           = I.get<double>("mycorrhized");
    investment_from_myco  = I.get<double>("investment_from_myco");
    u_c_B                 = I.get<double>("u_c_B");
    e_u_root              = I.get<double>("e_u_root");
    e_u_myco              = I.get<double>("e_u_myco");
    q                     = I.get<double>("q");
    
  }

  void Uptake::nitrogen_based_root_optimisation(PlantArchitecture& G, PlantTraits& T, Uptake& U) {
    
    double cost = 0.0;
    double assimilation = 0.0;
    
    // Optimisation
    double optimisation = assimilation - U.q * cost;
    
    // TODO: calculate the zeta term and feed that to the outputs
    
    
    // These are the initial conditions, whilst this isn't working just have constant roots
    G.root_length = 1.5;
    G.root_no = 20;
  }

} // End namespace