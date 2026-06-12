#ifndef PLANT_UPTAKE_H
#define PLANT_UPTAKE_H

#include "plant_architecture.h"
#include "traits_params.h"
#include "utils/initializer_v2.h"

namespace plant {

class Uptake {

public:

  double U_root  = 0.0;
  double U_myco  = 0.0;
  double I_b     = 1.0;  ///< Belowground infrastructure index (Eq. belowground_infra)
  bool   using_Ib = true; ///< Toggle: if false, I_b is fixed at 1 (no infrastructure cost)

  // Diagnostics (set by uptake_core, readable for logging)
  double N_bar_val    = 0.0;  ///< Mean N concentration at absorbing surface [kg N m^-3]
  double r_zone_val   = 0.0;  ///< Root zone radius [m]
  double alpha_val    = 0.0;  ///< Diffusion-demand ratio alpha_tip [-]
  double SA_active_val = 0.0; ///< Active absorbing surface area [m^2]

  double mycorrhized;
  double mycorrhizal_root_reduction;
  double transfer_efficiency_photosynthesis;
  double myco_diameter;
  double rho_myco;
  double D;
  double N_s;
  double u_max;
  double depth;

  double k_8;
  double k_9;
  double k_13;   ///< Zone radius factor: R_zone = k_13 * crown_radius
  double k_15;   ///< Biochemical half-saturation constant [kg N m^-3]

  // Uptake equations
  /// @{brief Uptake Functions
  double uptake_age(const PlantArchitecture& G, PlantTraits& T);
  double nitrogen_gate(const PlantArchitecture& G, PlantTraits& T);

  double compute_N_bar(double SA_active, double A_zone, double r_zone) const;

  void uptake_core(const PlantArchitecture& G, PlantTraits& traits, PlantParameters& par);

  template<class _Climate>
  void nitrogen_plant(_Climate C, PlantArchitecture& G, PlantParameters& par, PlantTraits& T);
  /// @}

  // Initialisation function
  void init(io::Initializer& I);
};

} // end of namespace

#include "uptake.tpp"

#endif // PLANT_UPTAKE_H
