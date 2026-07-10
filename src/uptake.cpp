#include "plant.h"
#include "traits_params.h"
#include "plant_architecture.h"
#include "uptake.h"
using namespace std;

namespace plant {

  void Uptake::init(io::Initializer& I){
    using_Ib    = (I.get<std::string>("using_Ib") == "true");
    mycorrhized = I.get<double>("mycorrhized");
    u_max       = I.get<double>("u_max");
    myco_diameter = I.get<double>("myco_diameter");
    rho_myco    = I.get<double>("rho_myco");
    D           = I.get<double>("D");
    N_s         = I.get<double>("N_s");
    f_myco_pool = I.get<double>("f_myco_pool");
    depth       = I.get<double>("depth");
    k_8         = I.get<double>("k_8");
    k_13        = I.get<double>("k_13");
    k_15        = I.get<double>("k_15");
    u_transfer  = I.get<double>("u_transfer");
    c_transfer  = I.get<double>("c_transfer");
  }

  double Uptake::uptake_age(const PlantArchitecture& G, PlantTraits& T) {
    return 1.0 / (1.0 + exp(G.root_lifespan(T) - k_8));
  }

  // Numerically stable positive root of:
  //   N_bar^2 + (k_15 + alpha - N_s)*N_bar - k_15*N_s = 0
  // Uses the standard/conjugate form depending on sign of b to avoid
  // catastrophic cancellation when alpha >> N_s (diffusion-limited regime).
  double Uptake::compute_N_bar(double SA_active, double A_zone, double r_zone, double N_s_eff) const {
    if (SA_active <= 0.0 || A_zone <= 0.0) return N_s_eff;
    double alpha = u_max * SA_active * r_zone / (D * A_zone);
    double b     = N_s_eff - k_15 - alpha;
    double disc  = std::sqrt(b * b + 4.0 * k_15 * N_s_eff);
    return (b >= 0.0) ? 0.5 * (b + disc) : 2.0 * k_15 * N_s_eff / (disc - b);
  }

  void Uptake::uptake_core(const PlantArchitecture& G, PlantTraits& traits, PlantParameters& par) {

    // Surface areas
    double SA_fr = G.root_surface_area(traits);
    double SA_m  = 4.0 * std::max(0.0, G.ectomycorrhiza_mass) / (rho_myco * myco_diameter);

    // Zone geometry: semi-ellipsoidal zone, radius R = k_13 * crown_radius
    double crown_radius = std::sqrt(G.crown_area / M_PI);
    double R      = k_13 * crown_radius;
    double A_zone;
    if (R > depth) {
      double e = std::sqrt(R * R - depth * depth);
      A_zone = M_PI * R * R + M_PI * (depth * depth / e) * std::log((R + e) / depth);
    } else {
      // fallback: sphere (R == depth) or approximate
      A_zone = M_PI * R * R + M_PI * R * depth;
    }
    r_zone_val    = R;

    // --- Near-field: roots and mycorrhiza compete for mineral N pool ---
    // Roots only access (1-f_myco_pool) of N_s; mycorrhiza compete in the same zone.
    double N_s_near = (1.0 - f_myco_pool) * N_s;
    SA_active_val   = (1.0 - mycorrhized) * SA_fr + SA_m;

    // alpha diagnostic (near-field)
    alpha_val = (SA_active_val > 0.0 && A_zone > 0.0)
        ? u_max * SA_active_val * R / (D * A_zone)
        : 0.0;

    N_bar_val       = compute_N_bar(SA_active_val, A_zone, R, N_s_near);
    double mm_near  = N_bar_val / (N_bar_val + k_15);

    U_root          = SA_fr * u_max * mm_near * par.years_per_tunit_avg;
    double U_myco_near = SA_m * u_max * mm_near * par.years_per_tunit_avg;

    // --- Far-field: mycorrhiza-only access to organic N pool ---
    // Same zone geometry (average tree in a stand, not isolated), but only
    // mycorrhizal surface area can exploit this fraction of the total N_s.
    double N_s_far  = f_myco_pool * N_s;
    double N_bar_far = compute_N_bar(SA_m, A_zone, R, N_s_far);
    double mm_far   = N_bar_far / (N_bar_far + k_15);
    double U_myco_far = SA_m * u_max * mm_far * par.years_per_tunit_avg;

    U_myco = U_myco_near + U_myco_far;
  }

} // End namespace
