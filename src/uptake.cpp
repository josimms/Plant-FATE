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
    D_static    = I.get<double>("D_static");
    N_s         = I.get<double>("N_s");
    f_static    = I.get<double>("f_static");
    depth       = I.get<double>("depth");
    k_8         = I.get<double>("k_8");
    k_13        = I.get<double>("k_13");
    k_15        = I.get<double>("k_15");
    u_transfer  = I.get<double>("u_transfer");
    investment_from_mycorrhiza = I.get<double>("investment_from_mycorrhiza");
    nc_ecm_min                 = I.get<double>("nc_ecm_min");
  }

  double Uptake::uptake_age(const PlantArchitecture& G, PlantTraits& T) {
    return 1.0 / (1.0 + exp(G.root_lifespan(T) - k_8));
  }

  // Numerically stable positive root of:
  //   N_bar^2 + (k_15 + alpha - N_s)*N_bar - k_15*N_s = 0
  // Uses the standard/conjugate form depending on sign of b to avoid
  // catastrophic cancellation when alpha >> N_s (diffusion-limited regime).
  double Uptake::compute_N_bar(double SA_active, double A_zone, double r_zone, double N_s_eff, double D_eff) const {
    if (SA_active <= 0.0 || A_zone <= 0.0) return N_s_eff;
    double alpha = u_max * SA_active * r_zone / (D_eff * A_zone);
    double b     = N_s_eff - k_15 - alpha;
    double disc  = std::sqrt(b * b + 4.0 * k_15 * N_s_eff);
    return (b >= 0.0) ? 0.5 * (b + disc) : 2.0 * k_15 * N_s_eff / (disc - b);
  }

  void Uptake::uptake_core(const PlantArchitecture& G, PlantTraits& traits, PlantParameters& par, double f_temp) {

    // Surface areas
    double SA_fr = G.root_surface_area(traits);
    double SA_m     = 4.0 * std::max(0.0, G.ectomycorrhiza_mass) / (rho_myco * myco_diameter);

    // Smooth downregulation: reduce effective SA before computing N_bar so
    // soil depletion reflects actually-active fungal surface area.
    double nc_ecm   = G.ectomycorrhiza_N_free / std::max(G.ectomycorrhiza_C_free, 1e-12);
    double x_nc     = (nc_ecm - nc_ecm_min) / (traits.nc_myco - nc_ecm_min);
    double f_uptake = 1.0 / (1.0 + std::exp(6.0 * (x_nc - 0.5)));
    double SA_m_eff = SA_m * f_uptake;

    // Zone geometry: semi-ellipsoidal zone, radius R = k_13 * crown_radius
    double crown_radius = std::sqrt(G.crown_area / M_PI);
    double R      = k_13 * crown_radius;
    double A_zone;
    if (R > depth) {
      double e = std::sqrt(R * R - depth * depth);
      A_zone = M_PI * R * R + M_PI * R * (depth * depth / e) * std::log((R + e) / depth);
    } else {
      // fallback: sphere (R == depth) or approximate
      A_zone = M_PI * R * R + M_PI * R * depth;
    }
    r_zone_val    = R;

    // --- Near-field: roots and mycorrhiza compete for diffusible (mineral) N pool ---
    double N_diffused = (1.0 - f_static) * N_s;
    SA_active_val     = (1.0 - mycorrhized) * SA_fr + SA_m_eff;

    // alpha diagnostic (near-field)
    alpha_val = (SA_active_val > 0.0 && A_zone > 0.0)
        ? u_max * SA_active_val * R / (D * A_zone)
        : 0.0;

    N_bar_roots        = compute_N_bar(SA_active_val, A_zone, R, N_diffused, D);
    double mm_near     = N_bar_roots / (N_bar_roots + k_15);

    U_root             = SA_fr * u_max * mm_near * par.years_per_tunit_avg;
    double U_myco_near = SA_m_eff * u_max * mm_near * par.years_per_tunit_avg;

    // --- Static (organic) pool: mycorrhiza mine via enzymatic depolymerisation ---
    // N_bar_static is the local organic N concentration at the hyphal surface.
    // It decreases with SA_m as more hyphae compete for the same finite pool.
    // k_SA (half-saturation fungal SA) is derived from supply/demand balance — no free parameter:
    //   supply = D_static * N_static * V_zone  [kg N yr^-1]
    //   demand per unit SA = u_max             [kg N m^-2 yr^-1]
    //   k_SA = supply / u_max                  [m^2]
    // Roots cannot access this pool.
    double N_static = f_static * N_s;
    N_static_val    = N_static;
    double V_zone   = (2.0 / 3.0) * M_PI * R * R * depth;
    double N_bar_static = 0.0;
    // Guard against near-zero (not just exactly-zero) SA_m_eff: as effective fungal surface
    // area shrinks toward zero (e.g. ECM population functionally extinct), N_bar_static's
    // closed-form inverse (k_15*k_SA/SA_m_eff) diverges even though the actual flux
    // (U_myco_static) correctly vanishes. With no fungal surface, nothing accesses this
    // pool, so accessible N via mycorrhiza should read 0, not diverge.
    if (SA_m_eff < 1e-9 || N_static <= 0.0) {
      U_myco_static = 0.0;
    } else {
      // Series combination of enzymatic supply (G_e) and transporter capacity (G_t):
      //   1/U = 1/G_e + 1/G_t  =>  U = G_e * SA_m / (SA_m + k_SA)
      // where G_e = D_eff*N_static*V_zone [kg N yr-1] and k_SA = G_e/u_max [m2].
      // See vignettes/organic_n_derivation.pdf for derivation.
      double D_eff             = D_static * f_temp;
      double enzymatic_supply  = D_eff * N_static * V_zone;
      double k_SA              = enzymatic_supply / u_max;
      U_myco_static            = enzymatic_supply * SA_m_eff / (SA_m_eff + k_SA) * par.years_per_tunit_avg;
      N_bar_static             = k_15 * k_SA / SA_m_eff;
    }
    N_bar_static_val = N_bar_static;

    U_myco = U_myco_near + U_myco_static;
  }

} // End namespace
