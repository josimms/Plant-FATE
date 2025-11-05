#include "assimilation.h"

#include <cmath>

namespace plant{

// adjusted for time unit
void Assimilator::les_update_lifespans(double lai, PlantParameters& par, PlantTraits& traits){
	double hT = plant_assim.vcmax_avg / plant_assim.vcmax25_avg;
	double f = 1;
	double fac = sqrt(((par.les_k1 * par.les_k2) * (par.les_k1 * par.les_k2) * f * hT * plant_assim.mc_avg) / (2 * par.les_u * par.les_cc));

	kappa_l = 365 * plant_assim.vcmax25_avg / (traits.lma * 1e3 * lai) * fac * par.years_per_tunit_avg; // convert yr-1 --> t_unit-1
	// kappa_r no longer used
	//kappa_r = 365 * plant_assim.vcmax25_avg / (0.1333 * 1e3) * fac * par.years_per_tunit_avg;           // convert yr-1 --> t_unit-1
	//kappa_r = kappa_l * (par.les_cc/lai - 1) / (traits.zeta / traits.lma);
}


double Assimilator::les_assim_reduction_factor(phydro::PHydroResultNitrogen& res, PlantParameters& par){
	double hT = res.vcmax / res.vcmax25;
	double f = 1;
	return 1; // Not applying age-related reduction factor because Phydro is already calibrated for average leaves (not young leaves)
	// return 1 - sqrt(par.les_cc / (2 * par.les_u * res.mc * hT * f));
}


//// leaf respiration rate - should be calculated AFTER asimialtion (needs updated Phydro outputs)
double Assimilator::leaf_respiration_rate(PlantArchitecture* G, PlantParameters& par, PlantTraits& traits){
	//double vcmax_kg_yr = photo_leaf.vcmax * par.cbio * G->leaf_area;  // mol-CO2 m-2 year-1 * kg / mol-CO2 * m2
	//return par.rd * vcmax_kg_yr;
	return plant_assim.rleaf; // + par.rl * G->leaf_mass(traits);
}

// rate adjusted for time unit
double Assimilator::root_respiration_rate(PlantArchitecture* G, PlantParameters& par, PlantTraits& traits){
  // (par.rr * par.years_per_tunit_avg) * G->root_mass(traits)
  double gpp_annual = plant_assim.gpp / par.years_per_tunit_avg;
	return (par.rr * par.years_per_tunit_avg) * G->root_mass(traits) * (gpp_annual / G->crown_area);
}

// cost for the root arhcitexture optimisation
double Assimilator::root_cost(PlantArchitecture* G, PlantParameters& par, PlantTraits& traits) {
  double biomass_cost = 1e-12/2.0 * G->root_mass(traits) / G->root_lifespan(traits); // carbon not biomass
  double respiration = root_respiration_rate(G, par, traits);
  // TODO: shouldn't there also be exudate cost here?
  // TODO: should this function be here?
  return biomass_cost + respiration;
}

// rate adjusted for time unit
double Assimilator::sapwood_respiration_rate(PlantArchitecture* G, PlantParameters& par, PlantTraits& traits){
	//return par.rs * G->sapwood_mass(traits);
//	double dpsi_gravity = (1000*10*G->height/1e6);
	double factor = traits.p50_xylem;
	double factor1 = (1 + par.p50x_cost * factor * factor); // 3e3 7e3
	return (par.rs * par.years_per_tunit_avg) * G->sapwood_mass(traits) * factor1; // * (plant_assim.gpp/G->crown_area);
}

double Assimilator::leaf_turnover_rate(double _kappa_l, PlantArchitecture* G, PlantParameters& par, PlantTraits& traits){
	return G->leaf_mass(traits) * _kappa_l; // / traits.ll;	
}

//double Assimilator::root_turnover_rate(double _kappa_r, PlantArchitecture* G, PlantParameters& par, PlantTraits& traits){
//  return G->root_mass(traits)/G->root_lifespan(traits); // / par.lr;
//}

double Assimilator::root_turnover_rate(PlantArchitecture* G, const PlantTraits& traits){
  // TODO: Does root mass include the zeta / z term?
  return  G->root_mass(traits) / G->root_lifespan(traits) * 365.25;
}

double Assimilator::day_length_fraction(double lat_deg, double day_of_year) {
  const double pi = M_PI;  // from <cmath>
  double lat = lat_deg * pi / 180.0;
  double z0 = 90.833 * pi / 180.0; // Zenith angle (sunrise/sunset)
  
  int N = day_of_year;
  
  // Solar declination (radians)
  double delta = -asin(0.39779 * cos(0.98565 * pi / 180.0 * (N + 10) +
                       1.914 * pi / 180.0 * sin(0.98565 * pi / 180.0 * (N - 2))));
  
  // Cosine of hour angle at sunrise/sunset
  double cos_h0 = (cos(z0) - sin(lat) * sin(delta)) / (cos(lat) * cos(delta));
  
  double D; // day length in hours
  
  if (cos_h0 <= -1.0) {
    D = 24.0; // Sun never sets
  } else if (cos_h0 >= 1.0) {
    D = 0.0; // Sun never rises
  } else {
    double H0 = acos(cos_h0);   // hour angle in radians
    D = (24.0 / pi) * H0;      // convert to hours
  }
  
  double L = D / 24.0;  // Fractional day length

  return L;
}

bool Assimilator::is_leap_year(int year) {
  return (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0));
}

int Assimilator::decimal_year_to_day_of_year(double dec_year) {
  int year = static_cast<int>(std::floor(dec_year));
  double fraction = dec_year - year;
  
  int days_in_year = is_leap_year(year) ? 366 : 365;
  
  // Convert fraction of year to day-of-year (1-based)
  int day_of_year = static_cast<int>(std::round(fraction * days_in_year)) + 1;
  
  // Cap to days in year (just in case rounding pushed it to 0 or > days_in_year)
  if (day_of_year < 1) day_of_year = 1;
  if (day_of_year > days_in_year) day_of_year = days_in_year;
  
  return day_of_year;
}

} // namespace plant



