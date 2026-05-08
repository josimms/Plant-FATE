#include "plant_architecture.h"

#include "utils/rk4.h"
#include "utils/incbeta.h"
#include "traits_params.h"

#include <cmath>

namespace plant{

void PlantArchitecture::init(PlantParameters& par, PlantTraits& traits){
	geom.m = traits.m; geom.n = traits.n;
	geom.a = traits.a; geom.c = traits.c;
	geom.fg = par.fg;

	geom.pic_4a = M_PI * geom.c / (4 * geom.a);

	double m = geom.m, n = geom.n;
	geom.zm_H = pow((n - 1) / (m * n - 1), 1 / n);
	geom.qm = m * n * pow((n - 1) / (m * n - 1), 1 - 1 / n) * pow((m - 1) * n / (m * n - 1), m - 1);

	geom.eta_c = geom.zm_H - m * m * n / (geom.qm * geom.qm) * beta(2 - 1 / n, 2 * m - 1) * (incbeta(2 - 1 / n, 2 * m - 1, (n - 1) / (m * n - 1)) - (1 - geom.fg));

	// std::cout << "Init Geometry: m = " << m << ", n = " << n << ", zm/H = " << geom.zm_H << ", qm = " << geom.qm << ", eta_c = " << geom.eta_c << "\n";

	geom.dmat = -(traits.hmat / geom.a) * log(1 - traits.fhmat);

//	lai = par.lai0;
//	set_size(diameter_0, traits);
}

void PlantArchitecture::init_nitrogen(double _nu, double _nt, PlantTraits& traits){
  nitrogen_uptake_roots = _nu; 
  ectomycorrhiza_N_free = 0.01 * _nu;
  set_nitrogen(_nt, traits);
}

// **
// ** Crown geometry
// **
double PlantArchitecture::q(double z){
	if (z > height || z < 0) return 0;
	else{
		double m = geom.m, n = geom.n;
		double zHn_1 = pow(z / height, n - 1);
		double zHn   = zHn_1 * z / height;
		return m * n * pow(1 - zHn, m - 1) * zHn_1;
	}
}

double PlantArchitecture::zm(){
	return geom.zm_H * height;
}


/// @details   This is the total area that can  
///            be potentially occupied by leaves, including the area that currently consists of gaps. 
///            \f[A_{cp} = \pi r(z)^2 = \pi r_0^2 q(z)^2 = (A_c/q_m^2) q(z)^2 = A_c (q(z)/q_m)^2\f]
/// @ingroup   ppa_module
double PlantArchitecture::crown_area_extent_projected(double z, PlantTraits& traits){
	if (z >= zm()){
		double fq = q(z) / geom.qm;
		return crown_area * fq * fq;
	}
	else{
		return crown_area;
	}
}

/// @details This is the area within
///          the potential crown that is actually occupied by leaves  
/// @ingroup ppa_module
double PlantArchitecture::crown_area_above(double z, PlantTraits& traits){
	if (z == 0) return crown_area; // shortcut because z=0 is used often

	double fq = q(z) / geom.qm;
	if (z >= zm()){
		return crown_area * fq * fq * (1 - geom.fg);
	}
	else{
		return crown_area * (1 - fq * fq * geom.fg);
	}
}

double PlantArchitecture::diameter_at_height(double z, PlantTraits& traits){
	double as_z = crown_area_above(z, traits) / geom.c;
	double a_z  = as_z / sapwood_fraction;
	return sqrt(4 * a_z / M_PI);
}

// In plant_architecture.h/cpp
double PlantArchitecture::n_demand_per_lai_biomass(const PlantTraits& traits) const {
  double fine_root_per_unit_lai = M_PI * pow(root_diameter(traits)/2.0, 2.0) * root_length * root_density(traits) * 1e-9 * root_no;
  double l2m_total = traits.lma + fine_root_per_unit_lai;
  double leaf_fraction = traits.lma / l2m_total;
  double root_fraction = 1.0 - leaf_fraction;
  return 0.5 * (leaf_fraction * traits.nc_leaf + root_fraction * traits.nc_root);
}

// **
// ** Biomass partitioning
// **
std::vector<double> PlantArchitecture::dsize_dmass(PlantParameters& par, PlantTraits& traits) const{
	double dh_dd = geom.a * exp(-geom.a * diameter / traits.hmat);
	double dmleaf_dd = traits.lma * lai * geom.pic_4a * (height + diameter * dh_dd);	// LAI variation is accounted for in biomass production rate
	double dmtrunk_dd = (geom.eta_c * M_PI * traits.wood_density / 4) * (2 * height + diameter * dh_dd) * diameter;
	double dmbranches_dd = (sqrt(geom.c / geom.a) * M_PI * traits.wood_density / 12) * (2.5 * height + 0.5 * diameter * dh_dd) * diameter * sqrt(diameter / height);
	double dmroot_dd = M_PI * pow(root_diameter(traits)/2.0, 2.0) * root_length * root_density(traits) * 1e-9 * root_no / traits.lma * dmleaf_dd;
	double dmcroot_dd = (dmbranches_dd + dmtrunk_dd) * traits.fcr;

	double dmass_dd = dmleaf_dd + dmtrunk_dd + dmbranches_dd + dmroot_dd + dmcroot_dd;
	double dnitrogen_dd = dmleaf_dd * traits.nc_leaf * par.cbio + 
	  dmtrunk_dd * traits.nc_wood * par.cbio + 
	  dmbranches_dd * traits.nc_wood * par.cbio + 
	  dmcroot_dd * traits.nc_wood * par.cbio + 
	  dmroot_dd * traits.nc_root * par.cbio;
	
	std::vector<double> out(2);
	out[0] = 1 / dmass_dd;
	out[1] = dnitrogen_dd / dmass_dd;
	return out;
}

double PlantArchitecture::dreproduction_dmass(PlantParameters& par, PlantTraits& traits){
	return par.a_f1 / (1.0 + exp(par.a_f2 * (1.0 - diameter / geom.dmat)));
}

/// @param  dL_dt Desired LAI increment 
/// @param  dmass_dt_max Maximum allowed biomass increment resulting from LAI change 
/// @details Given the LAI change (dL_dt) and the maximum allowed mass increment (dmass_dt_max), this function calculates
///          the mass increment (of leaves and fine roots) needed to achive the specified LAI increment. If this exceeds the maximum 
///          allowed mass increment, then mass increment is set to dmass_dt_max and dL_dt is revised accordingly. 
///          Complete coordination between fine roots and leaves is assumed. Thus, both leaves and fine roots need to increase for increasing LAI, 
///          and both are simultaneously shed if LAI decreases.
double PlantArchitecture::dmass_dt_lai(double& dL_dt, double dmass_dt_max, PlantTraits& traits){
	double l2m = crown_area * (traits.lma + M_PI * pow(root_diameter(traits)/2.0, 2.0) * root_length * root_density(traits) * 1e-9 * root_no);    // biomass required to support a unit LAI
	double dm_dt_lai = std::min(dL_dt * l2m, dmass_dt_max);  // biomass change resulting from LAI change. 
	dL_dt = dm_dt_lai / l2m;   // Revise dL_dt, in case dm_lai_dt was capped at the maximum
	return dm_dt_lai;
}

// **
// ** Root allometry
// **

double PlantArchitecture::root_diameter(const PlantTraits& traits) const {
  // mm length gives mm of diameter
  return traits.k_1 / pow(root_length, 0.5);
}

double PlantArchitecture::root_density(const PlantTraits& traits) const {
  double percentage_root_cortex = pow(traits.k_2 + traits.k_3 / root_diameter(traits), 2.0);
  // density kg m-3
  return traits.k_4 * percentage_root_cortex + traits.k_5;
}

double PlantArchitecture::root_lifespan(const PlantTraits& traits) const {
  return traits.k_6 * (1.0 - exp(- traits.k_7 * root_diameter(traits)));
}

// **
// ** Carbon pools
// **
double PlantArchitecture::leaf_mass(const PlantTraits& traits) const{
	return crown_area * lai * traits.lma;
}

double PlantArchitecture::root_mass(const PlantTraits& traits) const{
  double diameter_root = root_diameter(traits);
  double density_root = root_density(traits);

  // kg / m3 * mm2 * mm * no * 1e-9 * no = kg biomass

  return density_root * pow(diameter_root/2.0, 2.0) * root_length * M_PI * root_no * 1e-9 * crown_area * lai;
}

double PlantArchitecture::root_surface_area(const PlantTraits& traits) const{
  double d_m = root_diameter(traits) / 1000.0;
  double L_m = root_length / 1000.0;
  return M_PI * d_m * L_m * root_no * crown_area * lai;
}

void PlantArchitecture::dmyco_dt(
    double exudates,
    const PlantTraits& traits,
    double U_myco,
    double mycorrhizal_root_reduction,
    const PlantParameters& par
) {
  // --- Carbon-driven potential growth ---
  double growth_C_potential = exudates * traits.mycorrhizal_biomass_conversion * 0.44;
  
  // --- Nitrogen demand ---
  double N_needed = growth_C_potential * traits.nc_myco;
  
  // --- Nitrogen limitation ---
  double nitrogen_transfer = ectomycorrhiza_N_free * traits.mycorrhizal_turnover * par.years_per_tunit_avg;
  double fN = (N_needed > 0.0) ? std::min(1.0, (nitrogen_transfer + U_myco) / N_needed) : 1.0;
  
  N_export = std::max(0.0, nitrogen_transfer) * mycorrhizal_root_reduction;
  
  dmass_myco_dt = growth_C_potential * fN - ectomycorrhiza_mass * traits.mycorrhizal_turnover;
  // TODO: is this kg biomass or kg carbon?
  dN_myco_dt_free = U_myco - growth_C_potential * fN * traits.nc_myco - N_export + ectomycorrhiza_mass * traits.nc_myco * traits.mycorrhizal_turnover * traits.k_14 * par.years_per_tunit_avg;
}

double PlantArchitecture::coarse_root_mass(const PlantTraits& traits) const{
	return stem_mass(traits) * traits.fcr;
}

//double sapwood_mass(PlantTraits &traits){
	//return traits.wood_density*(hvlc/geom.c)*crown_area*geom.eta_l*height;
//}

double PlantArchitecture::sapwood_mass(const PlantTraits& traits) const{
	return stem_mass(traits) * sapwood_fraction;
}

double PlantArchitecture::sapwood_mass_real(const PlantTraits& traits) const{
	return sapwood_mass(traits) * functional_xylem_fraction;
}

double PlantArchitecture::stem_mass(const PlantTraits& traits) const{
	double trunk_mass = traits.wood_density * (M_PI * diameter * diameter / 4) * height * geom.eta_c;
	double branch_mass = traits.wood_density * (M_PI * diameter * diameter / 12) * height * sqrt((geom.c / geom.a) * (diameter / height));
	return trunk_mass + branch_mass;
}

double PlantArchitecture::heartwood_mass(const PlantTraits& traits) const{
	return stem_mass(traits) * (1 - sapwood_fraction);
}

double PlantArchitecture::total_mass(const PlantTraits& traits) const{
  
  double fine_root_mass = root_mass(traits);
  
	return stem_mass(traits) * (1 + traits.fcr) + leaf_mass(traits) + fine_root_mass;
}

double PlantArchitecture::total_mass_nitrogen(const PlantTraits& traits) const{
  // kg
  
  double fine_root_mass = root_mass(traits) * 0.5 * traits.nc_root;
  
  return stem_mass(traits) * 0.5 * (1 + traits.fcr) * traits.nc_wood + leaf_mass(traits) * 0.5 * traits.nc_leaf + fine_root_mass;
}

// **
// ** state manipulations
// **	
/// @details Sets the following properties: diameter
double PlantArchitecture::get_size() const{
	return diameter;
}

/// @details Sets the following properties: lai
void PlantArchitecture::set_lai(double _l){
	lai = _l;
}

/// @details Sets the following properties: root_no, root_length
void PlantArchitecture::set_root(double _rn, double _rl, PlantTraits& traits){
  root_no = _rn;
  root_length = _rl;
  ectomycorrhiza_mass = root_mass(traits);
};

/// @details Sets the following properties: nitrogen_tree, nitrogen_uptake, potential_nitrogen_leaf, ectomycorrhiza_mass 
void PlantArchitecture::set_nitrogen(double _nt, PlantTraits& traits) {
  nitrogen_tree = _nt;
  potential_nitrogen_leaf = traits.k_10 * std::max(nitrogen_tree, 0.0);
  nitrogen_in_biomass = total_mass_nitrogen(traits);
}

/// @details Sets the following properties: diameter, height, crown area, sapwood fraction 
void PlantArchitecture::set_size(double _x, PlantTraits& traits){
	diameter = _x;
	height = traits.hmat * (1 - exp(-geom.a * diameter / traits.hmat));
	crown_area = geom.pic_4a * height * diameter;
	sapwood_fraction = height / (diameter * geom.a);
}

std::vector<double>::iterator PlantArchitecture::set_state(std::vector<double>::iterator S, PlantTraits& traits){
	set_lai(*S++);             // must be set first as it is used bt set_size() - not required any more
	set_size(*S++, traits);
//	litter_pool = *S++;
	return S;
}


// ** 
// ** Simple growth simulator for testing purposes
// ** - simulates growth over dt with constant assimilation rate A
// ** 

void PlantArchitecture::grow_for_dt(double t, double dt, double& prod, double& litter_pool, double A, PlantParameters& par, PlantTraits& traits){

	auto derivs = [A, &traits, &litter_pool, &par, this](double t, std::vector<double>& S, std::vector<double>& dSdt){
		set_lai(S[5]);
		set_size(S[1], traits);
		litter_pool = S[6];

		double dh_dd = geom.a * exp(-geom.a * diameter / traits.hmat);

		//double dL_dt = -0.05*lai; /[>lai;

		// ignoring reproduction in these biomass pools
		double dB_dt = A * crown_area;	// total biomass production rate
		double dL_dt = -0.01; //dlai_dt(traits);
		double dLA_dt = dmass_dt_lai(dL_dt, dB_dt, traits);  // biomass going into leaf area increment
		double dLit_dt = std::max(-dLA_dt, 0.0);  // biomass going into litter (through leaf loss)
		// TODO: nitrogen here?
		double dG_dt = dB_dt - std::max(dLA_dt, 0.0); // biomass going into geometric growth
		double dN_dd = 0; // NOTE: not relevant in this function! Used for the nitrogen balance calculated in the Life History
		double dD_dt = dsize_dmass(par, traits)[0] * dG_dt;	// size (diameter) growth rate

		dSdt[0] = dB_dt;	// biomass that goes into allometric increments
		dSdt[1] = dD_dt;
		dSdt[2] = 1 / (geom.a * diameter * diameter) * (diameter * dh_dd - height) * dD_dt;

		double dmtrunk_dd = (geom.eta_c * M_PI * traits.wood_density / 4) * (2 * height + diameter * dh_dd) * diameter;
		double dmbranches_dd = (sqrt(geom.c / geom.a) * M_PI * traits.wood_density / 12) * (2.5 * height + 0.5 * diameter * dh_dd) * diameter * sqrt(diameter / height);

		double dsap_trunk_dd = traits.wood_density * M_PI / (4 * geom.a) * geom.eta_c * (2 * diameter * dh_dd + height) * height;
		double dsap_branch_dd = traits.wood_density * M_PI / (8 * geom.a) * sqrt(geom.c / geom.a) * (diameter * dh_dd + height) * sqrt(diameter * height);

		dSdt[3] = (S[3] < sapwood_mass(traits)) ? (dmtrunk_dd + dmbranches_dd) * dD_dt : (dsap_trunk_dd + dsap_branch_dd) * dD_dt;
		dSdt[4] = (dmtrunk_dd + dmbranches_dd - dsap_trunk_dd - dsap_branch_dd) * dD_dt;
		dSdt[5] = dL_dt;
		dSdt[6] = dLit_dt; //(dL_dt < 0)? dLex_dt : 0;

		k_sap = dSdt[3] / sapwood_mass(traits) * dSdt[1];
		};

	std::vector<double> S = {prod, get_size(), sap_frac_ode, sapwood_mass_ode, heart_mass_ode, lai, litter_pool};
	RK4(t, dt, S, derivs);
	//Euler(t, dt, S, derivs);
	litter_pool = S[6];
	heart_mass_ode = S[4];
	sapwood_mass_ode = S[3];
	sap_frac_ode = S[2];
	set_lai(S[5]);
	set_size(S[1], traits);
	prod = S[0];
	functional_xylem_fraction = S[3] / sapwood_mass(traits);
}


} // namespace plant



