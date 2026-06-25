#ifndef PLANT_FATE_PLANT_ARCHITECTURE_H_
#define PLANT_FATE_PLANT_ARCHITECTURE_H_

#define _USE_MATH_DEFINES
#include <cmath>

#include "traits_params.h"

namespace plant{

/// @brief   Everything about the plant's architecture, i.e. relationships among various physical dimensions.
/// @ingroup physiology
/// @details Two variables completely define dimensional state of the 
///          plant: diameter and crown LAI. All other dimensions (height, 
///          crown area, leaf area, biomass, etc) are calculated from these two.
class PlantArchitecture{
	public:
	/// @brief Traits that control dimensional scaling
	struct{
		// crown traits
		double m, n;    ///< crown shape paramaters
		double a;       ///< Seedling height-to-diameter ratio
		double c;       ///< Crown area to sapwood area ratio
		double fg;      ///< upper canopy gap fraction

		// Precomputed Geometric parameters
		double eta_c;   ///< Stem taper coefficient
		double pic_4a;  ///< \f$\pi c / (4a)\f$
		double zm_H;    ///< \f$z_m / H\f$
		double qm;      ///< \f$q(z_m)\f$

		// allocation
		double dmat;    ///< Diameter at reproductive maturity, calculated as diameter when H = `fhmat x` hmat
	} geom;

	public:
	// current state
	double lai;                  ///< Crown leaf area index 
	double diameter;             ///< basal diameter (diameter at ground level)
	double root_no;              ///< Number of root tips [no]
	double root_length;          ///< Root length, [mm]
	double ectomycorrhiza_mass;  ///< Mycorrhizal biomass [kg]
	double ectomycorrhiza_N_free;  ///< Mycorrhizal free nitrogen [kg]
	
	// nitrogen state
	double nitrogen_tree;                ///< Nitrogen in the tree [g] TODO: is this in kg rather than g
	double leaf_nitrogen_concentration;  ///< Nitrogen concentration in leaf [kg N kg-1 leaf], set by set_nitrogen()
	double nitrogen_in_biomass;          ///< Nitrogen in the tree's biomass [kg N]

	// variables calculated from state variables
	double height;                       ///< Plant height
	double crown_area;                   ///< Crown area
	double sapwood_fraction;             ///< Fraction of stem cross sectional area that is sapwood
	double functional_xylem_fraction;    ///< Fraction of functional xylem in sapwood
	double rooting_depth;                ///< Rooting depth, calculated from coarse root biomass
	
	// mass rates
	double nitrogen_uptake_roots;        ///< Nitrogen uptake [kgN per time unit]
	double N_export = 0.0;               ///< Nitrogen transferred from mycorrhiza to tree [kg N per time unit]
	double I_b = 1.0;                    ///< Belowground infrastructure index [m] (Eq. belowground_infra)
	bool using_Ib = false;               ///< Mirror of Uptake::using_Ib; set each timestep before assimilation
	double dmass_myco_dt;                ///< Mycorrhizal mass difference [kg C per time unit]
	double dN_myco_dt_free;              ///< Nitrogen free changed [kg N per time unit]
	
	// ode-based calculations of sapwood and heartwood (for debug)
	double sap_frac_ode = 1;
	double sapwood_mass_ode = 0;
	double heart_mass_ode = 0;
	double k_sap;

	public:

	/// @brief  Initialize geometry from traits, precompute any necessary variables
	void init(PlantParameters& par, PlantTraits& traits);
	void init_nitrogen(double _nu, double _nt, PlantTraits& traits);

	/// @brief  The height at which crown radius is maximum.
	double zm();

	/// @brief Vertical profiles of crown and stem  
	/// @{
	double q(double z);
	double diameter_at_height(double z, PlantTraits& traits);
	/// @brief  Potential crown projection area at height z. 
	double crown_area_extent_projected(double z, PlantTraits& traits);
	/// @brief  Realized crown projection area at height z. 
	double crown_area_above(double z, PlantTraits& traits);
	double n_demand_per_lai_biomass(const PlantTraits& traits, double nc_leaf_actual) const;
	/// @}


	/// @brief Derivatives required for biomass partitioning
	/// @{  
	std::vector<double> dsize_dmass(PlantParameters& par, PlantTraits& traits) const;
	double dreproduction_dmass(PlantParameters& par, PlantTraits& traits);
	/// @}


	/// Rate of change of leaf mass due to change in LAI
	double dmass_dt_lai(double& dL_dt, double dmass_dt_max, PlantTraits& traits);
	
	
	/// @brief Root allometry equations
	/// @{
	double root_diameter(const PlantTraits& traits) const;
	double root_density(const PlantTraits& traits) const;
	double root_lifespan(const PlantTraits& traits) const;
	double root_mass(const PlantTraits& traits) const;
	double root_surface_area(const PlantTraits& traits) const;
	/// @}
	

	/// @brief Get biomass in various carbon pools.
	/// @{
	double leaf_mass(const PlantTraits& traits) const;
	double sapwood_mass(const PlantTraits& traits) const;
	double sapwood_mass_real(const PlantTraits& traits) const;
	double stem_mass(const PlantTraits& traits) const;
	double coarse_root_mass(const PlantTraits& traits) const;
	double heartwood_mass(const PlantTraits& traits) const;
	double total_mass(const PlantTraits& traits) const;
	/// @}
	
	///@brief Nitrogen value of biomass and ectomycorrhizal operations
	/// @{
	double total_mass_nitrogen(const PlantTraits& traits) const;
	void dmyco_dt(double exudates, const PlantTraits& traits, double U_myco, double mycorrhizal_root_reduction, double max_N_transfer, const PlantParameters& par);
	/// @}

	// These functions are used to get and set state variables
	/// @{
	/// Get size (diameter) - does not alter state
	double get_size() const;
	/// Set the crown LAI and properties that change with LAI
	void set_lai(double _l);
	/// Set plant size (diameter) and other variables that scale with size  
	void set_root(double _rn, double _rl, PlantTraits& traits);
	void set_size(double _x, PlantTraits& traits);
	void set_nitrogen(double _nt, PlantTraits& traits);
	/// Set size and lai, the two state variables that define plant geometry
	/// And nor also size and nitrogen
	std::vector<double>::iterator set_state(std::vector<double>::iterator S, PlantTraits& traits);
	/// @}


	// ** 
	// ** Simple growth simulator for testing purposes
	// ** - simulates growth over dt with constant assimilation rate A
	// ** 
	void grow_for_dt(double t, double dt, double& prod, double& litter_pool, double A, PlantParameters& par, PlantTraits& traits);

};


} // namespace plant

#endif


