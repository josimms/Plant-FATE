#ifndef PLANT_FATE_PLANT_ASSIMILATION_H_
#define PLANT_FATE_PLANT_ASSIMILATION_H_

#include <phydro.h>

#include "plant_params.h"
#include "plant_geometry.h"

namespace plant{

/// \ingroup physiology
struct PlantAssimilationResult{
	double gpp = 0;          ///< Gross plant-level production [kg-biomass yr-1] 
	double npp = 0;          ///< Net plant-level production [kg-biomass yr-1]
	double trans = 0;        ///< Transpiration [kg-h2o yr-1]

	double dpsi_avg = 0;     ///< Soil-leaf water potential difference \f$\Delta\psi\f$
	double vcmax_avg = 0;    ///< Crown-area weighted average Vcmax across canopy layers [umol m-2 s-1] 
	double vcmax25_avg = 0;  ///< Average Vcmax at 25 degC [umol m-2 s-1] 
	double mc_avg = 0;       ///< \f$m_c = (\chi c_a - \Gamma^*)/(\chi c_a + K_M) \f$
	double gs_avg = 0;       ///< Crown-area weighted average stomatal conductance across canopy layers
	double c_open_avg = 0;   ///< Crown-area weighted average canopy opennness experience by the plant

	double rleaf = 0;        ///< Leaf dark respiration rate [kg-biomass yr-1]
	double rroot = 0;        ///< Fine root respiration rate [kg-biomass yr-1]
	double rstem = 0;        ///< Sapwood respiration rate (excluding coarse root) [kg-biomass yr-1]

	double tleaf = 0;        ///< Leaf turnover rate [kg-biomass yr-1]
	double troot = 0;        ///< Fine root turnover rate [kg-biomass yr-1]
};

/// \ingroup physiology
class Assimilator{
	public:
	// ~~ Last calculated values of rates
	// ~~ These are defined here rather than in local scope for debugging purposes. 
	PlantAssimilationResult plant_assim;
	// ~~

	double kappa_l;
	double kappa_r;

	public:	
	// **
	// ** Gross and Net Assimilation 
	// **
	template<class _Climate>
	phydro::PHydroResult leaf_assimilation_rate(double I0, double fapar, _Climate &clim, PlantParameters &par, PlantTraits &traits);
	
	template<class Env>
	void  calc_plant_assimilation_rate(Env &env, PlantGeometry *G, PlantParameters &par, PlantTraits &traits);

	template<class Env>
	PlantAssimilationResult net_production(Env &env, PlantGeometry *G, PlantParameters &par, PlantTraits &traits);


	// ** 
	// ** Leaf economics
	// **
	void   les_update_lifespans(double lai, PlantParameters &par, PlantTraits &traits);
	double les_assim_reduction_factor(phydro::PHydroResult& res, PlantParameters &par);

	// ** 
	// ** Respiration and turnover
	// **
	//// leaf respiration rate - should be calculated AFTER asimialtion (needs updated Phydro outputs)
	double leaf_respiration_rate(PlantGeometry *G, PlantParameters &par, PlantTraits &traits);
	double root_respiration_rate(PlantGeometry *G, PlantParameters &par, PlantTraits &traits);
	double sapwood_respiration_rate(PlantGeometry *G, PlantParameters &par, PlantTraits &traits);

	double leaf_turnover_rate(double _kappa_l, PlantGeometry *G, PlantParameters &par, PlantTraits &traits);
	double root_turnover_rate(double _kappa_r, PlantGeometry *G, PlantParameters &par, PlantTraits &traits);
	

};

} // namespace plant

#include "assimilation.tpp"

#endif


