#include "life_history.h"
#include <io_utils.h>
using namespace std;

namespace pfate{

ErgodicEnvironment::ErgodicEnvironment() : LightEnvironment(), Climate(){
	// z_star and canopy_openness set dynamically by updateBackgroundCanopy()
}

void ErgodicEnvironment::init(io::Initializer& I){
	bg_density_ini = I.get<double>("bg_canopy_density_ini");
	bg_density_eq  = I.get<double>("bg_canopy_density_eq");
	bg_tau         = I.get<double>("bg_canopy_tau");
	bg_t0          = I.get<double>("bg_canopy_t0");

}

void ErgodicEnvironment::updateBackgroundCanopy(double t, plant::PlantArchitecture& geom,
                                                plant::PlantTraits& traits, const plant::PlantParameters& par){
	double elapsed = t - bg_t0;
	double density = (elapsed <= 0) ? bg_density_ini
	               : bg_density_eq + (bg_density_ini - bg_density_eq) * exp(-elapsed / bg_tau);

	// Guard: uninitialised or zero-height plant → fully open canopy
	if (geom.height <= 0 || density <= 0){
		n_layers = 0;
		z_star = {0.0};
		canopy_openness = {1.0};
		return;
	}

	// PPA: n_layers from total projected crown area (mirrors patch logic)
	double fG = 0.99;
	double total_ca = density * geom.crown_area_extent_projected(0, traits);
	n_layers = int(total_ca / fG);

	// z_star: root-find where density * crown_area_extent_projected(z) = layer * fG
	z_star.clear();
	for (int layer = 1; layer <= n_layers; ++layer){
		auto f = [&](double z){
			return density * geom.crown_area_extent_projected(z, traits) - layer * fG;
		};
		z_star.push_back(pn::zero(0.0, geom.height, f, 1e-4).root);
	}
	z_star.push_back(0.0);

	// Propagate light down through layers using crown_area_above (mirrors patch fapar_layer)
	canopy_openness.resize(n_layers + 1);
	canopy_openness[0] = 1.0;
	for (int layer = 0; layer < n_layers; ++layer){
		double cap_z    = density * geom.crown_area_above(z_star[layer],     traits);
		double cap_ztop = (layer > 0) ? density * geom.crown_area_above(z_star[layer - 1], traits) : 0.0;
		double cap_layer = cap_z - cap_ztop;
		double fapar = cap_layer * (1.0 - exp(-par.k_light * geom.lai));
		canopy_openness[layer + 1] = canopy_openness[layer] * (1.0 - fapar);
	}
}

void ErgodicEnvironment::print(double t){
	Climate::print_line(t);
	// LightEnvironment::print();
	cout << "z_star = " << z_star;
	cout << "canopy_openness = " << canopy_openness;
}

void ErgodicEnvironment::computeEnv(double t, Solver* sol, std::vector<double>::iterator S, std::vector<double>::iterator dSdt){
	// do nothing
}

LifeHistoryOptimizer::LifeHistoryOptimizer(std::string params_file){
	//paramsFile = params_file; // = "tests/params/p.ini";
	I.parse(params_file);

	C.init(I);

  // NOTE: PlantFATE seems to work off confif.time_unit
	ts.set_units(I.get_verbatim("time_unit"));

	traits0.init(I);
	par0.init(I);
	uptake0.init(I);
	
	par0.set_tscale(ts.get_tscale()); // default time unit is year or month
	std::cout << " ts.get_tscale() " << ts.get_tscale();

	c_stream.i_metFile = ""; //"tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE.csv";
	c_stream.a_metFile = ""; //"tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE.csv";
	c_stream.co2File = ""; //"tests/data/CO2_AMB_AmzFACE2000_2100.csv";

}

void LifeHistoryOptimizer::set_i_metFile(std::string file){
	c_stream.i_metFile = file;
	c_stream.update_i_met = (file == "") ? false : true;
}

void LifeHistoryOptimizer::set_a_metFile(std::string file){
	c_stream.a_metFile = file;
	c_stream.update_a_met = (file == "") ? false : true;
}

void LifeHistoryOptimizer::set_co2File(std::string co2file){
	c_stream.co2File = co2file;
	c_stream.update_co2 = (co2file == "") ? false : true;
}

void LifeHistoryOptimizer::init_co2(double _co2){
	C.init_co2(_co2);
}

void LifeHistoryOptimizer::set_soil_nitrogen(double _N){
  C.set_soil_nitrogen(_N);
  uptake0.N_s = _N;
}

void LifeHistoryOptimizer::root_override(double _rn, double _rl) {
  P.geometry.set_root(_rn, _rl, P.traits);
}

void LifeHistoryOptimizer::init(){
	rep = 0;
	litter_pool = 0;
	seeds = 0;
	prod = 0;

	C.set_elevation(0);
	C.set_acclim_timescale(7);
	c_stream.init();
	C.updateBackgroundCanopy(C.bg_t0, P.geometry, P.traits, P.par);

	// We are tracking the life-cycle of a seed: how many seeds does a single seed produce (having gone through dispersal, germination, and plant life stages)
	P = plant::Plant();
	// P.initFromFile(params_file);
	P.init(par0, traits0, uptake0);

	P.geometry.set_lai(P.par.lai0);
	P.set_size(0.01);  // must be before set_root so crown_area is non-zero when ectomycorrhiza_mass is initialised
	P.geometry.set_root(P.par.root_no0, P.par.root_length0, P.traits);
	// Simulation below starts at seedling stage. So account for survival until seedling stage
	P.geometry.init_nitrogen(P.par.nitrogen_uptake0, P.par.nitrogen_start0, P.traits);

	// TODO: temperarily set the day of the year to midyear in case
	P.state.mortality = -log(P.p_survival_dispersal(C) * P.p_survival_germination(C, 182)); // p{fresh seed is still alive after germination} = p{it survives dispersal}*p{it survives germination}

	// double total_prod = P.get_biomass();
	// cout << "Starting biomass = " << total_prod << "\n";
	// cout << "Mortality until seedling stage = " << P.state.mortality << "\n";

}

vector<std::string> LifeHistoryOptimizer::get_header(){
	return {
		  "i"
		, "ppfd"
		, "assim_net"
		, "assim_gross"
		, "rl"
		, "rr"
		, "rs"
		, "tl"
		, "tr"
		, "dpsi"
		, "vcmax"
		, "transpiration"
    , "height"
		, "diameter"
		, "crown_area"
		, "lai"
		, "sapwood_fraction"
		, "leaf_mass"
		, "root_mass"
    , "ectomycorrhiza_mass"
		, "stem_mass"
		, "coarse_root_mass"
		, "total_mass"
    , "tree_nitrogen"
    , "nitrogen_in_biomass"
    , "optimal_leaf_nitrogen"
    , "leaf_nitrogen_concentration"
		, "total_rep"
		// , "seed_pool"
		// , "germinated"
		, "fitness"
		, "total_prod"
		, "litter_mass"
		, "mortality"
		, "mortality_inst"
		, "mortrate_0"
		, "mortrate_growth"
		, "mortrate_d"
		, "mortrate_hyd"
		, "leaf_lifespan"
		, "fineroot_lifespan"
    , "root_no"
    , "root_length"
    , "belowground_infrastructure"
    , "mycorrhizal_export_to_tree"
    , "root_uptake"
    , "myco_uptake"
    , "N_bar"
    , "r_zone"
    , "alpha"
    , "SA_active"
	};
}

void LifeHistoryOptimizer::printHeader(ostream& lfout){
	vector<std::string> s = get_header();
	for (auto& vv : s) lfout << vv << "\t";
	lfout << '\n';
}

vector<double> LifeHistoryOptimizer::get_state(double t){
  // NOTE: root_lifespan is in years!
  
	return {
		  ts.to_julian(t)
		, C.clim_inst.ppfd
		, P.assimilator.plant_assim.npp
		, P.assimilator.plant_assim.gpp
		, P.assimilator.plant_assim.rleaf
		, P.assimilator.plant_assim.rroot
		, P.assimilator.plant_assim.rstem
		, P.assimilator.plant_assim.tleaf
		, P.assimilator.plant_assim.troot
		, P.assimilator.plant_assim.dpsi_avg
		, P.assimilator.plant_assim.vcmax_avg
		, P.assimilator.plant_assim.trans
    , P.geometry.height
		, P.geometry.diameter
		, P.geometry.crown_area
		, P.geometry.lai
		, P.geometry.sapwood_fraction
		, P.geometry.leaf_mass(P.traits)
		, P.geometry.root_mass(P.traits)
    , P.geometry.ectomycorrhiza_mass
		, P.geometry.stem_mass(P.traits)
		, P.geometry.coarse_root_mass(P.traits)
		, P.get_biomass()
    , P.geometry.nitrogen_tree
    , P.geometry.nitrogen_in_biomass
    , P.assimilator.plant_assim.nitrogen_avg
    , P.geometry.leaf_nitrogen_concentration
		, rep
	//  , P.state.seed_pool
	//  , germinated
		, seeds
		, prod
		, litter_pool
		, P.state.mortality
		, P.rates.dmort_dt
		, P.mort.mu_0
		, P.mort.mu_growth
		, P.mort.mu_d
		, P.mort.mu_hyd
		, 1 / P.assimilator.kappa_l
		, P.geometry.root_lifespan(P.traits)
    , P.geometry.root_no
    , P.geometry.root_length
    , P.geometry.I_b
    , P.geometry.N_export
    , P.uptake.U_root
    , P.uptake.U_myco
    , P.uptake.N_bar_val
    , P.uptake.r_zone_val
    , P.uptake.alpha_val
    , P.uptake.SA_active_val
	};
}

void LifeHistoryOptimizer::printState(double t, ostream& lfout){
	vector<double> v = get_state(t);
	for (auto& vv : v) lfout << vv << "\t";
	lfout << '\n';
}

// void LifeHistoryOptimizer::set_traits(std::vector<double> tvec){
// 	P.set_evolvableTraits(tvec);
// }


// std::vector<double> LifeHistoryOptimizer::get_traits(){
// 	return P.get_evolvableTraits();
// }


void LifeHistoryOptimizer::printMeta(){
	P.print();
	C.print(0);
}


void LifeHistoryOptimizer::set_state(vector<double>::iterator it){
	P.geometry.set_lai(*it++);
	P.set_size(*it++);
	prod = *it++;
	litter_pool = *it++;
	rep = *it++;
//		P.state.seed_pool = *it++;
	seeds = *it++;
	P.state.mortality = *it++;
	P.geometry.ectomycorrhiza_mass      = *it++;
	P.geometry.ectomycorrhiza_N_free    = *it++;
	P.set_nitrogen(*it++);
}

void LifeHistoryOptimizer::get_rates(vector<double>::iterator it){
	*it++ = P.rates.dlai_dt;       // lai growth rate
	*it++ = P.rates.dsize_dt;      // size (diameter) growth rate
	*it++ = P.bp.dmass_dt_tot;	   // biomass production rate
	*it++ = P.bp.dmass_dt_lit;     // litter biomass growth rate
	*it++ = P.bp.dmass_dt_rep;     //(1-fg)dBdt;  // reproduction biomass growth rate
//		*it++ = P.rates.dseeds_dt_pool;
	*it++ = P.rates.dseeds_dt;
	*it++ = P.rates.dmort_dt;
	*it++ = P.rates.dmass_myco_dt;
	*it++ = P.rates.dN_myco_dt_free;
	*it++ = P.rates.dnitrogen_dt_free;
}


void LifeHistoryOptimizer::update_climate(double julian_time){
	c_stream.updateClimate(julian_time, C);
	C.t_clim = flare::julian_to_yearsCE(julian_time);
}

void LifeHistoryOptimizer::grow_for_dt(double t, double dt){

	auto derivs = [this](double t, std::vector<double>& S, std::vector<double>& dSdt){
		//if (fabs(t - 2050) < 1e-5)
		update_climate(ts.to_julian(t));
		C.updateBackgroundCanopy(t, P.geometry, P.traits, P.par);

		// C.Climate::print(t);
		set_state(S.begin());
		
		P.calc_demographic_rates(C, t);

		// Override Plant-FATE fecundity calculations 
		// We need to explicitly include plant mortality here for fitness calcs
		double fec = P.fecundity_rate(P.bp.dmass_dt_rep, C);
		P.rates.dseeds_dt =  fec * exp(-P.state.mortality);  // Fresh seeds produced = fecundity rate * p{plant is alive}
		// P.rates.dseeds_dt_germ =   P.state.seed_pool/P.par.ll_seed;   // seeds that leave seed pool proceed for germincation

		get_rates(dSdt.begin());
		};

	std::vector<double> S = {P.geometry.lai, P.geometry.get_size(), 
                          prod, litter_pool, rep, seeds, P.state.mortality, 
                          P.geometry.ectomycorrhiza_mass,
                          P.geometry.ectomycorrhiza_N_free,
                          P.geometry.nitrogen_tree};
	RK4(t, dt, S, derivs);
	//Euler(t, dt, S, derivs);
	set_state(S.begin());
	C.updateBackgroundCanopy(t + dt, P.geometry, P.traits, P.par);
	P.calc_demographic_rates(C, t + dt);
}


double LifeHistoryOptimizer::calcFitness(){
	// lho_set_traits(tvec);
	for (double t=2000; t <= 2500; t=t + dt){
		grow_for_dt(t, dt);
	}
	return seeds;
}

} // namespace pfate
