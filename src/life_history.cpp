#include "life_history.h"
#include <io_utils.h>
#include <deque>
#include <iostream>
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
		, "r_myco"
		, "resp_myco_total"
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
    , "ectomycorrhiza_N_free"
    , "ectomycorrhiza_C_free"
    , "ectomycorrhiza_N_biomass"
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
    , "root_uptake_actual"
    , "myco_uptake"
    , "N_bar_roots"
    , "N_bar_static"
    , "N_static"
    , "r_zone"
    , "alpha"
    , "SA_active"
    , "C_export_to_myco"
    , "myco_uptake_static"
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
		, P.par.r_myco * std::max(0.0, P.geometry.ectomycorrhiza_mass) * P.par.years_per_tunit_avg
		, P.geometry.resp_myco_total
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
    , P.geometry.ectomycorrhiza_N_free
    , P.geometry.ectomycorrhiza_C_free
    , P.geometry.ectomycorrhiza_mass * 0.44 * P.traits.nc_myco
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
    , P.geometry.nitrogen_uptake_roots
    , P.uptake.U_myco
    , P.uptake.N_bar_roots
    , P.uptake.N_bar_static_val
    , P.uptake.N_static_val
    , P.uptake.r_zone_val
    , P.uptake.alpha_val
    , P.uptake.SA_active_val
    , P.geometry.C_export_to_myco
    , P.uptake.U_myco_static
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
	P.geometry.ectomycorrhiza_C_free    = *it++;
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
	*it++ = P.rates.dC_myco_dt_free;
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
                          P.geometry.ectomycorrhiza_C_free,
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


// Yearly re-optimization objective: npp NET OF the mycorrhizal carbon export,
// normalized per unit crown area x LAI (fixes three problems vs. the
// alternatives tried first). Not raw npp (assimilation.tpp: par.y*(A-R)-T) --
// that's an absolute flow, and comparing absolute flows biases the yearly
// re-optimizer toward whichever candidate has the smallest scale. Not the
// manuscript's static-grid net_gpp_ca (annual_gs_all$net_gpp_ca, Publication
// Plots.Rmd) either -- that's GPP minus belowground carbon costs only, and
// never charges the extra leaf respiration (rleaf = vcmax*par.rd,
// assimilation.tpp) that comes with the higher vcmax more root-delivered N
// buys, over-rewarding N acquisition. plant_assim.npp already nets out
// rleaf+rstem+rroot and tleaf+troot correctly (assimilation.tpp:246-250);
// dividing by crown_area*lai adds the missing per-area normalization.
//
// 2026-09-23 fix: plant_assim.npp is computed BEFORE the mycorrhizal carbon
// export is deducted (plant.tpp:180-183: npp_exudates = max(npp,0) *
// investment_from_tree; the tree's own growth actually uses npp - exudates).
// The trial score therefore never charged ecto_allo's carbon cost at all --
// confirmed empirically: with ecto_max raised from 0.3 to 1.0, ecto_allo
// climbed monotonically, one search-step/year, to the ceiling (1.0) by 1978
// with no interior optimum, and height flatlined once the tree retained zero
// carbon for itself (notes_root_no_belowground_economy.md section 17).
// Subtracting C_export_to_myco (kg C/unit_t, plant_architecture.h) here makes
// this the manuscript's F_net (Eq. in Tree Fitness, MAIN.tex: (A_gross - C_a
// - R_r - R_t)/(A_c*L)) PLUS leaf respiration correctly charged, rather than
// F_net's own known gap (missing R_l) or npp_per_ca's now-fixed gap (missing
// C_a).
static double npp_per_ca(plant::Plant& P){
	double ca_lai = std::max(P.geometry.crown_area * P.geometry.lai, 1e-12);
	double npp_net_of_myco = P.assimilator.plant_assim.npp - P.geometry.C_export_to_myco;
	return npp_net_of_myco / ca_lai;
}


std::vector<std::vector<double>> LifeHistoryOptimizer::run_with_relaxed_local_reopt_trajectory(
    double start_year, double end_year, double reopt_dt, double trial_horizon_years,
    double root_no_step_factor, double root_no_min, double root_no_max,
    double root_length_step_factor, double root_length_min, double root_length_max,
    double ecto_step, double ecto_min, double ecto_max,
    double mycorrhized_step, double mycorrhized_min, double mycorrhized_max
){
	std::vector<std::vector<double>> log;

	auto make_levels_mult = [](double cur, double factor, double lo, double hi){
		return std::vector<double>{ std::max(cur / factor, lo), cur, std::min(cur * factor, hi) };
	};
	auto make_levels_add = [](double cur, double step, double lo, double hi){
		return std::vector<double>{ std::max(cur - step, lo), cur, std::min(cur + step, hi) };
	};

	// The tree's actual physical root configuration; only gradually catches
	// up to whatever target the search commits to each year. Initialized
	// from whatever root_override() last set (continuous, no discontinuity).
	double n_eff = P.geometry.root_no;
	double l_eff = P.geometry.root_length;

	// Adaptive step sizes (2026-09-24): the *_step_factor/*_step arguments
	// are now only the INITIAL step sizes -- each of the 4 traits' own step
	// independently halves (see the shrink logic below, after each year's
	// candidate is chosen) whenever the search stalls or oscillates at the
	// current resolution. A permanently fixed step cannot converge to a
	// point between grid levels: it can only ever bounce between its two
	// nearest neighbours forever. Confirmed empirically (notes_root_no_
	// belowground_economy.md section 22): ecto_allo/mycorrhized oscillated
	// between adjacent fixed-step levels indefinitely, even started from an
	// already-mature tree whose state was only slowly changing.
	double n_step = root_no_step_factor, l_step = root_length_step_factor;
	double e_step = ecto_step, m_step = mycorrhized_step;
	const double n_step_floor = 1.0 + (root_no_step_factor - 1.0) / 16.0;
	const double l_step_floor = 1.0 + (root_length_step_factor - 1.0) / 16.0;
	const double e_step_floor = ecto_step / 16.0;
	const double m_step_floor = mycorrhized_step / 16.0;
	// Direction (-1/0/+1) the previous year's commit moved each trait,
	// relative to that year's starting point; 0 = no prior move yet.
	int n_dir = 0, l_dir = 0, e_dir = 0, m_dir = 0;

	for (double yr = start_year; yr < end_year; yr += 1.0){
		plant::Plant P_backup = P;
		ErgodicEnvironment C_backup = C;
		double rep_backup = rep, litter_backup = litter_pool, seeds_backup = seeds, prod_backup = prod;

		// Candidate levels are centred on the tree's actual current physical
		// state (n_eff/l_eff), not last year's target.
		double n_cur = n_eff, l_cur = l_eff;
		double ecto_cur = P.traits.investment_from_tree;
		double myco_cur = P.uptake.mycorrhized;

		std::vector<double> n_levels = make_levels_mult(n_cur, n_step, root_no_min, root_no_max);
		std::vector<double> l_levels = make_levels_mult(l_cur, l_step, root_length_min, root_length_max);
		std::vector<double> e_levels = make_levels_add(ecto_cur, e_step, ecto_min, ecto_max);
		std::vector<double> m_levels = make_levels_add(myco_cur, m_step, mycorrhized_min, mycorrhized_max);

		double best_score = -1e300;
		double best_rn = n_cur, best_rl = l_cur, best_eb = ecto_cur, best_myc = myco_cur;

		// Trial scoring: steady-state destination value -- each candidate is
		// applied INSTANTLY for the trial (no relaxation here), judging
		// destinations fairly regardless of distance. The commit step below
		// is what enforces the realistic gradual transition.
		//
		// Receding/rolling horizon (2026-09-23): each candidate is trialled for
		// trial_horizon_years, not just one year, though only ONE real year is
		// ever committed below (re-evaluated with a fresh lookahead next year) --
		// standard rolling-horizon design, as in Model Predictive Control. Fixes
		// a real myopia bug: with a 1-year trial, any investment that only pays
		// off gradually (root_no/root_length via root_surface_area() -> N uptake
		// -> the slow-accumulating tree_nitrogen state -> leaf N -> Vcmax/GPP;
		// ectomycorrhiza_mass likewise, via dmyco_dt()) pays its full carbon
		// cost in the trial year but can't show its return within that same
		// year, so the search undervalues ANY slow-building belowground
		// investment regardless of its true long-run payoff. Confirmed
		// empirically at trial_horizon_years=1 after the C_export_to_myco fix
		// (see npp_per_ca() above): ecto_allo collapsed to 0 and root_no
		// collapsed to its floor even though the carbon cost was now correctly
		// charged (notes_root_no_belowground_economy.md section 18) -- a
		// symptom of trial horizon, not of the cost accounting.
		for (double rn : n_levels)
		for (double rl : l_levels)
		for (double eb : e_levels)
		for (double myc : m_levels){
			P = P_backup; C = C_backup;
			rep = rep_backup; litter_pool = litter_backup; seeds = seeds_backup; prod = prod_backup;

			P.geometry.set_root(rn, rl, P.traits, /*reset_ecto_mass=*/false);
			P.traits.investment_from_tree = eb;
			P.uptake.mycorrhized = myc;

			double score = 0.0;
			bool failed = false;
			for (double t = yr; t < yr + trial_horizon_years - 1e-9; t += reopt_dt){
				try {
					grow_for_dt(t, reopt_dt);
					score += npp_per_ca(P) * reopt_dt;
				} catch (std::exception& e) {
					// TEMP DIAGNOSTIC (2026-09-30): identify why grow_for_dt fails for
					// certain years/candidates (remove once root cause is found).
					std::cerr << "[trial-fail] yr=" << yr << " t=" << t
					          << " rn=" << rn << " rl=" << rl << " eb=" << eb << " myc=" << myc
					          << " what=" << e.what() << "\n";
					failed = true; break;
				}
			}
			if (!failed && score > best_score){
				best_score = score;
				best_rn = rn; best_rl = rl; best_eb = eb; best_myc = myc;
			}
		}

		// Adaptive step-size update: shrink a trait's step (halved, floored
		// at 1/16 of its initial value) whenever this year's winning choice
		// either stalled at the centre (best == cur -- no improvement found
		// at the current resolution) or reversed direction relative to last
		// year's move (best on the opposite side of cur from where it moved
		// last time -- the signature of oscillating between two fixed grid
		// points straddling the true optimum). Continuing in the same
		// direction as before leaves the step untouched, so genuine sustained
		// progress isn't slowed down.
		auto direction_of = [](double best, double cur) -> int {
			if (best == cur) return 0;
			return (best > cur) ? 1 : -1;
		};
		auto shrink_mult = [](double step, double floor){ return std::max(1.0 + (step - 1.0) * 0.5, floor); };
		auto shrink_add  = [](double step, double floor){ return std::max(step * 0.5, floor); };

		int n_new_dir = direction_of(best_rn, n_cur);
		int l_new_dir = direction_of(best_rl, l_cur);
		int e_new_dir = direction_of(best_eb, ecto_cur);
		int m_new_dir = direction_of(best_myc, myco_cur);

		if (n_new_dir == 0 || (n_dir != 0 && n_new_dir == -n_dir)) n_step = shrink_mult(n_step, n_step_floor);
		if (l_new_dir == 0 || (l_dir != 0 && l_new_dir == -l_dir)) l_step = shrink_mult(l_step, l_step_floor);
		if (e_new_dir == 0 || (e_dir != 0 && e_new_dir == -e_dir)) e_step = shrink_add(e_step, e_step_floor);
		if (m_new_dir == 0 || (m_dir != 0 && m_new_dir == -m_dir)) m_step = shrink_add(m_step, m_step_floor);
		n_dir = n_new_dir; l_dir = l_new_dir; e_dir = e_new_dir; m_dir = m_new_dir;

		// Commit: restore to the true start-of-year state, apply the winning
		// ecto_allo/mycorrhized instantly (no stock to relax), but relax
		// n_eff/l_eff toward (best_rn, best_rl) for real, at
		// tau=root_lifespan(traits) recomputed each substep from the current
		// (pre-update) l_eff -- this is what makes the logged, committed
		// trajectory physically honest.
		P = P_backup; C = C_backup;
		rep = rep_backup; litter_pool = litter_backup; seeds = seeds_backup; prod = prod_backup;
		P.traits.investment_from_tree = best_eb;
		P.uptake.mycorrhized = best_myc;

		for (double t = yr; t < yr + 1.0 - 1e-9; t += reopt_dt){
			try {
				double tau = std::max(P.geometry.root_lifespan(P.traits), 1e-6);
				double decay = std::exp(-reopt_dt / tau);
				n_eff = best_rn + (n_eff - best_rn) * decay;
				l_eff = best_rl + (l_eff - best_rl) * decay;
				P.geometry.set_root(n_eff, l_eff, P.traits, /*reset_ecto_mass=*/false);
				grow_for_dt(t, reopt_dt);
				std::vector<double> row = get_state(t + reopt_dt);
				row.push_back(best_eb);
				row.push_back(best_myc);
				row.push_back(best_rn);
				row.push_back(best_rl);
				log.push_back(row);
			} catch (std::exception& e) {
				// grow_for_dt fails here when Phydro's line search doesn't converge,
				// which happens in genuinely cold/dark winter months (notes_root_no_
				// belowground_economy.md section 26). Previously this did `break`,
				// which aborted the REST of the year on the first such failure --
				// freezing root/ecto/myco state for up to 11 more months over a
				// single bad month. `continue` instead skips only the failed month
				// (no row logged, state held over to the next step) and keeps going.
				std::cerr << "[commit-fail] yr=" << yr << " t=" << t
				          << " best_rn=" << best_rn << " best_rl=" << best_rl
				          << " best_eb=" << best_eb << " best_myc=" << best_myc
				          << " what=" << e.what() << "\n";
				continue;
			}
		}
	}

	return log;
}


// Trailing/backward variant (2026-09-30): see life_history.h for the design
// rationale. Each candidate is scored by restoring a checkpoint of the
// tree's own ALREADY-REALISED state from trial_horizon_years ago, applying
// the candidate there, and re-growing forward through the ACTUAL historical
// climate up to the present -- never a simulated future. The commit step
// (relaxation toward the winning candidate) is identical to the forward
// version above.
std::vector<std::vector<double>> LifeHistoryOptimizer::run_with_relaxed_local_reopt_trajectory_trailing(
    double start_year, double end_year, double reopt_dt, double trial_horizon_years,
    double root_no_step_factor, double root_no_min, double root_no_max,
    double root_length_step_factor, double root_length_min, double root_length_max,
    double ecto_step, double ecto_min, double ecto_max,
    double mycorrhized_step, double mycorrhized_min, double mycorrhized_max
){
	std::vector<std::vector<double>> log;

	struct Checkpoint {
		double year;
		plant::Plant P;
		ErgodicEnvironment C;
		double rep, litter_pool, seeds, prod;
	};
	std::deque<Checkpoint> history; // oldest first; one entry per committed year, START-of-year state

	auto make_levels_mult = [](double cur, double factor, double lo, double hi){
		return std::vector<double>{ std::max(cur / factor, lo), cur, std::min(cur * factor, hi) };
	};
	auto make_levels_add = [](double cur, double step, double lo, double hi){
		return std::vector<double>{ std::max(cur - step, lo), cur, std::min(cur + step, hi) };
	};

	double n_eff = P.geometry.root_no;
	double l_eff = P.geometry.root_length;

	double n_step = root_no_step_factor, l_step = root_length_step_factor;
	double e_step = ecto_step, m_step = mycorrhized_step;
	const double n_step_floor = 1.0 + (root_no_step_factor - 1.0) / 16.0;
	const double l_step_floor = 1.0 + (root_length_step_factor - 1.0) / 16.0;
	const double e_step_floor = ecto_step / 16.0;
	const double m_step_floor = mycorrhized_step / 16.0;
	int n_dir = 0, l_dir = 0, e_dir = 0, m_dir = 0;

	for (double yr = start_year; yr < end_year; yr += 1.0){
		plant::Plant P_backup = P;
		ErgodicEnvironment C_backup = C;
		double rep_backup = rep, litter_backup = litter_pool, seeds_backup = seeds, prod_backup = prod;

		// Record today's start-of-year state for future years to replay
		// against, then keep only as much history as any future year could
		// need (trial_horizon_years back, plus today).
		history.push_back(Checkpoint{yr, P_backup, C_backup, rep_backup, litter_backup, seeds_backup, prod_backup});
		while ((double)history.size() > std::ceil(trial_horizon_years) + 1.0) history.pop_front();

		double n_cur = n_eff, l_cur = l_eff;
		double ecto_cur = P.traits.investment_from_tree;
		double myco_cur = P.uptake.mycorrhized;

		std::vector<double> n_levels = make_levels_mult(n_cur, n_step, root_no_min, root_no_max);
		std::vector<double> l_levels = make_levels_mult(l_cur, l_step, root_length_min, root_length_max);
		std::vector<double> e_levels = make_levels_add(ecto_cur, e_step, ecto_min, ecto_max);
		std::vector<double> m_levels = make_levels_add(myco_cur, m_step, mycorrhized_min, mycorrhized_max);

		double best_score = -1e300;
		double best_rn = n_cur, best_rl = l_cur, best_eb = ecto_cur, best_myc = myco_cur;

		// Lookback depth: as much real history as has accumulated, capped at
		// trial_horizon_years. Only in the very first committed year (no
		// history at all yet) is there nothing already-realised to replay;
		// there, and only there, fall back to a single reopt_dt step FORWARD
		// from today's own state (the smallest possible peek, unavoidable at
		// t = start_year, rather than borrowing a whole future horizon).
		double elapsed = yr - start_year;
		bool have_history = elapsed >= reopt_dt / 2.0;

		const Checkpoint* base = &history.back();
		double trial_start, trial_end;
		if (have_history){
			double lookback = std::min(trial_horizon_years, elapsed);
			double replay_start = yr - lookback;
			for (const auto& h : history){
				if (h.year <= replay_start + 1e-9) base = &h;
			}
			trial_start = base->year;
			trial_end   = yr;
		} else {
			trial_start = yr;
			trial_end   = yr + reopt_dt;
		}

		for (double rn : n_levels)
		for (double rl : l_levels)
		for (double eb : e_levels)
		for (double myc : m_levels){
			P = base->P; C = base->C;
			rep = base->rep; litter_pool = base->litter_pool; seeds = base->seeds; prod = base->prod;

			P.geometry.set_root(rn, rl, P.traits, /*reset_ecto_mass=*/false);
			P.traits.investment_from_tree = eb;
			P.uptake.mycorrhized = myc;

			double score = 0.0;
			bool failed = false;
			for (double t = trial_start; t < trial_end - 1e-9; t += reopt_dt){
				try {
					grow_for_dt(t, reopt_dt);
					score += npp_per_ca(P) * reopt_dt;
				} catch (std::exception& e) { failed = true; break; }
			}
			if (!failed && score > best_score){
				best_score = score;
				best_rn = rn; best_rl = rl; best_eb = eb; best_myc = myc;
			}
		}

		auto direction_of = [](double best, double cur) -> int {
			if (best == cur) return 0;
			return (best > cur) ? 1 : -1;
		};
		auto shrink_mult = [](double step, double floor){ return std::max(1.0 + (step - 1.0) * 0.5, floor); };
		auto shrink_add  = [](double step, double floor){ return std::max(step * 0.5, floor); };

		int n_new_dir = direction_of(best_rn, n_cur);
		int l_new_dir = direction_of(best_rl, l_cur);
		int e_new_dir = direction_of(best_eb, ecto_cur);
		int m_new_dir = direction_of(best_myc, myco_cur);

		if (n_new_dir == 0 || (n_dir != 0 && n_new_dir == -n_dir)) n_step = shrink_mult(n_step, n_step_floor);
		if (l_new_dir == 0 || (l_dir != 0 && l_new_dir == -l_dir)) l_step = shrink_mult(l_step, l_step_floor);
		if (e_new_dir == 0 || (e_dir != 0 && e_new_dir == -e_dir)) e_step = shrink_add(e_step, e_step_floor);
		if (m_new_dir == 0 || (m_dir != 0 && m_new_dir == -m_dir)) m_step = shrink_add(m_step, m_step_floor);
		n_dir = n_new_dir; l_dir = l_new_dir; e_dir = e_new_dir; m_dir = m_new_dir;

		// Commit: restore to the TRUE start-of-year state (today's own,
		// not the lookback checkpoint), same as the forward version.
		P = P_backup; C = C_backup;
		rep = rep_backup; litter_pool = litter_backup; seeds = seeds_backup; prod = prod_backup;
		P.traits.investment_from_tree = best_eb;
		P.uptake.mycorrhized = best_myc;

		for (double t = yr; t < yr + 1.0 - 1e-9; t += reopt_dt){
			try {
				double tau = std::max(P.geometry.root_lifespan(P.traits), 1e-6);
				double decay = std::exp(-reopt_dt / tau);
				n_eff = best_rn + (n_eff - best_rn) * decay;
				l_eff = best_rl + (l_eff - best_rl) * decay;
				P.geometry.set_root(n_eff, l_eff, P.traits, /*reset_ecto_mass=*/false);
				grow_for_dt(t, reopt_dt);
				std::vector<double> row = get_state(t + reopt_dt);
				row.push_back(best_eb);
				row.push_back(best_myc);
				row.push_back(best_rn);
				row.push_back(best_rl);
				log.push_back(row);
			} catch (std::exception& e) {
				break;
			}
		}
	}

	return log;
}

} // namespace pfate
