#ifndef PLANT_FATE_PFATE_LIFEHISTORY_OPTIMIZER_H_
#define PLANT_FATE_PFATE_LIFEHISTORY_OPTIMIZER_H_

#include <vector>
#include <ostream>

#include "traits_params.h"
#include "plant_architecture.h"
#include "assimilation.h"
#include "plant.h"
#include <cmath>

#include "climate.h"
#include "climate_stream.h"
#include "light_environment.h"

#include <time_stepper.h>

namespace pfate{

class ErgodicEnvironment : public env::LightEnvironment, public env::Climate{
	public:
	double bg_density_ini = 0.0;    // stem density at t0 [stems/m2]
	double bg_density_eq  = 0.05;   // equilibrium stem density [stems/m2]
	double bg_tau         = 30.0;   // e-folding timescale [years]
	double bg_t0          = 1960.0; // year succession begins

	ErgodicEnvironment();
	void init(io::Initializer& I);
	void updateBackgroundCanopy(double t, plant::PlantArchitecture& geom,
	                            plant::PlantTraits& traits, const plant::PlantParameters& par);
	void print(double t) override;

	// override computeEnv() to NOT update the light profile
	void computeEnv(double t, Solver* sol, std::vector<double>::iterator S, std::vector<double>::iterator dSdt) override;
};


class LifeHistoryOptimizer{
	public:
	flare::TimeStepper ts;
	plant::Plant P;
	ErgodicEnvironment C;
	env::ClimateStream c_stream;

	// std::string paramsFile;
	// std::string met_file = "";
	// std::string co2_file = "";

	plant::PlantParameters par0;
	plant::PlantTraits traits0;
	plant::Uptake uptake0;

	io::Initializer I;

	double dt = 0.1;

	double rep;
	double litter_pool;
	double seeds;
	double prod;

	public:

	LifeHistoryOptimizer(std::string params_file);

	void set_i_metFile(std::string file);
	void set_a_metFile(std::string file);
	void set_co2File(std::string co2file);
	void init_co2(double _co2);
	void set_soil_nitrogen(double _N);
	void root_override(double _rn, double _rl);

	void init();

	void update_climate(double julian_time);

	std::vector<std::string> get_header();
	void printHeader(std::ostream& lfout);

	std::vector<double> get_state(double t);
	void printState(double t, std::ostream& lfout);

	void printMeta();

	// void set_traits(std::vector<double> tvec);
	// std::vector<double> get_traits();

	void set_state(std::vector<double>::iterator it);

	void get_rates(std::vector<double>::iterator it);

	void grow_for_dt(double t, double dt);


	double calcFitness();

	// Yearly re-optimization of (root_no, root_length, ecto investment,
	// mycorrhized) instead of one lifetime-fixed combination. See the "Making
	// the model internally dynamic" section of the manuscript. Each year,
	// builds a local 3-level {down, current, up} candidate set around the
	// CURRENT committed trait point and trials all 3^4=81 joint combinations
	// (not independent per-axis steps -- a full joint search captures
	// interactions between all four traits and has no order-dependence).
	// root_no/root_length steps are multiplicative (log-space); ecto_allo/
	// mycorrhized steps are additive.
	//
	// Adds root-system relaxation on the COMMIT step. Without it, root_no
	// walks monotonically to root_no_min every year: rroot/troot
	// (respiration/turnover costs) are driven by root_mass(), which is
	// instantaneous, while the BENEFIT of more root investment (via
	// root_surface_area() -> N uptake -> tree_nitrogen, a genuine integrated
	// state variable -> leaf N -> Vcmax/GPP) is already naturally gradual,
	// since tree_nitrogen only accumulates over time on its own. Relaxing
	// root_mass/root_surface_area re-synchronises the cost side to the pace
	// the benefit side already moves at.
	//
	// Two state variables local to this method (n_eff, l_eff -- NOT new
	// PlantArchitecture/Plant members, so root_mass()/root_surface_area()
	// and every other code path stay untouched) track the tree's actual
	// physical root configuration, initialised once from
	// P.geometry.root_no/root_length at call start (continuous with
	// root_override()) and persisting across years.
	//
	// Trial scoring (used to rank the 81 joint candidates each year) applies
	// each candidate INSTANTLY -- i.e. "steady-state destination" scoring: a
	// candidate is judged as if the tree were already fully at that root
	// configuration, not by how far a single partially-relaxed trial year
	// gets toward it. This
	// avoids unfairly penalising a good-but-distant destination for not
	// having arrived yet. The COMMIT step is where relaxation actually
	// happens: n_eff/l_eff exponentially chase the winning candidate at
	// tau = root_lifespan(traits) (recomputed each substep from the current,
	// pre-update l_eff), via set_root(n_eff, l_eff, traits,
	// reset_ecto_mass=false) -- this is what makes the logged, plotted
	// trajectory physically honest. ecto_allo/mycorrhized are not relaxed
	// (they gate a flow/fraction, not a standing stock) -- committed
	// instantly as in the other methods.
	//
	// Logs one row per reopt_dt step: get_state() columns (root_no/
	// root_length here reflect n_eff/l_eff, the smoothed values), then
	// trailing ecto_allo, mycorrhized, root_no_target, root_length_target
	// (this year's committed destination, constant across the year's rows
	// -- compare against the get_state() root_no/root_length columns to see
	// the relaxation lag directly).
	std::vector<std::vector<double>> run_with_relaxed_local_reopt_trajectory(
	    double start_year, double end_year, double reopt_dt,
	    double root_no_step_factor, double root_no_min, double root_no_max,
	    double root_length_step_factor, double root_length_min, double root_length_max,
	    double ecto_step, double ecto_min, double ecto_max,
	    double mycorrhized_step, double mycorrhized_min, double mycorrhized_max
	);

};

} // namespace pfate

#endif
