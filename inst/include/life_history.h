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

	// Checkpoint for delayed-perturbation sensitivity analysis: save_checkpoint()
	// snapshots the full grow_for_dt()-mutable state (P, C, and the four scalar
	// accumulators above); restore_checkpoint() rewinds to it, so the same
	// spin-up can be replayed with a different parameter value each time
	// without re-running the spin-up itself.
	plant::Plant P_checkpoint;
	ErgodicEnvironment C_checkpoint;
	double rep_checkpoint;
	double litter_pool_checkpoint;
	double seeds_checkpoint;
	double prod_checkpoint;

	public:

	LifeHistoryOptimizer(std::string params_file);

	void set_i_metFile(std::string file);
	void set_a_metFile(std::string file);
	void set_co2File(std::string co2file);
	void init_co2(double _co2);
	void set_soil_nitrogen(double _N);
	void root_override(double _rn, double _rl);

	void init();

	// See the "Checkpoint for delayed-perturbation sensitivity analysis" comment
	// above. reinit_params() re-copies par0/traits0/uptake0 into the live P and
	// recomputes P's derived geometry constants (via Plant::init(), which only
	// touches par/traits/uptake + coordinateTraits() -- see src/plant.cpp) WITHOUT
	// resetting P's accumulated size/mass/root-count/mycorrhizal-mass/N-pool state
	// (those live in P.geometry, untouched by Plant::init()). Use it after
	// restore_checkpoint() and editing par0/traits0/uptake0 to apply a new
	// parameter value to an already-grown tree, keeping everything else as-is.
	void save_checkpoint();
	void restore_checkpoint();
	void reinit_params();

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
	// trial_horizon_years (2026-09-23): each candidate is trialled for this
	// many simulated years (not just one) before scoring -- a receding/
	// rolling horizon, as in Model Predictive Control: only ONE real year is
	// ever committed per outer loop iteration (re-evaluated with a fresh
	// lookahead next year), but the lookahead itself runs trial_horizon_years
	// years so investments that only pay off gradually (root_no/root_length,
	// ectomycorrhiza_mass) get a chance to show their return before being
	// judged. With trial_horizon_years=1 (the original behaviour), any such
	// investment pays its full carbon cost in the trial year but can't show
	// its benefit within that same year, so the search undervalues ALL
	// slow-building belowground investment regardless of its true long-run
	// payoff -- confirmed empirically (notes_root_no_belowground_economy.md
	// section 18): after fixing npp_per_ca() to correctly charge
	// C_export_to_myco, ecto_allo collapsed to 0 and root_no collapsed to its
	// floor even with the cost now correctly priced.
	//
	// root_no_step_factor/root_length_step_factor/ecto_step/mycorrhized_step
	// (2026-09-24) are now only the INITIAL step sizes for each trait, not
	// fixed for the whole run. Each trait's step independently halves
	// (floored at 1/16 of its initial value) whenever that year's winning
	// choice stalls at the centre (no improvement found at the current
	// resolution) or reverses direction relative to the previous year's move
	// -- the signature of oscillating between two fixed grid points that
	// straddle the true optimum. A permanently fixed step cannot converge to
	// a point between grid levels; it can only ever bounce between its two
	// nearest neighbours forever. Confirmed empirically
	// (notes_root_no_belowground_economy.md section 22): with a fixed step,
	// ecto_allo/mycorrhized oscillated between adjacent levels indefinitely,
	// even starting from an already-mature tree with a slowly-changing
	// state, ruling out ontogeny as the sole explanation for the multi-year
	// settling time seen in earlier sections.
	//
	// Logs one row per reopt_dt step: get_state() columns (root_no/
	// root_length here reflect n_eff/l_eff, the smoothed values), then
	// trailing ecto_allo, mycorrhized, root_no_target, root_length_target
	// (this year's committed destination, constant across the year's rows
	// -- compare against the get_state() root_no/root_length columns to see
	// the relaxation lag directly).
	std::vector<std::vector<double>> run_with_relaxed_local_reopt_trajectory(
	    double start_year, double end_year, double reopt_dt, double trial_horizon_years,
	    double root_no_step_factor, double root_no_min, double root_no_max,
	    double root_length_step_factor, double root_length_min, double root_length_max,
	    double ecto_step, double ecto_min, double ecto_max,
	    double mycorrhized_step, double mycorrhized_min, double mycorrhized_max
	);

	// Trailing/backward variant of run_with_relaxed_local_reopt_trajectory
	// (2026-09-30): identical search, commit and relaxation logic, but each
	// candidate is scored against the tree's own ALREADY-REALISED previous
	// trial_horizon_years, not a simulated future. A rolling buffer of
	// yearly checkpoints (state as committed at the START of each past
	// year) is kept; to score a candidate at year yr, the checkpoint from
	// yr - trial_horizon_years is restored, the candidate applied instantly
	// there, and the tree is re-grown forward through the ACTUAL historical
	// climate from yr - trial_horizon_years to yr (flare's CsvStream is
	// random-access -- julian_to_indices() binary-searches an in-memory
	// table, so replaying past time points is exact and side-effect-free).
	// This tests "had this candidate been adopted trial_horizon_years ago,
	// how would it have fared through what actually happened since" --
	// biologically causal (no foresight of unrealised years), in contrast
	// to the forward/receding-horizon version above. For the first
	// trial_horizon_years of the run, before enough history has
	// accumulated, the lookback is truncated to whatever history exists
	// (minimum one reopt_dt step) rather than borrowing future years.
	std::vector<std::vector<double>> run_with_relaxed_local_reopt_trajectory_trailing(
	    double start_year, double end_year, double reopt_dt, double trial_horizon_years,
	    double root_no_step_factor, double root_no_min, double root_no_max,
	    double root_length_step_factor, double root_length_min, double root_length_max,
	    double ecto_step, double ecto_min, double ecto_max,
	    double mycorrhized_step, double mycorrhized_min, double mycorrhized_max
	);

};

} // namespace pfate

#endif
