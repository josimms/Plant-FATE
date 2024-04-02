#include <vector>
#include <iostream>
#include <fstream>
#include <cmath>
#include <numeric>
#include <functional>

#include "plantfate_patch.h"

using namespace std;

int is_equal(const vector<double>& v1, const vector<double>& v2, double tol=1e-6){
	bool b = true;
	for (int i=0; i<v1.size(); ++i){
		cout << "Comparing v["<<i<<"]: " << v1[i] << " " << v2[i] << '\n';
		b = b & (fabs(v1[i]-v2[i])< tol);
	}
	return b? 0:1;
}

int main(int argc, char ** argv){

	// string par_file = "tests/params/p_test_v2.ini";
	string par_file = "tests/params/p_test_v2_evol3_wdxhmat.ini";
	if (argc == 2){
		par_file = argv[1];
	}

	vector<double> ba, ba1;

	// reference run
	{
		pfate::Patch sim(par_file);
		sim.config.continuePrevious = false;
		sim.config.expt_dir = "cont_test_ref";
		sim.init(1000, 1350);
		sim.simulate();

		ba = sim.props.species.basal_area_vec;
		for (auto& b : ba) b*=1e4;
		cout << setprecision(10) << "Basal areas [m2/Ha]: " << ba << '\n';

		sim.close();
	}

	// spinup run
	{
		pfate::Patch sim(par_file);
		sim.config.continuePrevious = false;
		sim.config.expt_dir = "cont_test_spinup";
		sim.init(1000, 1200);
		sim.simulate();
		sim.close();
	}

	// continuation run
	{
		pfate::Patch sim(par_file);
		sim.config.continuePrevious = true;
		sim.config.continueFrom_stateFile    = sim.config.parent_dir + "/" + "cont_test_spinup/pf_saved_state.txt"; 
		sim.config.continueFrom_configFile   = sim.config.parent_dir + "/" + "cont_test_spinup/pf_saved_config.ini"; 
		sim.config.expt_dir = "cont_test_main";
		sim.init(1000, 1350);
		sim.simulate();

		ba1 = sim.props.species.basal_area_vec;
		for (auto& b : ba1) b*=1e4;

		sim.close();
	}

	cout << setprecision(10) << "Basal areas [m2/Ha]: " << ba  << '\n';
	cout << setprecision(10) << "Basal areas [m2/Ha]: " << ba1 << '\n';

	int err = is_equal(ba, ba1, 1e-2);

	return err;
}

