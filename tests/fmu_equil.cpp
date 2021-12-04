#include <vector>
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#include <solver.h>
#include "pspm_interface.h"

std::vector <double> myseq(double from, double to, int len){
	std::vector<double> x(len);
	for (size_t i=0; i<len; ++i) x[i] = from + i*(to-from)/(len-1);
	return x;
}

vector<double> generateDefaultCohortSchedule(double max_time){

	vector<double> tvec;

	const double multiplier=0.2, min_step_size=1e-5, max_step_size=2;
	
	assert(min_step_size > 0 && "The minimum step size must be greater than zero");
	
	double dt = 0.0, time = 0.0;
	tvec.push_back(time);
	while (time <= max_time) {
		dt = exp2(floor(log2(time * multiplier)));
		time += min(max(dt, min_step_size), max_step_size);
		tvec.push_back(time);
	}

	// Drop the last time; that's not going to be needed:
	if (tvec.size() >=1) 	// JAI: added to avoid overflow warning
		tvec.resize(tvec.size() - 1);

	return tvec;
}



class SolverIO{
	public:
	int nspecies;
	Solver * S;

	vector <vector<ofstream>> streams;

	void openStreams(vector<string> varnames){
		varnames.insert(varnames.begin(), "u");
		varnames.insert(varnames.begin(), "X");
		
		for (int s=0; s < S->species_vec.size(); ++s){
			auto spp = S->species_vec[s];
			vector<ofstream> spp_streams;
			
			for (int i=0; i<varnames.size(); ++i){
				stringstream sout;
				sout << "species_" << s << "_" << varnames[i] << ".txt";
				cout << sout.str() << endl;
				ofstream fout(sout.str().c_str());
				spp_streams.push_back(std::move(fout));
			}
			streams.push_back(std::move(spp_streams));
		}
	}

	void closeStreams(){
		for (int s=0; s<streams.size(); ++s){
			for (int j=0; j<streams[s].size(); ++j){
				streams[s][j].close();
			}
		}
	}

	void writeState(){
		for (int s=0; s < S->species_vec.size(); ++s){
			auto spp = (Species<PSPM_Plant>*)S->species_vec[s];

			for (int i=0; i<streams[s].size(); ++i) streams[s][i] << S->current_time << "\t";

			for (int j=0; j<spp->xsize(); ++j){
				auto& C = spp->getCohort(j);
				streams[s][0] << C.x << "\t";
				streams[s][1] << C.u << "\t";
				streams[s][2] << C.geometry.height << "\t";
				streams[s][3] << C.geometry.lai << "\t";
				streams[s][4] << C.state.mortality << "\t";
				streams[s][5] << C.state.seed_pool << "\t";

			}
			
			for (int i=0; i<streams[s].size(); ++i) streams[s][i] << "\n";
		}
	}
};


int main(){

	PSPM_Plant p1;
	p1.initParamsFromFile("tests/params/p.ini");
	p1.set_size(0.01);

	PSPM_Dynamic_Environment E;
	E.metFile = "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE.csv";
	E.co2File = "tests/data/CO2_AMB_AmzFACE2000_2100.csv";
	E.init();
	E.print(0);
	E.use_ppa = true;

	Species<PSPM_Plant> spp(p1);
	spp.set_bfin_is_u0in(true);

	Solver S(SOLVER_CM, "rk45ck");
	S.setEnvironment(&E);
//	S.addSpecies(30, 0.01, 2, true, &spp, 3, 1);
	S.addSpecies(vector<double>(1, p1.geometry.get_size()), &spp, 3, 1);
	//S.get_species(0)->set_bfin_is_u0in(true);	// say that input_birth_flux is u0
	S.resetState();
	S.initialize();
	S.print();
	S.current_time = 0;
//	S.control.update_cohorts = false;


	SolverIO sio;
	sio.S = &S;
	sio.openStreams({"height", "lai", "mort", "seeds"});

	
//	S.step_to(0.1);
//	S.print();
//	for (auto y : S.state) cout << y << "\t"; cout << "\n";

//	ofstream fout("fmu_PlantFATE.txt");
	for (double t=0.1; t <= 50; t=t+2) {
		cout << "t = " << t << "\n";
		S.step_to(t);		
		
//		vector<double> seeds = S.newborns_out();
//		for (int s=0; s< S.species_vec.size(); ++s){
//			double S_D = 0.25;
//			seeds_out[s].push_back(seeds[s] * env.patch_age_density(t));
//		}
//		fseed << t << "\t";
//		for (int i=0; i<S.n_species(); ++i) fseed << seeds[i] << "\t";
//		fseed << "\n";

//		cout << t << " " << S.species_vec[0]->xsize() << " ";
//		for (int i=0; i<S.n_species(); ++i) cout << seeds[i] << " ";
//		cout << " | n_lp = " << env.light_profile.npoints << " | PPA: nl = " << env.n_layers << ", z* = " << env.z_star[0] << "\n";

//		vector<double> xl = myseq(0, 20, 1000);
//		for (auto h : xl) fli << env.LA_above_z(t,h,&S) << "\t";
//		fli << endl;
//		
//		fzst << t << "\t";
//		for (auto z : env.z_star) fzst << z << "\t";
//		fzst << "\n";
			
		sio.writeState();
	}
	
	E.use_ppa = true;
	E.computeEnv(200, &S);  
	E.print(200);
	
	double caz=0, flz=0, caz0;
	caz = E.projected_crown_area_above_z(200, 0, &S);
	cout << "Cumm crown area above 0 = " << caz << "\n";
	
	caz = E.projected_crown_area_above_z(200, 4, &S);
	cout << "Cumm crown area above 4 = " << caz << "\n";

	caz = E.projected_crown_area_above_z(200, 10, &S);
	cout << "Cumm crown area above 10 = " << caz << "\n";

	caz = E.projected_crown_area_above_z(200, 14, &S);
	cout << "Cumm crown area above 14 = " << caz << "\n";

	caz = E.projected_crown_area_above_z(200, 16, &S);
	cout << "Cumm crown area above 16 = " << caz << "\n";

	caz = E.projected_crown_area_above_z(200, 22, &S);
	cout << "Cumm crown area above 22 = " << caz << "\n";

	for (int i=0; i<E.n_layers; ++i){
		flz = E.fapar_layer(200, i, &S);
		cout << "absorbed light at z* (" << E.z_star[i] << ") = " << flz << "\n";
	}
	
	E.use_ppa = true;
	E.computeEnv(200, &S);  
	E.print(200);
	
	auto &P = (static_cast<Species<PSPM_Plant>*>(S.species_vec[0]))->getCohort(0);
	auto res = P.assimilator.net_production(E, &P.geometry, P.par, P.traits);	

//	flz = E.fapar_layer(200, 1, &S);
//	cout << "absorbed light at z** (" << E.z_star[1] << ") = " << flz << "\n";

//	flz = E.fapar_layer(200, 2, &S);
//	cout << "absorbed light at z*3 (" << E.z_star[2] << ") = " << flz << "\n";

//	flz = E.fapar_layer(200, 3, &S);
//	cout << "absorbed light at z*4 (" << E.z_star[3] << ") = " << flz << "\n";

//	flz = E.fapar_layer(200, 4, &S);
//	cout << "absorbed light at z*5 (" << E.z_star[4] << ") = " << flz << "\n";

//	flz = E.fapar_layer(200, 5, &S);
//	cout << "absorbed light at z*6 (" << E.z_star[5] << ") = " << flz << "\n";

//	flz = E.fapar_layer(200, 6, &S);
//	cout << "absorbed light at z*7 = " << flz << "\n";

	// z* (6) = 13.2565 11.4822 9.83659 8.17456 6.35631 3.9639 0
//	fout.close();

//	// Expected 44.3530812 
//	cout << S.newborns_out()[0] << endl; 
//	//if (abs(S.u0_out()[0] - 1.468232) < 1e-5) return 0;
//	//else return 1;

}

