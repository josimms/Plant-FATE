#include <iomanip>
#include <fstream>

#include "plant_params.h"
#include "plant_geometry.h"
#include "assimilation.h"
#include "plant.h"

#include "climate.h"
#include "light_environment.h"

using namespace std;

class Environment : public env::Climate, public env::LightEnvironment {
	public:
	void print(double t){
		Climate::print(t);
		LightEnvironment::print();
	}
};

int main(){

	plant::Plant P;
	P.initParamsFromFile("tests/params/p.ini");

	Environment C;
	C.metFile = "tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE.csv";
	C.co2File = "tests/data/CO2_AMB_AmzFACE2000_2100.csv";
	C.init();
	C.print(0);


	ofstream fout("assim.txt");
	fout << "i" << "\t"
		 << "ppfd" <<"\t"
		 << "assim_net" << "\t"
		 << "assim_gross" << "\t"
		 << "rl" << "\t"
		 << "rr" << "\t"
		 << "rs" << "\t"
		 << "tl" << "\t"
		 << "tr" << "\t"
		 << "dpsi" << "\t"
		 << "vcmax" << "\t"
		 << "transpiration" << "\t"
		 << "height" << "\t"	
		 << "diameter" << "\t"	
		 << "crown_area" << "\t"	
		 << "lai" << "\t"	
		 << "sapwood_fraction" << "\t"
		 << "total_mass" << "\t"
		 << "total_rep" << "\t"
		 << "seed_pool" << "\t"
		 << "germinated" << "\t"
		 << "total_prod" << "\t"
		 << "litter_mass" << "\n";
	double dt = 0.1; 
	double total_prod = P.get_biomass();
	double total_rep = 0;
	double seed_pool = 0;
	double germinated = 0;
	cout << "Starting biomass = " << total_prod << "\n";
	for (double t=2000; t<=2200; t=t+dt){

		//cout << t << " " << P.geometry->total_mass(par, traits) << " " << total_prod << "\n";
		//if (abs(P.get_biomass() - total_prod) > 1e-6) return 1;
		
		fout << t << "\t"
			 << C.clim.ppfd << "\t"
			 << P.assimilator->plant_assim.npp << "\t" 
			 << P.assimilator->plant_assim.gpp << "\t" 
			 << P.assimilator->plant_assim.rleaf << "\t"
			 << P.assimilator->plant_assim.rroot << "\t"
			 << P.assimilator->plant_assim.rstem << "\t"
			 << P.assimilator->plant_assim.tleaf << "\t"
			 << P.assimilator->plant_assim.troot << "\t"
			 << P.assimilator->plant_assim.dpsi_avg << "\t" 
			 << P.assimilator->plant_assim.vcmax_avg << "\t" 
			 << P.assimilator->plant_assim.trans << "\t" 
			 << P.geometry->height << "\t"	
			 << P.geometry->diameter << "\t"	
			 << P.geometry->crown_area << "\t"	
			 << P.geometry->lai << "\t"	
			 << P.geometry->sapwood_fraction << "\t"	
			 << P.get_biomass() << "\t"
			 << total_rep << "\t"
			 << seed_pool << "\t"
			 << germinated << "\t"
			 << total_prod << "\t"
			 << P.geometry->litter_pool << "\n";
		
		//total_prod += P*P.geometry->leaf_area*dt;
		
		P.grow_for_dt(t, dt, C, total_prod, total_rep, seed_pool, germinated);
		//double dhdM = P.geometry->dheight_dmass(par, traits);
		//double h_new = P.geometry->height + dhdM*P*P.geometry->leaf_area*dt;
		//P.geometry->set_height(h_new, par, traits);

	}
	fout.close();

	cout << "At t = " << 100 << "\n" 
		 << setprecision(12) 
		 << "Total biomass    = " << P.get_biomass() << "\n"
		 << "Total litter     = " << P.geometry->litter_pool << "\n"
		 << "Total reproduc   = " << total_rep << "\n"
		 << "Total bio+lit+rep = " << P.get_biomass() + P.geometry->litter_pool + total_rep << "\n"
		 << "Total production = " << total_prod << "\n";
	
	if (abs((P.get_biomass()+P.geometry->litter_pool+total_rep)/total_prod - 1) > 2e-5) return 1;
	
	return 0;
}

//# R script to test:
//dat = read.delim("/home/jaideep/codes/plant_fate_ppa/assim.txt")
//dat$heartwood_fraction = 1-dat$sapwood_fraction

//par(mfrow=c(3,2), mar=c(4,4,1,1), oma=c(1,1,1,1))

//plot(dat$height~dat$diameter)
//plot(dat$crown_area~I(dat$height*dat$diameter))
//plot(dat$crown_area~dat$height)
//plot(dat$crown_area~dat$diameter)
//plot(dat$leaf_area~dat$crown_area)
//plot(dat$sapwood_fraction~dat$height)
//# plot(sqrt(4*dat$crown_area/pi)~dat$height)

//plot(dat$height~dat$i)
//plot(dat$diameter~dat$i)
//plot(dat$total_mass~dat$i)
//points(dat$total_prod~dat$i, type="l", col="red")

//matplot(y=cbind(dat$assim_gross,
//                dat$assim_net), 
//        x=dat$i, col=c("green3", "green4"), log="", pch=20)

//matplot(y=cbind(dat$rr,
//                dat$rs,
//                dat$rl), 
//        x=dat$i, col=c("yellow2", "yellow3", "yellow4"), log="", pch=20)

//matplot(y=cbind(dat$tr,
//                dat$tl), 
//        x=dat$i, col=c("orange3", "orange4"), log="", pch=20)


//D = dat$diameter
//H = dat$height
//-log(1-H/20)*20/D
