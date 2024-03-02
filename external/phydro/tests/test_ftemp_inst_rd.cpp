#include <iostream>
#include <iomanip>
#include <fstream>
#include "phydro.h"
using namespace std;


double err(double x, double ref){
	return std::min(abs(x-ref), abs(x/ref-1));
}

int check(double x, double ref, double err=1e-6){
	//cout << "err: " << abs(x-ref) << " " << abs(x/ref-1)<< "\n";
	//cout << "comp: " << x << " " << ref << "|" << abs(x-ref) << "\n";
	if (abs(x-ref) < err || abs(x/ref-1) < err) return 0;
	else return 1;
}


vector<double> seq(double start, double end, int length){
	vector<double> x;
	for (int i=0; i<length; ++i) x.push_back(start + double(i)*(end-start)/(length-1));
	return x;
}

vector<double> lseq(double start, double end, int length){
	vector<double> x;
	for (int i=0; i<length; ++i) x.push_back(exp(log(start) + double(i)*(log(end)-log(start))/(length-1)));
	return x;
}


int main(){
	vector<double> x = seq(-5,40,46);
	// ftemp_inst_vcmax
	vector<double> expected_heskel = {0.064829145532956, 0.0720568419433009, 0.0800102922066631, 0.0887528267881291, 0.0983522359287801, 0.108880971283499, 0.120416344940689, 0.133040724284698, 0.146841721026794, 0.161912372595275, 0.1783513139409, 0.196262937681662, 0.215757540382527, 0.236951452642668, 0.25996715054666, 0.284933345928884, 0.311985052803922, 0.341263627231896, 0.372916777818666, 0.407098543998545, 0.44396923921378, 0.483695356092695, 0.526449430739061, 0.572409863281007, 0.621760691890599, 0.674691317576872, 0.731396177177369, 0.792074362127597, 0.856929180775671, 0.926167662231788, 1, 1.0786389339335, 1.16229906938451, 1.25119613278587, 1.34554616330272, 1.44556464062873, 1.55146554947037, 1.6634603817633, 1.78175707819439, 1.9065589111587, 2.03806331185991, 2.17646064486058, 2.32193293400237, 2.47465254424016, 2.63478082456344, 2.80246671780802};

	for (size_t i=0; i<x.size(); ++i){
		double T = x[i];
		double f = phydro::calc_ftemp_inst_rd(T, phydro::FR_heskel16); //, 25, 2);
		if (check(f, expected_heskel[i]) == 1) return 1;
		cout << f << " " << expected_heskel[i] << "\n";
	}


	vector<double> expected_arrh = {0.392897537430827, 0.406687194213119, 0.420853339917828, 0.435402958451733, 0.450343085637899, 0.465680808291063, 0.481423263281946, 0.497577636591005, 0.514151162352092, 0.531151121886528, 0.548584842728063, 0.56645969763921, 0.584783103619426, 0.603562520905607, 0.622805451965381, 0.64251944048364, 0.662712070342797, 0.683390964597191, 0.704563784442118, 0.726238228177904, 0.748422030169483, 0.771122959801888, 0.794348820432106, 0.818107448337688, 0.842406711662556, 0.867254509360396, 0.892658770136046, 0.918627451385269, 0.945168538133308, 0.972290041972597, 1, 1.02830647375396, 1.05721754815189, 1.08674133042823, 1.11688594907341, 1.14765955277413, 1.17907030935531, 1.21112640472401, 1.24383604181556, 1.27720743954237, 1.31124883174558, 1.34596846614998, 1.38137460332234, 1.41747551563358, 1.45427948622495, 1.49179480797855};

	for (size_t i=0; i<x.size(); ++i){
		double T = x[i];
		double f = phydro::calc_ftemp_inst_rd(T, phydro::FR_arrhenius); //, 25, 2);
		if (check(f, expected_arrh[i]) == 1) return 1;
		cout << f << " " << expected_arrh[i] << "\n";
	}


	vector<double> expected_q10 = {7.33545652997899e-18, 3.73510666825735e-17, 1.86097538825313e-16, 9.06903631528611e-16, 4.32095068346284e-15, 2.01188525443981e-14, 9.15026900602985e-14, 4.0631689317261e-13, 1.76068653761586e-12, 7.44154116133021e-12, 3.06602882494726e-11, 1.23078357633247e-10, 4.81093112648767e-10, 1.83002970793811e-09, 6.7701350638213e-09, 2.43424209879202e-08, 8.50084687750431e-08, 2.88127208632827e-07, 9.47125984634247e-07, 3.01714063810842e-06, 9.30668014598595e-06, 2.77738238315195e-05, 8.01180535643255e-05, 0.00022318847912501, 0.000599835207654403, 0.00155367729780715, 0.00387422066364092, 0.00928972360726799, 0.0213938339547529, 0.0472589792060491, 0.1, 0.2024, 0.3912484, 0.7211429568, 1.2652220544016, 2.10906087424, 3.33376230926617, 4.98643311542492, 7.04168260648506, 9.36575929607089, 11.7019640702768, 13.6963307755545, 14.971360179937, 15.2335642992405, 14.3771683190652, 12.5367884074243};

	for (size_t i=0; i<x.size(); ++i){
		double T = x[i];
		double f = phydro::calc_ftemp_inst_rd(T, phydro::FR_q10); //, 25, 2);
		if (check(f/expected_q10[i], 1) == 1) return 1;
		cout << f << " " << expected_q10[i] << "\n";
	}

	return 0;

}
