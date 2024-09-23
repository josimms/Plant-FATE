//#ifndef PHYDRO_NITROGEN_H
//#define PHYDRO_NITROGEN_H

//#include "hyd_transpiration.h"
//#include "hyd_photosynthesis.h"
//#include "temperature_dependencies_photosynthesis.h"

//#ifdef USINGRCPP
//#include <RcppEigen.h>
//#else
//#include <Eigen/Core>
//#endif

//#include <LBFGSB.h>
//#include <LBFGS.h>

//using Eigen::VectorXd;

//namespace phydro{

//} // phydro

//#endif

/*
 inline ACi aj_from_jmax_n(double n_leaf, ParPhotosynthNitrogen par_photosynth) {
 double J = calc_J_from_jmax_nitrogen(n_leaf, par_photosynth);
 // TODO: define ci
 double ci = 1; // calculate_ci(jmax, par_photosynth); 
 double vcmax = 1; // TODO: add this formula - but there is a circular dependency!
 double Rd = par_photosynth.delta * vcmax;
 ACi resul;
 resul.a = J/4 * (ci - par_photosynth.gammastar)/(ci - 2*par_photosynth.gammastar) - Rd;
 resul.ci = ci;
 return resul;
 }
 
 inline double calculate_ci(double n, ParPhotosynthNitrogen par_photosynth) {
 double J = cal_J_from_jmax(jmax, par_photosynth); // Make this the N version of the formula
 // TODO: vcmax in terms of n?
 double Rd = calculate_rd(par_photosynth.ftemp_br_method, par_photosynth.ftemp); // TODO: Where does this vcmax come from?
 double g = 1; // TODO: make this formula?
 double a = 1;
 double b = J/(4*g) - par_photosynth.ca + 2*par_photosynth.gammastar - Rd/g;
 double c = -J*par_photosynth.gammastar/(4*g) - 2*par_photosynth.ca*par_photosynth.gammastar - 2*Rd*par_photosynth.gammastar/g;
 
 double out1 = QUADP(a, b, c);
 double out2 = QUADM(a, b, c);
 
 if (out1 < out2) {
 return out1;
 } else {
 return out2;
 }
 }
 
 inline double calc_J_nitrogen(double gs, double n, ParPhotosynthNitrogen par_photosynth){
 double g = par_photosynth.gammastar / par_photosynth.ca;
 double k = par_photosynth.kmm / par_photosynth.ca;
 double ca = par_photosynth.ca / par_photosynth.patm*1e6;
 double d = par_photosynth.delta;
 double x = calculate_ci(n) / par_photosynth.ca;
 return 4*gs*ca*(1-x)*(x+2*g)/(x*(1-d)-(g+d*k));
 }

inline double calc_J_from_jmax_nitrogen(double n_leaf, ParPhotosynthNitrogen par_photosynth){
  double p = 4 * par_photosynth.phi0 * par_photosynth.Iabs;
  double pj = p/(par_photosynth.a_jmax * n_leaf); // TODO: check the formulas here!
  return par_photosynth.a_jmax*n_leaf*p / sqrt(pj*pj - 1);
}

inline double calc_jmax_from_J_nitrogen(double J, ParPhotosynthNitrogen par_photosynth){
  double p = 4 * par_photosynth.phi0 * par_photosynth.Iabs;
  double pj = p/J; // TODO: there's no nitrogen here!
  return p / sqrt(pj*pj + 1);
}
 */

