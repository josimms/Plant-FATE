#ifndef PHYDRO_NITROGEN_H
#define PHYDRO_NITROGEN_H

#include "hyd_transpiration.h"
#include "hyd_photosynthesis.h"
#include "temperature_dependencies_photosynthesis.h"

#ifdef USINGRCPP
#include <RcppEigen.h>
#else
#include <Eigen/Core>
#endif

#include <LBFGSB.h>
#include <LBFGS.h>

using Eigen::VectorXd;

namespace phydro{

class ParPhotosynthNitrogen{
public:
  double kmm;
  double gammastar;
  double phi0;
  double ca;     // Partial pressure of CO2 [Pa]
  double delta;  // TODO: Replace name with brd / rdark
  
  FtempVcmaxJmaxMethod ftemp_vj_method;
  FtempRdMethod        ftemp_rd_method;
  FtempBrMethod        ftemp_br_method;
  
  double Iabs;  // Net absorbed PAR [umol m-2 s-1]
  double patm;  // Atmospheric pressure [Pa]
  double a_jmax; // a_jmax parameter
  
  double fT_vcmax;
  double fT_jmax;
  double fT_rd;
  
  inline ParPhotosynthNitrogen(double _tc, double _patm, double _kphio, double _co2, double _ppfd, double _nitrogen_store, double _fapar, double _rdark25, double _tcgrowth, double _tchome, double _a_jmax,
                               FtempVcmaxJmaxMethod _ftemp_vj_method = FV_kumarathunge19, 
                               FtempRdMethod        _ftemp_rd_method = FR_heskel16, 
                               FtempBrMethod        _ftemp_br_method = FB_atkin15){
    
    ftemp_vj_method = _ftemp_vj_method;
    ftemp_rd_method = _ftemp_rd_method;
    ftemp_br_method = _ftemp_br_method;
    
    // NOTE: this makes the parameters the 25 degree version
    fT_vcmax = calc_ftemp_inst_vcmax(_tc, _tcgrowth, 25.0, ftemp_vj_method);
    fT_jmax  = calc_ftemp_inst_jmax(_tc, _tcgrowth, _tchome, 25.0, ftemp_vj_method);
    fT_rd    = calc_ftemp_inst_rd(_tc, _ftemp_rd_method);
    
    a_jmax = _a_jmax;

    kmm = calc_kmm(_tc, _patm);
    gammastar = calc_gammastar(_tc, _patm);
    
    phi0 = _kphio * calc_ftemp_kphio(_tc);
    Iabs = _ppfd * _fapar;
    ca = _co2 * _patm * 1e-6;
    patm = _patm;
    delta = _rdark25 * fT_rd / fT_vcmax;
  }
  
  inline void print(){
    std::cout << "Par Photosynth:\n";
    std::cout << "   fT_vcmax = " << fT_vcmax << '\n';
    std::cout << "   fT_jmax = " << fT_jmax << '\n';
    std::cout << "   fT_rd = " << fT_rd << '\n';
    std::cout << "   kmm = " << kmm << '\n';
    std::cout << "   gammastar = " << gammastar << '\n';
    std::cout << "   phi0 = " << phi0 << '\n';
    std::cout << "   Iabs = " << Iabs << '\n';
    std::cout << "   ca = " << ca << '\n';
    std::cout << "   patm = " << patm << '\n';
    std::cout << "   delta = " << delta << '\n';
    std::cout << "   ftemp_vj_method = " << ftemp_vj_method << '\n';
    std::cout << "   ftemp_rd_method = " << ftemp_rd_method << '\n';
    std::cout << "   ftemp_br_method = " << ftemp_br_method << '\n';
    std::cout << "   a_jmax = " << a_jmax << '\n';
  }
};

inline ACi calc_assim_light_limited_nitrogen(double _gs, double n_leaf, ParPhotosynthNitrogen par_photosynth){
  double ca = par_photosynth.ca;             // ca is in Pa
  double gs = _gs * 1e6/par_photosynth.patm;  // convert to umol/m2/s/Pa
  
  double d = par_photosynth.delta;
  
  double phi0iabs = par_photosynth.phi0 * par_photosynth.Iabs;
  double jmax = n_leaf * par_photosynth.a_jmax;
  double jj = 4 * phi0iabs / jmax;
  double jlim = phi0iabs / sqrt(1 + jj*jj);
  
  double A = -1.0 * gs;
  // TODO: quadratic formula here!
  double B = gs * ca - gs * 2 * par_photosynth.gammastar - jlim * (1-d);
  double C = gs * ca * 2 * par_photosynth.gammastar + jlim * (par_photosynth.gammastar + d*par_photosynth.kmm);
  
  ACi res;
  res.ci = QUADM(A,B,C);
  res.a  = gs*(ca-res.ci);
  res.isVcmaxLimited = false;
  
  return res;
  
}

inline ACi calc_assim_light_limited_nitrogen_jmax(double _gs, double jmax, ParPhotosynthNitrogen par_photosynth){
  double ca = par_photosynth.ca;             // ca is in Pa
  double gs = _gs * 1e6/par_photosynth.patm;  // convert to umol/m2/s/Pa
  
  double d = par_photosynth.delta;
  
  double phi0iabs = par_photosynth.phi0 * par_photosynth.Iabs;
  double jj = 4 * phi0iabs / jmax;
  double jlim = phi0iabs / sqrt(1 + jj*jj);
  
  double A = -1.0 * gs;
  // TODO: quadratic formula here!
  double B = gs * ca - gs * 2 * par_photosynth.gammastar - jlim * (1-d);
  double C = gs * ca * 2 * par_photosynth.gammastar + jlim * (par_photosynth.gammastar + d*par_photosynth.kmm);
  
  ACi res;
  res.ci = QUADM(A,B,C);
  res.a  = gs*(ca-res.ci);
  res.isVcmaxLimited = false;
  
  return res;
  
}


inline ACi calc_assim_rubisco_limited_nitrogen(double _gs, double vcmax, ParPhotosynthNitrogen par_photosynth){
  double ca = par_photosynth.ca;            // ca is in Pa
  double gs = _gs * 1e6/par_photosynth.patm; // convert to umol/m2/s/Pa
  
  double d = par_photosynth.delta;
  
  double A = -1.0 * gs;
  double B = gs * ca - gs * par_photosynth.kmm - vcmax*(1-d);
  double C = gs * ca * par_photosynth.kmm + vcmax * (par_photosynth.gammastar + par_photosynth.kmm*d);
  
  ACi res;
  res.ci = QUADM(A,B,C);
  res.a  = gs*(ca-res.ci);
  res.isVcmaxLimited = true;
  
  return res;
  
}

inline ACi calc_assimilation_limiting_nitrogen(double vcmax, double jmax, double gs, ParPhotosynthNitrogen par_photosynth){
  
  auto Aj = calc_assim_light_limited_nitrogen_jmax(gs, jmax, par_photosynth);
  auto Ac = calc_assim_rubisco_limited_nitrogen(gs, vcmax, par_photosynth);
  
  if (Ac.ci > Aj.ci ) return Ac; 
  else return Aj;
}

} // phydro

#endif

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

