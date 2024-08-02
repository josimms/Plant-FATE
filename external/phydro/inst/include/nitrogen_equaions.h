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

class ParCostNitrogen {
public:
  double alpha;
  double gamma;
  double carbon_allocation;
  
  inline ParCostNitrogen(double _a, double _g, double _c){
    alpha = _a;
    gamma = _g;
    carbon_allocation = _c;
  }
};

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
    fT_jmax  = calc_ftemp_inst_jmax(_tc, _tcgrowth, _tchome, 25, ftemp_vj_method);
    fT_rd    = calc_ftemp_inst_rd(_tc, _ftemp_rd_method);
    
    a_jmax = _a_jmax;

    kmm = calc_kmm(_tc, _patm);
    gammastar = calc_gammastar(_tc, _patm);
    
    phi0 = _kphio*calc_ftemp_kphio(_tc);
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

struct PHydroResultNitrogen{
  double a;
  double e;
  double gs;
  double ci;
  double chi;
  double n_leaf;
  double vcmax;
  double jmax;
  double dpsi;
  double psi_l;
  double nfnct;
  double niter;
  double mc;
  double mj;
  double gammastar;
  double kmm;
  double vcmax25;
  double jmax25;
  double rd;
  bool   isVcmaxLimited;
  double ac;
  double aj;
  double le;
  double le_s_wet;
};


inline ACi calc_assim_light_limited_nitrogen(double _gs, double n_leaf, ParPhotosynthNitrogen par_photosynth){
  double ca = par_photosynth.ca;             // ca is in Pa
  double gs = _gs * 1e6/par_photosynth.patm;  // convert to umol/m2/s/Pa
  
  double d = par_photosynth.delta;
  
  double phi0iabs = par_photosynth.phi0 * par_photosynth.Iabs;
  double jmax = n_leaf * par_photosynth.a_jmax;
  double jj = 4*phi0iabs/jmax;
  double jlim = phi0iabs / sqrt(1 + jj*jj);
  
  double A = -1.0 * gs;
  // TODO: quadratic formula here!
  double B = gs * ca - gs * 2 * par_photosynth.gammastar - jlim*(1-d);
  double C = gs * ca * 2 * par_photosynth.gammastar + jlim * (par_photosynth.gammastar + d*par_photosynth.kmm);
  
  ACi res;
  res.ci = QUADM(A,B,C);
  res.a  = gs*(ca-res.ci);
  res.isVcmaxLimited = false;
  
  return res;
  
}

// Here the class is defined where the optmaisation will happen
class PHydro_Profit_Nitrogen{
private:
  
  int n = 2;
  
  double psi_soil;
  
  ParCostNitrogen               par_cost;
  ParEnv                        par_env;
  ParPhotosynthNitrogen         par_photosynth;
  ParPlant                      par_plant;
  
public:
  
  inline PHydro_Profit_Nitrogen(double _psi_soil, ParCostNitrogen _par_cost, ParPhotosynthNitrogen _par_photosynth, ParPlant _par_plant, ParEnv _par_env) : 
    psi_soil       ( _psi_soil),
    par_cost       ( _par_cost),
    par_env        ( _par_env),
    par_photosynth ( _par_photosynth),
    par_plant      ( _par_plant) {
  }
  
  inline double value(const VectorXd &x) {
    double n_leaf = exp(x[0]);
    double dpsi = x[1];
    
    double Q = calc_sapflux(dpsi, psi_soil, par_plant, par_env);
    double gs = calc_gs_from_Q(Q, psi_soil, par_plant, par_env);
    auto   aj = calc_assim_light_limited_nitrogen(gs, n_leaf, par_photosynth);  // Aj in umol/m2/s
    
    double jmax = n_leaf * par_photosynth.a_jmax;
    // NOTE: I know that I could just update alpha, but this makes it closer to the equations!
    double costs = par_cost.alpha / par_cost.carbon_allocation * jmax + par_cost.gamma * dpsi * dpsi;
    
    double profit = aj.a - costs;
    
    return -profit;
  }
  
  inline double operator()(const VectorXd& x, VectorXd& grad){
    double f = value(x);
    
    for (int i=0; i<n; ++i){
      VectorXd dx = VectorXd::Zero(n);
      double delta = 2.2204e-6;
      dx[i] = delta;
      double fplus = value(x+dx);
      double fminus = value(x-dx);
      grad[i] = (fplus-fminus)/delta/2;
    }
    
    return f;
  }
};

struct jmaxDpsi{
  double jmax;
  double dpsi;
};

inline jmaxDpsi optimize_midterm_multi_nitrogen(double psi_soil, double nitrogen_store, ParCostNitrogen _par_cost, ParPhotosynthNitrogen _par_photosynth, ParPlant _par_plant, ParEnv _par_env){
  
  const int q = 2; // dimensions of the vector for the foptimisation
  // Set up parameters
  LBFGSpp::LBFGSBParam<double> param;
  param.epsilon = 1e-6;
  param.max_iterations = 100;
  
  // Create solver and function object
  LBFGSpp::LBFGSBSolver<double> solver(param);
  PHydro_Profit_Nitrogen profit_fun_nitrogen(psi_soil, _par_cost, _par_photosynth, _par_plant, _par_env);
  
  // bounds
  VectorXd lb(q), ub(q);
  lb << -10, 0;
  ub << log(nitrogen_store), 50;
  
  // Initial guess
  VectorXd x(q);
  x << log(100), 1; 
  
  // x will be overwritten to be the best point found
  double fx;
  int niter = solver.minimize(profit_fun_nitrogen, x, fx, lb, ub);
  
  jmaxDpsi res;
  res.jmax = _par_photosynth.a_jmax * exp(x[0]);
  res.dpsi = x[1];
  
  return res;
}

inline double vcmax_coordinated_numerical_nitrogen(double aj, double ci, ParPhotosynthNitrogen par_photosynth){
  double d = par_photosynth.delta;
  double vcmax_coord = aj*(ci + par_photosynth.kmm)/(ci*(1-d) - (par_photosynth.gammastar+par_photosynth.kmm*d));
  return vcmax_coord;
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
  auto Ac = calc_assim_rubisco_limited_nitrogen(gs, vcmax, par_photosynth);
  auto Aj = calc_assim_light_limited_nitrogen(gs, jmax, par_photosynth);
  
  if (Ac.ci > Aj.ci ) return Ac; 
  else				return Aj;
}

class PHydro_Profit_Inst_Nitrogen{
private:
  
  int n = 1;
  
  double psi_soil, vcmax, jmax;
  
  ParCostNitrogen       par_cost;
  ParEnv                par_env;
  ParPhotosynthNitrogen par_photosynth;
  ParPlant              par_plant;
  
public:
  
  inline PHydro_Profit_Inst_Nitrogen(double _vcmax, double _jmax, double _psi_soil, ParCostNitrogen _par_cost, ParPhotosynthNitrogen _par_photosynth, ParPlant _par_plant, ParEnv _par_env) : 
    psi_soil       ( _psi_soil),
    vcmax          ( _vcmax),
    jmax           ( _jmax),
    par_cost       ( _par_cost),
    par_env        ( _par_env),
    par_photosynth ( _par_photosynth),
    par_plant      ( _par_plant) {
  }
  
  inline double value(const VectorXd &x) {
    double dpsi = x[0];
    
    double Q = calc_sapflux(dpsi, psi_soil, par_plant, par_env);
    double gs = calc_gs_from_Q(Q, psi_soil, par_plant, par_env);
    auto   A = calc_assimilation_limiting_nitrogen(vcmax, jmax, gs, par_photosynth);  // min(Ac, Aj) in umol/m2/s
    
    double costs =  par_cost.gamma * dpsi*dpsi;
    
    double profit = A.a - costs;
    
    //std::cout << "dpsi = " << dpsi << ", profit = " << profit << std::endl;
    return -profit;
  }
  
  inline double operator()(const VectorXd& x, VectorXd& grad){
    double f = value(x);
    
    for (int i=0; i<n; ++i){
      VectorXd dx = VectorXd::Zero(n);
      double delta = 2.2204e-6;
      dx[i] = delta;
      double fplus = value(x+dx);
      double fminus = value(x-dx);
      grad[i] = (fplus-fminus)/delta/2;
    }
    
    return f;
  }
};


inline double optimize_shortterm_multi_nitrogen(double vcmax, double jmax, double psi_soil, ParCostNitrogen _par_cost, ParPhotosynthNitrogen _par_photosynth, ParPlant _par_plant, ParEnv _par_env){
  
  const int n = 1;
  // Set up parameters
  LBFGSpp::LBFGSParam<double> param;
  param.epsilon = 1e-4;
  param.epsilon_rel = 1e-4;
  param.past = 1;
  param.delta = 5e-5;
  param.max_iterations = 100;
  
  // Create solver and function object
  LBFGSpp::LBFGSSolver<double> solver(param);
  PHydro_Profit_Inst_Nitrogen profit_fun_nitrogen(vcmax, jmax, psi_soil, _par_cost, _par_photosynth, _par_plant, _par_env);
  
  // bounds
  VectorXd lb(n), ub(n);
  lb << 0.0001;
  ub << 20;
  
  // Initial guess
  VectorXd x(n);
  x << 1.0; 
  
  // x will be overwritten to be the best point found
  double fx;
  int niter = solver.minimize(profit_fun_nitrogen, x, fx); //, lb, ub);
  
  double res = x[0];
  
  return res;
  
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
 
 inline double calc_dJ_dn(double gs, double n, ParPhotosynthNitrogen par_photosynth){ // TODO: redo this formula
 double x = calculate_ci(N) / par_photosynth.ca;
 return 4*gs*ca * ((d*(2*g*(k + 1) + k*(2*x - 1) + x*x) - ((x-g)*(x-g)+3*g*(1-g)))/(D*D));
 // gs*ca*(3*(g-1)*g/(g-x)^2 - 1)
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

