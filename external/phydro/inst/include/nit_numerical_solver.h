#ifndef PHYDRO_NIT_NUMERICAL_SOLVER_H
#define PHYDRO_NIT_NUMERICAL_SOLVER_H

#ifdef USINGRCPP
#include <RcppEigen.h>
#else
#include <Eigen/Core>
#endif

#include <LBFGSB.h>

#include "hyd_transpiration.h"
#include "nit_photosynthesis.h"

using Eigen::VectorXd;

namespace phydro{

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
    ub << log(100), 50;
    
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

} // end namespace

#endif