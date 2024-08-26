#include "life_history.h"
#include <io_utils.h>
using namespace std;

int parameter_test_lh(double lower_bound, double upper_bound, std::string params_file) {
  // Reading in the parameters
  LifeHistoryOptimizer::LifeHistoryOptimizer(params_file)
      // what is the output of this? How do I access the structures that are created in the function?
  
  // Parameter range
  PlantParameters parm:
  for (int i = lower_bound: i++; i < upper_bound) {
    // Change the parameter value
    parm.infra_translation = i;
    
    // Run the model for each parameter
    try {
      // TODO: How do you run the lh model?
      double result = runModel(parm);
      
    } catch (const std::exception& e) {
      // TODO: make an output file for this?
      cout << "Error with parameter " << parm << ": " << e.what() << endl;
      
    }
  }
  
  // Result should be 
  return 0;
}