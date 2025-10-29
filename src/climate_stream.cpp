#include "climate_stream.h"

namespace pfate{
namespace env{

void ClimateStream::init(const std::string& time_unit){
  // Determine axis name based on time unit
  std::string axis_name;
  if (time_unit.find("month") != std::string::npos || time_unit.find("Month") != std::string::npos) {
    axis_name = "Decimal_year";
  } else {
    axis_name = "Year";
  }
  
  // Initialize CO2 stream
  if (update_co2){
    co2_stream.set_tname(axis_name);
    co2_stream.periodic = false;
    co2_stream.centered_t = false;
    co2_stream.open({co2File}, time_unit);  // Use dynamic unit
  }
  
  // Initialize incoming meteorology stream
  if (update_i_met){
    i_met_stream.set_tname(axis_name);
    i_met_stream.periodic = true;
    i_met_stream.centered_t = false;
    i_met_stream.open({i_metFile}, time_unit);  // Use dynamic unit
  }
  
  // Initialize above-canopy meteorology stream
  if (update_a_met){
    a_met_stream.set_tname(axis_name);
    a_met_stream.periodic = true;
    a_met_stream.centered_t = false;
    a_met_stream.open({a_metFile}, time_unit);  // Use dynamic unit
  }
}



void ClimateStream::updateClimate(double julian_time, Climate& C){
	if (update_co2){
		co2_stream.advance_to_time(julian_time);
		// std::cout << co2_stream.current_row << std::endl;
		C.clim_inst.co2   = as<double>(co2_stream.current_row[1]); // co2 is in index 1
		C.clim_acclim.co2 = as<double>(co2_stream.current_row[1]); // co2 is in index 1
	}
	if (update_i_met){
	  i_met_stream.advance_to_time(julian_time);
		// std::cout << i_met_stream.current_row << std::endl;
		C.clim_inst.decimal_year = as<double>(i_met_stream.current_row[2]);
		C.clim_inst.tc   = as<double>(i_met_stream.current_row[3]);
		C.clim_inst.vpd  = as<double>(i_met_stream.current_row[4]) * 100; // convert hPa to Pa
		C.clim_inst.ppfd = as<double>(i_met_stream.current_row[5]);      // ppfd
		C.clim_inst.swp  = as<double>(i_met_stream.current_row[7]) * (-1); // convert -MPa to MPa
	}
	if (update_a_met){
		a_met_stream.advance_to_time(julian_time);
		// std::cout << a_met_stream.current_row << std::endl;
		Clim cnew = C.clim_acclim;
		cnew.tc   = as<double>(a_met_stream.current_row[3]);
		cnew.vpd  = as<double>(a_met_stream.current_row[4]) * 100; // convert hPa to Pa
		cnew.ppfd = as<double>(a_met_stream.current_row[6]);      // max ppfd
		cnew.swp  = as<double>(a_met_stream.current_row[7]) * (-1); // convert -MPa to MPa
		C.set_forcing_acclim(julian_time, cnew);
	}
	else {
		// TODO. Replace with a proper function that computes acclim forcing from inst
		Clim cnew = C.clim_acclim;
		cnew = C.clim_inst;
		cnew.ppfd = C.clim_inst.ppfd * 4;
		C.set_forcing_acclim(julian_time, cnew);
	}

}

} // namespace env
} // namespace pfate
