blank <- function() {
  
  lho <- new(LifeHistoryOptimizer, "tests/params/p_test_v3.ini")
  lho$set_i_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  lho$set_a_metFile("tests/data/MetData_AmzFACE_Monthly_2000_2015_PlantFATE_new.csv")
  lho$set_co2File("")
  lho$set_soil_nitrogen(0.5)
  lho$init()
  
  dt <- 1/12
  
  df <- data.frame(matrix(ncol = length(lho$get_header()), nrow = 0))
  colnames(df) <- lho$get_header()
  
  start_year = 2000
  end_year = 2015
  for (t in seq(start_year, end_year, dt)) {
    lho$grow_for_dt(t, dt)
    df[nrow(df) + 1, ] <- lho$get_state(t + dt)
  }
  df$date <- seq(as.Date(paste0(start_year, "-01-01")), 
                 as.Date(paste0(end_year, "-01-01")),
                 by = "month")
  
}


