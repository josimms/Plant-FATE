###
# Life History
###

# TODO: calibration files with different infrastructure parameter

# TODO: plot growth of one plant with different levels of belowground infrastructure
  # TODO: how should this relate to the root parameters?

###
# 
###

amazon_sim <- run_simulation("tests/params/p_test_v2.ini", 1000, 1050, "test_3spp_100yr")
amazon_sim_data <- read_sim_data(amazon_sim$dir_path)

plot(amazon_sim_data$D$YEAR, amazon_sim_data$D$GPP)

# TODO: calibration files updated with the infrastructure parameter

# TODO: plot the amazon with different levels of underground investment
  # TODO: how should this relate to the root parameters?




