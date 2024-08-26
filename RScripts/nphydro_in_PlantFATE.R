###
# Life History
###

lho <- create_lho("tests/params/p_test_boreal.ini", 
                  "tests/data/ERAS_Monthly.csv", 
                  "tests/data/ERAS_Monthly.csv", 
                  init_co2 = 365)

df <- run_for_dataset(lho, 1960, 1995, dt)

# TODO: vary the infrastructure parameter modifier

plot(df$date, df$height)
plot(df$date, df$root_mass, type = "l")
lines(df$date, df$root_mass, col = "blue")



###
# 
###

amazon_sim <- run_simulation("tests/params/p_test_v2.ini", 1000, 1050, "test_3spp_100yr")
amazon_sim_data <- read_sim_data(amazon_sim$dir_path)

plot(amazon_sim_data$D$YEAR, amazon_sim_data$D$GPP)

# TODO: calibration files updated with the infrastructure parameter

# TODO: plot the amazon with different levels of underground investment
  # TODO: how should this relate to the root parameters?




