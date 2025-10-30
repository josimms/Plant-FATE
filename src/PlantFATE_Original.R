####
# Install the github version
####

devtools::install_github("jaideep777/Plant-FATE@develop", force = TRUE)
library(PlantFATE)

####
# Run the original model
####

weather_file <- read.delim("tests/data/ERAS_Monthly.csv", sep = ",")

lho <- new(LifeHistoryOptimizer, "tests/params/p_test_boreal_original_model.ini")
lho$set_i_metFile("tests/data/ERAS_Monthly.csv")
lho$set_a_metFile("tests/data/ERAS_Monthly.csv")
lho$set_co2File("")
lho$init()

dt <- 1/12

df <- data.frame(matrix(ncol = length(lho$get_header()), nrow = 0))
col_names <- lho$get_header()

start_year <- 1960
end_year <- 2022

results <- vector("list", length(seq(start_year, end_year, dt)))
i <- 1

# end_year
for (t in seq(start_year, end_year, dt)) {
  results[[i]] <- tryCatch({
    lho$grow_for_dt(t, dt)
    state <- lho$get_state(t + dt)
    df_row <- as.data.frame(t(state), stringsAsFactors = FALSE)
    names(df_row) <- col_names   # force the names here
    df_row
  },
  error = function(e) {
    message("Error at t = ", t, ": ", conditionMessage(e))
    na_row <- as.data.frame(as.list(rep(NA, length(col_names))), stringsAsFactors = FALSE)
    names(na_row) <- col_names
    na_row
  })
  i <- i + 1
}

df <- do.call(rbind, results)
names(df) <- col_names

df$date <- seq(
  as.Date(paste0(start_year, "-01-01")),
  as.Date(paste0(end_year, "-01-01")),
  by = "month"
)[1:nrow(df)]


###
# Plots -- just checking ? for now
###

par(mfrow = c(2, 2))
plot(as.Date(df$date), df$assim_gross)
plot(as.Date(df$date), df$assim_net)
plot(as.Date(df$date), df$diameter)
plot(as.Date(df$date), df$root_mass)


