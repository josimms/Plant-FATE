# Builds the repeated-climate CSV for the delayed-perturbation sensitivity
# analysis (notes_delayed_perturbation_sensitivity.md, Step 3). A single
# representative year (1974 -- the 15th year of the 1960-2022 record,
# treating 1960 as year 1) is repeated for the whole simulated window,
# spin-up through response, with strictly ascending fake Decimal_year values
# so ErgodicEnvironment::updateBackgroundCanopy()'s elapsed-time-since-bg_t0
# background-canopy-closure calculation still advances correctly even
# though the weather itself never changes. Every tiled block is byte-for-byte
# identical, so periodic wraparound at the end of the file (ClimateStream is
# periodic -- see notes finding #5) is seamless if a run slightly overruns.
#
# Mirrors the column format of the existing data/ERAS_Monthly_constant1960.csv
# (built for a different, trajectory-based constclimate analysis -- see
# vignettes/run_phase2_sensitivity_root_no0_constclimate.R), but repeats 1974
# instead of 1960, and covers 75 years (spin-up 50y + response 20y + a 5y
# buffer) instead of 21.

suppressMessages(library(tidyverse))

in_file  <- "/home/josimms/Documents/Austria/Plant-FATE/data/ERAS_Monthly.csv"
out_file <- "/home/josimms/Documents/Austria/Plant-FATE/data/ERAS_Monthly_constant1974.csv"

REPEAT_YEAR <- 1974   # 15th year of the 1960-2022 record (1960 = year 1)
START_YEAR  <- 1960   # matches bg_canopy_t0 / existing scripts' start_year convention
N_YEARS     <- 90     # 50 (spin-up) + 30 (response) + 10 buffer

clim <- read_csv(in_file, show_col_types = FALSE)
template <- clim %>% filter(Year == REPEAT_YEAR) %>% arrange(Month)
stopifnot(nrow(template) == 12)

out <- map_dfr(0:(N_YEARS - 1), function(i) {
  yr <- START_YEAR + i
  template %>% mutate(
    Year = yr,
    Decimal_year = yr + (Month - 1) / 12,
    GPP = NA
  )
})

write_csv(out, out_file)
cat("Wrote", nrow(out), "rows (", N_YEARS, "years of repeated", REPEAT_YEAR,
    "weather) to", out_file, "\n")
