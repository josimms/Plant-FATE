sensitivity_nitrogen <- function(
    ini_file   = "tests/params/p_test_boreal.ini",
    met_file   = "tests/data/ERAS_Monthly.csv",
    start_year = 1960,
    end_year   = 2022,
    n_sweep    = 11,   # values per parameter (odd → includes baseline)
    n_cores    = max(1L, parallel::detectCores() - 1L)
) {
  library(PlantFATE)
  library(parallel)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)

  dir.create("RScripts/plots", showWarnings = FALSE, recursive = TRUE)

  dt        <- 1 / 12
  years_seq <- seq(start_year, end_year - dt, dt)

  # ---------------------------------------------------------------
  # Read the baseline N_s from the ini file before run_lho is called.
  # N_s must always be set explicitly: the ini-file value is overridden each
  # timestep by C.clim_acclim.nitrogen, which defaults to 0 unless set here.
  # ---------------------------------------------------------------
  {
    lho_tmp2 <- new(LifeHistoryOptimizer, ini_file)
    lho_tmp2$set_i_metFile(met_file)
    lho_tmp2$set_a_metFile(met_file)
    lho_tmp2$set_co2File("")
    N_s_base <- lho_tmp2$uptake0$N_s   # read from ini
    rm(lho_tmp2)
  }

  # ---------------------------------------------------------------
  # Helper: create, configure, and run one LifeHistoryOptimizer,
  # returning a data.frame of all timestep outputs.
  # param_fn(lho) is called after construction but before init()
  # to set the varied parameter.
  # ---------------------------------------------------------------
  run_lho <- function(param_fn, N_s = NULL) {
    lho <- new(LifeHistoryOptimizer, ini_file)
    lho$set_i_metFile(met_file)
    lho$set_a_metFile(met_file)
    lho$set_co2File("")
    lho$set_soil_nitrogen(if (!is.null(N_s)) N_s else N_s_base)
    param_fn(lho)      # modify uptake0 / traits0 / par0 fields
    lho$init()

    col_names <- lho$get_header()
    results   <- vector("list", length(years_seq))

    for (i in seq_along(years_seq)) {
      t <- years_seq[i]
      results[[i]] <- tryCatch({
        lho$grow_for_dt(t, dt)
        row        <- as.data.frame(t(lho$get_state(t + dt)))
        names(row) <- col_names
        row
      }, error = function(e) {
        setNames(as.data.frame(as.list(rep(NA_real_, length(col_names)))),
                 col_names)
      })
    }

    df        <- do.call(rbind, results)
    df$year   <- years_seq + dt
    df
  }

  # ---------------------------------------------------------------
  # Baseline run
  # ---------------------------------------------------------------
  message("Running baseline...")
  df_base <- run_lho(function(lho) invisible(NULL))

  # Scalar summary: mean of last 20 years (mature tree, stable N uptake)
  summarise_run <- function(df) {
    # Use last 20 years of simulation; fall back to all rows if shorter
    n_tail <- min(nrow(df), 20 * 12)
    tail20 <- tail(df, n_tail)
    list(
      # Total N arriving at the tree each timestep
      N_uptake   = mean(tail20$root_uptake + tail20$mycorrhizal_export_to_tree,
                        na.rm = TRUE),
      root_up    = mean(tail20$root_uptake,               na.rm = TRUE),
      myco_up    = mean(tail20$myco_uptake,               na.rm = TRUE),
      myco_exp   = mean(tail20$mycorrhizal_export_to_tree,na.rm = TRUE),
      N_bar      = mean(tail20$N_bar,                     na.rm = TRUE),
      alpha      = mean(tail20$alpha,                     na.rm = TRUE),
      ecm_mass   = mean(tail20$ectomycorrhiza_mass,       na.rm = TRUE),
      root_mass  = mean(tail20$root_mass,                 na.rm = TRUE),
      height     = mean(tail20$height,                    na.rm = TRUE),
      assim_net  = mean(tail20$assim_net,                 na.rm = TRUE)
    )
  }

  base_sum <- summarise_run(df_base)
  message("Baseline N_uptake = ", round(base_sum$N_uptake, 5), " kg N yr-1")

  # ---------------------------------------------------------------
  # Parameter definitions
  # Each row: param name (for label), setter fn, base value, sweep range
  # ---------------------------------------------------------------

  # Read base values from a freshly-constructed lho (before init)
  lho_tmp <- new(LifeHistoryOptimizer, ini_file)
  lho_tmp$set_i_metFile(met_file)
  lho_tmp$set_a_metFile(met_file)
  lho_tmp$set_co2File("")

  base_vals <- list(
    u_max               = lho_tmp$uptake0$u_max,
    D                   = lho_tmp$uptake0$D,
    N_s                 = 0.405,           # ini value; changed via set_soil_nitrogen
    k_15                = lho_tmp$uptake0$k_15,
    k_13                = lho_tmp$uptake0$k_13,
    investment_from_tree= lho_tmp$traits0$investment_from_tree,
    mycorrhized         = lho_tmp$uptake0$mycorrhized,
    alpha_ib            = lho_tmp$par0$alpha_ib,
    k_0                 = lho_tmp$traits0$k_0,
    k_1                 = lho_tmp$traits0$k_1,
    k_8                 = lho_tmp$uptake0$k_8,
    nc_myco             = lho_tmp$traits0$nc_myco,
    mycorrhizal_turnover= lho_tmp$traits0$mycorrhizal_turnover,
    k_mob_N             = lho_tmp$traits0$k_mob_N
  )
  rm(lho_tmp)

  # (param, label, tier, lo_mult, hi_mult, setter)
  param_meta <- list(
    # Tier 1 ─ supply control
    list(p="u_max",  lab="u[max]",    tier=1, lo=0.1, hi=5,
         set=function(lho, v) { lho$uptake0$u_max <- v }),
    list(p="D",      lab="D",         tier=1, lo=0.01,hi=10,
         set=function(lho, v) { lho$uptake0$D <- v }),
    list(p="N_s",    lab="N[s]",      tier=1, lo=0.1, hi=5,
         set=function(lho, v) NULL,           # N_s set via N_s arg in run_lho
         N_s_val=TRUE),
    list(p="k_15",   lab="k[15]",     tier=1, lo=0.05,hi=20,
         set=function(lho, v) { lho$uptake0$k_15 <- v }),
    # Tier 2 ─ trade-off structure
    list(p="k_13",   lab="k[13]",     tier=2, lo=0.2, hi=4,
         set=function(lho, v) { lho$uptake0$k_13 <- v }),
    list(p="investment_from_tree", lab="phi[m]", tier=2, lo=0.1, hi=5,
         set=function(lho, v) { lho$traits0$investment_from_tree <- v }),
    list(p="mycorrhized", lab="m",    tier=2, lo=0.2, hi=2,   # clipped to [0,1]
         set=function(lho, v) { lho$uptake0$mycorrhized <- min(v, 0.99) }),
    list(p="alpha_ib", lab="tilde(alpha)", tier=2, lo=0.1, hi=100,
         set=function(lho, v) { lho$par0$alpha_ib <- v }),
    # Tier 3 ─ root architecture
    list(p="k_0",    lab="k[0]",      tier=3, lo=0.4, hi=2.5,
         set=function(lho, v) { lho$traits0$k_0 <- v }),
    list(p="k_1",    lab="k[1]",      tier=3, lo=0.4, hi=2.5,
         set=function(lho, v) { lho$traits0$k_1 <- v }),
    list(p="k_8",    lab="k[8]",      tier=3, lo=0.2, hi=3,
         set=function(lho, v) { lho$uptake0$k_8 <- v }),
    list(p="nc_myco",lab="lambda[m]", tier=3, lo=0.1, hi=10,
         set=function(lho, v) { lho$traits0$nc_myco <- v }),
    # Tier 4 ─ seasonal / temporal   (E_a is constexpr, cannot be varied)
    list(p="mycorrhizal_turnover", lab="T[m]", tier=4, lo=0.1, hi=5,
         set=function(lho, v) { lho$traits0$mycorrhizal_turnover <- v }),
    list(p="k_mob_N", lab="k[mob]",  tier=4, lo=0.01,hi=2,
         set=function(lho, v) { lho$traits0$k_mob_N <- v })
  )

  # ---------------------------------------------------------------
  # OAT sweep — build flat list of all (param, value) jobs
  # ---------------------------------------------------------------
  jobs <- do.call(c, lapply(param_meta, function(pm) {
    bv   <- base_vals[[pm$p]]
    vals <- exp(seq(log(bv * pm$lo), log(bv * pm$hi), length.out = n_sweep))
    lapply(vals, function(v) list(pm = pm, bv = bv, v = v))
  }))

  message("Running ", length(jobs), " sweep jobs across ", n_cores, " cores...")

  run_job <- function(job) {
    pm           <- job$pm
    v            <- job$v
    bv           <- job$bv
    N_s_override <- if (isTRUE(pm$N_s_val)) v else NULL
    df_run <- tryCatch(
      run_lho(function(lho) pm$set(lho, v), N_s = N_s_override),
      error = function(e) {
        message("  Failed at ", pm$p, "=", v, ": ", e$message)
        NULL
      }
    )
    if (is.null(df_run)) return(NULL)
    s <- summarise_run(df_run)
    data.frame(
      param     = pm$p,
      label     = pm$lab,
      tier      = pm$tier,
      value     = v,
      value_rel = v / bv,
      N_uptake  = s$N_uptake,
      root_up   = s$root_up,
      myco_up   = s$myco_up,
      myco_exp  = s$myco_exp,
      N_bar     = s$N_bar,
      alpha     = s$alpha,
      ecm_mass  = s$ecm_mass,
      height    = s$height,
      assim_net = s$assim_net
    )
  }

  results_list <- if (n_cores > 1) {
    mclapply(jobs, run_job, mc.cores = n_cores, mc.preschedule = FALSE)
  } else {
    lapply(jobs, run_job)
  }

  df_sweep <- bind_rows(Filter(Negate(is.null), results_list))

  # ---------------------------------------------------------------
  # Normalise to baseline
  # ---------------------------------------------------------------
  df_sweep <- df_sweep |>
    mutate(
      N_uptake_rel = N_uptake / base_sum$N_uptake,
      root_rel     = root_up  / pmax(base_sum$root_up, 1e-30),
      myco_rel     = myco_up  / pmax(base_sum$myco_up, 1e-30),
      height_rel   = height   / base_sum$height
    )

  # ---------------------------------------------------------------
  # Tornado: ±50 % (closest value to 0.5x and 2x baseline)
  # ---------------------------------------------------------------
  tornado_df <- df_sweep |>
    group_by(param, label, tier) |>
    summarise(
      sens_lo = {
        idx <- which.min(abs(value_rel - 0.5))
        (N_uptake[idx] - base_sum$N_uptake) / base_sum$N_uptake * 100
      },
      sens_hi = {
        idx <- which.min(abs(value_rel - 2.0))
        (N_uptake[idx] - base_sum$N_uptake) / base_sum$N_uptake * 100
      },
      .groups = "drop"
    ) |>
    arrange(abs(sens_hi - sens_lo)) |>
    mutate(
      label  = factor(label, levels = unique(label)),
      tier_f = paste0("Tier ", tier)
    )

  tier_cols <- c("Tier 1" = "#0072B2", "Tier 2" = "#D55E00",
                 "Tier 3" = "#009E73", "Tier 4" = "#CC79A7")

  p_tornado <- ggplot(tornado_df) +
    geom_segment(aes(x = sens_lo, xend = sens_hi,
                     y = label,   yend = label,
                     colour = tier_f),
                 linewidth = 6, lineend = "round", alpha = 0.85) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30") +
    scale_colour_manual(values = tier_cols, name = NULL) +
    scale_y_discrete(labels = function(x) parse(text = x)) +
    labs(
      title = "Sensitivity of N uptake to ±50 % parameter perturbation",
      subtitle = "Full C++ LifeHistoryOptimizer — mean last-20-yr annual uptake",
      x = expression(Delta*N[uptake]~"(% change from baseline)"),
      y = NULL
    ) +
    theme_bw(base_size = 14) +
    theme(legend.position = "top", panel.grid.minor = element_blank())

  ggsave("RScripts/plots/SA_01_tornado.png", p_tornado, width = 10, height = 7)

  # ---------------------------------------------------------------
  # Tier line plots
  # ---------------------------------------------------------------
  tier_titles <- c(
    "1" = "Tier 1: Supply control",
    "2" = "Tier 2: Trade-off structure",
    "3" = "Tier 3: Root architecture",
    "4" = "Tier 4: Seasonal / temporal"
  )

  make_tier_plot <- function(t) {
    d   <- df_sweep |> filter(tier == t)
    pms <- unique(d$label)
    ggplot(d, aes(value_rel, N_uptake_rel, colour = label, group = label)) +
      geom_line(linewidth = 1.1) +
      geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
      geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
      scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10)) +
      scale_colour_discrete(name = NULL,
                            labels = function(x) parse(text = x)) +
      labs(x = "Parameter value (relative to baseline)",
           y = expression(N[uptake]~"(relative to baseline)"),
           title = tier_titles[as.character(t)]) +
      theme_bw(base_size = 13) +
      theme(legend.position = "right", panel.grid.minor = element_blank())
  }

  p_tiers <- (make_tier_plot(1) | make_tier_plot(2)) /
             (make_tier_plot(3) | make_tier_plot(4))
  ggsave("RScripts/plots/SA_02_tier_uptake.png", p_tiers, width = 14, height = 10)

  # ---------------------------------------------------------------
  # Sensitivity index table
  # ---------------------------------------------------------------
  sens_table <- tornado_df |>
    transmute(
      tier,
      param,
      S_lo   = sens_lo / -50,
      S_hi   = sens_hi /  50,
      S_mean = (abs(S_lo) + abs(S_hi)) / 2
    ) |>
    arrange(tier, desc(S_mean))

  print(sens_table)

  invisible(list(
    df_sweep   = df_sweep,
    tornado_df = tornado_df,
    sens_table = sens_table,
    base_sum   = base_sum
  ))
}
