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
    n_ok      <- 0L

    for (i in seq_along(years_seq)) {
      t   <- years_seq[i]
      err <- NULL
      row <- tryCatch({
        lho$grow_for_dt(t, dt)
        r        <- as.data.frame(t(lho$get_state(t + dt)))
        names(r) <- col_names
        r
      }, error = function(e) { err <<- e$message; NULL })

      if (is.null(row)) {
        message("  model crashed at year ", round(t, 3),
                " (step ", i, "/", length(years_seq), "): ", err)
        break
      }
      results[[i]] <- row
      n_ok         <- n_ok + 1L
    }

    n_total <- length(years_seq)
    frac    <- round(100 * n_ok / n_total)
    if (n_ok < n_total)
      message("  ", n_ok, "/", n_total, " steps (", frac, "% complete)")

    df      <- do.call(rbind, Filter(Negate(is.null), results))
    df$year <- years_seq[seq_len(n_ok)] + dt
    list(df = df, n_ok = n_ok, n_total = n_total)
  }

  # ---------------------------------------------------------------
  # Baseline run
  # ---------------------------------------------------------------
  message("Running baseline...")
  df_base <- run_lho(function(lho) invisible(NULL))$df

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
      C_to_myco  = mean(tail20$C_export_to_myco,         na.rm = TRUE),
      root_mass  = mean(tail20$root_mass,                 na.rm = TRUE),
      height     = mean(tail20$height,                    na.rm = TRUE),
      assim_net  = mean(tail20$assim_net,                 na.rm = TRUE),
      tree_N     = mean(tail20$tree_nitrogen,             na.rm = TRUE),
      biomass    = mean(tail20$total_mass,
                        na.rm = TRUE)
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
    nitrogen_start0     = lho_tmp$par0$nitrogen_start0,
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
    list(p="nitrogen_start0", lab="N[free0]", tier=1, lo=0.1, hi=10,
         set=function(lho, v) { lho$par0$nitrogen_start0 <- v }),
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
  jobs <- unlist(lapply(param_meta, function(pm) {
    bv   <- base_vals[[pm$p]]
    vals <- exp(seq(log(bv * pm$lo), log(bv * pm$hi), length.out = n_sweep))
    lapply(vals, function(v) list(pm = pm, bv = bv, v = v))
  }), recursive = FALSE)

  message("Running ", length(jobs), " sweep jobs across ", n_cores, " cores...")

  run_job <- function(job) {
    pm           <- job$pm
    v            <- job$v
    bv           <- job$bv
    N_s_override <- if (isTRUE(pm$N_s_val)) v else NULL
    res <- tryCatch(
      run_lho(function(lho) pm$set(lho, v), N_s = N_s_override),
      error = function(e) {
        message("  Failed at ", pm$p, "=", v, ": ", e$message)
        NULL
      }
    )
    if (is.null(res)) return(NULL)
    s <- summarise_run(res$df)
    data.frame(
      param     = pm$p,
      label     = pm$lab,
      tier      = pm$tier,
      value     = v,
      value_rel = v / bv,
      frac_ok   = res$n_ok / res$n_total,
      N_uptake  = s$N_uptake,
      root_up   = s$root_up,
      myco_up   = s$myco_up,
      myco_exp  = s$myco_exp,
      N_bar     = s$N_bar,
      alpha     = s$alpha,
      ecm_mass  = s$ecm_mass,
      C_to_myco = s$C_to_myco,
      root_mass = s$root_mass,
      height    = s$height,
      assim_net = s$assim_net,
      tree_N    = s$tree_N,
      biomass   = s$biomass
    )
  }

  results_list <- if (n_cores > 1) {
    mclapply(jobs, run_job, mc.cores = n_cores, mc.preschedule = FALSE)
  } else {
    lapply(jobs, run_job)
  }

  df_sweep <- bind_rows(Filter(Negate(is.null), results_list))

  # ---------------------------------------------------------------
  # Completion heatmap — fraction of timesteps completed per job
  # ---------------------------------------------------------------
  p_completion <- ggplot(df_sweep,
                         aes(x = value_rel, y = label, fill = frac_ok)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = ifelse(frac_ok < 1, paste0(round(frac_ok * 100), "%"), "")),
              size = 3.5, colour = "grey20") +
    scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10),
                  labels = c("0.1×", "0.5×", "1×", "2×", "5×", "10×")) +
    scale_y_discrete(labels = function(x) parse(text = x)) +
    scale_fill_gradientn(
      colours = c("#D55E00", "#E69F00", "#FFFFFF"),
      values  = c(0, 0.5, 1),
      limits  = c(0, 1),
      name    = "Fraction\ncompleted"
    ) +
    labs(x = "Parameter value relative to baseline", y = NULL,
         title = "Model completion rate across parameter sweep") +
    theme_bw(base_size = 16) +
    theme(
      text             = element_text(family = "serif"),
      panel.grid       = element_blank(),
      strip.background = element_blank(),
      legend.position  = "right"
    )

  ggsave("RScripts/plots/SA_00_completion.png", p_completion,
         width = 10, height = 7, dpi = 300)

  # ---------------------------------------------------------------
  # Normalise to baseline
  # ---------------------------------------------------------------
  safe_base <- function(x) pmax(x, 1e-30)

  df_sweep <- df_sweep |>
    mutate(
      N_uptake_rel  = N_uptake / safe_base(base_sum$N_uptake),
      biomass_rel   = biomass  / safe_base(base_sum$biomass),
      tree_N_rel    = tree_N   / safe_base(base_sum$tree_N),
      myco_exp_rel  = myco_exp / safe_base(base_sum$myco_exp),
      ecm_mass_rel  = ecm_mass  / safe_base(base_sum$ecm_mass),
      C_to_myco_rel = C_to_myco / safe_base(base_sum$C_to_myco),
      root_rel      = root_up  / safe_base(base_sum$root_up),
      myco_rel      = myco_up  / safe_base(base_sum$myco_up),
      height_rel    = height   / safe_base(base_sum$height)
    )

  # ---------------------------------------------------------------
  # Tornado: ±50 % perturbation for all 6 output variables
  # ---------------------------------------------------------------
  tornado_outputs <- list(
    list(col = "N_uptake",  base = base_sum$N_uptake,  label = "N~uptake"),
    list(col = "height",    base = base_sum$height,    label = "Height"),
    list(col = "tree_N",    base = base_sum$tree_N,    label = "Internal~N"),
    list(col = "myco_exp",  base = base_sum$myco_exp,  label = "N~transferred"),
    list(col = "C_to_myco", base = base_sum$C_to_myco, label = "C~to~mycorrhiza"),
    list(col = "ecm_mass",  base = base_sum$ecm_mass,  label = "ECM~biomass")
  )

  tornado_df <- bind_rows(lapply(tornado_outputs, function(out) {
    bv <- safe_base(out$base)
    df_sweep |>
      group_by(param, label, tier) |>
      summarise(
        sens_lo = {
          idx <- which.min(abs(value_rel - 0.5))
          (.data[[out$col]][idx] - bv) / bv * 100
        },
        sens_hi = {
          idx <- which.min(abs(value_rel - 2.0))
          (.data[[out$col]][idx] - bv) / bv * 100
        },
        .groups = "drop"
      ) |>
      mutate(output = out$label)
  }))

  # Order parameters by mean absolute sensitivity across all outputs
  param_order <- tornado_df |>
    group_by(param, label) |>
    summarise(mean_range = mean(abs(sens_hi - sens_lo), na.rm = TRUE),
              .groups = "drop") |>
    arrange(mean_range) |>
    pull(label)

  tornado_df <- tornado_df |>
    mutate(
      label    = factor(label,  levels = param_order),
      tier_f   = paste0("Tier ", tier),
      output_f = factor(output, levels = sapply(tornado_outputs, `[[`, "label"))
    )

  tier_cols <- c("Tier 1" = "#0072B2", "Tier 2" = "#D55E00",
                 "Tier 3" = "#009E73", "Tier 4" = "#CC79A7")

  p_tornado <- ggplot(tornado_df) +
    geom_segment(aes(x = sens_lo, xend = sens_hi,
                     y = label,   yend = label,
                     colour = tier_f),
                 linewidth = 4, lineend = "round", alpha = 0.85) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30",
               linewidth = 0.4) +
    facet_wrap(~ output_f, nrow = 1,
               labeller = labeller(output_f = label_parsed)) +
    scale_colour_manual(values = tier_cols, name = NULL) +
    scale_y_discrete(labels = function(x) parse(text = x)) +
    labs(
      x = "% change from baseline  (half-parameter sweep)",
      y = NULL
    ) +
    theme_bw(base_size = 16) +
    theme(
      text             = element_text(family = "serif"),
      legend.position  = "top",
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", size = 15),
      panel.spacing    = unit(0.6, "lines")
    )

  ggsave("RScripts/plots/SA_01_tornado.png", p_tornado,
         width = 18, height = 7, dpi = 300)

  # ---------------------------------------------------------------
  # SA_02: per-tier sweep curves × all output variables (4 × 6 grid)
  # ---------------------------------------------------------------
  tier_names <- c("1" = "Tier 1: Supply control",
                  "2" = "Tier 2: Trade-off structure",
                  "3" = "Tier 3: Root architecture",
                  "4" = "Tier 4: Seasonal / temporal")

  output_cols_all <- data.frame(
    col   = c("N_uptake_rel", "height_rel",  "tree_N_rel",
              "myco_exp_rel", "C_to_myco_rel", "ecm_mass_rel"),
    label = c("N~uptake",     "Height",      "Internal~N",
              "N~transferred","C~to~mycorrhiza", "ECM~biomass"),
    stringsAsFactors = FALSE
  )
  output_cols_all$label <- factor(output_cols_all$label,
                                   levels = output_cols_all$label)

  df_tier_long <- bind_rows(lapply(seq_len(nrow(output_cols_all)), function(i) {
    df_sweep |>
      mutate(
        output     = output_cols_all$label[i],
        value_norm = .data[[output_cols_all$col[i]]]
      ) |>
      select(param, label, tier, value_rel, output, value_norm)
  })) |>
    mutate(tier_f = factor(tier_names[as.character(tier)],
                            levels = tier_names))

  p_tier <- ggplot(df_tier_long,
                   aes(x = value_rel, y = value_norm,
                       colour = label, group = label)) +
    geom_hline(yintercept = 1, linetype = "dashed",
               colour = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = 1, linetype = "dashed",
               colour = "grey60", linewidth = 0.4) +
    geom_line(linewidth = 1.0) +
    facet_grid(tier_f ~ output, scales = "free_y",
               labeller = labeller(output  = label_parsed,
                                   tier_f  = label_value)) +
    scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10),
                  labels = c("0.1×", "0.5×", "1×",
                             "2×", "5×", "10×")) +
    scale_colour_discrete(labels = function(x) parse(text = x), name = NULL) +
    labs(x = "Parameter value (relative to baseline)",
         y = "Output (relative to baseline)") +
    theme_bw(base_size = 14) +
    theme(
      text             = element_text(family = "serif"),
      strip.background = element_blank(),
      strip.text.x     = element_text(face = "bold", size = 13),
      strip.text.y     = element_text(face = "bold", size = 13, angle = 0),
      panel.border     = element_rect(fill = NA, colour = "grey70"),
      panel.spacing    = unit(0.5, "lines"),
      legend.position  = "right",
      legend.text      = element_text(size = 12),
      axis.text        = element_text(size = 11),
      axis.title       = element_text(size = 13)
    )

  ggsave("RScripts/plots/SA_02_tier_uptake.png", p_tier,
         width = 22, height = 14, dpi = 300)

  # ---------------------------------------------------------------
  # Sensitivity index table (N uptake only, for parameter ranking)
  # ---------------------------------------------------------------
  sens_table <- tornado_df |>
    filter(output == "N~uptake") |>
    transmute(
      tier,
      param,
      label  = as.character(label),
      S_lo   = sens_lo / -50,
      S_hi   = sens_hi /  50,
      S_mean = (abs(S_lo) + abs(S_hi)) / 2
    ) |>
    arrange(tier, desc(S_mean))

  print(sens_table)

  # ---------------------------------------------------------------
  # Publication figure: top-3 parameters × 5 output variables
  # ---------------------------------------------------------------
  top3_params <- sens_table |>
    arrange(desc(S_mean)) |>
    head(3) |>
    pull(param)

  # Human-readable row labels (parseable by label_parsed)
  output_meta <- data.frame(
    col   = c("N_uptake_rel", "height_rel",  "tree_N_rel",
              "myco_exp_rel", "C_to_myco_rel", "ecm_mass_rel"),
    label = c("N~uptake",     "Height",      "Internal~N",
              "N~transferred","C~to~mycorrhiza", "ECM~biomass"),
    stringsAsFactors = FALSE
  )
  output_meta$label <- factor(output_meta$label,
                               levels = output_meta$label)

  df_pub <- df_sweep |>
    filter(param %in% top3_params)

  # Keep parameter label ordered by sensitivity rank
  param_order <- df_sweep |>
    filter(param %in% top3_params) |>
    distinct(param, label) |>
    slice(match(top3_params, param)) |>
    pull(label)

  df_pub <- df_pub |>
    mutate(param_lab = factor(label, levels = param_order))

  df_pub_long <- bind_rows(lapply(seq_len(nrow(output_meta)), function(i) {
    df_pub |>
      mutate(
        output     = output_meta$label[i],
        value_norm = .data[[output_meta$col[i]]]
      ) |>
      select(param_lab, value_rel, output, value_norm)
  }))

  # One colour per output variable (colourblind-safe palette)
  out_cols <- setNames(
    c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#F0E442"),
    levels(output_meta$label)
  )

  p_pub <- ggplot(df_pub_long,
                  aes(x = value_rel, y = value_norm,
                      colour = output, group = output)) +
    geom_hline(yintercept = 1, linetype = "dashed",
               colour = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = 1, linetype = "dashed",
               colour = "grey60", linewidth = 0.4) +
    geom_line(linewidth = 1.2) +
    facet_wrap(~ param_lab, nrow = 1,
               labeller = labeller(param_lab = label_parsed)) +
    scale_x_log10(
      breaks = c(0.1, 0.5, 1, 2, 5, 10),
      labels = c("0.1×", "0.5×", "1×", "2×", "5×", "10×")
    ) +
    scale_colour_manual(
      values = out_cols,
      labels = function(x) parse(text = as.character(x)),
      name   = NULL
    ) +
    labs(
      x = "Parameter value relative to baseline",
      y = "Output relative to baseline"
    ) +
    theme_classic(base_size = 16) +
    theme(
      text             = element_text(family = "serif"),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", size = 17),
      panel.border     = element_rect(fill = NA, colour = "grey70"),
      panel.spacing    = unit(0.8, "lines"),
      axis.text        = element_text(size = 14),
      axis.title       = element_text(size = 16),
      legend.position  = "bottom",
      legend.text      = element_text(size = 15),
      legend.key.width = unit(1.8, "lines")
    )

  ggsave("RScripts/plots/SA_03_publication.png", p_pub,
         width = 12, height = 5, dpi = 300)

  invisible(list(
    df_sweep   = df_sweep,
    tornado_df = tornado_df,
    sens_table = sens_table,
    base_sum   = base_sum
  ))
}
