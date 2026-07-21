suppressMessages({ library(tidyverse); library(ggplot2); library(patchwork) })
setwd("/home/josimms/Documents/Austria/Plant-FATE/vignettes")
load('/home/josimms/Documents/Austria/Plant-FATE/.RData')

# Old workspace uses N_bar / N_bar_far; current code outputs N_bar_roots / N_bar_myco
all_joint_df <- all_joint_df %>% rename(N_bar_roots = N_bar, N_bar_myco = N_bar_far)
joint_gs     <- joint_gs     %>% rename(N_bar_roots = N_bar, N_bar_myco = N_bar_far) %>%
  mutate(ecm_frac_gpp = C_export_to_myco / assim_gross)

pub_theme <- theme_classic(base_size = 22) +
  theme(axis.title = element_text(size=22,face="bold"), axis.text = element_text(size=20,colour="black"),
        axis.line = element_line(colour="black",linewidth=0.6), axis.ticks = element_line(colour="black",linewidth=0.5),
        legend.title = element_text(size=22,face="bold"), legend.text = element_text(size=20),
        legend.key.size = unit(0.9,"lines"), panel.grid.major = element_line(colour="grey90",linewidth=0.3),
        panel.grid.minor = element_blank(), plot.background = element_rect(fill="white",colour=NA),
        panel.background = element_rect(fill="white",colour=NA), plot.margin = margin(8,12,8,8),
        strip.text = element_text(size=22,face="bold"))

shape_vals <- c("Optimal parameters" = 3, "Model output" = 1, "Calculated" = 18)
ltype_vals <- c("Trend line" = "dashed")
make_row_title <- function(label) {
  wrap_elements(grid::textGrob(label, gp = grid::gpar(fontsize=20,fontface="bold"), hjust=0.5, x=0.5))
}

add_net_gpp_ca <- function(df) {
  df %>% mutate(net_gpp_ca = (assim_gross - C_export_to_myco - rr - tr * 0.44) / crown_area)
}

# Last year: add tr from all_joint_df then compute formula
joint_gs_lastyear <- all_joint_df %>%
  filter(as.integer(format(as.Date(date), "%m")) %in% 6:8,
         as.integer(format(as.Date(date), "%Y")) == 2021) %>%
  group_by(soil_nitrogen, ecto_allo, root_no, root_length) %>%
  summarise(
    height=mean(height,na.rm=T), ectomycorrhiza_mass=mean(ectomycorrhiza_mass,na.rm=T),
    ectomycorrhiza_N_biomass=mean(ectomycorrhiza_N_biomass,na.rm=T),
    ectomycorrhiza_C_free=mean(ectomycorrhiza_C_free,na.rm=T),
    ectomycorrhiza_N_free=mean(ectomycorrhiza_N_free,na.rm=T),
    crown_area=mean(crown_area,na.rm=T), lai=mean(lai,na.rm=T),
    N_bar_roots=mean(N_bar_roots,na.rm=T), N_bar_myco=mean(N_bar_myco,na.rm=T),
    C_export_to_myco=mean(C_export_to_myco,na.rm=T),
    mycorrhizal_export_to_tree=mean(mycorrhizal_export_to_tree,na.rm=T),
    root_uptake=mean(root_uptake,na.rm=T), myco_uptake=mean(myco_uptake,na.rm=T),
    assim_gross=mean(assim_gross,na.rm=T), optimal_leaf_nitrogen=mean(optimal_leaf_nitrogen,na.rm=T),
    leaf_nitrogen_concentration=mean(leaf_nitrogen_concentration,na.rm=T),
    tree_nitrogen=mean(tree_nitrogen,na.rm=T), root_mass=mean(root_mass,na.rm=T),
    coarse_root_mass=mean(coarse_root_mass,na.rm=T), total_mass=mean(total_mass,na.rm=T),
    fineroot_lifespan=mean(fineroot_lifespan,na.rm=T), total_prod=mean(total_prod,na.rm=T),
    rr=mean(rr,na.rm=T), tr=mean(tr,na.rm=T),
    ecm_frac_gpp=mean(C_export_to_myco/assim_gross,na.rm=T), .groups="drop"
  ) %>% add_net_gpp_ca()

# Multi-year mean
joint_gs_allyears <- all_joint_df %>%
  filter(as.integer(format(as.Date(date), "%m")) %in% 6:8) %>%
  group_by(soil_nitrogen, ecto_allo, root_no, root_length) %>%
  summarise(
    height=mean(height,na.rm=T), ectomycorrhiza_mass=mean(ectomycorrhiza_mass,na.rm=T),
    ectomycorrhiza_N_biomass=mean(ectomycorrhiza_N_biomass,na.rm=T),
    ectomycorrhiza_C_free=mean(ectomycorrhiza_C_free,na.rm=T),
    ectomycorrhiza_N_free=mean(ectomycorrhiza_N_free,na.rm=T),
    crown_area=mean(crown_area,na.rm=T), lai=mean(lai,na.rm=T),
    N_bar_roots=mean(N_bar_roots,na.rm=T), N_bar_myco=mean(N_bar_myco,na.rm=T),
    C_export_to_myco=mean(C_export_to_myco,na.rm=T),
    mycorrhizal_export_to_tree=mean(mycorrhizal_export_to_tree,na.rm=T),
    root_uptake=mean(root_uptake,na.rm=T), myco_uptake=mean(myco_uptake,na.rm=T),
    assim_gross=mean(assim_gross,na.rm=T), optimal_leaf_nitrogen=mean(optimal_leaf_nitrogen,na.rm=T),
    leaf_nitrogen_concentration=mean(leaf_nitrogen_concentration,na.rm=T),
    tree_nitrogen=mean(tree_nitrogen,na.rm=T), root_mass=mean(root_mass,na.rm=T),
    coarse_root_mass=mean(coarse_root_mass,na.rm=T), total_mass=mean(total_mass,na.rm=T),
    fineroot_lifespan=mean(fineroot_lifespan,na.rm=T), total_prod=mean(total_prod,na.rm=T),
    rr=mean(rr,na.rm=T), tr=mean(tr,na.rm=T),
    ecm_frac_gpp=mean(C_export_to_myco/assim_gross,na.rm=T), .groups="drop"
  ) %>% add_net_gpp_ca()

make_optima_figure <- function(opt_data, display_data, var_col, fig_title) {
  var_sym <- sym(var_col)
  join_keys <- c("soil_nitrogen", "ecto_allo", "root_no", "root_length")

  # Optimal parameter combo per soil_nitrogen, selected from opt_data
  opt_keys <- opt_data %>%
    group_by(soil_nitrogen) %>%
    mutate(max_v=max(!!var_sym,na.rm=T), near_opt=!!var_sym>=0.99*max_v,
           n_total=n(), n_near_opt=sum(near_opt), stab_frac=n_near_opt/n_total) %>%
    filter(near_opt) %>%
    slice_min(root_no, n=1, with_ties=FALSE) %>%
    select(all_of(join_keys), stab_frac) %>% ungroup()

  # Display values come from display_data (last-year summer mean) at those parameters
  opt <- opt_keys %>%
    left_join(display_data, by=join_keys)

  # Parameter stability bars (A/B/C): ranges from opt_data
  stab <- opt_data %>%
    group_by(soil_nitrogen) %>%
    mutate(max_v=max(!!var_sym,na.rm=T), near_opt=!!var_sym>=0.99*max_v) %>%
    filter(near_opt) %>%
    summarise(ecto_lo=min(ecto_allo,na.rm=T),ecto_hi=max(ecto_allo,na.rm=T),
              rn_lo=min(root_no,na.rm=T),rn_hi=max(root_no,na.rm=T),
              rl_lo=min(root_length,na.rm=T),rl_hi=max(root_length,na.rm=T),.groups="drop")

  # Output ribbons (D–L): near-opt keys from opt_data, values from display_data
  near_opt_keys <- opt_data %>%
    group_by(soil_nitrogen) %>%
    mutate(max_v=max(!!var_sym,na.rm=T), near_opt=!!var_sym>=0.99*max_v) %>%
    filter(near_opt) %>%
    select(all_of(join_keys)) %>% ungroup()

  stab_out <- near_opt_keys %>%
    left_join(display_data, by=join_keys) %>%
    group_by(soil_nitrogen) %>%
    summarise(
      ecm_lo=min(ectomycorrhiza_mass,na.rm=T),ecm_hi=max(ectomycorrhiza_mass,na.rm=T),
      ecm_area_lo=min(ectomycorrhiza_mass/crown_area,na.rm=T),ecm_area_hi=max(ectomycorrhiza_mass/crown_area,na.rm=T),
      height_lo=min(height,na.rm=T),height_hi=max(height,na.rm=T),
      nroots_lo=min(N_bar_roots,na.rm=T),nroots_hi=max(N_bar_roots,na.rm=T),
      nmyco_lo=min(N_bar_myco,na.rm=T),nmyco_hi=max(N_bar_myco,na.rm=T),
      cn_lo=min((ectomycorrhiza_mass*0.44+ectomycorrhiza_C_free)/pmax(ectomycorrhiza_N_biomass+ectomycorrhiza_N_free,1e-12),na.rm=T),
      cn_hi=max((ectomycorrhiza_mass*0.44+ectomycorrhiza_C_free)/pmax(ectomycorrhiza_N_biomass+ectomycorrhiza_N_free,1e-12),na.rm=T),
      gpp_lo=min(assim_gross/crown_area,na.rm=T),gpp_hi=max(assim_gross/crown_area,na.rm=T),
      n_upper_lo=min(leaf_nitrogen_concentration,na.rm=T),n_upper_hi=max(leaf_nitrogen_concentration,na.rm=T),
      n_realised_lo=min(optimal_leaf_nitrogen,na.rm=T),n_realised_hi=max(optimal_leaf_nitrogen,na.rm=T),
      n_free_lo=min(tree_nitrogen,na.rm=T),n_free_hi=max(tree_nitrogen,na.rm=T),
      bga_lo=min((C_export_to_myco+rr+tr*0.44)/assim_gross,na.rm=T),
      bga_hi=max((C_export_to_myco+rr+tr*0.44)/assim_gross,na.rm=T),
      cn_exch_lo=min(C_export_to_myco/pmax(mycorrhizal_export_to_tree,1e-12),na.rm=T),
      cn_exch_hi=max(C_export_to_myco/pmax(mycorrhizal_export_to_tree,1e-12),na.rm=T),
      .groups="drop")
  opt_cn <- opt %>% mutate(CN=(ectomycorrhiza_mass*0.44+ectomycorrhiza_C_free)/
                                pmax(ectomycorrhiza_N_biomass+ectomycorrhiza_N_free,1e-12))
  fn_colour <- c("Stability band"="grey70")
  fn_scales <- list(scale_shape_manual(values=shape_vals,name=NULL),
                    scale_linetype_manual(values=ltype_vals,name=NULL),
                    scale_colour_manual(values=fn_colour,name=NULL))
  sl <- geom_text(aes(label=paste0(round(stab_frac*100),"%")),vjust=-0.8,size=3,inherit.aes=TRUE)

  pA <- ggplot(opt,aes(soil_nitrogen,ecto_allo))+
    geom_linerange(data=stab,aes(x=soil_nitrogen,ymin=ecto_lo,ymax=ecto_hi,colour="Stability band"),linewidth=2,inherit.aes=FALSE)+
    geom_point(aes(shape="Optimal parameters"),colour="black",size=3,stroke=1.2)+sl+fn_scales+
    scale_y_continuous(labels=scales::label_percent())+
    labs(tag="A",x="Soil nitrogen (g N / m3)",y="Optimal ECM C allocation\n(% NPP)")+pub_theme
  pB <- ggplot(opt,aes(soil_nitrogen,root_no))+
    geom_linerange(data=stab,aes(x=soil_nitrogen,ymin=rn_lo,ymax=rn_hi,colour="Stability band"),linewidth=2,inherit.aes=FALSE)+
    geom_point(aes(shape="Optimal parameters"),colour="black",size=3,stroke=1.2)+sl+fn_scales+
    scale_y_log10(labels=scales::label_scientific())+
    labs(tag="B",x="Soil nitrogen (g N / m3)",y="Optimal root number\n(no / m2 crown)")+pub_theme
  pC <- ggplot(opt,aes(soil_nitrogen,root_length))+
    geom_linerange(data=stab,aes(x=soil_nitrogen,ymin=rl_lo,ymax=rl_hi,colour="Stability band"),linewidth=2,inherit.aes=FALSE)+
    geom_point(aes(shape="Optimal parameters"),colour="black",size=3,stroke=1.2)+sl+fn_scales+
    labs(tag="C",x="Soil nitrogen (g N / m3)",y="Optimal root length (m)")+pub_theme
  pD <- ggplot(opt,aes(soil_nitrogen,(C_export_to_myco+rr+tr*0.44)/assim_gross))+
    geom_ribbon(data=stab_out,aes(x=soil_nitrogen,ymin=bga_lo,ymax=bga_hi),fill="grey80",colour=NA,inherit.aes=FALSE)+
    geom_point(aes(shape="Calculated"),colour="black",size=3,stroke=1.2)+fn_scales+
    scale_y_continuous(labels=scales::label_percent())+expand_limits(y=0)+
    labs(tag="D",x="Soil nitrogen (g N / m3)",y="N-related belowground C\nallocation (% GPP)")+pub_theme
  pE <- ggplot(opt,aes(soil_nitrogen,ectomycorrhiza_mass))+
    geom_ribbon(data=stab_out,aes(x=soil_nitrogen,ymin=ecm_lo,ymax=ecm_hi),fill="grey80",colour=NA,inherit.aes=FALSE)+
    geom_point(aes(shape="Model output"),colour="black",size=3,stroke=1.2)+fn_scales+
    scale_y_continuous(labels=scales::label_scientific())+
    labs(tag="E",x="Soil nitrogen (g N / m3)",y="ECM biomass (kg)")+pub_theme
  pF <- ggplot(opt,aes(soil_nitrogen,ectomycorrhiza_mass/crown_area))+
    geom_ribbon(data=stab_out,aes(x=soil_nitrogen,ymin=ecm_area_lo,ymax=ecm_area_hi),fill="grey80",colour=NA,inherit.aes=FALSE)+
    geom_point(aes(shape="Model output"),colour="black",size=3,stroke=1.2)+fn_scales+
    scale_y_continuous(labels=scales::label_scientific())+
    labs(tag="F",x="Soil nitrogen (g N / m3)",y="ECM biomass per crown\n(kg / m2)")+pub_theme
  nr_rng<-range(opt$N_bar_roots,na.rm=T); nm_rng<-range(opt$N_bar_myco,na.rm=T)
  m2r<-function(x) nr_rng[1]+(x-nm_rng[1])/diff(nm_rng)*diff(nr_rng)
  r2m<-function(x) nm_rng[1]+(x-nr_rng[1])/diff(nr_rng)*diff(nm_rng)
  pG <- ggplot(opt,aes(soil_nitrogen,N_bar_roots))+
    geom_ribbon(data=stab_out,aes(x=soil_nitrogen,ymin=nroots_lo,ymax=nroots_hi),fill="grey80",colour=NA,inherit.aes=FALSE)+
    geom_ribbon(data=stab_out,aes(x=soil_nitrogen,ymin=m2r(nmyco_lo),ymax=m2r(nmyco_hi)),fill="steelblue",alpha=0.25,colour=NA,inherit.aes=FALSE)+
    geom_point(aes(shape="Model output"),colour="black",size=3,stroke=1.2)+
    geom_point(aes(y=m2r(N_bar_myco)),colour="steelblue",shape=1,size=3,stroke=1.2)+fn_scales+
    scale_y_continuous(name="N_bar roots (kg N / m3)",sec.axis=sec_axis(~r2m(.),name="N_bar myco (kg N / m3)"))+
    labs(tag="G",x="Soil nitrogen (g N / m3)")+pub_theme+
    theme(axis.title.y.right=element_text(colour="steelblue"),axis.text.y.right=element_text(colour="steelblue"),
          axis.ticks.y.right=element_line(colour="steelblue"),axis.line.y.right=element_line(colour="steelblue"))
  pH <- ggplot(opt_cn,aes(soil_nitrogen,CN))+
    geom_ribbon(data=stab_out,aes(x=soil_nitrogen,ymin=cn_lo,ymax=cn_hi),fill="grey80",colour=NA,inherit.aes=FALSE)+
    geom_point(aes(shape="Model output"),colour="black",size=3,stroke=1.2)+fn_scales+
    scale_y_continuous(labels=scales::label_scientific(digits=2))+
    labs(tag="H",x="Soil nitrogen (g N / m3)",y="Mycorrhizal C:N\n(kg C / kg N)")+pub_theme
  pI <- ggplot(opt,aes(soil_nitrogen,height))+
    geom_ribbon(data=stab_out,aes(x=soil_nitrogen,ymin=height_lo,ymax=height_hi),fill="grey80",colour=NA,inherit.aes=FALSE)+
    geom_point(aes(shape="Model output"),colour="black",size=3,stroke=1.2)+fn_scales+expand_limits(y=0)+
    labs(tag="I",x="Soil nitrogen (g N / m3)",y="Height (m)")+pub_theme
  pJ <- ggplot(opt,aes(soil_nitrogen,assim_gross/crown_area))+
    geom_ribbon(data=stab_out,aes(x=soil_nitrogen,ymin=gpp_lo,ymax=gpp_hi),fill="grey80",colour=NA,inherit.aes=FALSE)+
    geom_point(aes(shape="Model output"),colour="black",size=3,stroke=1.2)+fn_scales+expand_limits(y=0)+
    labs(tag="J",x="Soil nitrogen (g N / m3)",y="GPP per crown area\n(kg C / m2 / yr)")+pub_theme
  pK <- ggplot(opt,aes(soil_nitrogen))+
    geom_ribbon(data=stab_out,aes(x=soil_nitrogen,ymin=n_upper_lo,ymax=n_upper_hi),fill="steelblue",alpha=0.25,colour=NA,inherit.aes=FALSE)+
    geom_ribbon(data=stab_out,aes(x=soil_nitrogen,ymin=n_realised_lo,ymax=n_realised_hi),fill="grey60",alpha=0.25,colour=NA,inherit.aes=FALSE)+
    geom_point(aes(y=leaf_nitrogen_concentration,colour="Potential (leaf N pool)"),size=3,stroke=1.2)+
    geom_point(aes(y=optimal_leaf_nitrogen),colour="grey30",shape=1,size=3,stroke=1.2)+
    scale_colour_manual(values=c("Potential (leaf N pool)"="steelblue"),name=NULL)+expand_limits(y=0)+
    labs(tag="K",x="Soil nitrogen (g N / m3)",y="Leaf N concentration\n(kg N / kg leaf)")+pub_theme
  pL <- ggplot(opt,aes(soil_nitrogen,C_export_to_myco/pmax(mycorrhizal_export_to_tree,1e-12)))+
    geom_ribbon(data=stab_out,aes(x=soil_nitrogen,ymin=cn_exch_lo,ymax=cn_exch_hi),fill="grey80",colour=NA,inherit.aes=FALSE)+
    geom_point(aes(shape="Model output"),colour="black",size=3,stroke=1.2)+fn_scales+expand_limits(y=0)+
    labs(tag="L",x="Soil nitrogen (g N / m3)",y="C:N exchange cost\n(kg C / kg N received)")+pub_theme

  guide_area() /
    make_row_title("Parameters to be Optimised") / (pA|pB|pC|pD) /
    make_row_title("Model results: Mycorrhizal dynamics & N depletion") / (pE|pF|pG|pH) /
    make_row_title("Model results: Tree Status") / (pI|pJ|pK|pL) +
    plot_layout(heights=c(0.15,0.07,1,0.07,1,0.07,1),guides="collect") +
    plot_annotation(title=fig_title,
                    theme=theme(plot.margin=margin(5,30,5,5),plot.title=element_text(size=18,face="bold"))) &
    theme(plot.tag=element_text(size=18,face="bold"),legend.text=element_text(size=14),
          legend.key.size=unit(0.9,"lines"),legend.position="top",
          legend.direction="horizontal",legend.box="horizontal")
}

dir.create("plots", showWarnings=FALSE)
message("Generating last-year figure...")
ggsave("plots/11_obj_lastyear.png",
       plot = make_optima_figure(joint_gs_lastyear, joint_gs_lastyear, "net_gpp_ca",
                                 "Objective: net GPP / crown area  (summer 2021)"),
       width=22, height=18, dpi=300)
message("Generating all-years figure...")
ggsave("plots/11_obj_allyears.png",
       plot = make_optima_figure(joint_gs_allyears, joint_gs_lastyear, "net_gpp_ca",
                                 "Objective: net GPP / crown area  (optimum: multi-year mean; values: summer 2021)"),
       width=22, height=18, dpi=300)
message("Done.")
