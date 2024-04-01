library(tidyverse)
rm(list=ls())

dir = "pspm_output_test"
ref_dir = paste0(dir,"/cont_test_ref")
spinup_dir = paste0(dir,"/cont_test_spinup")
main_dir   = paste0(dir,"/cont_test_main")

setwd(paste0("~/codes/Plant-FATE/",spinup_dir))
dat2_spin = read.csv("Y_PFATE.csv")
dat_spin = read.csv("D_PFATE.csv")
Yend = max(dat2_spin$YEAR)

setwd(paste0("~/codes/Plant-FATE/",main_dir))
dat2_main = read.csv("Y_PFATE.csv")
dat_main = read.csv("D_PFATE.csv")

setwd(paste0("~/codes/Plant-FATE/",ref_dir))
dat2_ref = read.csv("Y_PFATE.csv")
dat_ref = read.csv("D_PFATE.csv")


dat2_main = dat2_main %>% filter(YEAR > Yend)
dat_main = dat_main %>% filter(YEAR > Yend)

dat2 = rbind(dat2_spin, dat2_main)
dat = rbind(dat_spin, dat_main)

plot_data = function(dat, dat2, main){
  n_species = length(unique(dat2$PID))
  n_year = length(unique(dat2$YEAR))
  
  par(mfrow=c(1,3), mar=c(6,6,1,1), oma=c(1,1,2,1), cex.lab=1.3, cex.axis=1.2, mgp=c(3.2,1,0), las=1)
  
  seeds = dat2 %>% select(YEAR, PID, SEEDS) %>% spread(value = "SEEDS", key = "PID")
  matplot(seeds$YEAR, seeds[,-1], lty=1, col=rainbow(n = n_species+1, start = 0, end = 0.85, alpha = 0.5), type="l",
          las=1, xlab="Time (years)", ylab="Species seed\noutput", log="", main=main)
  abline(v=1200, col="pink")
  
  BA = dat2 %>% select(YEAR, PID, BA) %>% spread(value = "BA", key = "PID")
  matplot(BA$YEAR, cbind(BA[,-1], rowSums(BA[,-1], na.rm=T))*1e4, lty=1, col=c(rainbow(n = n_species, start = 0, end = 0.85), "black"), type="l",
          las=1, xlab="Time (years)", ylab="Basal area", log="")
  abline(v=1200, col="pink")
  
  plot(dat$GPP~dat$YEAR, type="l", col="green4", xlab="Time (years)", ylab="GPP")
  abline(v=1200, col="pink")
}

plot_data(dat_ref, dat2_ref, "Ref")
plot_data(dat, dat2, "Main")
