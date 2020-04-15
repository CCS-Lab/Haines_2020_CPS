rm(list=ls())

library(rstan)
library(hBayesDM)
library(dplyr)
library(loo)
library(foreach)
library(doParallel)
library(cowplot)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

sv <- function(x) as.vector(scale(x))

datasets <- c("MTURK", "REP", "TAL")

# Distribution of k independent of traits
m0 <- stan_model("Code/Stan/base_descriptive_model.stan")
# Distribution of k dependent on traits
m1 <- stan_model("Code/Stan/trait_descriptive_model.stan")

for (d in datasets) {
  # Keep mturk data preprocessed by Andrew
  all_dat   <- readRDS(file = paste0("Data/Preprocessed/1_preprocessed_DDT_", d,".rds"))
  model_dat <- all_dat$survey_dat
  
  # Data for m0
  dat_m0 <- all_dat$ddt_dat
  # Data for m1
  dat_m1 <- list(N_subjects = length(model_dat$ID),
                 X          = cbind(sv(model_dat$bis_noplan),
                                    sv(model_dat$stai_s),
                                    sv(model_dat$bis_noplan) * sv(model_dat$stai_s)),
                 N_preds    = 3)
  dat_m1 <- append(dat_m0, dat_m1)
  
  # For parallel fitting
  all_m   <- list(m0, m1) 
  all_dat <- list(dat_m0, dat_m1)
  
  cl <- makeCluster(2)
  registerDoParallel(cl)
  fits <- foreach(m=1:2, .packages = "rstan") %dopar% {
    sampling(all_m[[m]], 
             data   = all_dat[[m]], 
             iter   = 4000, 
             warmup = 1500, 
             chains = 8, 
             cores  = 8,
             seed   = 43201)
  }
  stopCluster(cl)
  
  saveRDS(fits, file = paste0("Data/Fitted/descriptive_models_", d,".rds"))
}