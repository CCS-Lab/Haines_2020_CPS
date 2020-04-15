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

m1 <- stan_model("Code/Stan/trait_descriptive_model.stan")

for (d in datasets) {
  # Keep mturk data preprocessed by Andrew
  all_dat   <- readRDS(file = paste0("Data/Preprocessed/1_preprocessed_DDT_trait_descriptive_", d,".rds"))
  model_dat <- all_dat$survey_dat
  
  # Data for m1
  dat_m1 <- list(N_subjects = length(model_dat$ID),
                 X          = cbind(sv(model_dat$bis_noplan),
                                    sv(model_dat$stai_s)), # no interaction term
                 N_preds    = 2)
  dat_m1 <- append(all_dat$ddt_dat, dat_m1)
  
  fit <- sampling(m1, 
                  data   = dat_m1, 
                  iter   = 4000, 
                  warmup = 1500, 
                  chains = 8, 
                  cores  = 8,
                  seed   = 43201)
  
  saveRDS(fits, file = paste0("Data/Fitted/descriptive_models_", d,"_noINT.rds"))
}