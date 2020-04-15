rm(list=ls())

library(rstan)
library(hBayesDM)
library(dplyr)
library(loo)
library(foreach)
library(doParallel)
library(cowplot)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

sv <- function(x) {
  # Scale by student group scores
  scale_REP <- scale(x[801:932])
  # Scale MTURK by student group pars
  scale_mturk   <- (x[1:800] - attributes(scale_REP)$`scaled:center`) / attributes(scale_REP)$`scaled:scale`
  # Scale SUD by student group pars
  scale_TAL   <- (x[933:967] - attributes(scale_REP)$`scaled:center`) / attributes(scale_REP)$`scaled:scale`
  c(scale_mturk, scale_REP, scale_TAL)
}

# Distribution of k dependent on traits
m1 <- stan_model("Code/Stan/trait_explanatory_model_groupC.stan")

# Keep mturk data preprocessed by Andrew
all_dat   <- readRDS(file = paste0("Data/Preprocessed/1_preprocessed_DDT_trait_explanatory.rds"))
model_dat <- all_dat$survey_dat
test_ids <- all_dat$test_ids

# Data for m1
dat_m1 <- list(N_subjects = length(model_dat$ID),
               X          = cbind(sv(model_dat$bis_noplan),
                                  sv(model_dat$stai_s)),
               N_preds    = 2)
dat_m1 <- append(all_dat$ddt_dat, dat_m1)

# Initialize parameter estimates for trouble fitting the trait congruent model (convergence problems)
inits <- function() {
  list(beta_a = c(-.86, -.03),
       beta_k = c(-4.8, .35),
       mu_c_pr = -1,
       sigma = c(1.9, .34),
       k_pr = rnorm(967, 0, .1),
       a_pr = rnorm(967, 0, .1))
}

# Fit model (no testset here)
fit <- sampling(m1, 
                data   = dat_m1, 
                init   = inits,
                iter   = 4500, 
                warmup = 1500, 
                chains = 8, 
                cores  = 8,
                seed   = 43202)
saveRDS(fit, file = paste0("Data/Fitted/explanatory_models_trait_groupC.rds"))

