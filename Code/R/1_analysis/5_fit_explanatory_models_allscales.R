rm(list=ls())

library(rstan)
library(hBayesDM)
library(dplyr)
library(loo)
library(foreach)
library(doParallel)
library(cowplot)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

sv <- function(x, STUD_ids, MTURK_ids, SUD_ids) {
  # Scale by student group scores
  scale_STUD  <- scale(x[STUD_ids])
  # Scale MTURK by student group scores
  scale_MTURK <- (x[MTURK_ids] - attributes(scale_STUD)$`scaled:center`) / attributes(scale_STUD)$`scaled:scale`
  # Scale SUD by student group scores
  scale_SUD   <- (x[SUD_ids] - attributes(scale_STUD)$`scaled:center`) / attributes(scale_STUD)$`scaled:scale`
  c(scale_MTURK, scale_STUD, scale_SUD)
}

m1 <- stan_model("Code/Stan/trait_explanatory_model.stan")

# Read in pre-procesed data
all_dat   <- readRDS(file = paste0("Data/Preprocessed/1_preprocessed_DDT_trait_explanatory_allscales.rds"))
model_dat <- all_dat$survey_dat

# get ids
STUD_ids  <- grep(x = model_dat$ID, pattern = "^ADD")
SUD_ids   <- grep(x = model_dat$ID, pattern = "^TAL")
MTURK_ids <- which(!(seq_along(model_dat$ID) %in% c(STUD_ids, SUD_ids)))

# "base"
dat_m0 <- list(N_subjects = length(model_dat$ID),
               X          = cbind(rep(0, length(model_dat$ID)), 
                                  rep(0, length(model_dat$ID))),
               N_preds = 2)
# "trait_np_s"
dat_m1 <- list(N_subjects = length(model_dat$ID),
               X          = cbind(sv(model_dat$bis_noplan, STUD_ids, MTURK_ids, SUD_ids),
                                  sv(model_dat$stai_s, STUD_ids, MTURK_ids, SUD_ids)),
               N_preds    = 2)
# "trait_mot_s"
dat_m2 <- list(N_subjects = length(model_dat$ID),
               X          = cbind(sv(model_dat$bis_mot, STUD_ids, MTURK_ids, SUD_ids),
                                  sv(model_dat$stai_s, STUD_ids, MTURK_ids, SUD_ids)),
               N_preds    = 2)
# "trait_att_s"
dat_m3 <- list(N_subjects = length(model_dat$ID),
               X          = cbind(sv(model_dat$bis_att, STUD_ids, MTURK_ids, SUD_ids),
                                  sv(model_dat$stai_s, STUD_ids, MTURK_ids, SUD_ids)),
               N_preds    = 2)
# "trait_np_t"
dat_m4 <- list(N_subjects = length(model_dat$ID),
               X          = cbind(sv(model_dat$bis_noplan, STUD_ids, MTURK_ids, SUD_ids),
                                  sv(model_dat$stai_t, STUD_ids, MTURK_ids, SUD_ids)),
               N_preds    = 2)
# "trait_mot_t"
dat_m5 <- list(N_subjects = length(model_dat$ID),
               X          = cbind(sv(model_dat$bis_mot, STUD_ids, MTURK_ids, SUD_ids),
                                  sv(model_dat$stai_t, STUD_ids, MTURK_ids, SUD_ids)),
               N_preds    = 2)
# "trait_att_t"
dat_m6 <- list(N_subjects = length(model_dat$ID),
               X          = cbind(sv(model_dat$bis_att, STUD_ids, MTURK_ids, SUD_ids),
                                  sv(model_dat$stai_t, STUD_ids, MTURK_ids, SUD_ids)),
               N_preds    = 2)

# Append all
dat_m0 <- append(all_dat$ddt_dat, dat_m0)
dat_m1 <- append(all_dat$ddt_dat, dat_m1)
dat_m2 <- append(all_dat$ddt_dat, dat_m2)
dat_m3 <- append(all_dat$ddt_dat, dat_m3)
dat_m4 <- append(all_dat$ddt_dat, dat_m4)
dat_m5 <- append(all_dat$ddt_dat, dat_m5)
dat_m6 <- append(all_dat$ddt_dat, dat_m6)

# For parallel fitting
all_dat <- list(dat_m0, dat_m1, dat_m2, dat_m3, dat_m4, dat_m5, dat_m6)

model_names <- c("base", 
                 "trait_np_s", "trait_mot_s", "trait_att_s",
                 "trait_np_t", "trait_mot_t", "trait_att_t")
seeds <- c(rep(43202, 5), 43201, 43202)
deltas <- c(rep(.8, 5), .95, .8)

cl <- makeCluster(length(model_names))
registerDoParallel(cl)
fits <- foreach(m=seq_along(model_names), .packages = "rstan") %dopar% {
  fit <- sampling(m1, 
                  data    = all_dat[[m]], 
                  iter    = 3500, 
                  warmup  = 500, 
                  chains  = 4, 
                  cores   = 4,
                  seed    = seeds[m],
                  init    = function() {
                    list(beta_k = c(-4.8, .35),
                         beta_a = c(-.86, -.03),
                         sigma = c(1.9, .3),
                         k_pr = rnorm(940, 0, 1),
                         a_pr = rnorm(940, 0, 1))
                  },
                  pars    = c("beta_k", "beta_a", "sigma", "log_lik"),
                  control = list(adapt_delta = deltas[m]))
  saveRDS(fit, file = paste0("Data/Fitted/explanatory_models_", model_names[m], "_allscales.rds"))
  rm(fit)
  model_names[m]
}
stopCluster(cl)
# 
