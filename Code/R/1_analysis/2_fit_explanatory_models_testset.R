library(rstan)
library(hBayesDM)
library(foreach)
library(doParallel)

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
m1 <- stan_model("Code/Stan/trait_explanatory_model_testset.stan")

# Keep mturk data preprocessed by Andrew
all_dat   <- readRDS(file = paste0("Data/Preprocessed/1_preprocessed_DDT_trait_explanatory_testset.rds"))
model_dat <- all_dat$survey_dat
test_ids <- all_dat$test_ids

# Data for base model
dat_m0 <- list(X_train = cbind(rep(0, length(model_dat$ID[-test_ids])),
                               rep(0, length(model_dat$ID[-test_ids]))),
               X_test  = cbind(rep(0, length(test_ids)),
                               rep(0, length(test_ids))),
               N_preds = 2)
# Data for trait model
dat_m1 <- list(X_train = cbind(sv(model_dat$bis_noplan)[-test_ids],
                               sv(model_dat$stai_s)[-test_ids]),
               X_test  = cbind(sv(model_dat$bis_noplan)[test_ids],
                               sv(model_dat$stai_s)[test_ids]),
               N_preds = 2)
# Data for trait incongruent model
dat_m2 <- list(X_train = cbind(sv(model_dat$stai_s)[-test_ids],
                               sv(model_dat$bis_noplan)[-test_ids]),
               X_test  = cbind(sv(model_dat$stai_s)[test_ids],
                               sv(model_dat$bis_noplan)[test_ids]),
               N_preds = 2)

dat_m0 <- append(all_dat$ddt_dat, dat_m0)
dat_m1 <- append(all_dat$ddt_dat, dat_m1)
dat_m2 <- append(all_dat$ddt_dat, dat_m2)

# For parallel fitting
all_m   <- list(m1, m1, m1) 
all_dat <- list(dat_m0, dat_m1, dat_m2)
model_names <- c("base", "trait", "trait_incongruent")

# Initialize parameter estimates for trouble fitting the trait congruent model (convergence problems)
inits <- function() {
  list(beta_a = c(-.86, -.03),
       beta_k = c(-4.8, .35),
       sigma = c(1.9, .34),
       k_pr = rnorm(932, 0, .1),
       a_pr = rnorm(932, 0, .1))
}

cl <- makeCluster(length(model_names))
registerDoParallel(cl)
fits <- foreach(m=seq_along(model_names), .packages = "rstan") %dopar% {
  fit <- sampling(all_m[[m]], 
           data   = all_dat[[m]], 
           init   = inits,
           iter   = 4500, 
           warmup = 1500, 
           chains = 8, 
           cores  = 8,
           seed   = 43202)
  saveRDS(fit, file = paste0("Data/Fitted/explanatory_models_", model_names[m], "_testset.rds"))
  rm(fit)
  model_names[m]
}
stopCluster(cl)

# 
