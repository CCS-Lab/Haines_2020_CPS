library(dplyr)
library(rstan)
library(tidyr)
library(foreach)
library(ggplot2)
library(ggnewscale)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

sv <- function(x) {
  scale_REP <- scale(x[801:932])
  scale_mturk   <- (x[1:800] - attributes(scale_REP)$`scaled:center`) / attributes(scale_REP)$`scaled:scale`
  scale_TAL   <- (x[933:967] - attributes(scale_REP)$`scaled:center`) / attributes(scale_REP)$`scaled:scale`
  c(scale_mturk, scale_REP, scale_TAL)
}

# Softmax function 
logsumexp <- function (x) {
  y <- max(x)
  y + log(sum(exp(x - y)))
}
softmax <- function (x) {
  exp(x - logsumexp(x))
}

dd_hyper_dim <- function(ss_amount, ss_delay, ll_amount, ll_delay, a, k, c) {
  vals <- c(ss_amount^a / (1 + k*ss_delay),
            ll_amount^a / (1 + k*ll_delay))
  softmax(vals * c)
}
dd_exp_dim <- function(ss_amount, ss_delay, ll_amount, ll_delay, a, k, c) {
  vals <- c(ss_amount^a * exp(-1 * k *ss_delay),
            ll_amount^a * exp(-1 * k *ll_delay))
  softmax(vals * c)
}

# Real data ll-delay and ss-amount quantiles
dat <- readRDS("Data/Preprocessed/1_preprocessed_DDT_trait_explanatory.rds")
dat <- dat$ddt_dat
# Rank most common dd pairs for plotting purposes
dd_pairs <- data.frame(ll_delay = dat$delay_later[dat$delay_later!=0],
                       ss_amount = dat$amount_sooner[dat$amount_sooner!=0]) %>%
  count(ll_delay, ss_amount) %>%
  arrange(n) %>%
  mutate(ss_delay = 0, 
         ll_amount = 800)


# Parameter grid
par_grid <- expand.grid(k = seq(.0025, .03, length.out = 500), 
                        a = seq(.2, .6, length.out = 500))
par_grid$c <- 1

# Problem grid
prob_set <- NULL
prob_set$ss_amount <- c(750,750, 750, 750)
prob_set$ss_delay <- c(0,0, 0, 0)
prob_set$ll_amount <- c(800, 800, 800, 800)
prob_set$ll_delay <- c(1, 2, 4, 6)
prob_set <- as.data.frame(prob_set)

plot_dat <- foreach(p=1:nrow(prob_set), .combine = "rbind") %do% {
  foreach(i=1:nrow(par_grid), .combine = "rbind") %do% {
    prs <- dd_hyper_dim(prob_set$ss_amount[p], 
                        prob_set$ss_delay[p],
                        prob_set$ll_amount[p], 
                        prob_set$ll_delay[p],
                        par_grid$a[i], 
                        par_grid$k[i], 
                        par_grid$c[i])
    data.frame(problem = paste0("$", round(prob_set$ss_amount[p], 1),
                                " Now or\n", 
                                "$", prob_set$ll_amount[p],
                                " in ", 
                                round(prob_set$ll_delay[p], 1),
                                " week(s)"),
               pr_SS = prs[1],
               pr_LL = prs[2],
               a = par_grid$a[i],
               k = par_grid$k[i],
               c = par_grid$c[i])
  }
}

# Explanatory model 
fit <- readRDS("Data/Fitted/explanatory_models_trait.rds")
pars <- rstan::extract(fit)
beta_a0 <- mean(pars$beta_a[,1])
beta_a1 <- mean(pars$beta_a[,2])
beta_k0 <- mean(pars$beta_k[,1])
beta_k1 <- mean(pars$beta_k[,2])

# Survey responses
all_dat   <- readRDS(file = paste0("Data/Preprocessed/1_preprocessed_DDT_trait_explanatory.rds"))
model_dat <- all_dat$survey_dat

# Standardized trait measure for each group
stai_stud  <- sv(model_dat$stai_s)[801:932]
stai_mturk <- sv(model_dat$stai_s)[1:800]
stai_sud   <- sv(model_dat$stai_s)[933:967]
bis_stud   <- sv(model_dat$bis_noplan)[801:932]
bis_mturk  <- sv(model_dat$bis_noplan)[1:800]
bis_sud    <- sv(model_dat$bis_noplan)[933:967]

plot_pars <- data.frame(mu_a = c(exp(beta_a0 + mean(stai_stud) * beta_a1), 
                                 exp(beta_a0 + mean(stai_mturk) * beta_a1),
                                 exp(beta_a0 + mean(stai_sud) * beta_a1)),
                        mu_k = c(exp(beta_k0 + mean(bis_stud) * beta_k1), 
                                 exp(beta_k0 + mean(bis_mturk) * beta_k1),
                                 exp(beta_k0 + mean(bis_sud) * beta_k1)),
                        low_a = c(exp(beta_a0 + quantile(stai_stud, probs = .95) * beta_a1), 
                                  exp(beta_a0 + quantile(stai_mturk, probs = .95) * beta_a1),
                                  exp(beta_a0 + quantile(stai_sud, probs = .95) * beta_a1)),
                        high_a = c(exp(beta_a0 + quantile(stai_stud, probs = .05) * beta_a1), 
                                   exp(beta_a0 + quantile(stai_mturk, probs = .05) * beta_a1),
                                   exp(beta_a0 + quantile(stai_sud, probs = .05) * beta_a1)),
                        low_k = c(exp(beta_k0 + quantile(bis_stud, probs = .05) * beta_k1), 
                                  exp(beta_k0 + quantile(bis_mturk, probs = .05) * beta_k1),
                                  exp(beta_k0 + quantile(bis_sud, probs = .05) * beta_k1)),
                        high_k = c(exp(beta_k0 + quantile(bis_stud, probs = .95) * beta_k1), 
                                   exp(beta_k0 + quantile(bis_mturk, probs = .95) * beta_k1),
                                   exp(beta_k0 + quantile(bis_sud, probs = .95) * beta_k1)),
                        Group = factor(c("Student", "MTURK", "SUD"),
                                       levels = c("Student", "MTURK", "SUD"),
                                       labels = c("Student", "MTURK", "SUD")))


p1 <- ggplot(plot_dat, aes(x = a, y = k, fill = pr_LL, color = pr_LL)) +
  geom_tile(stat = "identity") +
  scale_fill_gradient2("Pr(LL)", low = "#b0101d", mid = "white", high = "#1810b0", midpoint = .5, limits = c(0,1)) +
  scale_color_gradient2("Pr(LL)", low = "#b0101d", mid = "white", high = "#1810b0", midpoint = .5, limits = c(0,1)) +
  new_scale("color") +
  geom_point(data = plot_pars, aes(x = mu_a, y = mu_k, color = Group, shape = Group), inherit.aes = F) +
  geom_errorbarh(data = plot_pars, aes(y = mu_k, xmin = low_a, xmax = high_a, color = Group), inherit.aes = F) +
  geom_errorbar(data = plot_pars, aes(x = mu_a, ymin = low_k, ymax = high_k, color = Group), inherit.aes = F) +
  scale_color_manual(values = c("Black", "#737373", "#d9d9d9")) +
  xlab(expression(alpha)) +
  ylab(expression(italic(k))) +
  facet_wrap("problem", ncol = 4) +
  theme_cowplot(font_size = 15) +
  theme(strip.background = element_blank(),
        plot.title = element_text(hjust = .5))

ggsave(p1, filename = "Figures/fig_3b.pdf",
       dpi = 300, height = 3, width = 10, unit = "in")
