library(dplyr)
library(tidyr)
library(foreach)
library(ggplot2)
library(cowplot)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

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
par_grid <- expand.grid(k = seq(.005, .05, length.out = 500), 
                        a = seq(0, 1, length.out = 500))
par_grid$c <- 1

# Problem grid
prob_set <- NULL
prob_set$ss_amount <- 750
prob_set$ss_delay <- 0
prob_set$ll_amount <- 800
prob_set$ll_delay <- 2
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
    data.frame(problem = paste0(round(prob_set$ss_amount[p], 1),
                                " in ", 
                                prob_set$ss_delay[p],
                                " OR ",
                                prob_set$ll_amount[p],
                                " in ", 
                                round(prob_set$ll_delay[p], 1)),
               pr_SS = prs[1],
               pr_LL = prs[2],
               a = par_grid$a[i],
               k = par_grid$k[i],
               c = par_grid$c[i])
  }
}

p1 <- ggplot(plot_dat, aes(x = a, y = k, fill = pr_LL, color = pr_LL)) +
  geom_tile(stat = "identity") +
  scale_fill_gradient2("Pr(LL)", low = "#b0101d", mid = "white", high = "#1810b0", midpoint = .5, limits = c(0,1)) +
  scale_color_gradient2("Pr(LL)", low = "#b0101d", mid = "white", high = "#1810b0", midpoint = .5, limits = c(0,1)) +
  ggtitle("$750 Now or $800 in 4 Weeks") +
  xlab(expression(alpha)) +
  ylab(expression(italic(k))) +
  theme_cowplot(font_size = 15) +
  theme(strip.background = element_blank(),
        plot.title = element_text(hjust = .5))

ggsave(p1, filename = "Figures/fig_1b.pdf",
       dpi = 300, height = 4, width = 7, unit = "in")
