library(dplyr)
library(tidyr)
library(foreach)
library(ggplot2)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

# Softmax function 
logsumexp <- function (x) {
  y <- max(x)
  y + log(sum(exp(x - y)))
}
softmax <- function (x) {
  exp(x - logsumexp(x))
}

amount_dis <- function(x, alpha) {
  x^alpha
}
delay_dis <- function(x, k) {
  1 / (1 + k*x)
}

# Real data ll-delay and ss-amount quantiles
dat <- readRDS("Data/Preprocessed/1_preprocessed_DDT_trait_explanatory.rds")
dat <- dat$ddt_dat

# Range of values for plotting
amount_vals <- seq(0, 800, length.out = 100)
delay_vals  <- seq(0, 520, length.out = 100)
alpha_vals <- seq(.3, .7, length.out = 10)
k_vals     <- seq(0.005, .1, length.out = 10)

plot_dat <- foreach(p=seq_along(alpha_vals), .combine = "rbind") %do% {
  data.frame(alpha       = rep(alpha_vals[p], length(amount_vals)),
             amount_true = amount_vals,
             amount_subj = amount_dis(amount_vals, alpha_vals[p]),
             delay_true  = delay_vals,
             delay_subj  = delay_dis(delay_vals, k_vals[p]),
             k           = rep(k_vals[p], length(delay_vals)))  
}

p1 <- plot_dat %>%
  ggplot(aes(x = amount_true, y = amount_subj, group = alpha, color = alpha)) +
  geom_path() +
  scale_color_continuous(expression(alpha), low = "gray", high = "#1810b0") +
  xlab("True Amount") +
  ylab("Subjective Amount") +
  theme_cowplot()

p2 <- plot_dat %>%
  ggplot(aes(x = delay_true, y = delay_subj, group = k, color = k)) +
  geom_path() +
  scale_color_continuous(expression(italic(k)), low = "gray", high = "#b0101d") +
  xlab("Delay") +
  ylab("Discounting Factor") +
  theme_cowplot()

p3 <- plot_grid(p1, p2, ncol = 2)

ggsave(p3, filename = "Figures/fig_1a.pdf",
       dpi = 300, height = 3, width = 8, unit = "in")
