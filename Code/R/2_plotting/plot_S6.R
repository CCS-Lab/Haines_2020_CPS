rm(list=ls())

library(rstan)
library(hBayesDM)
library(dplyr)
library(loo)
library(foreach)
library(cowplot)
library(latex2exp)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

bis <- c("np", "att", "mot")
stai <- c("s", "t")

trait_grid <- expand.grid(bis, stai)

plot_data <- foreach(i=1:nrow(trait_grid), .combine = "rbind") %do% {
  fit <- readRDS(file = paste0("Data/Fitted/explanatory_models_trait_", 
                               trait_grid[i,1], "_", trait_grid[i,2], 
                               "_allscales.rds"))
  pars <- rstan::extract(fit)
  data.frame(Model = rep(paste0(switch(trait_grid[i,1],
                                       "np" = "BIS Nonplanning",
                                       "att" = "BIS Attention",
                                       "motor" = "BIS Motor"),
                                "\n",
                                switch(trait_grid[i,2],
                                       "s" = "STAI State",
                                       "t" = "STAI Trait")), 2),
             Scale = c(switch(trait_grid[i,1],
                              "np" = "BIS Nonplanning",
                              "att" = "BIS Attention",
                              "motor" = "BIS Motor"),
                       switch(trait_grid[i,2],
                              "s" = "STAI State",
                              "t" = "STAI Trait")),
             Parameter = factor(c("k", "a"),
                                levels = c("a", "k"),
                                labels = c("a", "k")),
             y_mean = c(mean(pars$beta_k[,2]),
                        mean(pars$beta_a[,2])),
             y_min = c(HDIofMCMC(pars$beta_k[,2], credMass = .99)[1],
                       HDIofMCMC(pars$beta_a[,2], credMass = .99)[1]),
             y_max = c(HDIofMCMC(pars$beta_k[,2], credMass = .99)[2],
                       HDIofMCMC(pars$beta_a[,2], credMass = .99)[2]))
}

p1 <- plot_data %>%
  mutate(overlap = ifelse(sign(sign(y_min)*sign(y_max))==1, "0", "1")) %>%
  ggplot(aes(x = Parameter, y = y_mean, color = overlap)) +
  geom_hline(yintercept = 0, linetype = 2, color = I("red")) +
  geom_point(fill = I("black")) +
  geom_errorbar(aes(ymin = y_min, ymax = y_max, color = overlap), width = .2) +
  scale_color_manual(values = c("black", "lightgray")) +
  facet_wrap(c("Model"), ncol = 3) +
  ylab("Posterior Estimate") +
  theme_minimal(base_size = 15) +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  coord_flip()
  
ggsave(p1, filename = "Figures/fig_S6.pdf", 
       units = "in", dpi = 300, height = 3.5, width = 6)
