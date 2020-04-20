library(rstan)
library(hBayesDM)
library(foreach)
library(cowplot)
library(latex2exp)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

# Read in data
fit <- readRDS(file = "Data/Fitted/explanatory_models_trait.rds")
pars <- rstan::extract(fit)
plot_data <- data.frame(BIS  = pars$beta_k[,2],
                        STAI = pars$beta_a[,2])

bis_hdi  <- HDIofMCMC(plot_data$BIS)
stai_hdi <- HDIofMCMC(plot_data$STAI)

bis_plot <- ggplot(plot_data, aes(x = BIS)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_density(fill = I("gray"), alpha = .5) +
  geom_segment(aes(x = bis_hdi[1], xend = bis_hdi[2], y = 0, yend = 0), 
               color = I("black"), size = 1.5, alpha = .7) +
  xlab(TeX("BIS-NP ($\\beta_{k_{1}})")) +
  ylab("Posterior Density") +
  theme_cowplot(font_size = 18) +
  theme(legend.position = "none")

stai_plot <- ggplot(plot_data, aes(x = STAI)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_density(fill = I("gray"), alpha = .5) +
  geom_segment(aes(x = stai_hdi[1], xend = stai_hdi[2], y = 0, yend = 0), 
               color = I("black"), size = 1.5, alpha = .7) +
  xlab(TeX("STAI-S ($\\beta_{\\alpha_{1}}$)")) +
  ylab("Posterior Density") +
  theme_cowplot(font_size = 18) +
  theme(legend.position = "none")

eff_plot <- plot_grid(bis_plot, stai_plot, ncol = 2)
ggsave(eff_plot, filename = "Figures/fig_S4.pdf", 
       units = "in", height = 4, width = 8)
