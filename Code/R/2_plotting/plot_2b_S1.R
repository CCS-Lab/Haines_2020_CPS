rm(list=ls())

library(rstan)
library(hBayesDM)
library(dplyr)
library(loo)
library(foreach)
library(cowplot)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

datasets <- c("MTURK", "REP", "TAL")

plot_dat <- foreach(d=datasets, .combine = "rbind") %do% {
  # Read in data
  fits <- readRDS(file = paste0("Data/Fitted/descriptive_models_", d,".rds"))
  pars1 <- rstan::extract(fits[[1]]) # base model
  pars2 <- rstan::extract(fits[[2]]) # trait model
  data.frame(Dataset = rep(switch(d,
                                  "MTURK" = "MTURK",
                                  "REP" = "Student",
                                  "TAL" = "SUD"), length(pars1$mu_p[,1]) * 2),
             Model   = c(rep("Base", length(pars1$mu_p[,1])), 
                         rep("Trait", length(pars1$mu_p[,1]))),
             k       = c(pars1$mu_p[,1], pars2$beta[,1]),
             BIS     = c(rep(NA, length(pars1$mu_p[,1])), pars2$beta[,2]),
             STAI    = c(rep(NA, length(pars1$mu_p[,1])), pars2$beta[,3]),
             INT     = c(rep(NA, length(pars1$mu_p[,1])), pars2$beta[,4]),
             sigma_k = c(pars1$sigma[,1], pars2$sigma[,1]))
} %>% 
  mutate(Dataset = factor(Dataset, 
                          levels = c("Student", "MTURK", "SUD"),
                          labels = c("Student", "MTURK", "SUD")))

base_k <- ggplot(plot_dat %>% filter(Model == "Base"), 
                 aes(x = k, color = Dataset, fill = Dataset)) +
  geom_density(alpha = .8) +
  scale_color_manual(values = c("#1810b0", "gray", "#b0101d")) +
  scale_fill_manual(values = c("#1810b0", "gray", "#b0101d")) +
  xlab("Log Discounting Rate (k)") +
  ylab("Posterior Density") +
  theme_cowplot(font_size = 18)
ggsave(base_k, filename = "Figures/fig_2b.pdf", 
       units = "in", dpi = 300, width = 6.5, height = 4)

bis_stu <- plot_dat %>% 
  filter(Model == "Trait" & Dataset == "Student") %>% 
  select(BIS) %>%
  t() %>%
  HDIofMCMC()
bis_mtu <- plot_dat %>% 
  filter(Model == "Trait" & Dataset == "MTURK") %>% 
  select(BIS) %>%
  t() %>%
  HDIofMCMC()
bis_sud <- plot_dat %>% 
  filter(Model == "Trait" & Dataset == "SUD") %>% 
  select(BIS) %>%
  t() %>%
  HDIofMCMC()
bis_plot <- ggplot(plot_dat %>% filter(Model == "Trait"), aes(x = BIS, fill = Dataset, color = Dataset)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_density(alpha = .5) +
  geom_segment(aes(x = bis_stu[1], xend = bis_stu[2], y = -.1, yend = -.1), color = I("#1810b0"), size = 1.5, alpha = .7) +
  geom_segment(aes(x = bis_mtu[1], xend = bis_mtu[2], y = -.2, yend = -.2), color = I("gray"), size = 1.5, alpha = .7) +
  geom_segment(aes(x = bis_sud[1], xend = bis_sud[2], y = -.3, yend = -.3), color = I("#b0101d"), size = 1.5, alpha = .7) +
  scale_color_manual(values = c("#1810b0", "gray", "#b0101d")) +
  scale_fill_manual(values = c("#1810b0", "gray", "#b0101d")) +
  xlab("BIS-NP") +
  ylab("Posterior Density") +
  theme_cowplot(font_size = 18) +
  theme(legend.position = "none")
ggsave(bis_plot, filename = "Figures/fig_S1a.pdf", 
       units = "in", dpi = 300, height = 4, width = 3.53)


stai_stu <- plot_dat %>% 
  filter(Model == "Trait" & Dataset == "Student") %>% 
  select(STAI) %>%
  t() %>%
  HDIofMCMC()
stai_mtu <- plot_dat %>% 
  filter(Model == "Trait" & Dataset == "MTURK") %>% 
  select(STAI) %>%
  t() %>%
  HDIofMCMC()
stai_sud <- plot_dat %>% 
  filter(Model == "Trait" & Dataset == "SUD") %>% 
  select(STAI) %>%
  t() %>%
  HDIofMCMC()
stai_plot <- ggplot(plot_dat %>% filter(Model == "Trait"), aes(x = STAI, fill = Dataset, color = Dataset)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_density(alpha = .5) +
  geom_segment(aes(x = stai_stu[1], xend = stai_stu[2], y = -.1, yend = -.1), color = I("#1810b0"), size = 1.5, alpha = .7) +
  geom_segment(aes(x = stai_mtu[1], xend = stai_mtu[2], y = -.2, yend = -.2), color = I("gray"), size = 1.5, alpha = .7) +
  geom_segment(aes(x = stai_sud[1], xend = stai_sud[2], y = -.3, yend = -.3), color = I("#b0101d"), size = 1.5, alpha = .7) +
  scale_color_manual(values = c("#1810b0", "gray", "#b0101d")) +
  scale_fill_manual(values = c("#1810b0", "gray", "#b0101d")) +
  xlab("STAI-S") +
  ylab("") +
  theme_cowplot(font_size = 18) +
  theme(legend.position = "none")
ggsave(stai_plot, filename = "Figures/fig_S1b.pdf", 
       units = "in", dpi = 300, height = 4, width = 3.53)

# Interaction plot
int_stu <- plot_dat %>% 
  filter(Model == "Trait" & Dataset == "Student") %>% 
  select(INT) %>%
  t() %>%
  HDIofMCMC()
int_mtu <- plot_dat %>% 
  filter(Model == "Trait" & Dataset == "MTURK") %>% 
  select(INT) %>%
  t() %>%
  HDIofMCMC()
int_sud <- plot_dat %>% 
  filter(Model == "Trait" & Dataset == "SUD") %>% 
  select(INT) %>%
  t() %>%
  HDIofMCMC()
int_plot <- ggplot(plot_dat %>% filter(Model == "Trait"), aes(x = INT, fill = Dataset, color = Dataset)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_density(alpha = .5) +
  geom_segment(aes(x = int_stu[1], xend = int_stu[2], y = -.1, yend = -.1), color = I("#1810b0"), size = 1.5, alpha = .7) +
  geom_segment(aes(x = int_mtu[1], xend = int_mtu[2], y = -.2, yend = -.2), color = I("gray"), size = 1.5, alpha = .7) +
  geom_segment(aes(x = int_sud[1], xend = int_sud[2], y = -.3, yend = -.3), color = I("#b0101d"), size = 1.5, alpha = .7) +
  scale_color_manual(values = c("#1810b0", "gray", "#b0101d")) +
  scale_fill_manual(values = c("#1810b0", "gray", "#b0101d")) +
  xlab("BIS-NP * STAI-S") +
  ylab("") +
  theme_cowplot(font_size = 18)
ggsave(int_plot, filename = "Figures/fig_S1c.pdf", 
       units = "in", dpi = 300, height = 4, width = 4.94)

eff_plot <- plot_grid(bis_plot, stai_plot, int_plot, ncol = 3, rel_widths = c(1, 1, 1.4))
ggsave(eff_plot, filename = "Figures/fig_S1.pdf", 
       units = "in", height = 4, width = 12)
