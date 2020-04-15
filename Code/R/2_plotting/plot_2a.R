rm(list=ls())

library(rstan)
library(hBayesDM)
library(dplyr)
library(loo)
library(foreach)
library(cowplot)
library(ggpubr)
library(BEST)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

datasets <- c("MTURK", "REP", "TAL")

plot_dat <- foreach(d=datasets, .combine = "rbind") %do% {
  # Read in data
  all_dat <- readRDS(file = paste0("Data/Preprocessed/1_preprocessed_DDT_", d, ".rds"))
  
  data.frame(Dataset = rep(switch(d,
                                  "MTURK" = "MTURK",
                                  "REP" = "Student",
                                  "TAL" = "SUD"), 
                           nrow(all_dat$survey_dat)),
             alpha   = ifelse(d == "MTURK", "light", "dark"),
             BIS     = all_dat$survey_dat$bis_noplan,
             STAI    = all_dat$survey_dat$stai_s,
             INT     = all_dat$survey_dat$bis_noplan*all_dat$survey_dat$stai_s)
} %>% 
  mutate(Dataset = factor(Dataset, 
                          levels = c("Student", "MTURK", "SUD"),
                          labels = c("Student", "MTURK", "SUD")))

# Frequentist
with(plot_dat, t.test(BIS[Dataset=="Student"], BIS[Dataset=="MTURK"]))
with(plot_dat, t.test(BIS[Dataset=="Student"], BIS[Dataset=="SUD"]))
with(plot_dat, t.test(BIS[Dataset=="MTURK"], BIS[Dataset=="SUD"]))
with(plot_dat, t.test(STAI[Dataset=="Student"], STAI[Dataset=="MTURK"]))
with(plot_dat, t.test(STAI[Dataset=="Student"], STAI[Dataset=="SUD"]))
with(plot_dat, t.test(STAI[Dataset=="MTURK"], STAI[Dataset=="SUD"]))
# Bayes
imp1 <- with(plot_dat, BESTmcmc(BIS[Dataset=="Student"], BIS[Dataset=="MTURK"], priors=NULL, parallel=FALSE))
round(HDIofMCMC(imp1$mu1 - imp1$mu2), 2)
imp2 <- with(plot_dat, BESTmcmc(BIS[Dataset=="Student"], BIS[Dataset=="SUD"], priors=NULL, parallel=FALSE))
round(HDIofMCMC(imp2$mu1 - imp2$mu2), 2)
imp3 <- with(plot_dat, BESTmcmc(BIS[Dataset=="MTURK"], BIS[Dataset=="SUD"], priors=NULL, parallel=FALSE))
round(HDIofMCMC(imp3$mu1 - imp3$mu2), 2)
anx1 <- with(plot_dat, BESTmcmc(STAI[Dataset=="Student"], STAI[Dataset=="MTURK"], priors=NULL, parallel=FALSE))
round(HDIofMCMC(anx1$mu1 - anx1$mu2), 2)
anx2 <- with(plot_dat, BESTmcmc(STAI[Dataset=="Student"], STAI[Dataset=="SUD"], priors=NULL, parallel=FALSE))
round(HDIofMCMC(anx2$mu1 - anx2$mu2), 2)
anx3 <- with(plot_dat, BESTmcmc(STAI[Dataset=="MTURK"], STAI[Dataset=="SUD"], priors=NULL, parallel=FALSE))
round(HDIofMCMC(anx3$mu1 - anx3$mu2), 2)

# Main plot
pmain <- ggplot(plot_dat, aes(x = BIS, y = STAI, color = Dataset, shape = Dataset, alpha = alpha)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#1810b0", "gray", "#b0101d")) +
  scale_alpha_manual(values = c(.3, .7), guide = F) +
  # geom_point(data = plot_dat %>% filter(Dataset == "MTURK"), alpha = .1, size = 2) +
  xlab("BIS-NP") +
  ylab("STAI-S") +
  theme_cowplot(font_size = 18)
# Marginal densities along x axis
xdens <- axis_canvas(pmain, axis = "x") +
  geom_density(data = plot_dat, aes(x = BIS, fill = Dataset), 
               alpha = 0.7, size = .2) +
  scale_fill_manual(values = c("#1810b0", "gray", "#b0101d"))
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE) +
  geom_density(data = plot_dat, aes(x = STAI, fill = Dataset), 
               alpha = 0.7, size = .2) +
  scale_fill_manual(values = c("#1810b0", "gray", "#b0101d")) +
  coord_flip()
p1 <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
bi_plot <- ggdraw(p2)

with(plot_dat, cor.test(BIS[Dataset=="Student"], STAI[Dataset=="Student"]))
with(plot_dat, cor.test(BIS[Dataset=="MTURK"], STAI[Dataset=="MTURK"]))
with(plot_dat, cor.test(BIS[Dataset=="SUD"], STAI[Dataset=="SUD"]))
ggsave(bi_plot, filename = "Data/Figures/fig_2a.pdf",
       units = "in", dpi = 300, width = 6.5, height = 4.8)
