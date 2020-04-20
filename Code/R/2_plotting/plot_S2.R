library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(foreach)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

# source functions for model comparison
source("Code/R/utils/compare.R")

# All data
datasets <- c("REP", "MTURK", "TAL")

plot_dat <- foreach(d=datasets, .combine = "rbind") %do% {
  
  fits <- readRDS(paste0("Data/Fitted/descriptive_models_", d,".rds"))
  fits_noINT <- readRDS(paste0("Data/Fitted/descriptive_models_", d,"_noINT.rds"))
  
  x <- compare(fits[[1]], fits[[2]], fits_noINT)
  data.frame(Dataset = rep(switch(d,
                                  "REP" = "Student",
                                  "MTURK" = "MTURK",
                                  "TAL" = "SUD"), 3),
             Model = factor(x      = c("Base", "Trait", "No INT"), 
                            levels = c("Base", "Trait", "No INT")),
             LOOIC = x@output$LOO - min(x@output$LOO),
             sem = x@output$dSE)
}

p1 <- ggplot(plot_dat, aes(x = Model, y = LOOIC)) +
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = LOOIC - sem*1, ymax = LOOIC + sem*1), width = 0.2) + 
  geom_hline(yintercept = 0, lty = 2) + 
  ylab("Relative LOOIC") +
  xlab("Model") + 
  facet_wrap("Dataset", ncol = 3) +
  theme_minimal(15) + 
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  coord_flip()
ggsave(p1, filename = "Figures/fig_S2.pdf",
       unit = "in", width = 6, height = 3)
