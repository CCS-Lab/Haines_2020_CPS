rm(list=ls())

library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(foreach)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

# All data
datasets <- c("REP", "MTURK", "TAL")

# Functions
# For plotting HDI (adapted from hBayesDM)
HDIofMCMC <- function (sampleVec, credMass = 0.80) {
  sortedPts = sort(sampleVec)
  ciIdxInc = floor(credMass * length(sortedPts))
  nCIs = length(sortedPts) - ciIdxInc
  ciWidth = rep(0, nCIs)
  for (i in 1:nCIs) {
    ciWidth[i] = sortedPts[i + ciIdxInc] - sortedPts[i]
  }
  HDImin = sortedPts[which.min(ciWidth)]
  HDImax = sortedPts[which.min(ciWidth) + ciIdxInc]
  HDIlim = c(y=mean(sampleVec), ymin=HDImin, ymax=HDImax)
  return(HDIlim)
}

all_dat <- NULL
for (d in datasets) {
  
  dat <- readRDS(paste0("Data/Fitted/descriptive_models_", d,".rds"))
  
  pars <- rstan::extract(dat[[2]]); rm(dat) # extract the trait version
  
  range <- c(-2.0, 0, 2.0)
  
  all_combo <- expand.grid(range, range); names(all_combo) <- c("bis", "stai")
  all_combo[["interact"]] <- with(all_combo, bis * stai)
  
  # Beta weights from regression model
  betas <- pars$beta
  
  # Compute proportional loss
  est_k <- foreach(j=1:nrow(all_combo), .combine = "rbind") %do% {
    foreach(i=seq_along(betas[,1]), .combine = "c") %do% {
      tmp_k <- betas[i,1] + betas[i,2] * all_combo[j, "bis"] + 
        betas[i,3] * all_combo[j, "stai"] +
        betas[i,4] * all_combo[j, "interact"]
      exp(tmp_k)
    }
  }
  
  rownames(est_k) <- 
    c("Low BIS\nLow STAI", "Med BIS\nLow STAI", "High BIS\nLow STAI",
      "Low BIS\nMed STAI", "Med BIS\nMed STAI", "High BIS\nMed STAI",
      "Low BIS\nHigh STAI", "Med BIS\nHigh STAI", "High BIS\nHigh STAI")
  
  prop_gathered <- est_k %>%
    t %>% 
    as.data.frame %>%
    gather(key = "Condition", value = "k")
  
  new_dat <- prop_gathered %>% 
    mutate(bis = ifelse(substr(Condition,1,3)=="Low", -2, +
                          ifelse(substr(Condition,1,3)=="Med", 0, 2)))
  new_dat$stai <- foreach(i=1:dim(prop_gathered)[1], .combine = "c") %do% {
    ifelse(substr(strsplit(prop_gathered$Condition[i], split = "\n")[[1]][2],1,3)=="Low", -2, +
             ifelse(substr(strsplit(prop_gathered$Condition[i], split = "\n")[[1]][2],1,3)=="Med", 0, 2))
  }
  
  plot_dat <- new_dat %>% 
    group_by(stai, bis) %>% 
    summarize(mu = hBayesDM::estimate_mode(k), # showing modes of posterior predictions
              ymin = HDIofMCMC(k)[2],
              ymax = HDIofMCMC(k)[3], 
              Dataset = d)
  all_dat <- rbind(all_dat, plot_dat)
}
all_dat$Dataset <- factor(all_dat$Dataset, 
                          levels = c("REP", "MTURK", "TAL"),
                          labels = c("Student", "MTURK", "SUD"))
all_dat$data_bis <- interaction(all_dat$Dataset, all_dat$bis)
all_dat$data_bis <- factor(all_dat$data_bis,
                           levels = c("Student.-2", "Student.0", "Student.2", "MTURK.-2", 
                                      "MTURK.0", "MTURK.2", "SUD.-2", "SUD.0", "SUD.2"),
                           labels = c("Student.-2", "Student.0", "Student.2", "MTURK.-2", 
                                      "MTURK.0", "MTURK.2", "SUD.-2", "SUD.0", "SUD.2"))

int_plot <- ggplot(all_dat %>% 
         mutate(mu = log(mu),
                ymin = log(ymin),
                ymax = log(ymax)), aes(x = stai, y = mu, color = as.factor(data_bis))) + 
  geom_path(position = position_dodge(.1), size = 1.5) + 
  geom_point(position = position_dodge(.1), size = 3, aes(shape = as.factor(bis))) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .2,
                position = position_dodge(.1), size = 1) +
  xlab("STAI-S (SD)") +
  ylab("Model Predicted\nLog Discounting Rate (k)") +
  facet_wrap(c("Dataset"), ncol = 3) +
  scale_color_manual(values = c("#c5c2ff", "#706bc9", "#1f1b6e", 
                                "#d2d2d4", "#9e9e9e", "#3d3d3d",
                                "#ffc4c9", "#ff8f97", "#800d15")) +
  theme_cowplot(font_size = 18) +
  theme(strip.background = element_blank(),
        legend.position = "none")
ggsave(int_plot, filename = "Figures/fig_3a.pdf",
       units = "in", dpi = 300, height = 4, width = 10)
