library(foreach)
library(rstan)
library(loo)
library(dplyr)
library(ggtern)
library(cowplot)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS/")

base <- readRDS("Data/Fitted/explanatory_models_base_testset.rds")
ll_base_test <- extract_log_lik(base, merge_chains = T, parameter_name = "log_lik_test")
rm(base)
trait <- readRDS("Data/Fitted/explanatory_models_trait_testset.rds")
ll_trait_test <- extract_log_lik(trait, merge_chains = T, parameter_name = "log_lik_test")
rm(trait)
trait_incon <- readRDS("Data/Fitted/explanatory_models_trait_incongruent_testset.rds")
trait_incon_test <- extract_log_lik(trait_incon, merge_chains = T, parameter_name = "log_lik_test")
rm(tin)

# Computing log pointwise predictive density (LPPD)
lppd_base <- sum(log(colMeans(exp(ll_base_test))))
lppd_trait <- sum(log(colMeans(exp(ll_trait_test))))
lppd_trait_incon <- sum(log(colMeans(exp(ll_trait_incon_test))))

# Slicing the log-likelihood matrix to get LPPD within subjects
end   <- seq(84, 2940, by = 84)
start <- end - 83
results <- foreach(i=1:35, .combine = "rbind") %do% {
  data.frame(Model   = c("Base", "Trait", "Trait Incongruent"),
             Subject = rep(i, 3),
             LPPD    = c(sum(log(colMeans(exp(ll_base_test[,start[i]:end[i]])))),
                         sum(log(colMeans(exp(ll_trait_test[,start[i]:end[i]])))),
                         sum(log(colMeans(exp(ll_trait_incon_test[,start[i]:end[i]]))))))
}


p1 <- results %>%
  group_by(Subject) %>%
  mutate(LPPD_mu = mean(LPPD)) %>%
  arrange(LPPD_mu) %>%
  ungroup(Subject) %>%
  mutate(Subject = as.numeric(
    factor(Subject, 
           levels = unique(Subject),
           labels = unique(Subject)))) %>%
  ggplot(aes(x = Subject, y = LPPD, color = Model)) +
  geom_path() +
  ggtitle("Test-set (SUD group) model performance") +
  scale_color_manual(values = c("black", "Blue", "Red")) +
  theme_cowplot(font_size = 15)
ggsave(p1, filename = "Figures/fig_S3.pdf",
       unit = "in", height = 5, width = 7, dpi = 300)
