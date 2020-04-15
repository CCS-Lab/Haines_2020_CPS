rm(list=ls())

library(stringr)
library(dplyr)
library(tidyr)
library(foreach)

setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS")

fac_to_num <- function(x) as.numeric(as.character(x))

datasets <- c("MTURK", "REP", "TAL")

bis_reverse <- c(1, 7, 8, 9, 10, 12, 13, 15, 20, 29, 30)
stai_reverse <- c(1, 2, 5, 8, 10, 11, 15, 16, 19, 20, 21, 
                  26, 27, 30, 33, 34, 36, 39)

# BIS subscales
att <- c(5,6,9,11,20,24,26,28)
mot <- c(2,3,4,16,17,19,22,23,25,30)
noplan <- c(1,7,8,10,12,13,14,15,18,27,29)

results <- foreach(d=datasets, .combine = "rbind") %do% {
  tmp_dat <- read.csv(paste0("Data/Survey/", d, ".csv"), header = T) %>%
    mutate(ID = substr(gsub(x = SubjID, pattern = " ", replacement = ""), 1, 8)) %>%
    mutate(Group = d,
           age   = ifelse(Group=="MTURK", Age, Dem_2 + 7),
           sex   = ifelse(Group=="MTURK", Sex - 1, 2 - Dem_1)) %>%
    select(-SubjID)
  if (d != "MTURK") {
    tmp_dat %>%
      select(Group, ID, age, sex, contains("bis"), 
             contains("stai"), contains("audit"), 
             paste0("DAST_", c(1, 3, 5, 6, 7, 8, 11, 15, 17, 18))) %>% # DAST-10 questions from full 20 question scale
      rename_at(vars(contains("DAST")), ~ paste0("DAST_", 1:10)) %>% # renaming to match DAST-10 
      mutate_at(vars(paste0("DAST_", 1:10)), function(x) 2 - x) %>% # Qualtrics codes 1 == Yes, 2 == No. here reversing this
      mutate_at(vars(paste0("AUDIT_", 1:8)), function(x) x - 1) %>%
      mutate_at(vars(paste0("AUDIT_", 9:10)), function(x) (x - 1)*2) %>%
      mutate(DAST_3  = (1 - DAST_3)) %>% # reverse scored question
      select(Group, ID, age, sex, contains("bis"), 
             contains("stai"), contains("audit"), contains("dast"))
  } else {
    tmp_dat %>%
      mutate(att_1 = ifelse(attn_1==0, 1, 0),
             att_3 = ifelse(attn_3==1, 1, 0),
             att_5 = ifelse(attn_5==1, 1, 0),
             att_6 = ifelse(attn_6==0, 1, 0)) %>%
      mutate(att_pass = select(., paste0("att_", c(1, 3, 5, 6))) %>% 
               rowSums(., na.rm = T)) %>%
      filter(att_pass >= 3) %>%
      mutate(AUDIT_10 = (AUDIT_10 - 1) * 2,
             DAST_3 = (1 - DAST_3)) %>%
      select(Group, ID, age, sex, contains("bis"), 
             contains("stai"), contains("audit"), contains("dast"))
  }
} %>%
  mutate_at(vars(paste0("BIS_", bis_reverse)), function(x) 5 - x) %>%
  mutate_at(vars(paste0("STAI_", stai_reverse)), function(x) 5 - x) %>%
  mutate(DAST = select(., contains("DAST")) %>% rowSums(.), 
         AUDIT = select(., contains("AUDIT")) %>% rowSums(.), 
         bis_att = select(., num_range("BIS_", att)) %>% rowSums(.),
         bis_mot = select(., num_range("BIS_", mot)) %>% rowSums(.),
         bis_noplan = select(., num_range("BIS_", noplan)) %>% rowSums(.),
         stai_s = select(., num_range("STAI_", 1:20)) %>% rowSums(.),
         stai_t = select(., num_range("STAI_", 21:40)) %>% rowSums(.))

# Save out data
write.table(results, file = paste0("Data/Preprocessed/0_preprocessed_allgroups_allscales.txt"), 
            row.names = F, col.names = T, sep = ",")
