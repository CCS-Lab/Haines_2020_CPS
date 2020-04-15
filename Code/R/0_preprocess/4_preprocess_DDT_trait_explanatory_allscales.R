library(dplyr)

# Set working directory 
setwd("~/Dropbox/Box/GitHub/Haines_2020_CPS")

datasets     <- c("MTURK", "REP", "TAL")
ddt_dat      <- NULL
survey_dat   <- NULL
subjID_count <- 0

# Read in scored survey data (REMOVE Subject TAL006 --> consistency with validation paper)
survey_dat <- read.table("Data/Preprocessed/0_preprocessed_allgroups_allscales.txt", 
                         header = T, sep = ",") %>%
  select(ID, contains("stai"), contains("bis")) %>% 
  filter(!is.na(stai_s) & !is.na(bis_noplan) &
         !is.na(stai_t) & !is.na(bis_att) &
         !is.na(bis_mot) & ID != "TAL006")
survey_dat$ID <- as.character(survey_dat$ID)

datasets <- c("MTURK", "REP", "TAL")

for (d in datasets) {
  # Find names of ADO files for subject who completed task
  ids_all <- list.files(paste0("Data/DDT/", d))
  
  for (i in survey_dat$ID) {
    # Read in subject's ADO estiamtes
    if (i %in% ids_all) {
      # mturk and REP/TAL formatted differently
      if (d == "MTURK") {
        tmp_obs1 <- try(read.table(paste0("Data/DDT/", d, "/", i, "/obs1.txt"), 
                                   fill = T, header = T, row.names = NULL), 
                        silent = T)
        tmp_obs2 <- try(read.table(paste0("Data/DDT/", d, "/", i, "/obs2.txt"), 
                                   fill = T, header = F, row.names = NULL), 
                        silent = T)
      } else {
        tmp_obs1 <- try(read.table(paste0("Data/DDT/", d, "/", i, "/ADO1/obs.txt"),
                                   fill = T, header = T, row.names = NULL, nrows = 42), 
                        silent = T)
        tmp_obs2 <- try(read.table(paste0("Data/DDT/", d, "/", i, "/ADO2/obs.txt"),# skip = 43,
                                   fill = T, header = T, row.names = NULL, nrows = 42), 
                        silent = T)
      } 
      if (is.data.frame(tmp_obs1) && is.data.frame(tmp_obs2) && (nrow(tmp_obs1) + nrow(tmp_obs2) > 0)) {
        names(tmp_obs2) <- names(tmp_obs1)
        subjID_count <- subjID_count + 1
        tmp_ado <- rbind(tmp_obs1, tmp_obs2)
        names(tmp_ado)[3:6] <- c("amount_sooner", "delay_sooner", "amount_later", "delay_later")
        tmp_ado$checkID <- i
        tmp_ado$subjID <- subjID_count
        ddt_dat <- rbind(ddt_dat, tmp_ado)
      }
    }
  }
}
  
numSubjs <- length(unique(ddt_dat$subjID))
Tsubj <- as.vector( rep( 0, numSubjs ) ) # number of trials for each subject

for ( i in 1:numSubjs )  {
  curSubj  <- unique(ddt_dat$subjID)[ i ]
  Tsubj[i] <- sum( ddt_dat$subjID == curSubj )  # Tsubj[N]
}

# Setting maxTrials
maxTrials <- max(Tsubj)

delay_later   <- array(0, c(numSubjs, maxTrials) )
amount_later  <- array(0, c(numSubjs, maxTrials) )
delay_sooner  <- array(0, c(numSubjs, maxTrials) )
amount_sooner <- array(0, c(numSubjs, maxTrials) )
choice <- array(-1, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
  curSubj      <- unique(ddt_dat$subjID)[i]
  useTrials    <- Tsubj[i]
  tmp          <- subset(ddt_dat, ddt_dat$subjID == curSubj)
  delay_later[i, 1:useTrials]   <- tmp$delay_later
  amount_later[i, 1:useTrials]  <- tmp$amount_later
  delay_sooner[i, 1:useTrials]  <- tmp$delay_sooner
  amount_sooner[i, 1:useTrials] <- tmp$amount_sooner
  choice[i, 1:useTrials] <- tmp$choice
}

# Keep survey data for subjects who have ADO data
survey_dat <- survey_dat[survey_dat$ID %in% ddt_dat$checkID,]

# Sanity check for ID order (should return true)
if((any(survey_dat$ID == rle(ddt_dat$checkID)$values)==F)) {
  stop("IDs do not match!!!")
} else {
  cat("IDs match!!! Congrats for not messing it up.\n")
}

# Stan-ready data
dataList <- list(
  N             = numSubjs,
  T             = maxTrials,
  Tsubj         = Tsubj,
  amount_later  = amount_later,
  delay_later   = delay_later,
  amount_sooner = amount_sooner,
  delay_sooner  = delay_sooner,
  choice        = choice
)

# Combine in list
all_dat <- list(survey_dat = survey_dat, ddt_dat = dataList)

saveRDS(all_dat, file = "Data/Preprocessed/1_preprocessed_DDT_trait_explanatory_allscales.rds")

