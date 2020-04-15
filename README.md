# Haines_2020_CPS
Data, R, and Stan code to reproduce analyses and figures from Haines et al. (2020) in Clinical Psychological Science

All analyses codes are in the Code/R/1_analysis/ directory. In there, individual R scripts are numbered in the order they should be run. The Stan model codes are all located in Code/Stan/. Note that fitted models are not included in the Data/Fitted/ directory, as they are large in size and could not be uploaded to the repository. Once all analysis scripts are run, plotting codes to reproduce the figures can be found in the Code/R/2_plotting/ directory.

The preprocessed data (located in Data/Preprocessed/) are all included and ready to be fit in Stan as is. However, we include the item-level survey responses in Data/Survey/, as well as the trial-level delay discounting data in Data/DDT/. All R scripts used to preprocess the trial/item-level data are included in Code/R/0_preprocess/, and they are numbered in the order in which they should be run to reproduce the results.

Please reach out if you have any questions!
