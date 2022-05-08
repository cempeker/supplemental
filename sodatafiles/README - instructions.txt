####
####
#### This document provides information on reproducing the bootstrap analysis in the following manuscript:
#### "Extracting the collective wisdom in probabilistic judgments" (Section 5)

#### Material:
# R sources: bootstrap_coin.R, analysis_coin.R, bootstrap_binary.R,
# R workspaces: bootstrap_coin.RData, bootstrap_binary.RData
# Raw data folders: data_coin, data_binary

The bootstrap analysis takes some time to run, especially in the Coin Flips data (Section 5.1) where I generate bootstrap samples for each crowd size. Thus, the material is organized such that the results can be reproduced without replicating the bootstrap analysis. 


The reader can follow one of the two approaches below:


(1) Reproduce the results only. This analysis loads the pre-saved bootstrap samples to save computing time.

a) For Section 5.1, open analysis_coin.R in RStudio and follow the directions in the top comments. The workspace "bootstrap_coin.RData" should be in the same folder so that it can be loaded to the R session.

b) For Section 5.2, open analysis_binary.R in RStudio and follow the directions in the top comments. The workspace "bootstrap_coin.RData" should be in the same folder so that it can be loaded to the R session.


(2) Re-run bootstrap analysis

For the analsis on sample size (Section 5.1), open "bootstrap_coin.R" and run the bootstrap analysis, which saves the bootstrap results to "bootstrap_coin.RData". Then, run the script "analysis_coin.R", which loads "bootstrap_coin.RData".

For the analsis on dispersion (Section 5.2), open "bootstrap_binary.R" and run the bootstrap analysis, which saves the bootstrap results to "bootstrap_binary.RData". Then, run the script "analysis_binary.R", which loads "bootstrap_binary.RData".
