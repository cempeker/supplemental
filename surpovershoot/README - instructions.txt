####
####
#### This document provides information on reproducing the bootstrap analysis in the following manuscript:
#### "Extracting the collective wisdom in probabilistic judgments" (Section 5)

#### Material:
# R sources: bootstrap_coin.R, analysis_coin.R, bootstrap_coin_nested.R, analysis_coin_nested.R, bootstrap_binary_samplesize.R, analysis_binary_samplesize.R, bootstrap_binary_dispersion.R, analysis_binary_dispersion.R, dispersion.R
# R workspaces: bootstrap_coin.RData, bootstrap_coin_nested.RData, bootstrap_binary_samplesize.RData, bootstrap_binary_dispersion.RData
# Raw data folders: data_coin, data_binary

The bootstrap analysis takes some time to run, especially for results on the crowd size where I generate 1000 bootstrap samples for each crowd size. Thus, the material is organized such that the results can be reproduced without replicating the bootstrap analysis. Note: dispersion.R generates Figure C1.


The reader can follow one of the two approaches below:


(1) Reproduce the results only. This analysis loads the pre-saved bootstrap samples to save computing time.

a) For Section 5.1, open analysis_coin.R in RStudio and follow the directions in the top comments. The workspace "bootstrap_coin.RData" should be in the same folder so that it can be loaded to the R session. 

b) For Section 5.2, there are two separate analysis. First, run analysis_binary_samplesize.R in RStudio and follow the directions in the top comments. The workspaces "bootstrap_binary_samplesize.RData" should be in the same folder so that it can be loaded to the R session. Then, you can do the same with "analysis_binary_dispersion.R" and "bootstrap_binary_samplesize.RData"

c) For Appendix E, open analysis_coin_nested.R in RStudio and follow the directions in the top comments. The workspace "bootstrap_coin_nested.RData" should be in the same folder so that it can be loaded to the R session. 


(2) Re-run bootstrap analysis

For the analsis on Coin Flips (Section 5.1), open "bootstrap_coin.R" and run the bootstrap analysis, which saves the bootstrap results to "bootstrap_coin.RData". Then, run the script "analysis_coin.R", which loads "bootstrap_coin.RData".

For section Section 5.2..

a) open "bootstrap_binary_samplesize.R" and run the bootstrap analysis, which saves the bootstrap results to "bootstrap_binary_samplesize.RData". Then, run "analysis_binary_samplesize.R", which loads "bootstrap_binary_samplesize.RData".

b) open "bootstrap_binary_dispersion.R" and run the bootstrap analysis, which saves the bootstrap results to "bootstrap_binary_dispersion.RData". Then, run "analysis_binary_dispersion.R", which loads "bootstrap_binary_dispersion.RData".

c) For Appendix E, open "bootstrap_coin_nested.R" and run the bootstrap analysis, which saves the bootstrap results to "bootstrap_coin_nested.RData". Then, run the script "analysis_coin_nested.R", which loads "bootstrap_coin_nested.RData".





