# Supplemental Material for "Peer betting to elicit unverifiable information" 

This repository provides a guide to the supplemental material, which includes experimental material, data sets and R source files to replicate the results in “Peer betting to elicit unverifiable information” by Aurelien Baillon, Cem Peker and Sophie van der Zee.

Results are generated using R version 4.3.1 and RStudio Version 2023.06.2+561. 

## Overview & Contents

The

- `data/`: folder of processed data files.
- `experiment/`: ...
- `figures/`: ...
- `study1_analysis.R`: ...
- `study2_analysis.R`: ...
- `study2_week0`:...

## Instructions For Replication

The R scripts "study1_analysis.R" and "study2_analysis.R" reproduce all figures, tables and statistical results in Study 1 and 2 respectively. 

The R script "study2_week0.R" uses week 0 data of Study 2 to calculate the "% last week" for the week 1 survey. The percentage of "True" picks in "once or more" and "twice or more" are used as last week's percentages in "at least once" and "at least twice" versions respectively. See "data/s2_week0.csv" for the "% last week" in week 1 survey. For the Flat-PastRate and PPM surveys in week 2, we used the percentage of True picks in week 1 surveys (of the same treatment/version) to calucate "% last week".

We also note that the week 0 survey included 9 questions. For question 3, the statement was "I had physical contact with someone who came from abroad in the last 10 days". For this question, only 2% responsed "True" for "once or more" and nobody picked True for "twice or more", "3 times or more", "4 times or more" and "5 times or more". Thus, we excluded this question in weeks 1 and 2 and included the other 8 questions only. See Table 2 in the paper for the full list of these questions in weeks 1 and 2.

<!--- And **bold**, *italics*, and even *italics and later **bold***. Even ~~strikethrough~~. [A link](https://markdowntohtml.com) to somewhere. -->
