
#### recalibr_analysis.R

# INPUT: recalibr_dataset.csv
# OUTPUT: Figures & tables in the "Robust recalibration of aggregate probability forecasts using meta-beliefs" by Peker and Wilkening.

# Execute the commands in order to reproduce the results. This will 1. load necessary LIBRARIES, 2. define FUNCTIONS in the console, 3. run the analyses in MAIN. Any missing libraries can be downloaded and installed by running install.packages("library_name") if necessary.

# The script loads recalibr_dataset.csv from the working directory. See construct_dataset.R to see how recalibr_dataset.csv is constructed from raw data.

# Seting working directory: Check getwd(), the working director should be .../supplemental (it is by default if recalibr_analysis.R is launched in RStudio). If not, copy the path of the folder and use setwd(path).


#----------LIBRARIES

library(reshape2)
library(tidyverse)
library(metaggR)
library(RColorBrewer)
library(grDevices)
library(gridExtra)
library(xtable)
options(scipen=999)

#----------FUNCTIONS

# get simple average prediction
getSimpleAggregates = function(dat){
  
  # average probability prediction an meta-prediction.
  dat = as.data.frame(
    dat %>%
      group_by(item) %>%
      mutate(
        avgprob = mean(prob, na.rm = TRUE),
        avgmetaprob = mean(metaprob,na.rm = TRUE)
      )
  )
  
  # calculate confidence of the average prediction, measured by distance to 0.5.
  # conf.level =1 if 0.5<avg<0.6 OR 0.4<avg<0.5; =2 if 0.6<avg<0.7 OR 0.3<avg<0.4; and so on.
  dat = as.data.frame(
    dat %>%
      mutate(
        avgprob_conflevel = as.numeric(cut(abs(avgprob-0.5),breaks = seq(0,0.5,by=0.1)))
      )
  )
  
  return(dat)
}
# correlation between pred and meta-pred, Figure E2
plotCorr = function(dat){
  
  d = as.data.frame(
    dat %>%
      group_by(task_type,item) %>%
      summarize(
        pm_cor_pearson = cor(prob,metaprob,method = 'pearson'),
        pm_cor_spearman = cor(prob,metaprob,method = 'pearson')
      )
  )
  
  pl_corr = ggplot(data = d) +
    geom_histogram(aes(x = pm_cor_pearson),bins = 15,fill='darkslategray3',color='black') +
    facet_wrap(.~task_type) +
    scale_x_continuous(
      name='Pearson correlation between predictions and meta-predictions'
    ) +
    coord_cartesian(xlim = c(-1,1)) +
    theme_bw(base_size = 15)
  
  ggsave(filename='./figures/corr.pdf',pl_corr,width=6.8,height=4.5)
  
  #quartz(type = 'pdf',file = "./corr.pdf",width=6.8,height=4.5)
  #pl_corr
  #dev.off()
  
  return(pl_corr)
  
}

# run other algorithms that use meta-predictions (comparative analysis)
runSOAlgorithm = function(dat){
  # function to implement the SO algorithm  
  
  dat$forecast_overshoot = ifelse(dat$prob > dat$avgprob,1,0)
  dat$meta_overshoot = ifelse(dat$metaprob > dat$avgprob,1,0)
  
  dat = as.data.frame(
    dat %>%
      group_by(item) %>%
      mutate(
        px = mean(forecast_overshoot),  # overshoot rate px
        pz = mean(meta_overshoot), # overshoot rate pz
        q = 1-pz,
        SOA = as.numeric(quantile(prob,q,type=1))
      )
  )
  
  return(dat)
}
runMinimalPivoting = function(dat){
  ##### get minimal pivoting estimate

  dat$MP = 2*dat$avgprob - dat$avgmetaprob
  dat$MP = ifelse(dat$MP < 0, 0, dat$MP)
  dat$MP = ifelse(dat$MP > 1, 1, dat$MP)
  
  return(dat)
}
runKnowledgeWeighted = function(dat){
  #### Knowledge weighted estimate of Palley and Satopaa 2022
  # if all predictions in a given Bootstrap sample are the same, knowledge_weighted_estimate throws an error. In such cases, KW estimate is simply set as the consensus estimate.
  
  dat = as.data.frame(
    dat %>%
      group_by(item) %>%
      mutate(
        not_all_E_same = ifelse(length(unique(prob)) > 1,1,0), 
        KW = ifelse(not_all_E_same, knowledge_weighted_estimate(E = prob,P = metaprob, no_inf_check=TRUE), unique(E))
      )
  )
  
  dat$KW = ifelse(dat$KW < 0, 0, dat$KW)
  dat$KW = ifelse(dat$KW > 1, 1, dat$KW)
  
  return(dat)
}
runMPW = function(dat){
  #### Meta-probability weighting (MPW) algorithm of Martinie et al (2020)
  
  # get forecast - metaprediction for each item and subject
  dat$f_min_g = abs(dat$prob - dat$metaprob)
  
  # compute weights and MPW estimate
  dat = as.data.frame(
    dat %>% 
      group_by(item) %>%
      mutate(
        weight = f_min_g / sum(f_min_g),
        MPW = as.vector(weight %*% prob)
      )
  )
  
  return(dat)
}

# estimate priors and plot (Figure 3)
runItemRegression = function(i, dat){
  # run regression and return estimates for a given forecasting task i
  dat = dat[dat$item == i,]
  linmod1 = lm(metaprob ~ prob,data = dat)
  
  EavgB = as.numeric(linmod1$coefficients[1])
  EavgG = as.numeric(linmod1$coefficients[1]) + as.numeric(linmod1$coefficients[2])
  
  df = data.frame(i,EavgB,EavgG)
  colnames(df) = c('item','EavgB','EavgG')
  
  return(df)
}
getEstimatedPrior = function(dat){
  
  # get regression estimates for each item, then calculate estimated priors
  item_list = unique(dat$item)
  dat_est = lapply(item_list, runItemRegression, dat = dat)
  dat_est = bind_rows(dat_est)
  dat2 = merge(dat,dat_est,by='item')
  dat2$estPrior = dat2$EavgB / (1-(dat2$EavgG - dat2$EavgB))
  
  return(dat2)
}
plotEstPriors = function(dat2){
  # plots the distribution of estimated priors in each data set
  d = unique(dat2[,c('item','task_type','estPrior')])
  d = as.data.frame(
    d %>%
      mutate(
        estPriorbin = cut(estPrior,breaks=c(-5,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,5),include.lowest=TRUE)
      )
  )
  
  d2 = as.data.frame(
    d %>%
      group_by(task_type,estPriorbin,.drop = FALSE) %>%
      tally()
  )
  
  pl_estprior = ggplot(d2,aes(x=estPriorbin,y=n)) +
    geom_bar(stat='identity',fill='darkslategray3',color='black') +
    geom_text(data = d2 %>% filter(n>0),aes(label=n),vjust=-0.25) +
    facet_wrap(~task_type) +
    scale_y_continuous(name='Count',limits = c(0,335)) +
    scale_x_discrete(name='Estimated Prior',labels=c('<0','[0,0.1]','(0.1,0.2]','(0.2,0.3]','(0.3,0.4]','(0.4,0.5]','(0.5,0.6]','(0.6,0.7]','(0.7,0.8]','(0.8,0.9]','[0.9,1]','>1')) +
    theme_bw(base_size = 15) +
    theme(
      axis.text.x = element_text(angle = 45, hjust=1)
    )
  
  ggsave(filename = "./figures/estprior.pdf",plot =pl_estprior,width=7.5,height=5.2)
  #quartz(type = 'pdf',file = "./estprior.pdf",width=7.5,height=5)
  #pl_estprior
  #dev.off()
  
  return(pl_estprior)
}

# exclude two items where estimated prior is not within [0,1]
filterEstimatedPrior = function(dat2){
  
  dat2 = dat2[!(dat2$estPrior < 0 | dat2$estPrior > 1),]
  
  return(dat2)
}
# two items where estimated prior is not within [0,1] - Appendix D material
plotFailedEstimates = function(dat2){
  
  # Estimated prior is outside (0,1) in two tasks, namely gr334 and gr382. Below, we find that these are the only tasks where at least one of estimated beta0 and beta0 + beta1 is outside (0,1). We plot the fitted lines for these two tasks.
  temp = dat2[dat2$EavgB < 0 | dat2$EavgG > 1,]
  #unique(temp$item)
  temp$itemtext = NA
  temp[temp$item == 'gr334', 'itemtext'] = "Centimetres are a measure of length"
  temp[temp$item == 'gr382', 'itemtext'] = "Fish have fur to keep them warm"
  
  pl_failed = ggplot(data = temp,aes(x = prob, y = metaprob)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x, fullrange = TRUE, se=FALSE) +
    geom_hline(yintercept = 1,linetype = 2,color = 'red') +
    geom_hline(yintercept = 0,linetype = 2,color = 'red') +
    facet_wrap(.~itemtext) +
    scale_x_continuous(name = 'Prediction', limits = c(0,1)) +
    scale_y_continuous(name = 'Meta-prediction', limits = c(-0.05,1.05),breaks = c(0,0.5,1)) +
    theme_bw(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  
  #quartz(type = 'pdf',file = "./priorfailed.pdf",width=6.8,height=3.5)
  #pl_failed
  #dev.off()
  
  ggsave(filename = "./figures/priorfailed.pdf", plot = pl_failed, width=7,height=3.5)
  
  return(pl_failed)
  
}

# average vs 0.5 and est prior by state, Table 1
tabulateAvgVsPrior = function(dat3){
  # function to produce Table 1 (avg prediction vs 0.5 or estimated prior)
  
  d = unique(dat3[,c('item','task_type','avgprob','estPrior','binary_outcome')])
  d$avg_higher_estprior = ifelse(d$avgprob > d$estPrior, 1, 0)
  d$avg_higher_0.5 = ifelse(d$avgprob > 0.5, 1, 0)
  
  d_estprior = as.data.frame(
    d %>%
      group_by(binary_outcome,avg_higher_estprior) %>%
      summarize(
        counts = n()
      )
  )
  
  d_0.5= as.data.frame(
    d %>%
      group_by(binary_outcome,avg_higher_0.5) %>%
      summarize(
        counts = n()
      )
  )
  
  return(list(d_estprior,d_0.5))
}

# distribution of averages, Figure E1
plotAverages = function(dat3){

  d = unique(dat3[,c('item','binary_outcome','task_type','avgprob')])
  d$wrongsided = ifelse(
    (d$avgprob > 0.5 & d$binary_outcome==0) | (d$avgprob < 0.5 & d$binary_outcome==1),
  1,
  0)
  
  d2 = d
  d2$binary_outcome = ifelse(d2$binary_outcome == 1, "Answer = \"True\"","Answer = \"False\"") 
  
  pl_avgpred = ggplot(d2,aes(x=avgprob)) +
    geom_histogram(fill='darkslategray3',color='black') +
    facet_grid(task_type~binary_outcome) +
    scale_x_continuous(name='Average Prediction',labels=c('0','0.25','0.50','0.75','1')) +
    theme_bw(base_size = 15)
  
  ggsave(filename = "./figures/avgpred.pdf", plot = pl_avgpred, width=6,height=6)
  #quartz(type = 'pdf',file = "./avgpred.pdf",width=6,height=6)
  #pl_avgpred
  #dev.off()
  
  return(pl_avgpred)
  
}
# number of wrong-sided averages, Figure 2
plotWrongSidedAverages = function(dat3){
  
  d = unique(dat3[,c('item','binary_outcome','task_type','avgprob')])
  d$wrongsided = ifelse(
    (d$avgprob > 0.5 & d$binary_outcome==0) | (d$avgprob < 0.5 & d$binary_outcome==1),
    1,
    0)
  
  d2 = as.data.frame(
    d %>%
      group_by(task_type,binary_outcome) %>%
      summarize(
        num_wrongsided = sum(wrongsided),
        num_notwrongsided = n() - num_wrongsided
      )
  )
  colnames(d2) = c('task_type','binary_outcome','Wrong-sided','Not wrong-sided')
  d2$binary_outcome = ifelse(d2$binary_outcome == 1, "Answer = \"True\"","Answer = \"False\"") 
  d2 = melt(d2,id.vars = c('task_type','binary_outcome'))
  colnames(d2) = c('task_type','binary_outcome','Average prediction','value')
  
  my_palette = c(brewer.pal(9,"Blues")[9],brewer.pal(9,"YlOrRd")[3])
  pl_wrongsided = ggplot(d2, aes(x = task_type, y=value, fill=`Average prediction`)) +
    geom_bar(stat='identity',position = 'dodge') +
    geom_text(data = d2 %>% filter(value>=0),aes(label=value),position=position_dodge(width=0.9),vjust=-0.25) +
    facet_wrap(.~binary_outcome) +
    scale_x_discrete(name='Task type') +
    scale_y_continuous(name='Number of tasks',limits = c(0,350),breaks = seq(0,350,50)) +
    scale_fill_manual(values=my_palette) +
    theme_bw(base_size = 15) + 
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  
  ggsave(filename = "./figures/wrongsided.pdf", plot = pl_wrongsided, width=6.4,height=4)
  #quartz(type = 'pdf',file = "./wrongsided.pdf",width=6.4,height=4)
  #pl_wrongsided
  #dev.off()
  
  return(pl_wrongsided)
}
# number of wrong-sided averages by confidence level, Table 3
tabulateWrongSidedByConfidence = function(dat3){
  
  d = unique(dat3[,c('item','binary_outcome','task_type','avgprob')])
  d$wrongsided = ifelse(
    (d$avgprob > 0.5 & d$binary_outcome==0) | (d$avgprob < 0.5 & d$binary_outcome==1),
    1,
    0)
  
  d = as.data.frame(
    d %>%
      mutate(
        avgprob_conflevel = as.numeric(cut(abs(avgprob-0.5),breaks = seq(0,0.5,by=0.1)))
      )
  )
  
  temp = data.frame(1:5,c('50-60','60-70','70-80','80-90','90-100'))
  colnames(temp) = c('avgprob_conflevel','avgprob_conf')
  d = merge(d,temp,by='avgprob_conflevel')
  rm(temp)
  
  d_conf = as.data.frame(
    d %>%
      group_by(wrongsided,avgprob_conf) %>%
      summarize(
        num_of_tasks = n()
      )
  )
  
  return(d_conf)
  
}

# Transformations
extremFunction = function(prob,gamma,prior){
  # LLO transformation of the average prediction
  delta = ((1-prior)/prior)^gamma
  prob_ext = (delta*(prob^(1+gamma))) / ( (delta*(prob^(1+gamma))) + (1-prob)^(1+gamma) )
  
  return(prob_ext)
}
transformAvgProb = function(dat3){
  # Transforms the average prediction for various gamma
  # t_i's are robust recalibrations with gamma = i
  # avgextrem_i's are extremization (prior = 0.5) with gamma = i
  d = as.data.frame(
    dat3 %>%
      mutate(
        avgextrem_0.5 = extremFunction(avgprob,0.5,0.5),
        avgextrem_1 = extremFunction(avgprob,1,0.5),
        avgextrem_1.5 = extremFunction(avgprob,1.5,0.5),
        avgextrem_2 = extremFunction(avgprob,2,0.5),
        avgextrem_2.5 = extremFunction(avgprob,2.5,0.5),
        avgextrem_3 = extremFunction(avgprob,3,0.5),
        t_0.5 = extremFunction(avgprob,0.5,estPrior),
        t_1 = extremFunction(avgprob,1,estPrior),
        t_1.5 = extremFunction(avgprob,1.5,estPrior),
        t_2 = extremFunction(avgprob,2,estPrior),
        t_2.5 = extremFunction(avgprob,2.5,estPrior),
        t_3 = extremFunction(avgprob,3,estPrior)
      )
  )
  
  d2 = unique(d[,c('item','binary_outcome','task_type','avgprob','MP','KW','SOA','MPW','avgprob_conflevel','avgextrem_0.5','avgextrem_1','avgextrem_1.5','avgextrem_2','avgextrem_2.5','avgextrem_3','t_0.5','t_1','t_1.5','t_2','t_2.5','t_3')])
  
  return(d2)
}

# get Brier scores
calculateBrierScores = function(dat_transformed){
  # calculates the Brier scores for average, extremized average and robust recalibration
  
  d1 = dat_transformed[,c('item', 'binary_outcome','task_type','avgprob','avgprob_conflevel','avgextrem_0.5','avgextrem_1','avgextrem_1.5','avgextrem_2', 'avgextrem_2.5', 'avgextrem_3', 't_0.5','t_1','t_1.5','t_2','t_2.5','t_3')]
  d1$avgprob_0.5 = d1$avgprob; d1$avgprob_1 = d1$avgprob; d1$avgprob_1.5 = d1$avgprob; d1$avgprob_2 = d1$avgprob; d1$avgprob_2.5 = d1$avgprob; d1$avgprob_3 = d1$avgprob;
  d1$avgprob = NULL
  
  d1 = melt(d1, id.vars = c('item', 'binary_outcome','task_type','avgprob_conflevel'))
  d1 = d1 %>% separate(variable,into=c('variable','gamma'),sep='_')
  colnames(d1) = c('item','binary_outcome','task_type','avgprob_conflevel','method','gamma','value')
  d1$method = recode(d1$method, 'avgprob' = 'average','avgextrem'='extrem.average','t' = 'robust.recalibr')
  d1$method = factor(d1$method,levels=c('average','extrem.average','robust.recalibr'))
  
  d1$brierscore = (d1$binary_outcome - d1$value)^2 
  d1$gamma = paste0("\U03B3 = ",d1$gamma)
  
  return(d1)
  
}
summarystatsBrierScores = function(dat_brier){
  
  temp = data.frame(1:5,c('50-60','60-70','70-80','80-90','90-100'))
  colnames(temp) = c('avgprob_conflevel','confidence of the average prediction (%)')
  d = merge(dat_brier,temp,by='avgprob_conflevel')
  rm(temp)
  
  d2 = as.data.frame(
    d %>%
      group_by(method,gamma) %>%
      summarize(
        min_score = min(brierscore),
        max_core = max(brierscore),
        mean_score = mean(brierscore),
        q25_score = quantile(brierscore, probs = 0.25),
        q50_score = quantile(brierscore, probs = 0.5),
        q75_score = quantile(brierscore, probs = 0.75)
      )
  )
  
  print(xtable(d2, digits = 4), include.rownames=FALSE)

}

# plot Brier scores
plotBrierScores = function(dat_brier,fname){
  # plots the Brier scores for average, extremized average and robust recalibration
  
  temp = data.frame(1:5,c('50-60','60-70','70-80','80-90','90-100'))
  colnames(temp) = c('avgprob_conflevel','confidence of the average prediction (%)')
  dat_brier = merge(dat_brier,temp,by='avgprob_conflevel')
  rm(temp)
  
  my_palette = brewer.pal(5,"Spectral")
  
  pl_brier = ggplot(dat_brier,aes(x=brierscore,fill = `confidence of the average prediction (%)`)) +
    geom_histogram(binwidth = 0.1) +
    scale_fill_manual(values = my_palette) +
    facet_grid(method ~ gamma) +
    #facet_grid(method ~ paste0(gamma)) +
    scale_x_continuous(name = 'Brier score',breaks = seq(0,1,by=0.25),labels = as.character(c('0','.25','.5','.75','1'))) +
    coord_cartesian(xlim = c(0,1)) +
    theme_bw(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  
  ggsave(filename = fname, plot = pl_brier, width=9.5,height=6,device=cairo_pdf)
  #quartz(type = 'pdf',file = "./brier1.pdf",width=9.5,height=5.6)
  #pl_brier
  #dev.off()
  
  return(pl_brier)
}
plotBrierScoresByDataset = function(dat_brier){
  # plots the Brier scores for average, extremized average and robust recalibration
  
  pl_brier_artwork = plotBrierScores(dat_brier[dat_brier$task_type == 'Artwork',],'./figures/brier1artwork.pdf')
  pl_brier_NFL = plotBrierScores(dat_brier[dat_brier$task_type == 'NFL',],'./figures/brier1NFL.pdf')
  pl_brier_science = plotBrierScores(dat_brier[dat_brier$task_type == 'Science',],'./figures/brier1science.pdf')
  pl_brier_states = plotBrierScores(dat_brier[dat_brier$task_type == 'States',],'./figures/brier1states.pdf')
  
  return(list(pl_brier_artwork,pl_brier_NFL,pl_brier_science,pl_brier_states))
  
}

# test for pairwise difference in Brier Scores, robust recalibration vs extremized average
testBrierScore = function(dat_brier){
  # for each gamma, run a two-sided pairwise test of differences in Brier scores between robust recalibration and extremized average. Also returns a plot of pairwise differences in Brier scores.

  d = dat_brier
  d$value = NULL
  
  d_avg = d[d$method == 'average',]
  d_extr = d[d$method == 'extrem.average',]
  d_rr = d[d$method == 'robust.recalibr',]
  d2 = data.frame()
  
  temp = merge(d_extr,d_avg,by = c('item','binary_outcome','task_type','avgprob_conflevel','gamma'))
  temp$scorediff = temp$brierscore.x - temp$brierscore.y
  d2 = rbind(d2,temp)

  temp = merge(d_rr,d_extr,by = c('item','binary_outcome','task_type','avgprob_conflevel','gamma'))
  temp$scorediff = temp$brierscore.x - temp$brierscore.y
  d2 = rbind(d2,temp)
  
  
  d_diff = as.data.frame(
    d2 %>% 
      group_by(gamma,method.x,method.y) %>%
      summarize(
        avg_diff = mean(scorediff),
        med_diff = median(scorediff)
      )
  )  
  
  d_test = as.data.frame(
    d2 %>% 
      group_by(gamma,method.x,method.y) %>%
      summarize(
        stat = wilcox.test(brierscore.x,brierscore.y,paired=TRUE)$statistic,
        pval = wilcox.test(brierscore.x,brierscore.y,paired=TRUE)$p.value
      )
  )  
  
  d_all = merge(d_diff,d_test,by = c('gamma','method.x','method.y'))
  
  d_all2 = d_all %>% mutate_if(is.numeric,round,digits=4)
  #d_all2 = d_all
  d_all2$stat = paste0('V=',d_all2$stat)
  d_all2$signif_better = ifelse(d_all2$med_diff < 0 & d_all2$pval < 0.05, 'Method.1', NA)
  d_all2$pval = ifelse(d_all$pval < 0.0001,'<0.0001',d_all2$pval)
  colnames(d_all2) = c('Gamma','Method.1','Method.2','Avg.diff','Med.diff','Test stat.','p-value','Signif. better?')
  
  return(xtable(d_all2,digits = 4))

}
testBrierScoreByDataset = function(dat_brier){
  
  testbrier_artwork = testBrierScore(dat_brier[dat_brier$task_type == 'Artwork',])
  testbrier_NFL = testBrierScore(dat_brier[dat_brier$task_type == 'NFL',])
  testbrier_science = testBrierScore(dat_brier[dat_brier$task_type == 'Science',])
  testbrier_states = testBrierScore(dat_brier[dat_brier$task_type == 'States',])
  
  return(list(testbrier_artwork,testbrier_NFL,testbrier_science,testbrier_states))
}

# plot pairwise difference in Brier Scores, robust recalibration vs extremized average
plotBrierScoreDiff = function(dat_brier){
  
  gamma_vals = c('\U03B3 = 0.5','\U03B3 = 1','\U03B3 = 1.5','\U03B3 = 2','\U03B3 = 2.5','\U03B3 = 3')
  d_all = NULL
  for(g in gamma_vals){
    d = dat_brier[dat_brier$gamma == g,]
    d$value = NULL
    d2 = dcast(d, item + binary_outcome + task_type + avgprob_conflevel + gamma  ~ method, value.var = 'brierscore' )
    d_all = rbind(d_all,d2)
  }
  rm(d,d2)
  
  d_all$diff = d_all$robust.recalibr - d_all$extrem.average
  temp = data.frame(1:5,c('50-60','60-70','70-80','80-90','90-100'))
  colnames(temp) = c('avgprob_conflevel','confidence of the average prediction (%)')
  d_all = merge(d_all,temp,by='avgprob_conflevel')
  rm(temp)
  
  my_palette = brewer.pal(5,"Spectral")
  pl_diff = ggplot(d_all,aes(x=diff,fill = `confidence of the average prediction (%)`)) +
    geom_histogram(binwidth = 0.1) +
    scale_fill_manual(values = my_palette) +
    geom_vline(xintercept = 0, linetype = 2, color = 'black') +
    facet_wrap(~gamma,ncol=3) +
    scale_x_continuous(name = 'Difference in Brier score',breaks = seq(-1,1,by=0.25),labels = as.character(c('-1','-.75','-.5','-.25','0','.25','.5','.75','1'))) +
    coord_cartesian(xlim = c(-1,1)) +
    theme_bw(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  
  ggsave(filename = "./figures/brier1diff.pdf", plot = pl_diff, width=9.5,height=6,device=cairo_pdf)
  #quartz(type = 'pdf',file = "./brier1diff.pdf",width=11,height=6)
  #pl_diff
  #dev.off()
  
  return(pl_diff)
}

# Calibration curve
calibrationPlot = function(dat_transformed){
  # Produces the calibration curves
  
  ## Robust recalibration vs average and extremized average
  d1 = dat_transformed[,c('item', 'binary_outcome','task_type','avgprob','avgextrem_0.5','avgextrem_1','avgextrem_1.5','avgextrem_2', 'avgextrem_2.5', 'avgextrem_3','t_0.5','t_1','t_1.5','t_2','t_2.5','t_3')]
  d1$avgprob_0.5 = d1$avgprob; d1$avgprob_1 = d1$avgprob; d1$avgprob_1.5 = d1$avgprob; d1$avgprob_2 = d1$avgprob; d1$avgprob_2.5 = d1$avgprob; d1$avgprob_3 = d1$avgprob;
  d1$avgprob = NULL
  d1 = melt(d1, id.vars = c('item', 'binary_outcome','task_type'))
  d1 = d1 %>% separate(variable,into=c('variable','gamma'),sep='_')
  colnames(d1) = c('item','binary_outcome','task_type','method','gamma','value')
  d1$method = recode(d1$method, 'avgprob' = 'average','avgextrem'='extrem.average','t' = 'robust.recalibr')
  d1$method = factor(d1$method,levels=c('average','extrem.average','robust.recalibr'))
  
  # predicted percentage of True/Yes
  d1 = as.data.frame(
    d1 %>%
      mutate(
        prob_cut = cut(value,breaks=seq(0,1,by=0.1),include.lowest = TRUE)
      )
  )
  
  # actual percentage of True/Yes
  dplot1 = as.data.frame(
    d1 %>%
      group_by(prob_cut,method,gamma) %>%
      summarize(
        true_rate = mean(binary_outcome),
        num_tasks = n() 
      )
  )
  dplot1 = dplot1[dplot1$num_tasks >= 10,]
  dplot1$gamma = paste0("\U03B3 = ",dplot1$gamma)
  
  my_palette1 = as.vector(c(
    brewer.pal(12,'Set3')[12],
    #brewer.pal(9,'YlOrRd')[3],
    #brewer.pal(9,'YlOrBr')[3],
    brewer.pal(9,'BuPu')[8],
    brewer.pal(9,'Oranges')[5]
  ))
  my_shapes1 = c(17,18,16)
  
  # construct the calibration plot (robust recalibr vs average and extrem.average)
  pl_calibr1 = ggplot(data = dplot1, aes(x= as.numeric(prob_cut), y=true_rate, color=method, shape=method)) +
    geom_line() +
    geom_point(size = 3) +
    facet_wrap(~gamma) +
    scale_color_manual(values=my_palette1) +
    scale_shape_manual(values=my_shapes1) +
    annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 0.1, alpha = 0.2) +
    annotate("rect", xmin = 1.5, xmax = 2.5, ymin = 0.1, ymax = 0.2, alpha = 0.2) +
    annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 0.2, ymax = 0.3, alpha = 0.2) +
    annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 0.3, ymax = 0.4, alpha = 0.2) +
    annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 0.4, ymax = 0.5, alpha = 0.2) +
    annotate("rect", xmin = 5.5, xmax = 6.5, ymin = 0.5, ymax = 0.6, alpha = 0.2) +
    annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 0.6, ymax = 0.7, alpha = 0.2) +
    annotate("rect", xmin = 7.5, xmax = 8.5, ymin = 0.7, ymax = 0.8, alpha = 0.2) +
    annotate("rect", xmin = 8.5, xmax = 9.5, ymin = 0.8, ymax = 0.9, alpha = 0.2) +
    annotate("rect", xmin = 9.5, xmax = 10.5, ymin = 0.9, ymax = 1, alpha = 0.2) +
    scale_x_continuous(name='Predicted probability of True',breaks = seq(1,10,by=1), labels = levels(dplot1$prob_cut)) +
    scale_y_continuous(name='Proportion True',breaks = seq(0,1,by=0.1)) +
    theme_bw(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45,hjust=1),
      legend.position = 'top',
      legend.direction = 'horizontal'
    )
  
  ggsave(filename = "./figures/calibr1.pdf", plot = pl_calibr1, width=8.5,height=6.5,device=cairo_pdf)
  #quartz(type = 'pdf',file = "./calibr1.pdf",width=8.5,height=6.5)
  #pl_calibr1
  #dev.off()
  
  # return
  return(pl_calibr1)
}

# get Brier scores (benchmarks)
calculateBrierScoresBenchmarks = function(dat_transformed){
  # calculates the Brier scores for robust recalibration and other aggregation algorithms
  
  d = dat_transformed[,c('item', 'binary_outcome','task_type','avgprob_conflevel','MP', 'KW','MPW','SOA','t_0.5','t_1','t_1.5','t_2','t_2.5','t_3')]
  d = melt(d, id.vars = c('item', 'binary_outcome','task_type','avgprob_conflevel'))
  
  colnames(d) = c('item','binary_outcome','task_type','avgprob_conflevel','method','value')
  d$method = recode(d$method, 'MP' = 'min.pivot', 'KW' = 'know.weight','MPW' = 'meta.prob.weight','SOA' = 'surp.overshoot','t_0.5' = 'robust.recalibr.\U03B3=0.5','t_1' = 'robust.recalibr.\U03B3=1','t_1.5' = 'robust.recalibr.\U03B3=1.5','t_2' = 'robust.recalibr.\U03B3=2','t_2.5' = 'robust.recalibr.\U03B3=2.5','t_3' = 'robust.recalibr.\U03B3=3')
  d$method = factor(d$method,c('min.pivot','know.weight','meta.prob.weight','surp.overshoot','robust.recalibr.\U03B3=0.5','robust.recalibr.\U03B3=1','robust.recalibr.\U03B3=1.5','robust.recalibr.\U03B3=2','robust.recalibr.\U03B3=2.5','t_3' = 'robust.recalibr.\U03B3=3'))
  
  d$brierscore = (d$binary_outcome - d$value)^2 
  return(d)
  
}
summarystatsBrierScoresBenchmarks = function(dat_brier2){
  
  temp = data.frame(1:5,c('50-60','60-70','70-80','80-90','90-100'))
  colnames(temp) = c('avgprob_conflevel','confidence of the average prediction (%)')
  d = merge(dat_brier2,temp,by='avgprob_conflevel')
  rm(temp)
  
  d2 = as.data.frame(
    d %>%
      group_by(method) %>%
      summarize(
        #num_obs = n(),
        min_score = min(brierscore),
        max_core = max(brierscore),
        mean_score = mean(brierscore),
        q25_score = quantile(brierscore, probs = 0.25),
        q50_score = quantile(brierscore, probs = 0.5),
        q75_score = quantile(brierscore, probs = 0.75)
      )
  )
  
  print(xtable(d2, digits = 4), include.rownames=FALSE)
  
}

# plot Brier scores
plotBrierScoresBenchmarks = function(dat_brier2,fname){
  # plots the Brier scores for robust recalibration and other aggregation algorithms
  
  temp = data.frame(1:5,c('50-60','60-70','70-80','80-90','90-100'))
  colnames(temp) = c('avgprob_conflevel','confidence of the average prediction (%)')
  dat_brier2 = merge(dat_brier2,temp,by='avgprob_conflevel')
  rm(temp)
  
  my_palette = brewer.pal(5,"Spectral")
  
  #dat_brier2 = dat_brier2[dat_brier2$method %in% c('min.pivot','know.weight','meta.prob.weight','surp.overshoot','robust.recalibr_1'),]
  pl_brier2 = ggplot(dat_brier2,aes(x=brierscore,fill = `confidence of the average prediction (%)`)) +
    geom_histogram(binwidth = 0.1) +
    scale_fill_manual(values = my_palette) +
    facet_wrap(~method,ncol=4) +
    scale_x_continuous(name = 'Brier score',breaks = seq(0,1,by=0.25),labels = as.character(c('0','.25','.5','.75','1'))) +
    coord_cartesian(xlim = c(0,1)) +
    theme_bw(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  
  ggsave(filename = fname, plot = pl_brier2, width=9.5,height=7,device=cairo_pdf)
  #quartz(type='pdf',file = "./brier2.pdf",width=9.5,height=6.5)
  #pl_brier2
  #dev.off()
  
  return(pl_brier2)
}
plotBrierScoresBenchmarksByDataset = function(dat_brier2){
  
  pl_brier_artwork = plotBrierScoresBenchmarks(dat_brier2[dat_brier2$task_type == 'Artwork',],'./figures/brier2artwork.pdf')
  pl_brier_NFL = plotBrierScoresBenchmarks(dat_brier2[dat_brier2$task_type == 'NFL',],'./figures/brier2NFL.pdf')
  pl_brier_science = plotBrierScoresBenchmarks(dat_brier2[dat_brier2$task_type == 'Science',],'./figures/brier2science.pdf')
  pl_brier_states = plotBrierScoresBenchmarks(dat_brier2[dat_brier2$task_type == 'States',],'./figures/brier2states.pdf')
  
  return(list(pl_brier_artwork,pl_brier_NFL,pl_brier_science,pl_brier_states))
  
}

# test Brier scores
testBrierScoresBenchmarks = function(dat_brier2){
  
  res_aov = aov(data = dat_brier2, brierscore~method)
  summ_res_aov = summary(res_aov)
  
  d = dat_brier2
  d$value = NULL
  recalibr_methods = c('robust.recalibr.γ=0.5','robust.recalibr.γ=1','robust.recalibr.γ=1.5', 'robust.recalibr.γ=2','robust.recalibr.γ=2.5','robust.recalibr.γ=3')
  benchmark_methods = c('min.pivot','know.weight','meta.prob.weight','surp.overshoot')
  
  d2 = data.frame()
  for(rrm in recalibr_methods){
    for(bnm in benchmark_methods){
      d_rrm = d[d$method == rrm,]
      d_bnm = d[d$method == bnm,]
      
      temp = merge(d_rrm,d_bnm,by = c('item','binary_outcome','task_type','avgprob_conflevel'))
      temp$scorediff = temp$brierscore.x - temp$brierscore.y
      d2 = rbind(d2,temp)
    }
  }
  
  colnames(d2) = c('item','binary_outcome','task_type','avgprob_conflevel','robust_recalibr','brier_rr','benchmark','brier_benchmark','scorediff')
  
  d_avg = as.data.frame(
    d2 %>% 
      group_by(robust_recalibr,benchmark) %>%
      summarize(
        avg_diff = mean(scorediff),
        med_diff = median(scorediff)
      )
  )  
  
  d_test = as.data.frame(
    d2 %>% 
      group_by(robust_recalibr,benchmark) %>%
      summarize(
        stat = wilcox.test(brier_rr,brier_benchmark,paired=TRUE)$statistic,
        pval = wilcox.test(brier_rr,brier_benchmark,paired=TRUE)$p.value
      )
  )  
    
  d_all = merge(d_avg,d_test,by = c('robust_recalibr','benchmark'))
  
  d_all2 = d_all %>% mutate_if(is.numeric,round,digits=4)
  #d_all2 = d_all
  d_all2$stat = paste0('V=',d_all2$stat)
  d_all2$signif_better = ifelse(d_all2$med_diff < 0 & d_all2$pval < 0.05, 'robust.recalibr', NA)
  d_all2$pval = ifelse(d_all$pval < 0.0001,'<0.0001',d_all2$pval)
  colnames(d_all2) = c('Method','Benchmark','Avg.diff','Med.diff','Test stat.','p-value','Signif. better?')
  
  return(list(summ_res_aov,xtable(d_all2,digits=4)))
}
testBrierScoresBenchmarksByDataset = function(dat_brier2){
  
  testbrier_artwork = testBrierScoresBenchmarks(dat_brier2[dat_brier2$task_type == 'Artwork',])
  testbrier_NFL = testBrierScoresBenchmarks(dat_brier2[dat_brier2$task_type == 'NFL',])
  testbrier_science = testBrierScoresBenchmarks(dat_brier2[dat_brier2$task_type == 'Science',])
  testbrier_states = testBrierScoresBenchmarks(dat_brier2[dat_brier2$task_type == 'States',])
  
  return(list(testbrier_artwork,testbrier_NFL,testbrier_science,testbrier_states))
}

# Calibration curve (benchmarks)
calibrationPlotBenchmarks = function(dat_transformed){
  
  ## Robust recalibration vs advanced benchmarks
  
  d2 = dat_transformed[,c('item', 'binary_outcome','task_type','MP', 'KW','MPW','SOA','t_0.5','t_1','t_1.5','t_2','t_2.5','t_3')]
  d2$MP_0.5 = d2$MP; d2$MP_1 = d2$MP; d2$MP_1.5 = d2$MP; d2$MP_2 = d2$MP; d2$MP_2.5 = d2$MP; d2$MP_3 = d2$MP;
  d2$KW_0.5 = d2$KW; d2$KW_1 = d2$KW; d2$KW_1.5 = d2$KW; d2$KW_2 = d2$KW; d2$KW_2.5 = d2$KW; d2$KW_3 = d2$KW;
  d2$MPW_0.5 = d2$MPW; d2$MPW_1 = d2$MPW; d2$MPW_1.5 = d2$MPW; d2$MPW_2 = d2$MPW; d2$MPW_2.5 = d2$MPW; d2$MPW_3 = d2$MPW;
  d2$SOA_0.5 = d2$SOA; d2$SOA_1 = d2$SOA; d2$SOA_1.5 = d2$SOA; d2$SOA_2 = d2$SOA; d2$SOA_2.5 = d2$SOA; d2$SOA_3 = d2$SOA;
  d2$MP = NULL; d2$KW = NULL; d2$MPW = NULL; d2$SOA = NULL;
  
  d2 = melt(d2, id.vars = c('item', 'binary_outcome','task_type'))
  d2 = d2 %>% separate(variable,into=c('variable','gamma'),sep='_')
  colnames(d2) = c('item','binary_outcome','task_type','method','gamma','value')
  
  d2$method = recode(d2$method, 'MP' = 'min.pivot', 'KW' = 'know.weight','MPW' = 'meta.prob.weight','SOA' = 'surp.overshoot','t' = 'robust.recalibr')
  d2$method = factor(d2$method,c('min.pivot','know.weight','meta.prob.weight','surp.overshoot','robust.recalibr'))
  
  # predicted percentage of True/Yes
  d2 = as.data.frame(
    d2 %>%
      mutate(
        prob_cut = cut(value,breaks=seq(0,1,by=0.1),include.lowest=TRUE)
      )
  )
  
  # actual percentage of True/Yes 
  dplot2 = as.data.frame(
    d2 %>%
      group_by(prob_cut,method,gamma) %>%
      summarize(
        true_rate = mean(binary_outcome),
        num_tasks = n() 
      )
  )
  dplot2 = dplot2[dplot2$num_tasks >= 10,]
  dplot2$gamma = paste0("\U03B3 = ",dplot2$gamma)
  
  my_palette2 = as.vector(c(
    brewer.pal(9,'YlOrBr')[4],
    brewer.pal(9,'BuPu')[6],
    brewer.pal(9,'YlGnBu')[9],
    brewer.pal(9,'Reds')[7],
    brewer.pal(9,'Oranges')[5]
  ))
  my_shapes2 = c(3,4,7,8,16)
  
  # construct the calibration plot (robust recalibr vs benchmarks)
  pl_calibr2 = ggplot(data = dplot2, aes(x= as.numeric(prob_cut), y=true_rate, color=method, shape=method)) +
    geom_line() +
    geom_point(size = 3) +
    facet_wrap(~gamma, ncol = 3) +
    scale_color_manual(values=my_palette2) +
    scale_shape_manual(values=my_shapes2) +
    annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 0.1, alpha = 0.2) +
    annotate("rect", xmin = 1.5, xmax = 2.5, ymin = 0.1, ymax = 0.2, alpha = 0.2) +
    annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 0.2, ymax = 0.3, alpha = 0.2) +
    annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 0.3, ymax = 0.4, alpha = 0.2) +
    annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 0.4, ymax = 0.5, alpha = 0.2) +
    annotate("rect", xmin = 5.5, xmax = 6.5, ymin = 0.5, ymax = 0.6, alpha = 0.2) +
    annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 0.6, ymax = 0.7, alpha = 0.2) +
    annotate("rect", xmin = 7.5, xmax = 8.5, ymin = 0.7, ymax = 0.8, alpha = 0.2) +
    annotate("rect", xmin = 8.5, xmax = 9.5, ymin = 0.8, ymax = 0.9, alpha = 0.2) +
    annotate("rect", xmin = 9.5, xmax = 10.5, ymin = 0.9, ymax = 1, alpha = 0.2) +
    scale_x_continuous(name='Predicted probability of True',breaks = seq(1,10,by=1), labels = levels(dplot2$prob_cut)) +
    scale_y_continuous(name='Proportion True',breaks = seq(0,1,by=0.1)) +
    theme_bw(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45,hjust=1),
      legend.position = 'top'
    )
  
  ggsave(filename = "./figures/calibr2.pdf", plot = pl_calibr2, width=9.3,height=7,device=cairo_pdf)
  #quartz(type='pdf',file = "./calibr2.pdf",width=8.7,height=6.5)
  #pl_calibr2
  #dev.off()
  
  
  return(pl_calibr2)
}

#----------MAIN

# read dataset
dataset_link = './data/recalibr_dataset.csv'
dat = read.csv(dataset_link, header = TRUE)

# plot correlations between prediction and meta-prediction (Appendix E)
pl_corr = plotCorr(dat)

# calculate prob aggregates
dat = getSimpleAggregates(dat)
dat = runMinimalPivoting(dat)
dat = runKnowledgeWeighted(dat)
dat = runSOAlgorithm(dat)
dat = runMPW(dat)

# get estimated priors and produce Figure 3
dat2 = getEstimatedPrior(dat)
pl_estprior = plotEstPriors(dat2)

# plots in Appendix D
pl_failed = plotFailedEstimates(dat2)

# exclude two items where estimated prior is not within [0,1] 
dat3 = filterEstimatedPrior(dat2)

# table average vs est prior by state (Table 1)
table_avgvsprior = tabulateAvgVsPrior(dat3)
table_avgvsprior

# plot averages, Figure E1
pl_avgpred = plotAverages(dat2)
# plot the number of wrong-sided averages, Figure 2
pl_wrongsided = plotWrongSidedAverages(dat3)
# tabulate the number of wrong-sided averages by confidence, Table 3
table_wsbyconfidence = tabulateWrongSidedByConfidence(dat3)
table_wsbyconfidence

# implement robust recalibration and extremization away from 0.5
dat_transformed = transformAvgProb(dat3)

## comparative analysis: robust recalibration vs average and extremized average
dat_brier = calculateBrierScores(dat_transformed)
# summary stats, Brier scores (Table E1)
summarystatsBrierScores(dat_brier) 

# plot Brier scores, Figures 4 and F1
pl_brier = plotBrierScores(dat_brier,'./figures/brier1.pdf')
pl_brierbydataset = plotBrierScoresByDataset(dat_brier)

# Testing Brier scores, robust.recalibr vs extrem.average reported in Table 2
testBrier = testBrierScore(dat_brier)
print(testBrier, include.rownames=FALSE)

# Testing Brier scores by data set, Table F1
testBrierByDataset = testBrierScoreByDataset(dat_brier)
print(testBrierByDataset[[1]], include.rownames=FALSE) #artwork
print(testBrierByDataset[[2]], include.rownames=FALSE) #nfl
print(testBrierByDataset[[3]], include.rownames=FALSE) #science
print(testBrierByDataset[[4]], include.rownames=FALSE) #state

# pairwise differences in Brier score, Figure 5
pl_diff = plotBrierScoreDiff(dat_brier)

# Calibration curve, Figure 6
pl_calibr1 = calibrationPlot(dat_transformed)


## comparative analysis: robust recalibration vs aggregation algorithms that use meta-predictions
dat_brier2 = calculateBrierScoresBenchmarks(dat_transformed)
# summary stats, Brier scores (Table E2)
summarystatsBrierScoresBenchmarks(dat_brier2) 

# plot Brier scores, Figures 7 and F2
pl_brier2 = plotBrierScoresBenchmarks(dat_brier2,'./figures/brier2.pdf')
pl_brier2bydataset = plotBrierScoresBenchmarksByDataset(dat_brier2)


# Testing Brier scores
testBrier2 = testBrierScoresBenchmarks(dat_brier2)
# ANOVA test for differences in Brier scores (Section 5.3.2, "The difference between the Brier scores of algorithms is significant")
testBrier2[[1]]
# Pairwise comparisons, Table 4 and Table F3
print(testBrier2[[2]], include.rownames=FALSE)

# Testing Brier scores by dataset
testBrier2ByDataset = testBrierScoresBenchmarksByDataset(dat_brier2)

# ANOVA test for differences in Brier scores in each dataset, Table F2
testBrier2ByDataset[[1]][1] #artwork
testBrier2ByDataset[[2]][1] #nfl data
testBrier2ByDataset[[3]][1] #science data
testBrier2ByDataset[[4]][1] #states data

# Pairwise comparisons, Table F4
print(testBrier2ByDataset[[1]][2], include.rownames=FALSE) #artwork
print(testBrier2ByDataset[[2]][2], include.rownames=FALSE) #nfl data
print(testBrier2ByDataset[[3]][2], include.rownames=FALSE) #science data
print(testBrier2ByDataset[[4]][2], include.rownames=FALSE) #states data

# Calibration curve, Figure 8
pl_calibr2 = calibrationPlotBenchmarks(dat_transformed)


