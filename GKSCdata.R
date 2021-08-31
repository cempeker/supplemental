########### GKSCdata.R : R script for the Surprising Overshoot paper, Section 5.2
########## Structure of the script: 1. Loading packages, 2. Function declarations, 3. Reading data, 4. Main.

### Set working directory 
#setwd('...')

### libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)
library(latex2exp)
library(MASS)
library(RColorBrewer)
library(jcolors)
library(mltools)
library(entropy)
library(moments)
library(xtable)
#library(ggdark)


################----------------FUNCTION DECLARATIONS--------------##############


constructGeneralKnowledgeData = function(){
# reads General Knowledge Data from Martinie,Wilkening,Howe
  dataset_names = c('r2a_rawdata','r2b_rawdata','r2c_rawdata','r2d_rawdata','r2e_rawdata')
  dataset_links = paste0('./AnalysisCode/Raws/',dataset_names,'.xlsx')
  dat_gk = NULL
  subject_count = 0
  for(i in 1:5){
    # read raw data
    dat_gk_temp = as.data.frame(read_xlsx(dataset_links[i],col_names = FALSE))
    dat_gk_temp$Dataset = i
    dat_gk_temp$subject = subject_count + 1:nrow(dat_gk_temp)
    subject_count = subject_count + nrow(dat_gk_temp)
    
    # rename columns, f and g represent probabilistic predictions and meta-predictions
    colnames(dat_gk_temp) = c(1:400,'Dataset','subject')
    colnames(dat_gk_temp)[seq(1,397,by=4)]= paste0('binaryf.',100*(i-1) + 1:100)
    colnames(dat_gk_temp)[seq(2,398,by=4)]= paste0('binaryg.',100*(i-1) + 1:100)
    colnames(dat_gk_temp)[seq(3,399,by=4)]= paste0('f.',100*(i-1) + 1:100)
    colnames(dat_gk_temp)[seq(4,400,by=4)]= paste0('g.',100*(i-1) + 1:100)
    
    # wide to long
    dat_gk_temp = melt(dat_gk_temp,id.vars=c('Dataset','subject'))
    dat_gk_temp = dat_gk_temp %>% separate(variable,into=c('report','item'))
    dat_gk_temp$item = paste0('gk',dat_gk_temp$item)
    dat_gk_temp$task_type = 'General Knowledge'
    
    # bind to whole data
    dat_gk = rbind(dat_gk,dat_gk_temp)
    
    # remove temp vars
    rm(dat_gk_temp)
  }
  
  
  # question list and outcomes
  dat_gk_questions = as.data.frame(read_xlsx('./AnalysisCode/Raws/QuestionsList.xlsx',col_names = TRUE))
  colnames(dat_gk_questions)[which(colnames(dat_gk_questions) == 'Order')] = 'item'
  dat_gk_questions$item = paste0('gk',dat_gk_questions$item) 
  
  # merge outcomes with subject reports
  dat_gk = merge(dat_gk,dat_gk_questions[,c('item','Dataset','Difficulty','Actual')],by=c('item','Dataset'))
  colnames(dat_gk)[which(colnames(dat_gk) == 'Actual')] = 'binary_outcome'
  
  rm(dataset_links,dataset_names,dat_gk_questions)
  
  # cast to f and g being in different columns
  dat_gk = dcast(dat_gk, item + Dataset + subject + binary_outcome + task_type + Difficulty ~  report, value.var = 'value')
  dat_gk$binaryf = dat_gk$binaryf - 1
  
  # remove subjects with NA in one of the reports
  dat_gk = dat_gk[complete.cases(dat_gk),]
  
  # return data
  return(dat_gk)
}

categorizeGKitems = function(dat){
# categorizes items by dispersion
  
  # calculate std.dev and reverse kurtosis (measures of dispersion)
  d = as.data.frame(
    dat %>%
      group_by(item) %>%
      summarize(
        f_sd = sd(f),
        f_kurtosis = (-1)*kurtosis(f)
      )
  )

  # calculate 10% and 90% quantiles for std.dev and reverse kurtosis
  d = as.data.frame(
    d %>%
      mutate(
        f_sd_10 = quantile(f_sd,probs = 0.10),
        f_sd_90 = quantile(f_sd,probs = 0.90),
        f_kurtosis_10 = quantile(f_kurtosis, probs = 0.10),
        f_kurtosis_90 = quantile(f_kurtosis, probs = 0.90),
    )
  )

  # label items according to the dispersion of predictions
  d$inf_asymmetry_f = 'interm'
  d$inf_asymmetry_entropy = 'interm'

  d[d$f_sd <= d$f_sd_10,'inf_asymmetry_f'] = 'lowest'
  d[d$f_sd >= d$f_sd_90,'inf_asymmetry_f'] = 'highest'
  d[d$f_kurtosis <= d$f_kurtosis_10,'inf_asymmetry_kurtosis'] = 'lowest'
  d[d$f_kurtosis >= d$f_kurtosis_90,'inf_asymmetry_kurtosis'] = 'highest'

  dat = merge(dat,d[c('item','inf_asymmetry_f','inf_asymmetry_kurtosis')], by = 'item')
  
  return(dat)
}

constructStateCapitalData = function(){
# reads General Knowledge Data from Martinie,Wilkening,Howe

  # read raw data
  dataset_link = './sca_data/Files/m4data.csv'
  dat_raw <- read.csv(dataset_link, header = FALSE)
  dat_raw$subject = 1:89
  
  # rename columns, f and g represent probabilistic predictions and meta-predictions
  colnames(dat_raw) = c(1:200,'subject')
  colnames(dat_raw)[seq(1,197,by=4)]= paste0('binaryf.',1:50)
  colnames(dat_raw)[seq(2,198,by=4)]= paste0('binaryg.',1:50)
  colnames(dat_raw)[seq(3,199,by=4)]= paste0('f.',1:50)
  colnames(dat_raw)[seq(4,200,by=4)]= paste0('g.',1:50)
  
  # wide to long
  dat_s = melt(dat_raw,id.vars=c('subject'))
  dat_s = dat_s %>% separate(variable,into=c('report','item'))
  dat_s$item = paste0('state',dat_s$item)
  dat_s$task_type = 'State Capital'
  
  
  # question list and outcomes
  dat_s_questions = read.csv('./sca_data/Files/m4data_details.csv',header = TRUE)
  colnames(dat_s_questions)[which(colnames(dat_s_questions) == 'Question')] = 'item'
  dat_s_questions$item = paste0('state',dat_s_questions$item) 
  
  # merge outcomes with subject reports
  dat_s = merge(dat_s,dat_s_questions[,c('item','Outcome')],by=c('item'))
  colnames(dat_s)[which(colnames(dat_s) == 'Outcome')] = 'binary_outcome'
  
  # cast to f and g being in differen columns
  dat_s = dcast(dat_s, item + subject + binary_outcome + task_type ~  report, value.var = 'value')
  dat_s$binaryf = dat_s$binaryf - 1
  
  # remove subjects with NA in one of the reports
  dat_s = dat_s[complete.cases(dat_s),]
  
  # return data
  return(dat_s)
}

categorizeSCitems = function(dat){
# categorizes items by dispersion 

  # calculate std.dev and reverse kurtosis (measures of dispersion) 
  d = as.data.frame(
    dat %>%
      group_by(item) %>%
      summarize(
        f_sd = sd(f),
        f_kurtosis = (-1)*kurtosis(f)
      )
  )
  
  # calculate 25% and 75% quantiles for std.dev and reverse kurtosis
  d = as.data.frame(
    d %>%
      mutate(
        f_sd_25 = quantile(f_sd,probs = 0.25),
        f_sd_75 = quantile(f_sd,probs = 0.75),
        f_kurtosis_25 = quantile(f_kurtosis, probs = 0.25),
        f_kurtosis_75 = quantile(f_kurtosis, probs = 0.75)
      )
  )

  # label items according to the dispersion of predictions
  d$inf_asymmetry_f = 'interm'
  d$inf_asymmetry_entropy = 'interm'
  d[d$f_sd <= d$f_sd_25,'inf_asymmetry_f'] = 'lowest'
  d[d$f_sd >= d$f_sd_75,'inf_asymmetry_f'] = 'highest'
  d[d$f_kurtosis <= d$f_kurtosis_25,'inf_asymmetry_kurtosis'] = 'lowest'
  d[d$f_kurtosis >= d$f_kurtosis_75,'inf_asymmetry_kurtosis'] = 'highest'
  
  dat = merge(dat,d[c('item','inf_asymmetry_f','inf_asymmetry_kurtosis')], by = 'item')
  
  return(dat)
}



getSimpleAggregates = function(dat){
# calculates simple aggregates such as average prediction, median prediction, majority vote etc.
  
  if(unique(dat$task_type) == 'Coin Flips'){
    
    dat = as.data.frame(
      dat %>%
        group_by(item) %>%
        mutate(
          xbar = mean(f, na.rm = TRUE),
          xhat_median = median(f, na.rm = TRUE),
        ))
    
  } else {
    
    dat = as.data.frame(
      dat %>%
        group_by(item) %>%
        mutate(
          xbar = mean(f, na.rm = TRUE),
          xhat_median = median(f, na.rm = TRUE),
          xhat_majority = 100*round(mean(binaryf, na.rm = TRUE)),
          xhat_confweight = ifelse(xbar > 50,100,0),
        ))
    
  }
  
  return(dat)
}

runSOA = function(dat){
# calculates the SO estimate
  
  dat$forecast_overshoot = ifelse(dat$f > dat$xbar,1,0)
  #dat$meta_overshoot = ifelse(dat$g > dat$xbar_others,1,0)
  dat$meta_overshoot = ifelse(dat$g > dat$xbar,1,0)
  
  dat = as.data.frame(
    dat %>%
      group_by(item) %>%
      mutate(
        px = mean(forecast_overshoot),
        pz = mean(meta_overshoot),
        q = 1-pz,
        xhat_SOA = as.numeric(quantile(f,q,type=1))
      ))
  
  return(dat)
}

runMinimalPivoting = function(dat){
# get minimal pivoting estimate

  dat = as.data.frame(
    dat %>%
      group_by(item) %>%
      mutate(zbar = mean(g,na.rm = TRUE))
  )
  dat$xhat_MP = 2*dat$xbar - dat$zbar
  
  return(dat)
}

estimateKWatItem = function(f,g){
  
  #E = dat[dat$item == i,'f']
  #P = dat[dat$item == i,'g']
  E = f 
  P = g
  # get the number of reports
  J = length(E)
  # Judges' implied predictions
  O = (1/J)*E + (J-1)/J*P 
  # Equally weighted average
  Ebar = mean(E) 
  
  # estimate linear model
  lin_mdl = stats::lm(P ~ E)
  r = stats::residuals(lin_mdl)
  sigmae2 = stats::deviance(lin_mdl)/stats::df.residual(lin_mdl)
  sigmaE2 = mean(E^2) - Ebar^2
  Q = (O - (J - 1)/J * r) %*% t(O - (J - 1)/J * r) + E %*% 
    t(E) * (((J - 1)/J)^2 * (J - 2)/(J * (J - 4)) * sigmae2/sigmaE2)
  c = -2 * Ebar * (O - (J - 1)/J * r + (((J - 1)/J)^2 * (J - 2)/(J * (J - 4)) * sigmae2/sigmaE2) * E)
  
  V = MASS::ginv(Q %*% Q + matrix(0.25, J, J)) %*% Q
  R = 1 - Q %*% V %*% matrix(1, J, 1)
  L = (t(R) %*% R + 0.25 * sum(V)^2)[1, 1]
  #alpha = (0.5 * (R %*% matrix(1, 1, J) - L * diag(J)) %*% V %*% c + R)/L ## OLD
  alpha = (0.5 * (R %*% matrix(1, 1, J) - L * diag(J)) %*% t(V) %*% c + R)/L ## NEW
  
  # knowledge weighted estimate
  kw_estimate = as.numeric(t(alpha) %*% E)
  
  return(kw_estimate)
}

runKnowledgeWeighted = function(dat){
# get Knowledge weighted estimate (Palley and Satopaa 2020)
  
  dat = as.data.frame(
    dat %>%
      group_by(item) %>%
      mutate(
        xhat_KW = estimateKWatItem(f,g)
      )
  )
  
  # return
  return(dat)
}

runMPW = function(dat){
# function to implement meta-probability weighting (MPW) algorithm of Martinie, Wilkening, Howe
  
  # get forecast - metaprediction for each item and subject
  dat$f_min_g = abs(dat$f - dat$g)
  
  # compute weights
  dat = as.data.frame(
    dat %>% 
      group_by(item) %>%
      mutate(
        weight = f_min_g / sum(f_min_g),
        xhat_MPW = weight %*% f
      )
  )
  
  return(dat)
}


calibrateAggPredictions = function(dat){
# Transform probabilistic predictions (from percentage to 0-1 scale)
  dat$xbar = dat$xbar / 100
  dat$xhat_median = dat$xhat_median/100
  dat$xhat_MP = dat$xhat_MP/100
  dat$xhat_KW = dat$xhat_KW/100
  dat$xhat_MPW = dat$xhat_MPW/100
  dat$xhat_SOA = dat$xhat_SOA/100
  
  return(dat)
}



sampleItems= function(i, dat2,s_size){
# Sampling for bootstrap procedure  
  dat3 = as.data.frame(
    dat2 %>% 
      sample_n(s_size, replace = TRUE) 
  )
  
  dat3$brierscore = (dat3$binary_outcome - dat3$estimate)^2
  
  dat4 = as.data.frame(
    dat3 %>%
      group_by(method) %>%
      summarize(
        tr_brierscore = 100-100*mean(brierscore)
      )
  ) 
  
  dat4$benchmark = dat4[dat4$method == 'xhat_SOA','tr_brierscore']
  dat4$brierdiff = dat4$benchmark - dat4$tr_brierscore
  
  dat5 = dat4[dat4$method != 'xhat_SOA',c('method','brierdiff')]
  
  dat5$method = recode(dat5$method,'xbar' = 'Simp.Average','xhat_median' = 'Median','xhat_MP' = 'Min.Pivot','xhat_KW'='Know.Weight','xhat_MPW'='Meta.Prob.Weight') 
  dat5$method = droplevels(dat5$method)
  
  return(dat5)
  
}

bootstrapBrierByInfo = function(i, dat){
# run bootstrap procedure    
  cat('calculating pairwise differences for Inf Asymmetry=',i,'\n')
  dat2 = dat[dat$inf_asymmetry == i,]
  s_size = nrow(dat2)
  
  dat6 = lapply(1:1000,sampleItems, dat2 = dat2, s_size = s_size)
  dat6 = bind_rows(dat6)
  dat6$iteration = rep(1:1000,each=5)
  dat6$inf_asymmetry = i
  
  return(dat6)
}


calculateTransformedBrierByInfo = function(dat,info_type){
# calculate transformed brier score for each method
  
  if(info_type =='f'){
    dat$inf_asymmetry = dat$inf_asymmetry_f
  } else if(info_type == 'kurtosis'){
    dat$inf_asymmetry = dat$inf_asymmetry_kurtosis
  }
  
  id_vars = c('item','task_type','binary_outcome','inf_asymmetry')
  dat = melt(dat,id.vars = id_vars, measure.vars = c('xbar','xhat_median','xhat_SOA','xhat_MP','xhat_KW','xhat_MPW'), variable.name = 'method', value.name = 'estimate')
  dat = unique(dat)
  
  dat$brierscore = (dat$binary_outcome - dat$estimate)^2 
  dat_brier = as.data.frame(
    dat %>%
      group_by(inf_asymmetry,method,task_type) %>%
      summarize(
        tr_brierscore = 100-100*mean(brierscore)
      )
  )
  
  return(dat_brier)
} 

calculateTransformedBrierDiffByInfo = function(dat, info_type){
#### function to calculate error reduction from using xhat_SOA instead of benchmarks (pairwise comparison)
  
  if(info_type =='f'){
    dat$inf_asymmetry = dat$inf_asymmetry_f
  } else if(info_type == 'kurtosis'){
    dat$inf_asymmetry = dat$inf_asymmetry_kurtosis
  }
  
  id_vars = c('item','task_type','binary_outcome','inf_asymmetry')
  
  dat = melt(dat,id.vars = id_vars, measure.vars = c('xbar','xhat_median','xhat_SOA','xhat_MP','xhat_KW','xhat_MPW'), variable.name = 'method', value.name = 'estimate')
  
  dat = unique(dat)

  # run bootstrap procedure for each dispersion category  
  d_highest = bootstrapBrierByInfo('highest',dat = dat)
  d_interm = bootstrapBrierByInfo('interm',dat = dat)
  d_lowest = bootstrapBrierByInfo('lowest',dat = dat)
  
  d = bind_rows(d_highest,d_interm,d_lowest)
  d$task_type = unique(dat$task_type)
  
  # return bootstrap data
  return(d)
}

plotTransformedBrierByInfo2 = function(dat_brier_gk,dat_brier_sc, measure_type){
# function to plot transformed brier scores (by method and category of dispersion)
 
  my_palette = c( as.vector(jcolors(palette = 'pal2')[1:5]),as.vector(jcolors(palette = 'default')[3]) )[1:6]
  
  if(measure_type == 'std_dev'){
    xaxis_title = 'Dispersion (std.dev of predictions)'
  } else if(measure_type == 'kurtosis'){
    xaxis_title = 'Dispersion (reverse order of kurtosis)'
  }

  dat_brier_gk$inf_asymmetry = recode(dat_brier_gk$inf_asymmetry, 'lowest' = 'Bottom 10%','interm' = 'Middle 80%','highest' = 'Top 10%')
  dat_brier_gk$inf_asymmetry = factor(dat_brier_gk$inf_asymmetry, levels =c('Bottom 10%','Middle 80%','Top 10%')) 

  dat_brier_sc$inf_asymmetry = recode(dat_brier_sc$inf_asymmetry, 'lowest' = 'Bottom 25%','interm' = 'Middle 50%','highest' = 'Top 25%')
  dat_brier_sc$inf_asymmetry = factor(dat_brier_sc$inf_asymmetry, levels =c('Bottom 25%','Middle 50%','Top 25%'))
  
  dat_brier = bind_rows(dat_brier_gk,dat_brier_sc)
  
  dat_brier$method = recode(dat_brier$method,'xbar' = 'Simp.Average','xhat_median' = 'Median','xhat_SOA' = 'Surp.Overshoot','xhat_MP' = 'Min.Pivot','xhat_KW'='Know.Weight','xhat_MPW'='Meta.Prob.Weight') 
  dat_brier$method = factor(dat_brier$method , levels = c('Simp.Average','Median','Min.Pivot','Know.Weight','Meta.Prob.Weight','Surp.Overshoot'))
   
  pl = ggplot(dat_brier, aes(x=inf_asymmetry, y=tr_brierscore, fill=method)) + 
    geom_bar(
      stat="identity", 
      color="black", 
      position=position_dodge(),
      width = 0.7) +
    scale_fill_manual(values=my_palette) +
    facet_wrap(~task_type, scales = 'free_x') +
    #geom_errorbar(
    #  aes(ymin=tr_brier_lb, ymax=tr_brier_ub), 
    #  width=.2,
    #  position=position_dodge(.9)) +
    coord_cartesian(ylim = c(60,100)) +
    scale_x_discrete(name = xaxis_title) +
    scale_y_continuous(name = 'Transformed Brier Score') +
    theme_bw(base_size = 14) + 
    theme(
      #axis.text.x = element_text(face="bold",size=12, angle=30, hjust=1)
      axis.text.x = element_text(face="bold",size=12)
    )
  
  return(pl)
  
}

plotTransformedBrierDiffByInfo2 = function(dat_diff_gk, dat_diff_sc, measure_type){
# function to plot 95% bootstrap confidence intervals (by benchmark and category of dispersion)
  
  my_palette = c( as.vector(jcolors(palette = 'pal2')[1:5]),as.vector(jcolors(palette = 'pal2')[c(3,5,6)]) )[1:5]
  
  if(measure_type == 'std_dev'){
    xaxis_title = 'Dispersion (std.dev of predictions)'
  } else if(measure_type == 'kurtosis'){
    xaxis_title = 'Dispersion (reverse order of kurtosis)'
  }
  
  dat_diff_gk$inf_asymmetry = recode(dat_diff_gk$inf_asymmetry, 'lowest' = 'Bottom 10%','interm' = 'Middle 80%','highest' = 'Top 10%')
  dat_diff_gk$inf_asymmetry = factor(dat_diff_gk$inf_asymmetry, levels =c('Bottom 10%','Middle 80%','Top 10%')) 
  
  dat_diff_sc$inf_asymmetry = recode(dat_diff_sc$inf_asymmetry, 'lowest' = 'Bottom 25%','interm' = 'Middle 50%','highest' = 'Top 25%')
  dat_diff_sc$inf_asymmetry = factor(dat_diff_sc$inf_asymmetry, levels =c('Bottom 25%','Middle 50%','Top 25%'))
  
  colnames(dat_diff_gk)[colnames(dat_diff_gk) == "method"] = "comparison"
  colnames(dat_diff_sc)[colnames(dat_diff_sc) == "method"] = "comparison"
  
  d_gk = as.data.frame(
    dat_diff_gk %>%
      group_by(task_type, comparison, inf_asymmetry) %>%
      summarize(
        ymin = quantile(brierdiff, 0.025, names = FALSE),
        lower = quantile(brierdiff, 0.25, names = FALSE),
        middle = quantile(brierdiff, 0.5, names = FALSE),
        upper = quantile(brierdiff, 0.75, names = FALSE),
        ymax = quantile(brierdiff, 0.975, names = FALSE)
      )
  )
  
  d_sc = as.data.frame(
    dat_diff_sc %>%
      group_by(task_type, comparison, inf_asymmetry) %>%
      summarize(
        ymin = quantile(brierdiff, 0.025, names = FALSE),
        lower = quantile(brierdiff, 0.25, names = FALSE),
        middle = quantile(brierdiff, 0.5, names = FALSE),
        upper = quantile(brierdiff, 0.75, names = FALSE),
        ymax = quantile(brierdiff, 0.975, names = FALSE)
      )
  )
  
  d = bind_rows(d_gk,d_sc)
  
  pl = ggplot(d,aes(x = as.factor(inf_asymmetry),fill=comparison)) +
    geom_boxplot(aes(
      ymin = ymin, 
      lower = lower, 
      middle = middle, 
      upper = upper, 
      ymax = ymax) , stat='identity') +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), position = "dodge", size = 0.4) +
    geom_hline(aes(yintercept = 0),linetype = 'dashed',size=0.5) +
    facet_wrap(~task_type, scales = 'free_x') +
    scale_y_continuous(name = 'Pairwise differences in Score') +
    scale_x_discrete(name = xaxis_title) +
    scale_fill_manual(values=my_palette) +
    #ggtitle(TeX('Percentge error reduction, \\hat{x} vs alternative')) +
    theme_bw(base_size = 14) + 
    theme(legend.position = 'bottom', legend.direction = 'horizontal')
  
  return(pl)
  
}


getConfIntervalsGK = function(gk_brier_diff){
# get exact Bootstrap confidence intervals, General Knowledge data

  d = as.data.frame(
    gk_brier_diff %>%
      group_by(method, inf_asymmetry) %>%
      summarize(
        low_bound = quantile(brierdiff, 0.025, names = FALSE),
        upp_bound = quantile(brierdiff, 0.975, names = FALSE),
      )
  )

  colnames(d) = c('Comparison','Dispersion','Low.B.','Upp.B.')
  d$Dispersion <- recode_factor(d$Dispersion, highest ="Top 10%", interm ="Middle 80%", lowest ="Bottom 10%")
  
  d2 = d[order(d$Dispersion, decreasing = TRUE),]
  
  return(print(xtable(d2), include.rownames=FALSE))
}

getConfIntervalsSC = function(sc_brier_diff){
# get exact Bootstrap confidence intervals, State Capital data

  d = as.data.frame(
    sc_brier_diff %>%
      group_by(method, inf_asymmetry) %>%
      summarize(
        low_bound = quantile(brierdiff, 0.025, names = FALSE),
        upp_bound = quantile(brierdiff, 0.975, names = FALSE),
      )
  )
  
  colnames(d) = c('Comparison','Dispersion','Low.B.','Upp.B.')
  d$Dispersion <- recode_factor(d$Dispersion, highest ="Top 25%", interm ="Middle 50%", lowest ="Bottom 25%")
  
  d2 = d[order(d$Dispersion, decreasing = TRUE),]
  
  return(print(xtable(d2), include.rownames=FALSE))
}


################----------READ RAW DATA & CONSTRUCT DATA SET--------------################


##### General knowledge data
dat_gk = constructGeneralKnowledgeData()
dat_gk = categorizeGKitems(dat_gk)

#### State capital data
dat_sc = constructStateCapitalData()
dat_sc = categorizeSCitems(dat_sc)


####################------------------MAIN--------------################


dat_gk = getSimpleAggregates(dat_gk)
dat_sc = getSimpleAggregates(dat_sc)

dat_gk = runMinimalPivoting(dat_gk)
dat_sc = runMinimalPivoting(dat_sc)

dat_gk = runKnowledgeWeighted(dat_gk)
dat_sc = runKnowledgeWeighted(dat_sc)

dat_gk = runMPW(dat_gk)
dat_sc = runMPW(dat_sc)

dat_gk = runSOA(dat_gk)
dat_sc = runSOA(dat_sc)


#Transform probabilistic predictions (from percentage to 0-1 scale)
dat_gk = calibrateAggPredictions(dat_gk)
dat_sc = calibrateAggPredictions(dat_sc)

### Analysis

# General Knowledge
#gk_brier = calculateTransformedBrierByInfo(dat_gk,'kurtosis')
gk_brier = calculateTransformedBrierByInfo(dat_gk,'f')
#gk_brier_diff = calculateTransformedBrierDiffByInfo(dat_gk,'kurtosis')
gk_brier_diff = calculateTransformedBrierDiffByInfo(dat_gk,'f')

# State Capital
#sc_brier = calculateTransformedBrierByInfo(dat_sc,'kurtosis')
sc_brier = calculateTransformedBrierByInfo(dat_sc,'f')
#sc_brier_diff = calculateTransformedBrierDiffByInfo(dat_sc,'kurtosis')
sc_brier_diff = calculateTransformedBrierDiffByInfo(dat_sc,'f')


# Plots
plotTransformedBrierByInfo2(gk_brier,sc_brier,'std_dev')
plotTransformedBrierDiffByInfo2(gk_brier_diff,sc_brier_diff,'std_dev')
#plotTransformedBrierByInfo2(gk_brier,sc_brier,'kurtosis')
#plotTransformedBrierDiffByInfo2(gk_brier_diff,sc_brier_diff,'kurtosis') 


# Confidence Intervals
#getConfIntervalsGK(gk_brier_diff)
#getConfIntervalsSC(sc_brier_diff)

