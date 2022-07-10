########### Generate bootstrap samples, General Knowledge and State Capital data
# Instructions:
# 1. Load LIBRARIES
# 2. Define FUNCTIONS
# 3. Run the commands in MAIN

################----------------LIBRARIES--------------##############

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(moments)
library(latex2exp)
library(jcolors)
library(metaggR)
library(xtable)

################----------------FUNCTIONS--------------##############

# Read and prepare data, including the categorization of items in terms of dispersion

constructGeneralKnowledgeData = function(){
  
  # read raw data
  dataset_names = c('r2a_rawdata','r2b_rawdata','r2c_rawdata','r2d_rawdata','r2e_rawdata')
  dataset_links = paste0('./data_binary/',dataset_names,'.csv')
  dat_gk = NULL
  subject_count = 0
  for(i in 1:5){
    dat_gk_temp = read.csv(dataset_links[i], header = FALSE)
    dat_gk_temp$Dataset = i
    dat_gk_temp$subject = subject_count + 1:nrow(dat_gk_temp)
    subject_count = subject_count + nrow(dat_gk_temp)
    
    # get forecasts and name columns
    # f: probabilistic pred, g: probabilistic meta-pred, votef: binary vote (0,1), voteg: metavote (estimates on percentage endorsements) 
    colnames(dat_gk_temp) = c(1:400,'Dataset','subject')
    colnames(dat_gk_temp)[seq(1,397,by=4)]= paste0('votef.',100*(i-1) + 1:100)
    colnames(dat_gk_temp)[seq(2,398,by=4)]= paste0('voteg.',100*(i-1) + 1:100)
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
  
  # get question list and outcomes
  dat_gk_questions = read.csv('./data_binary/r2_outcomes.csv',header = FALSE)
  colnames(dat_gk_questions) = c('item','Actual','Difficulty','Text')
  dat_gk_questions$item = paste0('gk',dat_gk_questions$item) 
  
  # merge outcomes with prediction data
  dat_gk = merge(dat_gk,dat_gk_questions[,c('item','Difficulty','Actual')],by=c('item'))
  colnames(dat_gk)[which(colnames(dat_gk) == 'Actual')] = 'binary_outcome'
  
  rm(dataset_links,dataset_names,dat_gk_questions)
  
  # cast data so that predictions and meta-predictions are in different columns
  dat_gk = dcast(dat_gk, item + Dataset + subject + binary_outcome + task_type + Difficulty ~  report, value.var = 'value')
  dat_gk$votef = dat_gk$votef - 1
  
  # remove cases with NA entry (no such rows are found in data)
  dat_gk = dat_gk[complete.cases(dat_gk),]
  
  # return data
  return(dat_gk)
}

categorizeGKitems = function(dat){
  
  d = as.data.frame(
    dat %>%
      group_by(item) %>%
      summarize(
        f_sd = sd(f),   # std deviation of predictions
        f_kurtosis = (-1)*kurtosis(f),  # reverse kurtosis of predictions
      )
  )
  
  # calculate quantiles for low, medium and high dispersion levels
  d = as.data.frame(
    d %>%
      mutate(
        f_sd_10 = quantile(f_sd,probs = 0.10),
        f_sd_90 = quantile(f_sd,probs = 0.90),
        f_sd_33 = quantile(f_sd,probs = 0.33),
        f_sd_66 = quantile(f_sd,probs = 0.66),
        f_kurtosis_10 = quantile(f_kurtosis, probs = 0.10),
        f_kurtosis_90 = quantile(f_kurtosis, probs = 0.90),
        f_kurtosis_33 = quantile(f_kurtosis, probs = 0.33),
        f_kurtosis_66 = quantile(f_kurtosis, probs = 0.66),
    )
  )
  
  # set dispersion categories (for both 10/80/10 and 33/33/33 splits)
  d$dispersion_sd_1 = 'middle'
  d$dispersion_sd_2 = 'middle'
  d$dispersion_kurtosis_1 = 'middle'
  d$dispersion_kurtosis_2 = 'middle'
  d[d$f_sd <= d$f_sd_10,'dispersion_sd_1'] = 'bottom'
  d[d$f_sd >= d$f_sd_90,'dispersion_sd_1'] = 'top'
  d[d$f_sd <= d$f_sd_33,'dispersion_sd_2'] = 'bottom'
  d[d$f_sd >= d$f_sd_66,'dispersion_sd_2'] = 'top'
  d[d$f_kurtosis <= d$f_kurtosis_10,'dispersion_kurtosis_1'] = 'bottom'
  d[d$f_kurtosis >= d$f_kurtosis_90,'dispersion_kurtosis_1'] = 'top'
  d[d$f_kurtosis <= d$f_kurtosis_33,'dispersion_kurtosis_2'] = 'bottom'
  d[d$f_kurtosis >= d$f_kurtosis_66,'dispersion_kurtosis_2'] = 'top'
  
  dat = merge(dat,d[c('item','dispersion_sd_1','dispersion_sd_2','dispersion_kurtosis_1','dispersion_kurtosis_2')], by = 'item')
  
  return(dat)
}

constructStateCapitalData = function(){
  
  # read raw data
  dataset_link = './data_binary/m4data.csv'
  dat_raw <- read.csv(dataset_link, header = FALSE)
  dat_raw$subject = 1:89
  
  # get forecasts and name columns
  # f: probabilistic pred, g: probabilistic meta-pred, votef: binary vote (0,1), voteg: metavote (estimates on percentage endorsements)
  colnames(dat_raw) = c(1:200,'subject')
  colnames(dat_raw)[seq(1,197,by=4)]= paste0('votef.',1:50)
  colnames(dat_raw)[seq(2,198,by=4)]= paste0('voteg.',1:50)
  colnames(dat_raw)[seq(3,199,by=4)]= paste0('f.',1:50)
  colnames(dat_raw)[seq(4,200,by=4)]= paste0('g.',1:50)
  
  # wide to long
  dat_s = melt(dat_raw,id.vars=c('subject'))
  dat_s = dat_s %>% separate(variable,into=c('report','item'))
  dat_s$item = paste0('state',dat_s$item)
  dat_s$task_type = 'State Capital'
  
  
  # get question list and outcomes
  dat_s_questions = read.csv('./data_binary/m4data_details.csv',header = TRUE)
  colnames(dat_s_questions)[which(colnames(dat_s_questions) == 'Question')] = 'item'
  dat_s_questions$item = paste0('state',dat_s_questions$item) 
  
  # merge outcomes with prediction data
  dat_s = merge(dat_s,dat_s_questions[,c('item','Outcome')],by=c('item'))
  colnames(dat_s)[which(colnames(dat_s) == 'Outcome')] = 'binary_outcome'
  
  # cast data so that predictions and meta-predictions are in different columns
  dat_s = dcast(dat_s, item + subject + binary_outcome + task_type ~  report, value.var = 'value')
  
  # transform vote data so that 1 corresponds to reporting TRUE, 0 to reporting FALSE
  dat_s$votef = dat_s$votef - 1
  
  # remove cases with NA entry (no such rows are found in data)
  dat_s = dat_s[complete.cases(dat_s),]
  
  # return data
  return(dat_s)
}

categorizeSCitems = function(dat){
  
  d = as.data.frame(
    dat %>%
      group_by(item) %>%
      summarize(
        f_sd = sd(f),   # std deviation of predictions
        f_kurtosis = (-1)*kurtosis(f),  # reverse kurtosis of predictions
      )
  )
  
  # calculate quantiles for low, medium and high dispersion levels
  d = as.data.frame(
    d %>%
      mutate(
        f_sd_25 = quantile(f_sd,probs = 0.25),
        f_sd_75 = quantile(f_sd,probs = 0.75),
        f_sd_33 = quantile(f_sd,probs = 0.33),
        f_sd_66 = quantile(f_sd,probs = 0.66),
        f_kurtosis_25 = quantile(f_kurtosis, probs = 0.25),
        f_kurtosis_75 = quantile(f_kurtosis, probs = 0.75),
        f_kurtosis_33 = quantile(f_kurtosis, probs = 0.33),
        f_kurtosis_66 = quantile(f_kurtosis, probs = 0.66)
      )
  )

  # set dispersion categories (for both 25/50/25 and 33/33/33 splits)
  d$dispersion_sd_1 = 'middle'
  d$dispersion_sd_2 = 'middle'
  d$dispersion_kurtosis_1 = 'middle'
  d$dispersion_kurtosis_2 = 'middle'
  d[d$f_sd <= d$f_sd_25,'dispersion_sd_1'] = 'bottom'
  d[d$f_sd >= d$f_sd_75,'dispersion_sd_1'] = 'top'
  d[d$f_sd <= d$f_sd_33,'dispersion_sd_2'] = 'bottom'
  d[d$f_sd >= d$f_sd_66,'dispersion_sd_2'] = 'top'
  d[d$f_kurtosis <= d$f_kurtosis_25,'dispersion_kurtosis_1'] = 'bottom'
  d[d$f_kurtosis >= d$f_kurtosis_75,'dispersion_kurtosis_1'] = 'top'
  d[d$f_kurtosis <= d$f_kurtosis_33,'dispersion_kurtosis_2'] = 'bottom'
  d[d$f_kurtosis >= d$f_kurtosis_66,'dispersion_kurtosis_2'] = 'top'
  
  dat = merge(dat,d[c('item','dispersion_sd_1','dispersion_sd_2','dispersion_kurtosis_1','dispersion_kurtosis_2')], by = 'item')
  
  return(dat)
}


# Aggregation algorithms (SO and benchmarks)

getSimpleAggregates = function(dat){

  # calculate average and median for each item (also majority vote and confidence weighting using votes)  
  dat = as.data.frame(
      dat %>%
        group_by(item) %>%
        mutate(
          xbar = mean(f, na.rm = TRUE),
          xhat_median = median(f, na.rm = TRUE),
          xhat_majority = 100*round(mean(votef, na.rm = TRUE)),
          xhat_confweight = ifelse(xbar > 50,100,0),
        )
  )
  
  return(dat)
}

runSOAlgorithm = function(dat){
# Calculates and appends the SO estimate (a new column in data frame)
    
  dat$forecast_overshoot = ifelse(dat$f > dat$xbar,1,0)
  dat$meta_overshoot = ifelse(dat$g > dat$xbar,1,0)
  
  dat = as.data.frame(
    dat %>%
      group_by(item) %>%
      mutate(
        px = mean(forecast_overshoot),  # overshoot rate px
        pz = mean(meta_overshoot), # overshoot rate pz
        q = 1-pz,
        xhat_SOA = as.numeric(quantile(f,q,type=1)), # quantile type 1
        xhat_SOA2 = as.numeric(quantile(f,q,type=4)) # quantile type 4, interpolation
      )
  )
  
  return(dat)
}

runMinimalPivoting = function(dat){
# Calculates and appends the MP estimate (a new column in data frame)
  dat = as.data.frame(
    dat %>%
      group_by(item) %>%
      mutate(zbar = mean(g,na.rm = TRUE))
  )
  dat$xhat_MP = 2*dat$xbar - dat$zbar
  
  return(dat)
}

runKnowledgeWeighted = function(dat){
  # Calculates and appends the KW estimate (a new column in data frame)
  # if all predictions in a given Bootstrap sample are the same, knowledge_weighted_estimate throws an error. In such cases, KW estimate is simply set as the consensus estimate.
  
  dat = as.data.frame(
    dat %>%
      group_by(item) %>%
      mutate(
        not_all_E_same = ifelse(length(unique(f)) > 1,1,0), 
        xhat_KW = ifelse(not_all_E_same, knowledge_weighted_estimate(E = f,P = g, no_inf_check=TRUE), unique(E))
      )
  )
  
  # return
  return(dat)
}

runMPW = function(dat){
# Calculates and appends the MPW estimate (a new column in data frame)
  
  # get forecast - metaprediction for each item and subject
  dat$f_min_g = abs(dat$f - dat$g)
  
  # compute weights and MPW estimate
  dat = as.data.frame(
    dat %>% 
      group_by(item) %>%
      mutate(
        weight = f_min_g / sum(f_min_g),
        xhat_MPW = as.vector(weight %*% f)
      )
  )
  
  return(dat)
}


# Run aggregation methods and calculate Brier scores

transformAggPredictions = function(dat){
# transform aggregate predictions to probabilistic scale (from [0,100] to [0,1])  
  dat$xbar = dat$xbar / 100
  dat$xhat_median = dat$xhat_median/100
  dat$xhat_MP = dat$xhat_MP/100
  dat$xhat_KW = dat$xhat_KW/100
  dat$xhat_MPW = dat$xhat_MPW/100
  dat$xhat_SOA = dat$xhat_SOA/100
  dat$xhat_SOA2 = dat$xhat_SOA2/100
  
  return(dat)
}

calculateBrierScores = function(dat){

  # run aggreation algorithms  
  dat = getSimpleAggregates(dat)
  dat = runMinimalPivoting(dat)
  dat = runKnowledgeWeighted(dat)
  dat = runSOAlgorithm(dat)
  dat = runMPW(dat)
  
  dat2 = unique(dat[,c('item','binary_outcome','task_type','dispersion_sd_1','dispersion_sd_2','dispersion_kurtosis_1', 'dispersion_kurtosis_2','xbar','xhat_median','xhat_MP','xhat_KW','xhat_SOA','xhat_SOA2','xhat_MPW')])
  
  # transform aggregates to probability scale (% to [0,1])
  dat2 = transformAggPredictions(dat2)

  id_vars = c('item','task_type','binary_outcome','dispersion_sd_1','dispersion_sd_2','dispersion_kurtosis_1', 'dispersion_kurtosis_2')
  dat2 = melt(dat2,id.vars = id_vars, measure.vars = c('xbar','xhat_median','xhat_SOA','xhat_SOA2','xhat_MP','xhat_KW','xhat_MPW'), variable.name = 'method', value.name = 'estimate')
  
  # calculate Brier scores
  dat2$brierscore = (dat2$binary_outcome - dat2$estimate)^2 
  
  return(dat2)
  
}


# run the bootstrap analysis

sampleItems = function(i, dat3,s_size){
# generates bootstrap sample of items of s_size and calculates transformed Brier score
  
  d = as.data.frame(
    dat3 %>% 
      sample_n(s_size, replace = TRUE) 
  )
  
  # Brier score for each item in the bootstrap sample
  d2 = melt(d,id.vars=c('item','task_type','binary_outcome','dispersion'),variable.name = 'method',value.name = 'brierscore')
  
  # calculate transformed Brier scores per method in the bootstrap sample
  d3 = as.data.frame(
    d2 %>%
      group_by(method) %>%
      summarize(
        tr_brierscore = 100-100*mean(brierscore)
      )
  )
  
  d3$iteration = i
  
  return(d3)
  
}

bootstrapByDispersion = function(dispersion_category, dat){
# run bootstrap analysis for a given dispersion_category
  
  # select data from the given dispersion_category only
  cat('generating Bootstrap transformed Brier scores for Dispersion =',dispersion_category,'\n')
  dat2 = dat[dat$dispersion == dispersion_category,]
  dat3 = dcast(dat2, item + task_type + binary_outcome + dispersion ~ method, value.var = 'brierscore')
  
  # bootstrap items and calculate transformed Brier score
  dat4 = lapply(1:1000,sampleItems, dat3 = dat3, s_size = nrow(dat3))
  dat4 = bind_rows(dat4)
  dat4$dispersion = dispersion_category
  
  return(dat4)
}

runBootstrapBrier = function(dat,threshold_type,dispersion_type){
# runs bootstrap analysis
  
  task_type = unique(dat$task_type)
  
  # which dispersion categorization and measure will be used?
  if(threshold_type == 1 & dispersion_type =='sd'){
    dat$dispersion = dat$dispersion_sd_1
  } else if(threshold_type == 1 & dispersion_type =='kurtosis'){
    dat$dispersion = dat$dispersion_kurtosis_1
  } else if(threshold_type == 2 & dispersion_type =='sd'){
    dat$dispersion = dat$dispersion_sd_2
  } else if(threshold_type == 2 & dispersion_type =='kurtosis'){
    dat$dispersion = dat$dispersion_kurtosis_2
  }

  dat = dat[,c('item','task_type','binary_outcome','dispersion','method','brierscore')]
  
  # run bootsrap analysis for each dispersion category and combine them in a single data set
  d_top = bootstrapByDispersion('top',dat = dat)
  d_middle = bootstrapByDispersion('middle',dat = dat)
  d_bottom = bootstrapByDispersion('bottom',dat = dat)
  
  d = bind_rows(d_top,d_middle,d_bottom)
  d$task_type = task_type
  
  return(d)
  
}


# calculate and plot bootstrap Transformed Brier scores 

calculateTransformedBrierDiff = function(dat, benchmark){
# calculate pairwise differences in transformed Brier scores, uses SO with two different quantile functions

  d = dcast(dat, iteration + task_type + dispersion ~ method, value.var = 'tr_brierscore')
  
  if(benchmark == 'SOA'){
    
    d$xbar = d$xhat_SOA - d$xbar
    d$xhat_median = d$xhat_SOA - d$xhat_median
    d$xhat_MP = d$xhat_SOA - d$xhat_MP
    d$xhat_KW = d$xhat_SOA - d$xhat_KW
    d$xhat_MPW = d$xhat_SOA - d$xhat_MPW
    
    d$xhat_SOA = NULL
    d$xhat_SOA2 = NULL
    
  } else if(benchmark == 'SOA2') {
    
    d$xbar = d$xhat_SOA2 - d$xbar
    d$xhat_median = d$xhat_SOA2 - d$xhat_median
    d$xhat_MP = d$xhat_SOA2 - d$xhat_MP
    d$xhat_KW = d$xhat_SOA2 - d$xhat_KW
    d$xhat_MPW = d$xhat_SOA2 - d$xhat_MPW
    
    d$xhat_SOA = NULL
    d$xhat_SOA2 = NULL
    
  }
  
  id_vars = c('iteration','task_type','dispersion')
  d2 = melt(d,id.vars = id_vars, measure.vars = c('xbar','xhat_median','xhat_MP','xhat_KW','xhat_MPW'), variable.name = 'comparison', value.name = 'tr_brier_diff')
  d2$comparison = recode(d2$comparison,'xbar' = 'Simp.Average','xhat_median' = 'Median','xhat_MP' = 'Min.Pivot','xhat_KW'='Know.Weight','xhat_MPW'='Meta.Prob.Weight') 
  d2$comparison = droplevels(d2$comparison)
  
  # return
  return(d2)
}

plotTransformedBrierDiff = function(brier_diff_gk,brier_diff_sc){
# Plot pairwise differences in Score
    
  my_palette = c( as.vector(jcolors(palette = 'pal2')[1:5]),as.vector(jcolors(palette = 'pal2')[c(3,5,6)]) )[1:5]
  
  
  brier_diff_gk$dispersion = recode(brier_diff_gk$dispersion, 'bottom' = 'Dispersion: Low','middle' = 'Dispersion: Medium','top' = 'Dispersion: High')
  brier_diff_gk$dispersion = factor(brier_diff_gk$dispersion, levels =c('Dispersion: Low','Dispersion: Medium','Dispersion: High')) 
  
  brier_diff_sc$dispersion = recode(brier_diff_sc$dispersion, 'bottom' = 'Dispersion: Low','middle' = 'Dispersion: Medium','top' = 'Dispersion: High')
  brier_diff_sc$dispersion = factor(brier_diff_sc$dispersion, levels =c('Dispersion: Low','Dispersion: Medium','Dispersion: High'))
  
  
  b_gk = as.data.frame(
    brier_diff_gk %>%
      group_by(task_type, comparison, dispersion) %>%
      summarize(
        ymin = quantile(tr_brier_diff, 0.025, names = FALSE),
        lower = quantile(tr_brier_diff, 0.25, names = FALSE),
        middle = quantile(tr_brier_diff, 0.5, names = FALSE),
        upper = quantile(tr_brier_diff, 0.75, names = FALSE),
        ymax = quantile(tr_brier_diff, 0.975, names = FALSE)
      )
  )
  
  b_sc = as.data.frame(
    brier_diff_sc %>%
      group_by(task_type, comparison, dispersion) %>%
      summarize(
        ymin = quantile(tr_brier_diff, 0.025, names = FALSE),
        lower = quantile(tr_brier_diff, 0.25, names = FALSE),
        middle = quantile(tr_brier_diff, 0.5, names = FALSE),
        upper = quantile(tr_brier_diff, 0.75, names = FALSE),
        ymax = quantile(tr_brier_diff, 0.975, names = FALSE)
      )
  )
  
  d = bind_rows(b_gk,b_sc)
  
  pl = ggplot(d,aes(x = comparison,fill=comparison)) +
    geom_boxplot(aes(
      ymin = ymin, 
      lower = lower, 
      middle = middle, 
      upper = upper, 
      ymax = ymax) , stat='identity') +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), position = "dodge", size = 0.4) +
    geom_hline(aes(yintercept = 0),linetype = 'dashed',size=0.5) +
    facet_wrap(task_type~as.factor(dispersion), scales = 'free') +
    scale_y_continuous(name = 'Pairwise differences in Score') +
    scale_x_discrete(labels = c('SA','Med','MP','KW','MPW')) +
    scale_fill_manual(values=my_palette) +
    theme_bw(base_size = 14) +
    theme(legend.position = 'none')
  
  return(pl)
  
}


# Generate bootstrap confidence intervals

getConfIntervalsGK = function(brier_diff_gk){
  d = as.data.frame(
    brier_diff_gk %>%
      group_by(comparison, dispersion) %>%
      summarize(
        low_bound = quantile(tr_brier_diff, 0.025, names = FALSE),
        upp_bound = quantile(tr_brier_diff, 0.975, names = FALSE),
      )
  )

  colnames(d) = c('Comparison','Dispersion','Low.B.','Upp.B.')
  d$Dispersion <- recode_factor(d$Dispersion, lowest ="Bottom 10%", interm ="Middle 80%", highest ="Top 10%")
  
  d2 = d[order(d$Dispersion, decreasing = FALSE),]
  
  return(print(xtable(d2), include.rownames=FALSE))
}

getConfIntervalsSC = function(brier_diff_sc){
  d = as.data.frame(
    brier_diff_sc %>%
      group_by(comparison, dispersion) %>%
      summarize(
        low_bound = quantile(tr_brier_diff, 0.025, names = FALSE),
        upp_bound = quantile(tr_brier_diff, 0.975, names = FALSE),
      )
  )
  
  colnames(d) = c('Comparison','Dispersion','Low.B.','Upp.B.')
  d$Dispersion <- recode_factor(d$Dispersion, lowest ="Bottom 25%", interm ="Middle 50%", highest ="Top 25%")
  
  d2 = d[order(d$Dispersion, decreasing = FALSE),]
  
  return(print(xtable(d2), include.rownames=FALSE))
}



####################------------------MAIN--------------################

## Read General knowledge data and categorize items according to dispersion
dat_gk = constructGeneralKnowledgeData()
dat_gk = categorizeGKitems(dat_gk)

## Read State capital data and categorize items according to dispersion
dat_sc = constructStateCapitalData()
dat_sc = categorizeSCitems(dat_sc)

# Calculate Brier score for each item in GK and SC data
brier_gk = calculateBrierScores(dat_gk)
brier_sc = calculateBrierScores(dat_sc)

# Run bootstrap analysis for GK data, std.dev as dispersion, category thresholds: 10/80/10
bootstrap_gk_1sd = runBootstrapBrier(brier_gk, threshold_type = 1, dispersion_type = 'sd')
# Run bootstrap analysis for GK data, reverse kurtosis as dispersion, category thresholds: 10/80/10
bootstrap_gk_1krt = runBootstrapBrier(brier_gk, threshold_type = 1, dispersion_type = 'kurtosis')
# Run bootstrap analysis for GK data, std.dev as dispersion, category thresholds: 33/33/33
bootstrap_gk_2sd = runBootstrapBrier(brier_gk, threshold_type = 2, dispersion_type = 'sd')
# Run bootstrap analysis for GK data, reverse kurtosis as dispersion, category thresholds: 33/33/33
bootstrap_gk_2krt = runBootstrapBrier(brier_gk, threshold_type = 2, dispersion_type = 'kurtosis')

# Run bootstrap analysis for SC data, std.dev as dispersion, category thresholds: 25/50/25
bootstrap_sc_1sd = runBootstrapBrier(brier_sc, threshold_type = 1, dispersion_type = 'sd')
# Run bootstrap analysis for SC data, reverses kurtosis as dispersion, category thresholds: 25/50/25
bootstrap_sc_1krt = runBootstrapBrier(brier_sc, threshold_type = 1, dispersion_type = 'kurtosis')
# Run bootstrap analysis for SC data, std.dev as dispersion, category thresholds: 33/33/33
bootstrap_sc_2sd = runBootstrapBrier(brier_sc, threshold_type = 2, dispersion_type = 'sd')
# Run bootstrap analysis for SC data, reverses kurtosis as dispersion, category thresholds: 33/33/33
bootstrap_sc_2krt = runBootstrapBrier(brier_sc, threshold_type = 2, dispersion_type = 'kurtosis')

# save bootstrap data
save.image('bootstrap_binary.RData')


