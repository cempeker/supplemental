########### Generate bootstrap samples, General Knowledge and State Capital data
# Instructions:
# 1. Load LIBRARIES
# 2. Define FUNCTIONS
# 3. Run the commands in MAIN

################----------------LIBRARIES--------------##############

library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(reshape2)
library(moments)
library(latex2exp)
library(ggplot2)
library(jcolors) # color palettes in the figures
library(metaggR) # for implementing Knowledge-Weighting
library(xtable)



################----------------FUNCTIONS--------------##############

# Read and prepare data

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
        xhat_KW = ifelse(not_all_E_same, knowledge_weighted_estimate(E = f,P = g, no_inf_check=TRUE), unique(f))
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


# Function to calculate Brier scores

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
  #dat = getSimpleAggregates(dat)
  #dat = runMinimalPivoting(dat)
  #dat = runKnowledgeWeighted(dat)
  #dat = runSOAlgorithm(dat)
  #dat = runMPW(dat)
  
  dat2 = unique(dat[,c('item','crowd_size','iteration','binary_outcome','task_type','xbar','xhat_median','xhat_MP','xhat_KW','xhat_SOA','xhat_SOA2','xhat_MPW')])
  
  # transform aggregates to probability scale (% to [0,1])
  dat2 = transformAggPredictions(dat2)

  id_vars = c('item','crowd_size','iteration','task_type','binary_outcome')
  dat2 = melt(dat2,id.vars = id_vars, measure.vars = c('xbar','xhat_median','xhat_SOA','xhat_SOA2','xhat_MP','xhat_KW','xhat_MPW'), variable.name = 'method', value.name = 'estimate')
  
  # calculate Brier scores
  dat2$brierscore = (dat2$binary_outcome - dat2$estimate)^2 
  
  # calculate Transformed Brier scores
  dat3 = as.data.frame(
    dat2 %>% 
      group_by(iteration,crowd_size,task_type,method) %>%
      summarize(
        tr_brierscore = 100-100*mean(brierscore)
      )
  )
  
  return(dat3)
  
}


# run the bootstrap analysis

sampleCrowd = function(i, dat,c_size){
  # generates and returns bootstrap sample of a given size c_size
  
  dat2 = as.data.frame(
    dat %>% 
      group_by(item) %>% 
      sample_n(c_size, replace = TRUE) 
  )
  
  dat2$iteration = i
  dat2$crowd_size = c_size
  
  return(dat2)
}

runBootstrapCrowdSize = function(dat,c_size){
  # runs the bootstrap analysis and returns bootstrap error data
  
  # set number of iterations and generate bootstrap samples  
  #iter = 1:100
  iter = 1:1000
  cat('crowd size',c_size,'\n')
  cat('generating Bootstrap samples\n')
  sample_list = lapply(iter,sampleCrowd, dat = dat, c_size = c_size)
  
  # implement SO algorithm and benchmarks per item and bootstrap sample
  cat('calculating simple aggregates (average, median) in Bootstrap samples\n')
  sample_list = lapply(sample_list, getSimpleAggregates)
  
  cat('running SO Algorithm\n')
  sample_list = lapply(sample_list, runSOAlgorithm)
  
  cat('running Minimal Pivoting\n')
  sample_list = lapply(sample_list, runMinimalPivoting)
  
  cat('running Knowledge Weighting\n')
  sample_list = lapply(sample_list, runKnowledgeWeighted)
  
  cat('running Meta Probability Weighting\n')
  sample_list = lapply(sample_list, runMPW)
  
  # calculate forecast errors per task(item) in each bootstrap sample
  cat('calculating brier scores per method, item and iteration\n')
  error_list = lapply(sample_list, calculateBrierScores)
  
  # calculate average error (across tasks) in each bootstrap sample
  #cat('calculating transformed brier across items in each iteration\n')
  #error_list = lapply(error_list, calculateAverageErrors)
  
  # return bootstrap errors
  return(error_list)
  
}


# calculate and plot bootstrap Transformed Brier scores 

calculateTransformedBrierDiff = function(dat, benchmark){
# calculate pairwise differences in transformed Brier scores, uses SO with two different quantile functions

  d = dcast(dat, iteration + task_type + crowd_size ~ method, value.var = 'tr_brierscore')
  
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
  
  id_vars = c('iteration','task_type','crowd_size')
  d2 = melt(d,id.vars = id_vars, measure.vars = c('xbar','xhat_median','xhat_MP','xhat_KW','xhat_MPW'), variable.name = 'comparison', value.name = 'tr_brier_diff')
  d2$comparison = recode(d2$comparison,'xbar' = 'Simp.Average','xhat_median' = 'Median','xhat_MP' = 'Min.Pivot','xhat_KW'='Know.Weight','xhat_MPW'='Meta.Prob.Weight') 
  d2$comparison = droplevels(d2$comparison)
  
  # return
  return(d2)
}

plotTransformedBrierDiff = function(brier_diff){
# Plot pairwise differences in Score
    
  my_palette = c( as.vector(jcolors(palette = 'pal2')[1:5]),as.vector(jcolors(palette = 'pal2')[c(3,5,6)]) )[1:5]
  
  
  d = as.data.frame(
    brier_diff %>%
      group_by(task_type, comparison, crowd_size) %>%
      summarize(
        ymin = quantile(tr_brier_diff, 0.025, names = FALSE, na.rm=TRUE),
        lower = quantile(tr_brier_diff, 0.25, names = FALSE, na.rm=TRUE),
        middle = quantile(tr_brier_diff, 0.5, names = FALSE, na.rm=TRUE),
        upper = quantile(tr_brier_diff, 0.75, names = FALSE, na.rm=TRUE),
        ymax = quantile(tr_brier_diff, 0.975, names = FALSE, na.rm=TRUE)
      )
  )
  
  pl = ggplot(d,aes(x = as.factor(crowd_size),fill=comparison)) +
    geom_boxplot(aes(
      ymin = ymin, 
      lower = lower, 
      middle = middle, 
      upper = upper, 
      ymax = ymax) , stat='identity') +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), position = "dodge", size = 0.4) +
    geom_hline(aes(yintercept = 0),linetype = 'dashed',size=0.5) +
    #facet_wrap(task_type~as.factor(dispersion), scales = 'free') +
    scale_y_continuous(name = 'Pairwise differences in Score') +
    scale_x_discrete("Crowd size") +
    scale_fill_manual(values=my_palette) +
    theme_bw(base_size = 14)
  
  return(pl)
  
}

plotTransformedBrierDiffAll = function(brier_diff_gk,brier_diff_sc){
  
  my_palette = c( as.vector(jcolors(palette = 'pal2')[1:5]),as.vector(jcolors(palette = 'pal2')[c(3,5,6)]) )[1:5]
  
  d_gk = as.data.frame(
    brier_diff_gk %>%
      group_by(task_type, comparison, crowd_size) %>%
      summarize(
        ymin = quantile(tr_brier_diff, 0.025, names = FALSE, na.rm=TRUE),
        lower = quantile(tr_brier_diff, 0.25, names = FALSE, na.rm=TRUE),
        middle = quantile(tr_brier_diff, 0.5, names = FALSE, na.rm=TRUE),
        upper = quantile(tr_brier_diff, 0.75, names = FALSE, na.rm=TRUE),
        ymax = quantile(tr_brier_diff, 0.975, names = FALSE, na.rm=TRUE)
      )
  )
  
  d_sc = as.data.frame(
    brier_diff_sc %>%
      group_by(task_type, comparison, crowd_size) %>%
      summarize(
        ymin = quantile(tr_brier_diff, 0.025, names = FALSE, na.rm=TRUE),
        lower = quantile(tr_brier_diff, 0.25, names = FALSE, na.rm=TRUE),
        middle = quantile(tr_brier_diff, 0.5, names = FALSE, na.rm=TRUE),
        upper = quantile(tr_brier_diff, 0.75, names = FALSE, na.rm=TRUE),
        ymax = quantile(tr_brier_diff, 0.975, names = FALSE, na.rm=TRUE)
      )
  )
  
  d = rbind(d_gk, d_sc)
  
  d$benchmark_type = ifelse(d$comparison %in% c('Simp.Average','Median'), 'Simple Benchmarks', 'Advanced Benchmarks')
  
  d$benchmark_type = factor(d$benchmark_type, levels = c('Simple Benchmarks', 'Advanced Benchmarks'))
  
  pl = ggplot(d,aes(x = as.factor(crowd_size),fill=comparison)) +
    geom_boxplot(aes(
      ymin = ymin, 
      lower = lower, 
      middle = middle, 
      upper = upper, 
      ymax = ymax) , stat='identity') +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), position = "dodge", size = 0.4) +
    geom_hline(aes(yintercept = 0),linetype = 'dashed',size=0.5) +
    #facet_wrap(~task_type, scales = 'free') +
    facet_grid(task_type ~ ., scales = 'free') +
    scale_y_continuous(name = 'Pairwise differences in Score') +
    scale_x_discrete("Crowd size") +
    scale_fill_manual(values=my_palette) +
    theme_bw(base_size = 14) #+
    #theme(
    #  legend.position = 'bottom',
    #  legend.direction = 'horizontal'
    #)
  
    return(pl)
}


# Generate bootstrap confidence intervals

getConfIntervals = function(brier_diff){
  # generates bootstrap confidence intervals in Table C1
  d = as.data.frame(
    brier_diff %>%
      group_by(crowd_size,comparison) %>%
      summarize(
        low_bound = quantile(tr_brier_diff, 0.025, na.rm = TRUE, names = FALSE),
        upp_bound = quantile(tr_brier_diff, 0.975, na.rm = TRUE, names = FALSE),
      )
  )
  
  d$crowd_size = as.integer(d$crowd_size)
  d1 = d[d$crowd_size <= 40,]
  d2 = d[d$crowd_size > 40,]
  
  d3 = cbind(d1,d2)
  colnames(d3) = c('C.Size','Comparison','Low.B.','Upp.B.','C.Size','Comparison','Low.B.','Upp.B.')
  return(print(xtable(d3), include.rownames=FALSE))
}


####################------------------MAIN--------------################

## Read data
dat_gk = constructGeneralKnowledgeData()
dat_sc = constructStateCapitalData()

### Initialize Bootstrap analysis
# set crowd size
crowd_size= seq(10,80,by=10)
# initialize data frames to record forecast errors
brier_gk = list()
brier_sc = list()

### Run Bootstrap analysis for the General Knowledge data
### The codes below run the Bootstrap analysis separately for each crowd size and combines all results into a single data frame

d1 = runBootstrapCrowdSize(dat_gk,crowd_size[1])
brier_gk[[1]] = bind_rows(d1); rm(d1)
d2 = runBootstrapCrowdSize(dat_gk,crowd_size[2])
brier_gk[[2]] = bind_rows(d2); rm(d2)
d3 = runBootstrapCrowdSize(dat_gk,crowd_size[3])
brier_gk[[3]] = bind_rows(d3); rm(d3)
d4 = runBootstrapCrowdSize(dat_gk,crowd_size[4])
brier_gk[[4]] = bind_rows(d4); rm(d4)
d5 = runBootstrapCrowdSize(dat_gk,crowd_size[5])
brier_gk[[5]] = bind_rows(d5); rm(d5)
d6 = runBootstrapCrowdSize(dat_gk,crowd_size[6])
brier_gk[[6]] = bind_rows(d6); rm(d6)
d7 = runBootstrapCrowdSize(dat_gk,crowd_size[7])
brier_gk[[7]] = bind_rows(d7); rm(d7)
d8 = runBootstrapCrowdSize(dat_gk,crowd_size[8])
brier_gk[[8]] = bind_rows(d8); rm(d8)

# combine all bootstrap  data
brier_gk2 = bind_rows(brier_gk)


### Run Bootstrap analysis for the State Capital data
### The codes below run the Bootstrap analysis separately for each crowd size and combines all results into a single data frame

d1 = runBootstrapCrowdSize(dat_sc,crowd_size[1])
brier_sc[[1]] = bind_rows(d1); rm(d1)
d2 = runBootstrapCrowdSize(dat_sc,crowd_size[2])
brier_sc[[2]] = bind_rows(d2); rm(d2)
d3 = runBootstrapCrowdSize(dat_sc,crowd_size[3])
brier_sc[[3]] = bind_rows(d3); rm(d3)
d4 = runBootstrapCrowdSize(dat_sc,crowd_size[4])
brier_sc[[4]] = bind_rows(d4); rm(d4)
d5 = runBootstrapCrowdSize(dat_sc,crowd_size[5])
brier_sc[[5]] = bind_rows(d5); rm(d5)
d6 = runBootstrapCrowdSize(dat_sc,crowd_size[6])
brier_sc[[6]] = bind_rows(d6); rm(d6)
d7 = runBootstrapCrowdSize(dat_sc,crowd_size[7])
brier_sc[[7]] = bind_rows(d7); rm(d7)
d8 = runBootstrapCrowdSize(dat_sc,crowd_size[8])
brier_sc[[8]] = bind_rows(d8); rm(d8)

# combine all bootstrap  data
brier_sc2 = bind_rows(brier_sc)


# save bootstrap data
save.image('bootstrap_binary_samplesize.RData')


