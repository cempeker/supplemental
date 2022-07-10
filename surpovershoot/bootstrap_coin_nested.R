########### Generate bootstrap samples, Coin Flips data, Nested structure only
# Instructions:
# 1. Load LIBRARIES
# 2. Define FUNCTIONS
# 3. Run the commands in MAIN

################----------------LIBRARIES--------------##############

library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(reshape2)
library(latex2exp)
library(ggplot2)
library(jcolors)  # color palettes in the figures
library(metaggR)  # for implementing Knowledge-Weighting
library(xtable)

################----------------FUNCTIONS--------------##############

# Read and prepare data

constructCoinDataNested = function(){
  
  # get predictions (f) and meta-predictions (g) from raw data 
  coinFlip1 <- read.csv("./data_coin/Study1.csv")
  coinTheta1 = unique(coinFlip1[,c('Condition','subCondition',paste0('theta',1:8))])
  coinFlip1 = coinFlip1[,c('SubjectID','Condition','subCondition', 'f1', 'g1', 'f2', 'g2','f3', 'g3', 'f4', 'g4', 'f5', 'g5', 'f6', 'g6', 'f7','g7', 'f8', 'g8')]
  
  # convert wide to long
  dat_coin1 = melt(coinFlip1,id.vars=c('SubjectID','Condition','subCondition'))
  dat_coin1$variable = as.character(dat_coin1$variable)
  dat_coin1 = dat_coin1 %>% separate(variable,into=c('report','item'),sep=1)
  colnames(dat_coin1)[which(colnames(dat_coin1)=='SubjectID')] = 'subject'
  dat_coin1$item = paste0('c',dat_coin1$Condition,'.',dat_coin1$subCondition,'.',dat_coin1$item,'.','s1')
  
  # merge true values (theta) to data
  dat_theta1 = melt(coinTheta1, id.vars=c('Condition','subCondition'))
  dat_theta1$variable = as.character(dat_theta1$variable)
  dat_theta1 = dat_theta1 %>% separate(variable,into=c('a','item'),sep=5)
  dat_theta1$item = paste0('c',dat_theta1$Condition,'.',dat_theta1$subCondition,'.',dat_theta1$item,'.','s1')
  dat_theta1$Condition = NULL; dat_theta1$subCondition = NULL; dat_theta1$a = NULL
  colnames(dat_theta1) = c('item','theta')
  dat_coin1 = merge(dat_coin1,dat_theta1,by='item', all.x = TRUE)
  dat_coin1$task_type = 'Coin Flips, Study 1'
  
  rm(coinFlip1,coinTheta1,dat_theta1)
  
  # cast pred and meta-pred to different columns
  dat_coin1 = dcast(dat_coin1,item + subject + Condition +subCondition + theta + task_type ~  report, value.var = 'value')
  
  # if there are any subjects with NA in pred or meta-pred, drop them
  dat_coin1 = dat_coin1[complete.cases(dat_coin1),]
  
  # transform theta from [0,1] to [0,100] (expected number of heads in 100 flips)
  dat_coin1$theta = 100 * dat_coin1$theta
  
  # only include cases where private signals are identical ("Nested" structure in Palley and Soll (2019))
  dat_coin1 = dat_coin1[dat_coin1$Condition == 3,]
  
  # return data
  return(dat_coin1)
}


# Aggregation algorithms (SO and benchmarks)

getSimpleAggregates = function(dat){
  
  # calculate average and median for each item
  dat = as.data.frame(
    dat %>%
    group_by(item) %>%
      mutate(
        xbar = mean(f, na.rm = TRUE),
        xhat_median = median(f, na.rm = TRUE),
    ))
  
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
      ))
  
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


# Calculate bootstrap errors and pairwise differences

calculateErrorOnTheta = function(dat){
#### Calculate forecast errors for each method, item and iteration

  dat = melt(dat,id.vars = c('item','task_type','theta','crowd_size','iteration'),measure.vars = c('xbar','xhat_median','xhat_SOA','xhat_SOA2','xhat_MP','xhat_KW','xhat_MPW'), variable.name = 'method', value.name = 'estimate')
  
  dat = unique(dat)
  
  dat$abserror = abs(dat$estimate - dat$theta)
  dat$sqrderror = (dat$estimate - dat$theta)^2
  
  dat$method = recode(dat$method, 'xbar' = 'Simp.Average','xhat_median' = 'Median','xhat_SOA' = 'SOA','xhat_SOA2' = 'SOA2','xhat_MP' = 'Min.Pivot','xhat_KW'='Know.Weight','xhat_MPW'='Meta.Prob.Weight')
  
  return(dat)
}

calculateAverageErrors = function(dat_err){
# Calculate average errors (across items) and log(average error) for each crowd size, method and iteration
  
  dat_avgerr = as.data.frame(
    dat_err %>%
      group_by(method,crowd_size,iteration) %>%
      summarize(
        rootMSE = sqrt(mean(sqrderror)),
        mean_abserror = mean(abserror),
        log_mean_abserror = log(mean_abserror)
      ) 
  )
  
  # Set log avg error to log 0.01 when avg error=0. No such case occurred in any of the runs 
  dat_avgerr[!is.finite(dat_avgerr$log_mean_abserror),'log_mean_abserror'] = log(0.01)
  
  return(dat_avgerr)
}

calculateErrorDiff = function(dat_avgerr, SOAtype){
# calculate pairwise differences in errors, SO vs the benchmarks
  
  dat_avgerr = dat_avgerr[complete.cases(dat_avgerr),]
  
  dat_diff = as.data.frame(
    dat_avgerr %>%
      group_by(iteration,crowd_size) %>%
      summarize(
        Simp.Average = mean_abserror[method=='Simp.Average'] - mean_abserror[method==SOAtype],
        Median = mean_abserror[method=='Median'] - mean_abserror[method==SOAtype],
        Min.Pivot = mean_abserror[method=='Min.Pivot'] - mean_abserror[method==SOAtype],
        Know.Weight = mean_abserror[method=='Know.Weight'] - mean_abserror[method==SOAtype],
        Meta.Prob.Weight = mean_abserror[method=='Meta.Prob.Weight'] - mean_abserror[method==SOAtype]
      )
  )
  
  dat_diff = melt(dat_diff,id.vars = c('iteration','crowd_size'),measure.vars = c('Simp.Average','Median','Min.Pivot','Know.Weight','Meta.Prob.Weight'), variable.name = 'comparison', value.name = 'abserror_diff')
  
  return(dat_diff)
}

calculateErrorDiffLog = function(dat_avgerr, SOAtype){
# calculate pairwise differences in log errors, SO vs the benchmarks
    
  dat_avgerr = dat_avgerr[complete.cases(dat_avgerr),]
  
  dat_diff = as.data.frame(
    dat_avgerr %>%
      group_by(iteration,crowd_size) %>%
      summarize(
        Simp.Average = log_mean_abserror[method=='Simp.Average'] - log_mean_abserror[method==SOAtype],
        Median = log_mean_abserror[method=='Median'] - log_mean_abserror[method==SOAtype],
        Min.Pivot = log_mean_abserror[method=='Min.Pivot'] - log_mean_abserror[method==SOAtype],
        Know.Weight = log_mean_abserror[method=='Know.Weight'] - log_mean_abserror[method==SOAtype],
        Meta.Prob.Weight = log_mean_abserror[method=='Meta.Prob.Weight'] - log_mean_abserror[method==SOAtype]
      )
  )
  
  dat_diff = melt(dat_diff,id.vars = c('iteration','crowd_size'),measure.vars = c('Simp.Average','Median','Min.Pivot','Know.Weight','Meta.Prob.Weight'), variable.name = 'comparison', value.name = 'log_diff')
  
  return(dat_diff)
}

calculateErrorDiffRMSE = function(dat_avgerr, SOAtype){
# calculate pairwise differences in RMSE, SO vs the benchmarks
  
  dat_avgerr = dat_avgerr[complete.cases(dat_avgerr),]
  
  dat_diff = as.data.frame(
    dat_avgerr %>%
      group_by(iteration,crowd_size) %>%
      summarize(
        Simp.Average = rootMSE[method=='Simp.Average'] - rootMSE[method==SOAtype],
        Median = rootMSE[method=='Median'] - rootMSE[method==SOAtype],
        Min.Pivot = rootMSE[method=='Min.Pivot'] - rootMSE[method==SOAtype],
        Know.Weight = rootMSE[method=='Know.Weight'] - rootMSE[method==SOAtype],
        Meta.Prob.Weight = rootMSE[method=='Meta.Prob.Weight'] - rootMSE[method==SOAtype]
      )
  )
  
  dat_diff = melt(dat_diff,id.vars = c('iteration','crowd_size'),measure.vars = c('Simp.Average','Median','Min.Pivot','Know.Weight','Meta.Prob.Weight'), variable.name = 'comparison', value.name = 'rmse_diff')
  
  return(dat_diff)
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

runBootstrapOnTheta = function(dat,c_size){
# runs the bootstrap analysis and returns bootstrap error data
  
  # set number of iterations and generate bootstrap samples  
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
  cat('calculating errors of aggregate forecasts per method, item and iteration\n')
  error_list = lapply(sample_list, calculateErrorOnTheta)
  
  # calculate average error (across tasks) in each bootstrap sample
  cat('calculating average errors across items in each iteration\n')
  error_list = lapply(error_list, calculateAverageErrors)
  
  # return bootstrap errors
  return(error_list)
  
}


# plot bootstrap results

quantiles_95 <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

plotError = function(dat_err, SOA_type){
# plot average RMSE
  
  dat_err = dat_err[complete.cases(dat_err),]
  
  my_palette = c( as.vector(jcolors(palette = 'pal2')[1:5]),as.vector(jcolors(palette = 'default')[3]) )[1:6]
  
  dat2 = as.data.frame(
    dat_err %>%
      group_by(method,crowd_size) %>%
      summarize(
        avg_rootMSE = mean(rootMSE,na.rm = TRUE)
      )
  )
  
  if(SOA_type == 'SOA'){
    dat2 = dat2[dat2$method != 'SOA2',]
    dat2$method = recode(dat2$method, 'SOA' = 'Surp.Overshoot')
  } else {
    dat2 = dat2[dat2$method != 'SOA',]
    dat2$method = recode(dat2$method, 'SOA2' = 'Surp.Overshoot')
  }
  dat2$method = factor(dat2$method , levels = c('Simp.Average','Median','Min.Pivot','Know.Weight','Meta.Prob.Weight','Surp.Overshoot'))
  
  pl = ggplot(data = dat2, aes(x=crowd_size,y=avg_rootMSE, color = method, shape = method)) +
    geom_point(size = 3) +
    geom_line() + 
    scale_color_manual(values=my_palette) +
    scale_x_continuous(name='Crowd size',breaks = seq(10,100,by = 10)) +
    scale_y_continuous(name = 'RMSE') +
    theme_bw(base_size = 14) 
  
  return(pl)
}

plotErrorDiff = function(dat_diff, errordiff_type){
# plot differences in errors
# errordiff_type determines if the function calculates absolute diff, log diff or RMSE diff
  
  my_palette = c( as.vector(jcolors(palette = 'pal2')[1:5]),as.vector(jcolors(palette = 'pal2')[c(3,5,6)]) )[1:5]
  
  if(errordiff_type == 'logdiff'){
    
    d = as.data.frame(
      dat_diff %>%
        group_by(crowd_size, comparison) %>%
        summarize(
          ymin = quantile(log_diff, 0.025, names = FALSE),
          lower = quantile(log_diff, 0.25, names = FALSE),
          middle = quantile(log_diff, 0.5, names = FALSE),
          upper = quantile(log_diff, 0.75, names = FALSE),
          ymax = quantile(log_diff, 0.975, names = FALSE)
        )
    )
    yaxis_name = 'Reduction in log absolute error (% Error reduction)'
    
  } else if(errordiff_type == 'rmsediff'){
    
    d = as.data.frame(
      dat_diff %>%
        group_by(crowd_size, comparison) %>%
        summarize(
          ymin = quantile(rmse_diff, 0.025, names = FALSE),
          lower = quantile(rmse_diff, 0.25, names = FALSE),
          middle = quantile(rmse_diff, 0.5, names = FALSE),
          upper = quantile(rmse_diff, 0.75, names = FALSE),
          ymax = quantile(rmse_diff, 0.975, names = FALSE)
        )
    )
    yaxis_name = 'Reduction in RMSE'
    
  } else if(errordiff_type == 'absdiff'){
    
    d = as.data.frame(
      dat_diff %>%
        group_by(crowd_size, comparison) %>%
        summarize(
          ymin = quantile(abserror_diff, 0.025, names = FALSE),
          lower = quantile(abserror_diff, 0.25, names = FALSE),
          middle = quantile(abserror_diff, 0.5, names = FALSE),
          upper = quantile(abserror_diff, 0.75, names = FALSE),
          ymax = quantile(abserror_diff, 0.975, names = FALSE)
        )
    )
    yaxis_name = 'Reduction in Absolute Error'
    
  }
  
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
    facet_wrap(~benchmark_type) +
    #stat_boxplot(aes(ymin = ymin, ymax = ymax),geom ='errorbar') +
    geom_hline(yintercept = 0, linetype="dashed", color = "red") +
    scale_y_continuous(name = yaxis_name) +
    scale_x_discrete(name = 'Crowd Size') +
    scale_fill_manual(values=my_palette) +
    #ggtitle(TeX('Percentge error reduction, \\hat{x} vs alternative')) +
    theme_bw(base_size = 14) + 
    theme(legend.position = 'bottom', legend.direction = 'horizontal')
  
  return(pl)
}


# Generate bootstrap confidence intervals

getConfIntervals = function(diff_log){
# generates bootstrap confidence intervals in Table C1
  d = as.data.frame(
    diff_log %>%
      group_by(crowd_size,comparison) %>%
      summarize(
        low_bound = quantile(log_diff, 0.025, names = FALSE),
        upp_bound = quantile(log_diff, 0.975, names = FALSE),
      )
  )
  
  d$crowd_size = as.integer(d$crowd_size)
  d1 = d[d$crowd_size <= 50,]
  d2 = d[d$crowd_size > 50,]
  
  d3 = cbind(d1,d2)
  colnames(d3) = c('C.Size','Comparison','Low.B.','Upp.B.','C.Size','Comparison','Low.B.','Upp.B.')
  return(print(xtable(d3), include.rownames=FALSE))
}



######################------------------MAIN--------------################

### Read data
dat_coin = constructCoinDataNested()
dat_coin$task_type = 'Coin Flips'


### Initialize Bootstrap analysis
# set crowd size
crowd_size_coin = seq(10,100,by=10)
# initialize data frame to record forecast errors
coin_err = list()

### Run Bootstrap analysis
### The codes below run the Bootstrap analysis separately for each crowd size and combines all results into a single data frame

# Boostrap, sample size = 10
d1 = runBootstrapOnTheta(dat_coin,crowd_size_coin[1])
coin_err[[1]] = bind_rows(d1); rm(d1)
# Boostrap, sample size = 20
d2 = runBootstrapOnTheta(dat_coin,crowd_size_coin[2])
coin_err[[2]] = bind_rows(d2); rm(d2)
# Boostrap, sample size = 30
d3 = runBootstrapOnTheta(dat_coin,crowd_size_coin[3])
coin_err[[3]] = bind_rows(d3); rm(d3)
# Boostrap, sample size = 40
d4 = runBootstrapOnTheta(dat_coin,crowd_size_coin[4])
coin_err[[4]] = bind_rows(d4); rm(d4)
# Boostrap, sample size = 50
d5 = runBootstrapOnTheta(dat_coin,crowd_size_coin[5])
coin_err[[5]] = bind_rows(d5); rm(d5)
# Boostrap, sample size = 60
d6 = runBootstrapOnTheta(dat_coin,crowd_size_coin[6])
coin_err[[6]] = bind_rows(d6); rm(d6)
# Boostrap, sample size = 70
d7 = runBootstrapOnTheta(dat_coin,crowd_size_coin[7])
coin_err[[7]] = bind_rows(d7); rm(d7)
# Boostrap, sample size = 80
d8 = runBootstrapOnTheta(dat_coin,crowd_size_coin[8])
coin_err[[8]] = bind_rows(d8); rm(d8)
# Boostrap, sample size = 90
d9 = runBootstrapOnTheta(dat_coin,crowd_size_coin[9])
coin_err[[9]] = bind_rows(d9); rm(d9)
# Boostrap, sample size = 100
d10 = runBootstrapOnTheta(dat_coin,crowd_size_coin[10])
coin_err[[10]] = bind_rows(d10); rm(d10)

# combine all bootstrap error data
coin_err2 = bind_rows(coin_err)

# save bootstrap data
save.image('bootstrap_coin_nested.RData')
