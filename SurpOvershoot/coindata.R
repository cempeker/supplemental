########### coindata.R : R script for the Surprising Overshoot paper, Section 5.1
########## Structure of the script: 1. Loading packages, 2. Function declarations, 3. Reading data, 4. Main.

### Set working directory 
setwd('/home/acp/Desktop/Research/surp_overshoot')

### libraries
library(ggplot2)
library(readxl)
#library(plyr)
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(reshape2)
library(latex2exp)
library(MASS)
library(RColorBrewer)
library(jcolors)
#library(ggdark)
library(xtable)

################----------------FUNCTION DECLARATIONS--------------##############

constructCoinData1 = function(){
# Read Study 1 data (Palley and Soll, 2019)
  coinFlip1 <- read.csv("./Data Files/Study1.csv")
  coinTheta1 = unique(coinFlip1[,c('Condition','subCondition',paste0('theta',1:8))])
  coinFlip1 = coinFlip1[,c('SubjectID','Condition','subCondition', 'f1', 'g1', 'f2', 'g2','f3', 'g3', 'f4', 'g4', 'f5', 'g5', 'f6', 'g6', 'f7','g7', 'f8', 'g8')]
  
  # convert wide to long
  dat_coin1 = melt(coinFlip1,id.vars=c('SubjectID','Condition','subCondition'))
  dat_coin1$variable = as.character(dat_coin1$variable)
  dat_coin1 = dat_coin1 %>% separate(variable,into=c('report','item'),sep=1)
  colnames(dat_coin1)[which(colnames(dat_coin1)=='SubjectID')] = 'subject'
  dat_coin1$item = paste0('c',dat_coin1$Condition,'.',dat_coin1$subCondition,'.',dat_coin1$item,'.','s1')
  
  # merge true value (probability of heads, unknown to subjects) to data
  dat_theta1 = melt(coinTheta1, id.vars=c('Condition','subCondition'))
  dat_theta1$variable = as.character(dat_theta1$variable)
  dat_theta1 = dat_theta1 %>% separate(variable,into=c('a','item'),sep=5)
  dat_theta1$item = paste0('c',dat_theta1$Condition,'.',dat_theta1$subCondition,'.',dat_theta1$item,'.','s1')
  dat_theta1$Condition = NULL; dat_theta1$subCondition = NULL; dat_theta1$a = NULL
  colnames(dat_theta1) = c('item','theta')
  dat_coin1 = merge(dat_coin1,dat_theta1,by='item', all.x = TRUE)
  dat_coin1$task_type = 'Coin Flips, Study 1'
  
  rm(coinFlip1,coinTheta1,dat_theta1)
  
  # set subCondition
  
  # cast to f and g being in differen columns
  dat_coin1 = dcast(dat_coin1,item + subject + Condition +subCondition + theta + task_type ~  report, value.var = 'value')
  
  # remove subjects with NA in one of the reports
  dat_coin1 = dat_coin1[complete.cases(dat_coin1),]
  
  # scale theta from [0,1] to [0,100]
  dat_coin1$theta = 100 * dat_coin1$theta
  
  # exclude 'same private signal' cases in PS2019
  dat_coin1 = dat_coin1[dat_coin1$Condition != 3,]
  
  # return data
  return(dat_coin1)
}

getSimpleAggregates = function(dat){
# calculates simple aggregates such as average prediction, median prediction, majority vote etc.
  dat = as.data.frame(
    dat %>%
    group_by(item) %>%
      mutate(
        xbar = mean(f, na.rm = TRUE),
        xhat_median = median(f, na.rm = TRUE),
    ))
  
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



calculateErrorOnTheta = function(dat){
#### function to calculate errors per method per task

  # calculate log errors
  dat = melt(dat,id.vars = c('item','task_type','theta','crowd_size','iteration'),measure.vars = c('xbar','xhat_median','xhat_SOA','xhat_MP','xhat_KW','xhat_MPW'), variable.name = 'method', value.name = 'estimate')
  
  dat = unique(dat)
  
  dat$abserror = abs(dat$estimate - dat$theta)
  dat$sqrderror = (dat$estimate - dat$theta)^2
  
  dat$method = recode(dat$method, 'xbar' = 'Simp.Average','xhat_median' = 'Median','xhat_SOA' = 'SOA','xhat_MP' = 'Min.Pivot','xhat_KW'='Know.Weight','xhat_MPW'='Meta.Prob.Weight')
  
  # return
  return(dat)
}

calculateAverageErrors = function(dat_err){
# get average error (average errors across items)
  
  dat_avgerr = as.data.frame(
    dat_err %>%
      group_by(method,crowd_size,iteration) %>%
      summarize(
        rootMSE = sqrt(mean(sqrderror)),
        mean_abserror = mean(abserror),
        log_mean_abserror = log(mean_abserror)
      ) 
  )
	
  # set log error to log(0.01) if error = 0
  dat_avgerr[!is.finite(dat_avgerr$log_mean_abserror),'log_mean_abserror'] = log(0.01)
  
  return(dat_avgerr)
}

calculateErrorDiff = function(dat_avgerr, benchmark){
#### function to calculate error reduction (in abs error) from using xhat_SOA instead of benchmarks (pairwise comparison)
  
  dat_avgerr = dat_avgerr[complete.cases(dat_avgerr),]
  
  # take pairwise diff of logerrors, Density based vs comparison
  dat_diff = as.data.frame(
    dat_avgerr %>%
      group_by(iteration,crowd_size) %>%
      summarize(
        Simp.Average = mean_abserror[method=='Simp.Average'] - mean_abserror[method==benchmark],
        Median = mean_abserror[method=='Median'] - mean_abserror[method==benchmark],
        Min.Pivot = mean_abserror[method=='Min.Pivot'] - mean_abserror[method==benchmark],
        Know.Weight = mean_abserror[method=='Know.Weight'] - mean_abserror[method==benchmark],
        Meta.Prob.Weight = mean_abserror[method=='Meta.Prob.Weight'] - mean_abserror[method==benchmark]
      )
  )
  
  dat_diff = melt(dat_diff,id.vars = c('iteration','crowd_size'),measure.vars = c('Simp.Average','Median','Min.Pivot','Know.Weight','Meta.Prob.Weight'), variable.name = 'comparison', value.name = 'abserror_diff')
  
  return(dat_diff)
}

calculateErrorDiffLog = function(dat_avgerr, benchmark){
#### function to calculate error reduction (in log scale) from using xhat_SOA instead of benchmarks (pairwise comparison)
  
  dat_avgerr = dat_avgerr[complete.cases(dat_avgerr),]
  
  dat_diff = as.data.frame(
    dat_avgerr %>%
      group_by(iteration,crowd_size) %>%
      summarize(
        Simp.Average = log_mean_abserror[method=='Simp.Average'] - log_mean_abserror[method==benchmark],
        Median = log_mean_abserror[method=='Median'] - log_mean_abserror[method==benchmark],
        Min.Pivot = log_mean_abserror[method=='Min.Pivot'] - log_mean_abserror[method==benchmark],
        Know.Weight = log_mean_abserror[method=='Know.Weight'] - log_mean_abserror[method==benchmark],
        Meta.Prob.Weight = log_mean_abserror[method=='Meta.Prob.Weight'] - log_mean_abserror[method==benchmark]
      )
  )
  
  dat_diff = melt(dat_diff,id.vars = c('iteration','crowd_size'),measure.vars = c('Simp.Average','Median','Min.Pivot','Know.Weight','Meta.Prob.Weight'), variable.name = 'comparison', value.name = 'log_diff')
  
  return(dat_diff)
}

calculateErrorDiffRMSE = function(dat_avgerr, benchmark){
#### function to calculate error reduction (in RMSE) from using xhat_SOA instead of benchmarks (pairwise comparison)
  
  dat_avgerr = dat_avgerr[complete.cases(dat_avgerr),]
  
  dat_diff = as.data.frame(
    dat_avgerr %>%
      group_by(iteration,crowd_size) %>%
      summarize(
        Simp.Average = rootMSE[method=='Simp.Average'] - rootMSE[method==benchmark],
        Median = rootMSE[method=='Median'] - rootMSE[method==benchmark],
        Min.Pivot = rootMSE[method=='Min.Pivot'] - rootMSE[method==benchmark],
        Know.Weight = rootMSE[method=='Know.Weight'] - rootMSE[method==benchmark],
        Meta.Prob.Weight = rootMSE[method=='Meta.Prob.Weight'] - rootMSE[method==benchmark]
      )
  )
  
  dat_diff = melt(dat_diff,id.vars = c('iteration','crowd_size'),measure.vars = c('Simp.Average','Median','Min.Pivot','Know.Weight','Meta.Prob.Weight'), variable.name = 'comparison', value.name = 'rmse_diff')
  
  return(dat_diff)
}



sampleCrowd = function(i, dat,c_size){
# Sampling for bootstrap procedure 
  
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
# run bootstrap procedure  
  iter = 1:1000
  cat('crowd size',c_size,'\n')
  cat('generating Bootstrap samples\n')
  sample_list = lapply(iter,sampleCrowd, dat = dat, c_size = c_size)
  
  # run the methods
  cat('processing simple aggregates (average, median etc.) in Bootstrap samples\n')
  sample_list = lapply(sample_list, getSimpleAggregates)
  
  cat('running SOA\n')
  sample_list = lapply(sample_list, runSOA)
  
  cat('running Minimal Pivoting\n')
  sample_list = lapply(sample_list, runMinimalPivoting)
  
  cat('running Knowledge Weighting\n')
  sample_list = lapply(sample_list, runKnowledgeWeighted)
  
  cat('running Meta Probability Weighting\n')
  sample_list = lapply(sample_list, runMPW)
  
  cat('calculating errors of aggregate forecasts per method, item and iteration\n')
  error_list = lapply(sample_list, calculateErrorOnTheta)
  
  cat('calculating average errors across items in each iteration\n')
  error_list = lapply(error_list, calculateAverageErrors)
  
  # return bootstrap errors
  return(error_list)
  
}


quantiles_95 <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

plotError = function(dat_err){
# function to plot errors (by method)  

  dat_err = dat_err[complete.cases(dat_err),]
  
  my_palette = c( as.vector(jcolors(palette = 'pal2')[1:5]),as.vector(jcolors(palette = 'default')[3]) )[1:6]
  
  dat2 = as.data.frame(
    dat_err %>%
      group_by(method,crowd_size) %>%
      summarize(
        avg_rootMSE = mean(rootMSE,na.rm = TRUE)
      )
  )
  
  dat2$method = recode(dat2$method, 'SOA' = 'Surp.Overshoot')
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



plotErrorDiff = function(dat_diff, comparison_type, errordiff_type){
# function to plot 95% bootstrap confidence intervals  

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


getConfIntervals = function(diff_log){
# get Boostrap confidence intervals
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


################----------READ RAW DATA & CONSTRUCT DATA SET--------------################

#### Coin Data (Palley and Soll Study 1)
dat_coin = constructCoinData1()
dat_coin$task_type = 'Coin Flips'

rm(dat_coin1,dat_coin2)

####################------------------MAIN--------------################

### Run Bootstrap procedure for each sample size in (10,20,...,100)

crowd_size_coin = seq(10,100,by=10)
coin_err = list()

d1 = runBootstrapOnTheta(dat_coin,crowd_size_coin[1])
coin_err[[1]] = bind_rows(d1); rm(d1)
d2 = runBootstrapOnTheta(dat_coin,crowd_size_coin[2])
coin_err[[2]] = bind_rows(d2); rm(d2)
d3 = runBootstrapOnTheta(dat_coin,crowd_size_coin[3])
coin_err[[3]] = bind_rows(d3); rm(d3)
d4 = runBootstrapOnTheta(dat_coin,crowd_size_coin[4])
coin_err[[4]] = bind_rows(d4); rm(d4)
d5 = runBootstrapOnTheta(dat_coin,crowd_size_coin[5])
coin_err[[5]] = bind_rows(d5); rm(d5)
d6 = runBootstrapOnTheta(dat_coin,crowd_size_coin[6])
coin_err[[6]] = bind_rows(d6); rm(d6)
d7 = runBootstrapOnTheta(dat_coin,crowd_size_coin[7])
coin_err[[7]] = bind_rows(d7); rm(d7)
d8 = runBootstrapOnTheta(dat_coin,crowd_size_coin[8])
coin_err[[8]] = bind_rows(d8); rm(d8)
d9 = runBootstrapOnTheta(dat_coin,crowd_size_coin[9])
coin_err[[9]] = bind_rows(d9); rm(d9)
d10 = runBootstrapOnTheta(dat_coin,crowd_size_coin[10])
coin_err[[10]] = bind_rows(d10); rm(d10)

coin_err2 = bind_rows(coin_err)

# calculate Bootstrap error differences
diff_abs = calculateErrorDiff(coin_err2, 'SOA')
diff_log = calculateErrorDiffLog(coin_err2, 'SOA')
diff_rmse = calculateErrorDiffRMSE(coin_err2, 'SOA')

# generate plots
plotError(coin_err2)
plotErrorDiff(diff_log,'SOA','logdiff')

#getConfIntervals(diff_log)
