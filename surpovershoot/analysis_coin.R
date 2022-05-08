########### Bootstrap Analysis, Coin Flips data.
# To generate the results: 
# 1. Load R packages in LIBRARIES (install if necessary)
# 2. Execute the commands in MAIN in the given order. Comments give information on which commands produce the figures & tables in the manuscript.
# FUNCTIONS are already included in the workspace (see MAIN), they are included below for reference.

################----------------LIBRARIES--------------##############

library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(reshape2)
library(latex2exp)
library(ggplot2)
library(jcolors)  # color palettes in the figures
library(xtable)


######################-----------------FUNCTIONS----------###############
# the functions below are included in workspace 'bootstrap_coin.RData'. Thus, it is not necessary to redefine them separately.

# Calculate pairwise differences and bootstrap errors

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


# plot bootstrap results

quantiles_95 <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

plotError = function(dat_err, SOA_type){
  # plot average RMSE. Produces Figure 5a for SOA_type = SOA, Figure D1a for SOA_type = SOA2 
  
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
  # plot differences in errors. Produces Figure 5b if dat_diff gives the differences with SO algorithm type-1 quantile, Figure D1b if type-2 quantile
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

# load workspace with bootstrap errors and functions
load('bootstrap_coin.RData')

# calculate pairwise differences for SOA-1 (quantile type 1)
diff_abs = calculateErrorDiff(coin_err2, 'SOA')
diff_log = calculateErrorDiffLog(coin_err2, 'SOA')
diff_rmse = calculateErrorDiffRMSE(coin_err2, 'SOA')

# calculate pairwise differences for SOA-2 (quantile type 2, averaging at discontinuities)
diff_abs2 = calculateErrorDiff(coin_err2, 'SOA2')
diff_log2 = calculateErrorDiffLog(coin_err2, 'SOA2')
diff_rmse2 = calculateErrorDiffRMSE(coin_err2, 'SOA2')

### Generate plots

# Average RMSE plots
plotError(coin_err2, 'SOA') # in the main text
plotError(coin_err2, 'SOA2') # in Appendix D

# Bootstrap error difference (confidence interval) plots
plotErrorDiff(diff_log,'logdiff') # in the main text
plotErrorDiff(diff_log2,'logdiff') # in Appendix D

### Tabulate Bootstrap confidence intervals for pairwise differences
getConfIntervals(diff_log)
