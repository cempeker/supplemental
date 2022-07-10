########### Bootstrap Analysis on Sample size, General Knowledge and State Capital data.
# To generate the results: 
# 1. Load R packages in LIBRARIES (install if necessary)
# 2. Execute the commands in MAIN in the given order. Comments give information on which commands produce the figures & tables in the manuscript.
# FUNCTIONS are already included in the workspace (see MAIN), they are included below for reference.

################----------------LIBRARIES--------------##############

library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(reshape2)
library(moments)
library(latex2exp)
library(jcolors)
library(metaggR)
library(xtable)


################----------------FUNCTIONS--------------##############

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

# load workspace with bootstrap errors and functions
load('bootstrap_binary_samplesize.RData')

# calculate pairwise differences in transformed Brier scores
brier_gk2_diff = calculateTransformedBrierDiff(brier_gk2,'SOA')
brier_sc2_diff = calculateTransformedBrierDiff(brier_sc2,'SOA')

# plot pairwise differences in transformed Brier scores
#plotTransformedBrierDiff(brier_gk2_diff)
#plotTransformedBrierDiff(brier_sc2_diff)
plotTransformedBrierDiffAll(brier_gk2_diff,brier_sc2_diff)

### Tabulate Bootstrap confidence intervals for pairwise differences
getConfIntervals(brier_gk2_diff)
getConfIntervals(brier_sc2_diff)

