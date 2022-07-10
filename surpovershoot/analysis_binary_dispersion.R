########### Bootstrap Analysis for General Knowledge and State Capital data
# To generate the results: 
# 1. Load R packages in LIBRARIES (install if necessary)
# 2. Execute the commands in MAIN in the given order. Comments give information on which commands produce the figures & tables in the manuscript.
# FUNCTIONS are already included in the workspace (see MAIN), they are included below for reference.

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


##################----------------FUNCTIONS--------------###############

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

plotTransformedBrierDiff = function(brier_diff_gk,brier_diff_sc,dispersion_type,threshold_type){
  
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
    #ggtitle(TeX('Percentge error reduction, \\hat{x} vs alternative')) +
    theme_bw(base_size = 14) +
    theme(legend.position = 'none')
  #theme(legend.position = 'top', 
  #      legend.direction = 'horizontal',
  #axis.text.x=element_blank()
  #)
  
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

# load workspace with bootstrap errors and functions
load('bootstrap_binary_dispersion.RData')

# Bootstrap Brier score data loaded from the workspace:
# bootstrap_gk_1sd, bootstrap_gk_1krt, bootstrap_gk_2sd, bootstrap_gk_2krt
# bootstrap_sc_1sd, bootstrap_sc_1krt, bootstrap_sc_2sd, bootstrap_sc_2krt
# See bootstrap_binary.R for more information

#### Main text and SOA2
brier_diff_gk = calculateTransformedBrierDiff(bootstrap_gk_1sd, benchmark = 'SOA')
brier_diff_sc = calculateTransformedBrierDiff(bootstrap_sc_1sd, benchmark = 'SOA')

brier_diff_gk2 = calculateTransformedBrierDiff(bootstrap_gk_1sd, benchmark = 'SOA2')
brier_diff_sc2 = calculateTransformedBrierDiff(bootstrap_sc_1sd, benchmark = 'SOA2')


# Pairwise difference in Score plots in the main text (std. dev is the measure of dispersion)
plotTransformedBrierDiff(brier_diff_gk,brier_diff_sc) # Benchmark is SO quantile type 1
plotTransformedBrierDiff(brier_diff_gk2,brier_diff_sc2)  # Benchmark is SO quantile type 2

# Confidence Intervals

getConfIntervalsGK(brier_diff_gk)
getConfIntervalsSC(brier_diff_sc)

rm(brier_diff_gk,brier_diff_gk2,brier_diff_sc,brier_diff_sc2)


# Pairwise difference in Score plots, reverse kurtosis is used as the measure of dispersion

brier_diff_gk = calculateTransformedBrierDiff(bootstrap_gk_1krt, benchmark = 'SOA')
brier_diff_sc = calculateTransformedBrierDiff(bootstrap_sc_1krt, benchmark = 'SOA')

plotTransformedBrierDiff(brier_diff_gk,brier_diff_sc)

rm(brier_diff_gk,brier_diff_sc)

# Pairwise difference in Score plots, std dev is measure of dispersion, category thresholds are 33/33/33

brier_diff_gk = calculateTransformedBrierDiff(bootstrap_gk_2sd, benchmark = 'SOA')
brier_diff_sc = calculateTransformedBrierDiff(bootstrap_sc_2sd, benchmark = 'SOA')

plotTransformedBrierDiff(brier_diff_gk,brier_diff_sc)

rm(brier_diff_gk,brier_diff_sc)

