########### Study 1

### libraries
library(ggplot2)
library(dplyr)
library(reshape2) 
library(texreg)
library(jcolors)
library(estimatr)
library(latex2exp)

######### Functions...

## read and prepare raw data for analysis
readData = function(){
  dat = read.csv(paste0(getwd(),'/data/study1.csv'), header = TRUE)
  return(dat)
}
calculateMeasures = function(dat_temp){
  
  dat_temp$exp_cond = factor(dat_temp$exp_cond , levels = c('individual','crowd5CG','crowd10CG','crowd30CG'))
  
  dat_temp$theta = 100*dat_temp$prob_heads
  dat_temp$US_citizen = ifelse(dat_temp$Nationality == 'United States',1,0)
  
  dat_temp = dat_temp[!is.na(dat_temp$imputed_omega),]
  
  # calculate extremizing adjustments
  dat_temp$priv_direction = ifelse(dat_temp$xi_private > dat_temp$xi_shared, 1, -1)
  dat_temp$xi_mid = (dat_temp$xi_shared + dat_temp$xi_private)/2
  dat_temp$extrem_adj = NA
  for(i in 1:nrow(dat_temp)){
    priv_direction = dat_temp[i,'priv_direction']
    if(priv_direction == 1){
      dat_temp[i,'extrem_adj'] = dat_temp[i,'xi'] - dat_temp[i,'xi_mid']
    } else if(priv_direction == -1) {
      dat_temp[i,'extrem_adj'] = dat_temp[i,'xi_mid'] - dat_temp[i,'xi']
    }
  }
  
  dat_temp$is_extrem = ifelse(dat_temp$extrem_adj > 0, 1, 0)
  dat_temp$is_antiextrem = ifelse(dat_temp$extrem_adj < 0, 1, 0)
  dat_temp$is_noextrem = ifelse(dat_temp$extrem_adj == 0, 1, 0)
  
  return(dat_temp)
}
calculateOptimalExtremAdj = function(dat){
  
  for(i in 1:nrow(dat)){
    
    if(dat[i,'exp_cond'] == 'crowd5CG'){
      dat[i,'opt_report'] = (-1.5)*dat[i,'xi_shared'] + 2.5*dat[i,'xi_private']
    } else if(dat[i,'exp_cond'] == 'crowd10CG'){
      dat[i,'opt_report'] = (-4)*dat[i,'xi_shared'] + 5*dat[i,'xi_private']
    } else if(dat[i,'exp_cond'] == 'crowd30CG'){
      dat[i,'opt_report'] = (-14)*dat[i,'xi_shared'] + 15*dat[i,'xi_private']
    } else {
      dat[i,'opt_report'] = dat[i,'xi_mid']
    }
    
  }
  
  dat$opt_report = ifelse(dat$opt_report > 100, 100, dat$opt_report)
  dat$opt_report = ifelse(dat$opt_report < 0, 0, dat$opt_report)
  
  dat$opt_extrem_adj = ifelse(dat$priv_direction == -1, 
                              dat$xi_mid - dat$opt_report,
                              dat$opt_report - dat$xi_mid)
  
  dat$is_opt_extrem = ifelse(dat$is_extrem == 1 & dat$extrem_adj >= dat$opt_extrem_adj,1,0)
  
  
  return(dat)
}
calculateSharedInfoDiff = function(dat){
  # calculate shared-information gap
  dat$sharedinfo_diff = abs(dat$xi_shared - dat$theta)
  dat$sharedprivate_diff = abs(dat$xi_shared - dat$xi_private)
  dat$sharedprivate_diff2 = ifelse(dat$sharedprivate_diff >=50, '50 or higher', as.character(dat$sharedprivate_diff))
  return(dat)
}

## functions to generate percentage of extremization plot, prediction level
selectBootstrapSample = function(i,dat){
  
  d = as.data.frame(
    dat %>% 
      group_by(exp_cond,sharedprivate_diff2) %>% 
      sample_n(n(), replace = TRUE) 
  )
  
  d = d[,c('exp_cond','sharedprivate_diff2','is_extrem')]
  d$iteration = i
  return(d)
  
}
runBootstrap = function(dat){
  
  iter = 1:1000
  d = lapply(iter, selectBootstrapSample, dat = dat)
  d = bind_rows(d)
  
  d2 = as.data.frame(
    d %>%
      group_by(iteration,exp_cond,sharedprivate_diff2) %>%
      summarize(
        perc_extrem = mean(is_extrem)
      )
  )
  
  d3 = as.data.frame(
    d2 %>%
      group_by(exp_cond,sharedprivate_diff2) %>%
      summarize(
        #mean_perc_extrem = mean(perc_extrem),
        se_perc_extrem = sd(perc_extrem)
      )
  )
  
  return(d3)
}
plotSelfExtrem = function(dat){
  
  d = as.data.frame(
    dat %>%
      group_by(exp_cond,sharedprivate_diff2) %>%
      summarize(
        perc_extrem = mean(is_extrem)
      )
  )
  
  d3 = runBootstrap(dat)
  d4 = merge(d,d3,by = c('exp_cond','sharedprivate_diff2'))
  d4$lb = d4$perc_extrem - d4$se_perc_extrem
  d4$ub = d4$perc_extrem + d4$se_perc_extrem
  d4 = as.data.frame(d4 %>% rename(condition = exp_cond))
  
  my_palette = as.vector(jcolors(palette = 'pal2')[1:4])
  
  plot1 = ggplot(d4, aes(x=sharedprivate_diff2,y=perc_extrem,fill=condition)) +
    geom_bar(stat='identity',position = 'dodge') +
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(0.9),width=0.6) +
    scale_fill_manual(values=my_palette) +
    scale_x_discrete(name = TeX("Absolute difference between $\\mathit{s}$ and $\\mathit{t_i}$")) +
    scale_y_continuous(name = '% of self-extremized predictions',breaks = seq(0,0.7,by=0.1),labels = seq(0,70,by=10)) +
    theme_bw(base_size = 14)
  
  return(plot1)
  
}

selectBootstrapSample2 = function(i,dat){
  
  d = as.data.frame(
    dat %>% 
      group_by(exp_cond) %>% 
      sample_n(n(), replace = TRUE) 
  )
  
  d = d[,c('exp_cond','is_extrem','is_antiextrem','is_noextrem')]
  d$iteration = i
  return(d)
  
}
runBootstrap2 = function(dat){
  
  iter = 1:1000
  d = lapply(iter, selectBootstrapSample2, dat = dat)
  d = bind_rows(d)
  
  d2 = as.data.frame(
    d %>%
      group_by(iteration,exp_cond) %>%
      summarize(
        perc_extrem = mean(is_extrem),
        perc_antiextrem = mean(is_antiextrem),
        perc_noextrem = mean(is_noextrem)
      )
  )
  
  d3 = as.data.frame(
    d2 %>%
      group_by(exp_cond) %>%
      summarize(
        #mean_perc_extrem = mean(perc_extrem),
        se_perc_extrem = sd(perc_extrem),
        se_perc_antiextrem = sd(perc_antiextrem),
        se_perc_noextrem = sd(perc_noextrem)
      )
  )
  
  return(d3)
}
plotExtremPerc = function(dat){
  
  d = as.data.frame(
    dat %>%
      group_by(exp_cond) %>%
      summarize(
        perc_extrem = mean(is_extrem),
        perc_antiextrem = mean(is_antiextrem),
        perc_noextrem = mean(is_noextrem)
      )
  )
  
  d3 = runBootstrap2(dat)
  
  #dd = melt(d,id.vars = 'exp_cond')
  
  d4 = merge(d,d3,by = c('exp_cond'))
  
  d4$lb_extrem = d4$perc_extrem - d4$se_perc_extrem
  d4$ub_extrem = d4$perc_extrem + d4$se_perc_extrem
  d4$lb_antiextrem = d4$perc_antiextrem - d4$se_perc_antiextrem
  d4$ub_antiextrem = d4$perc_antiextrem + d4$se_perc_antiextrem
  d4$lb_noextrem = d4$perc_noextrem - d4$se_perc_noextrem
  d4$ub_noextrem = d4$perc_noextrem + d4$se_perc_noextrem
  
  
  d4 = as.data.frame(d4 %>% rename(condition = exp_cond))
  d5 = subset(d4,select = -c(se_perc_extrem,se_perc_antiextrem,se_perc_noextrem))
  
  d5.m = d5[,c('condition','perc_extrem','perc_antiextrem','perc_noextrem')]
  colnames(d5.m) = c('condition','extrem','antiextrem','noextrem')
  d5.m = melt(d5.m, value.name = 'perc')
  
  d5.lb = d5[,c('condition','lb_extrem','lb_antiextrem','lb_noextrem')]
  colnames(d5.lb) = c('condition','extrem','antiextrem','noextrem')
  d5.lb = melt(d5.lb, value.name = 'lb')
  
  d5.ub = d5[,c('condition','ub_extrem','ub_antiextrem','ub_noextrem')]
  colnames(d5.ub) = c('condition','extrem','antiextrem','noextrem')
  d5.ub = melt(d5.ub, value.name = 'ub')
  
  
  d6 = merge(d5.m,d5.lb, by=c('condition','variable'))
  d6 = merge(d6,d5.ub, by=c('condition','variable'))
  d6$variable = factor(d6$variable, levels = c('noextrem','extrem','antiextrem'))
  
  my_palette = as.vector(jcolors(palette = 'pal2')[1:4])
  #my_palette = as.vector(jcolors(palette = 'pal2')[1:4])
  #my_palette = as.vector(c(jcolors(palette = 'pal2')[1],jcolors(palette = 'pal9')[c(2,4)])) 
  
  plot1 = ggplot(d6, aes(x=variable,y=perc,fill=condition)) +
    geom_bar(stat='identity',position = 'dodge',width = 0.8) +
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(0.8),width=0.5) +
    scale_fill_manual(values=my_palette) +
    scale_x_discrete(name = 'Expert prediction', labels = c('= posterior','extremized','anti-extremized')) +
    scale_y_continuous(name = 'Frequency (percentage)', breaks = seq(0,0.45,by=0.05),labels = seq(0,45,by=5)) +
    theme_bw(base_size = 14)
  
  return(plot1)
  
}

## generate optimal vs actual extrem adjustment plot, prediction level
plotOptVsActualExtremAdj = function(dat){
  
  d = dat
  d$opt_percent = 100*d$extrem_adj / d$opt_extrem_adj
  d = d[d$exp_cond != 'individual',]
  
  d = as.data.frame(
    d %>%
      mutate(
        opt_percent_bin = cut(opt_percent, breaks = c(-950,seq(0,100,by=10),300))
      )
  )
  levels(d$opt_percent_bin) = c('not self-extrem',levels(d$opt_percent_bin)[2:12])
  
  d2 = as.data.frame(
    d %>%
      group_by(exp_cond,opt_percent_bin) %>%
      summarize(
        number_of_cases = n()
      )
  )
  
  my_palette = as.vector(c('#000000',jcolors(palette = 'pal12')[2:12]))
  
  plot1 = ggplot(d2, aes(x=exp_cond,y=number_of_cases,fill=opt_percent_bin)) +
    geom_bar(stat='identity',position = position_stack(reverse = TRUE), width=0.5) +
    coord_flip() +
    scale_fill_manual(values=my_palette, name = '% of Optimal \n Extr. Adjustm.') +
    scale_x_discrete(name = element_blank()) +
    scale_y_continuous(name = 'Number of predictions',breaks = seq(0,700,by=50)) +
    theme_bw(base_size = 14)
  
  return(plot1)
  
}

## extremizing adjustments, individual level
plotExtremAdjInd = function(dat){
  
  d = dat
  #d = dat[dat$exp_cond %in% c('crowd5CG','crowd10CG','crowd30CG','crowd10'),]
  
  d_avg = as.data.frame(
    d %>%
      group_by(PROLIFIC_PID,exp_cond) %>%
      summarize(
        avg_extrem_adj = mean(extrem_adj)
      )
  ) 
  
  pl_avg = ggplot(d_avg,aes(x=avg_extrem_adj)) +
    geom_histogram() + 
    geom_vline(xintercept = 0, linetype="dashed", size=0.8, color='black') +
    facet_wrap(~exp_cond) +
    scale_x_continuous(name = 'Average extremizing adjustment',breaks = seq(-30,70,10)) +
    scale_y_continuous(name = 'Count',breaks = seq(0,50,4)) +
    theme_bw(base_size = 14)
  
  
  return(pl_avg)
  
}

# run extremizing adjustment regression
runExtremAdjRegression = function(dat){
  
  # run regressions in Table 1 (models 1,2,3 and 4)
  # also conduct pairwise tests (linear hypotheses)
  
  dat3 = dat
  linmod1 = lh_robust(extrem_adj ~ exp_cond, data = dat3, clusters = dat3$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('exp_condcrowd10CG - exp_condcrowd5CG = 0','exp_condcrowd10CG - exp_condcrowd30CG = 0'))
  linmod2 = lh_robust(extrem_adj ~ exp_cond + Gender + age + US_citizen, data = dat3, clusters = dat3$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('exp_condcrowd10CG - exp_condcrowd5CG = 0','exp_condcrowd10CG - exp_condcrowd30CG = 0'))
  
  
  dat4 = dat3[dat3$quiz_correct == 1 & dat3$clarity <= 3,]
  linmod3 = lh_robust(extrem_adj ~ exp_cond, data = dat4, clusters = dat4$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('exp_condcrowd10CG - exp_condcrowd5CG = 0','exp_condcrowd10CG - exp_condcrowd30CG = 0'))
  linmod4 = lh_robust(extrem_adj ~ exp_cond + Gender + age + US_citizen, data = dat4, clusters = dat4$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('exp_condcrowd10CG - exp_condcrowd5CG = 0','exp_condcrowd10CG - exp_condcrowd30CG = 0'))
  
  return(list(linmod1,linmod2,linmod3,linmod4))
  
}
tabulateRegResults = function(regr_results){
  # constructs Table 1 from regression results
  for(i in 1:length(regr_results)){
    regr_results[[i]] = regr_results[[i]]$lm_robust
  }
  res_table = texreg(regr_results, include.ci = FALSE, stars = c(0.01,0.05,0.1))
  return(res_table)
  
}


#############################  MAIN 

#### Read and prepare data
dat = readData()
dat = calculateMeasures(dat) 
dat = calculateOptimalExtremAdj(dat)
dat = calculateSharedInfoDiff(dat) 

#### Generate percentage extremization plots (Figures 2 and 3)
plt_selfextrem = plotSelfExtrem(dat)
pdf(file = "./s1_selfextrem.pdf",width=7,height=4)
plt_selfextrem
dev.off()

plt_extremperc = plotExtremPerc(dat)
pdf(file = "./s1_extremperc.pdf",width=6.4,height=3.8)
plt_extremperc
dev.off()

#### Generate optimal vs actual extrem adjustment plot (Figure 4)
plt_optVSact = plotOptVsActualExtremAdj(dat)
pdf(file = "./s1_optVSact.pdf",width=8,height=4)
plt_optVSact
dev.off()

#### Plot Average extremizing adjustment across individuals (Appendix)
plt_extremind = plotExtremAdjInd(dat)
pdf(file = "./s1_extremind.pdf",width=7,height=5)
plt_extremind
dev.off()

#### obtain regression results and generate Table 1
reg_extremeadj = runExtremAdjRegression(dat)
table_reg_extremeadj = tabulateRegResults(reg_extremeadj)

