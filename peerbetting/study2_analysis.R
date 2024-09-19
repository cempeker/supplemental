################## PPM Study 2 analysis

# libraries 
library(reshape2)
#library(dplyr)
#library(stringr)
library(tidyverse)
library(ggplot2)
#library(jcolors)
library(lmtest)
library(sandwich)
library(RColorBrewer)
library(estimatr)
library(margins)
library(texreg)


################----------------FUNCTIONS--------------##############

# get data
readData = function(){
  
  # Read choice data
  dat = read.csv('data/rawdata/s2_qualtrics.csv', header = TRUE)
  dat = dat[-(1:2),]
  dat$PROLIFIC_PID = as.character(dat$PROLIFIC_PID)
  # remove duplicates
  dat = dat[!duplicated(dat$PROLIFIC_PID),]
  dat$EndDate = as.Date(dat$EndDate)
  dat$Progress = as.numeric(as.character(dat$Progress))
  dat$Duration..in.seconds. = as.numeric(as.character(dat$Duration..in.seconds.))
  # exclude test
  dat = dat[dat$PROLIFIC_PID != '5e8709bdc740a73456838dbb',]
  
  # identify choice data columns
  q_plainOnce = paste0('X',1:8,'_q_plainOnce')
  q_plainTwice = paste0('X',1:8,'_q_plainTwice')
  q_peerinfoOnce = paste0('X',1:8,'_q_peerinfoOnce')
  q_peerinfoTwice = paste0('X',1:8,'_q_peerinfoTwice')
  response_cols =  c(q_plainOnce, q_plainTwice, q_peerinfoOnce, q_peerinfoTwice)
  
  # construct choice data
  dat_choice = dat[,c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.','clarity',response_cols)]
  rm(response_cols)
  dat_choice = melt(dat_choice,id.vars = c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.','clarity'))
  
  dat_choice$value = as.numeric(dat_choice$value)
  dat_choice = dat_choice[complete.cases(dat_choice),]
  dat_choice$value = (-1)*dat_choice$value + 2
  dat_choice$variable = as.character(dat_choice$variable)
  dat_choice$question = str_sub(dat_choice$variable,2,2)
  dat_choice$clarity = (-1)*as.numeric(dat_choice$clarity) + 6
  
  dat_choice = dat_choice[,c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.','question','value','clarity')]
  colnames(dat_choice) = c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.','question','response','clarity')
  
  dat_choice$threshold = ifelse(dat_choice$exp_cond %in% c('control1Once','control2Once','treatmentOnce'),'once','twice')
  dat_choice$week = ifelse(dat_choice$EndDate >= '2020-11-01', 2, 1)
  
  
  # identify select choice time columns
  time_plainOnce = paste0('X',1:8,'_time_plainOnce_Page.Submit')
  time_plainTwice = paste0('X',1:8,'_time_plainTwice_Page.Submit')
  time_peerinfoOnce = paste0('X',1:8,'_time_peerinfoOnce_Page.Submit')
  time_peerinfoTwice = paste0('X',1:8,'_time_peerinfoTwice_Page.Submit')
  time_cols = c(time_plainOnce,time_plainTwice,time_peerinfoOnce,time_peerinfoTwice)
  
  # construct time data
  dat_time = dat[,c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.',time_cols)]
  rm(time_cols)
  dat_time = melt(dat_time,id.vars = c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.'))
  
  dat_time$value = as.numeric(dat_time$value)
  dat_time = dat_time[complete.cases(dat_time),]
  dat_time$question = str_sub(dat_time$variable,2,2)
  
  dat_time = dat_time[,c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.','question','value')]
  colnames(dat_time) = c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.','question','response_time')
  
  dat_time$threshold = ifelse(dat_time$exp_cond %in% c('control1Once','control2Once','treatmentOnce'),'once','twice')
  dat_time$week = ifelse(dat_time$EndDate >= '2020-11-01', 2, 1)
  
  
  # merge response data and response time data
  dat2 = merge(dat_choice,dat_time,by=c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.','question','threshold','week'))
  dat2$EndDate = NULL
  
  
  # read participant data and merge
  dat_prolific1 = read.csv('data/rawdata/s2_prolific_week1.csv', header = TRUE)
  dat_prolific2 = read.csv('data/rawdata/s2_prolific_week2.csv', header = TRUE)
  dat_prolific = rbind(dat_prolific1,dat_prolific2)
  rm(dat_prolific1, dat_prolific2)
  #dat_prolific = dat_prolific[dat_prolific$status == 'AWAITING REVIEW',]
  dat_prolific = dat_prolific[dat_prolific$Student.Status != 'No',]
  dat_prolific = dat_prolific[,c('participant_id','age','Sex','Nationality')]
  colnames(dat_prolific) = c('PROLIFIC_PID','age','female','nationality')
  dat_prolific$female = ifelse(dat_prolific$female == 'Female',1,0)
  dat_prolific$UK_citizen = ifelse(dat_prolific$nationality == 'United Kingdom',1,0)
  
  dat3 = merge(dat2,dat_prolific, by='PROLIFIC_PID')
  
  dat3$condition = NA
  dat3[dat3$exp_cond %in% c('control1Once','control1Twice'),'condition'] = 'control1'
  dat3[dat3$exp_cond %in% c('control2Once','control2Twice'),'condition'] = 'control2'
  dat3[dat3$exp_cond %in% c('treatmentOnce','treatmentTwice'),'condition'] = 'peerbet'
  
  dat3$week = paste0('week ',dat3$week)
  
  dat3$condition = as.factor(dat3$condition)
  levels(dat3$condition) = list(Flat = 'control1', `Flat-PastRate` = 'control2', Peerbet = 'peerbet')

  # return  
  return(dat3)
}
# summarize bonuses
summaryBonus = function(dat){
  w1_bonus = read.csv('data/rawdata/s2_week1bonuses.csv', header = FALSE)
  colnames(w1_bonus) = c('PROLIFIC_PID','bonus')
  w2_bonus = read.csv('data/rawdata/s2_week2bonuses.csv', header = FALSE)
  colnames(w2_bonus) = c('PROLIFIC_PID','bonus')
  dat_bonus = rbind(w1_bonus,w2_bonus)
  rm(w1_bonus,w2_bonus)
  
  dat4 = merge(dat,dat_bonus,by='PROLIFIC_PID')
  dat4$reward = dat4$bonus + 0.75
  #dat4 = dat4[dat4$condition == 'treatment']
  dat5 = as.data.frame(
    dat4 %>%
      group_by(week, condition, threshold) %>%
      summarize(
        min_reward = min(reward,na.rm=TRUE),
        avg_reward = mean(reward,na.rm=TRUE),
        max_reward = max(reward,na.rm=TRUE)
      )
  )
  
  return(dat5)
}
# generate summary statistics
summaryStats = function(dat){
  
  dat2 = unique(dat[,c('PROLIFIC_PID','condition','age','female','UK_citizen','Duration..in.seconds.','threshold','week','threshold')])
  
  dat3 = as.data.frame(
    dat2 %>%
      group_by(week, condition, threshold) %>%
      summarize(
        avg_age = mean(age, na.rm=TRUE),
        num_UK_citizen = sum(UK_citizen, na.rm=TRUE),
        num_nonUK_citizen= n()-num_UK_citizen,
        avg_duration = mean(Duration..in.seconds., na.rm=TRUE),
        num_female = sum(female, na.rm=TRUE),
        num_male = n()-num_female
      )
  )
  
  return(dat3)
}

## Figures in the main text
# plot percentage true - Figure 6
plotPercentageTrue = function(dat){
  
  # Plot percentage of True vs exp cond for Once and Twice
  
  dat2 = dat[dat$response_time <= 60,]
  
  dat3 = as.data.frame(
    dat2 %>%
      group_by(condition,week,threshold) %>%
      summarize(n = n(),true_picks = sum(response))
  )
  
  dat3$perc_true_picks = 100*(dat3$true_picks / dat3$n)
  dat3$threshold = paste0('version: \'at least ',dat3$threshold,'\'')
  
  #my_palette = as.vector(jcolors(palette = 'pal5')[c(1,2,3)])
  my_palette = c( brewer.pal(9,"Blues")[7], brewer.pal(9,"YlOrBr")[3], brewer.pal(9,"PuRd")[6] )
  
  pl = ggplot(dat3, aes(x = week, y = perc_true_picks, fill = condition)) + 
    geom_bar(position="dodge", stat='identity',width=0.7) +
    scale_fill_manual(values=my_palette) +
    facet_wrap(vars(threshold)) +
    scale_y_continuous(name = '% of True') +
    theme_bw(base_size = 14)
  
  #pdf(file = "./s2_trueperc.pdf",width=7,height=3.5)
  #pl_true
  #dev.off()
  ggsave(filename = "./s2_perctrue.pdf", plot = pl, width=7, height=3.5, device=cairo_pdf)
  
  return(pl)
  
}
# plot response times - Figure 7
plotResponseTime = function(dat){
  # Plot percentage of True vs exp cond for Once and Twice
  dat2 = dat
  dat2$threshold = paste0('version: \'at least ',dat2$threshold,'\'')
  dat3 = dat2[dat2$response_time <= 60,]
  #dat3 = dat2
  
  dat3$response = ifelse(dat3$response == 1, 'True', 'False')
  #dat3$response = paste0('response = ',dat3$response)
  
  #my_palette = as.vector(jcolors(palette = 'pal5')[c(1,2,3)])
  my_palette = c( brewer.pal(9,"Blues")[7], brewer.pal(9,"YlOrBr")[3], brewer.pal(9,"PuRd")[6] )
  
  pl = ggplot(dat3, aes(x = as.factor(response), y = response_time, fill = condition)) + 
    geom_boxplot(width=0.7, outlier.shape = NA) +
    scale_fill_manual(values=my_palette) +
    facet_grid(week ~ threshold) +
    scale_y_continuous(name = 'Response time (in seconds)',breaks = seq(0,14,2)) +
    scale_x_discrete(name = 'Response') +
    coord_cartesian(ylim = c(0,14)) +
    theme_bw(base_size = 14)
  
  #pdf(file = "./s2_resptime.pdf",width=7,height=4)
  #pl_resptime
  #dev.off()
  ggsave(filename = "./s2_resptime.pdf", plot = pl, width=7, height=4, device=cairo_pdf)
  
  return(pl)
  
}

## Figures in the Appendix
# plot "How clear were the instructions in this experiment" by treatment - Figure C3
plotClarityByTreatment = function(dat){
  
  dat$clarity = factor(dat$clarity)
  dat2 = unique(dat[,c('PROLIFIC_PID','condition','week','clarity')])
  
  dat2 = as.data.frame(
    dat2 %>%
      group_by(condition,week,clarity, .drop = FALSE) %>%
      summarize(
        freq = n()
      )
  )
  
  dat3 = as.data.frame(
    dat2 %>%
      group_by(condition, .drop = FALSE) %>%
      mutate(
        perc_freq = 100*freq / sum(freq)
      )
  )
  
  #my_palette = as.vector(jcolors(palette = 'pal5')[c(1,2,3)])
  my_palette = c( brewer.pal(9,"Blues")[7], brewer.pal(9,"YlOrBr")[3], brewer.pal(9,"PuRd")[6] )
  
  pl = ggplot(dat3, aes(x = clarity, y = freq, fill = condition)) + 
    geom_bar(position="dodge", stat='identity',width=0.8) +
    #geom_label(aes(label = freq, color=condition), position = position_dodge()) +
    geom_text(data = dat3,aes(label=freq),position = position_dodge(0.9),vjust=-0.5) +
    facet_wrap(~week,nrow = 2) + 
    scale_fill_manual(name = 'Condition',values=my_palette) +
    scale_x_discrete(name='Self-reported clarity of the instructions (5:Very clear, 1:Very unclear)', labels = 1:5) +
    #scale_y_continuous(name = 'Proportion',breaks = c(0,0.2,0.4,0.6,0.8,1)) +
    scale_y_continuous(name = 'Number of subjects', limits=c(0,110)) +
    theme_bw(base_size = 14)
  
  #pdf(file = "./s2_clarity.pdf",width=7.2,height=5)
  #pl_clarity
  #dev.off()
  ggsave(filename = "./s2_clarity.pdf", plot = pl, width=7.2, height=5, device=cairo_pdf)
  
  return(pl)
  
}
# plot percentage true by task - Figure D2
plotPercentageTrueByTask = function(dat){
  
  # Plot percentage of True vs exp cond for Once and Twice
  
  dat2 = dat[dat$response_time <= 60,]
  
  dat3 = as.data.frame(
    dat2 %>%
      group_by(question,condition,week,threshold) %>%
      summarize(n = n(),true_picks = sum(response))
  )
  
  dat3$perc_true_picks = 100*(dat3$true_picks / dat3$n)
  dat3$threshold = paste0('version: \'at least ',dat3$threshold,'\'')
  
  #my_palette = as.vector(jcolors(palette = 'pal5')[c(1,2,3)])
  my_palette = c( brewer.pal(9,"Blues")[7], brewer.pal(9,"YlOrBr")[3], brewer.pal(9,"PuRd")[6] )
  
  pl = ggplot(dat3, aes(x = question, y = perc_true_picks, fill = condition)) + 
    geom_bar(position="dodge", stat='identity',width=0.7) +
    scale_fill_manual(values=my_palette) +
    facet_grid(week ~ threshold) +
    scale_y_continuous(name = '% of True') +
    theme_bw(base_size = 14)
  
  #pdf(file = "./s2_trueperc_bytask.pdf",width=7,height=4)
  #pl_true_bytask
  #dev.off()
  ggsave(filename = "./s2_perctrue_bytask.pdf", plot = pl, width=7, height=4, device=cairo_pdf)
  
  return(pl)
  
}
# plot response time distribution - Figure D3
plotResponseTimeDistr = function(dat){
  
  dat2 = dat
  dat2$threshold = paste0('version: \'at least ',dat2$threshold,'\'')
  
  dat3 = as.data.frame(
    dat2 %>%
      mutate(
        resp_time_bin = cut(response_time,breaks = c(0,2,4,6,8,seq(10,90,10),300))
      )
  )
  
  dat4 = as.data.frame(
    dat3 %>%
      group_by(condition,resp_time_bin) %>%
      summarize(
        bin_count = n()
      )
  )
  
  #my_palette = as.vector(jcolors(palette = 'pal5')[c(1,2,3)])
  my_palette = c( brewer.pal(9,"Blues")[7], brewer.pal(9,"YlOrBr")[3], brewer.pal(9,"PuRd")[6] )
  
  pl = ggplot(dat4, aes(x = resp_time_bin, y = bin_count,fill = condition)) + 
    geom_bar(position="dodge", stat='identity',width=0.7) +
    scale_fill_manual(values=my_palette) +
    #facet_grid(.~condition) +
    coord_flip() +
    scale_x_discrete(name = 'Response Time') +
    scale_y_continuous(name = 'Number of observations') +
    theme_bw(base_size = 14)
  
  #pdf(file = "./s2_resptime_distr.pdf",width=7,height=4)
  #pl_resptime_bins[[1]]
  #dev.off()
  ggsave(filename = "./s2_resptime_distr.pdf", plot = pl, width=7, height=4, device=cairo_pdf)
  
  return(pl)
  
}


## Run choice data regressions
runResponseRegressions = function(dat, survey_version){
  
  ##### WEEK 1
  dat_w1 = dat[dat$threshold == survey_version & dat$week == 'week 1',]
  dat_w1f = dat[dat$threshold == survey_version & dat$week == 'week 1' & dat$response_time <= 60,]
  
  # Logit model, no controls, filtered
  mod1 = glm(response ~ condition, family = binomial(link = "logit"), data = dat_w1f, na.action = na.omit)
  #logit_mod1_mfx = logitmfx(formula = response ~ condition, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID') 
  mod1_mfx = margins_summary(mod1, data = dat_w1f, vcov = vcovCL(mod1,cluster = dat_w1f$PROLIFIC_PID))
  mod1_coef = coeftest(mod1, vcov = vcovCL(mod1,cluster = dat_w1f$PROLIFIC_PID))
  mod1_stats = data.frame(
    number_of_obs = nobs(mod1),
    `Likl. Ratio` = with(mod1, null.deviance - deviance),
    `Pr(>Chisq)` = with(mod1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),
    AIC = mod1$aic
  )
  
  
  # Logit model, with controls, filtered
  mod2 = glm(response ~ condition + response_time + age + female + UK_citizen, family = binomial(link = "logit"), data = dat_w1f, na.action = na.omit)
  #a = miceadds::glm.cluster(response ~ condition + age + female + UK_citizen, family = binomial(link = "logit"), data = dat2, cluster = dat2$PROLIFIC_PID)
  #logit_mod2_mfx = logitmfx(formula = response ~ condition + age + female + UK_citizen, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  mod2_mfx = margins_summary(mod2, data = dat_w1f, vcov = vcovCL(mod2,cluster = dat_w1f$PROLIFIC_PID))
  mod2_coef = coeftest(mod2, vcov = vcovCL(mod2,cluster = dat_w1f$PROLIFIC_PID))
  mod2_stats = data.frame(
    number_of_obs = nobs(mod2),
    `Likl. Ratio` = with(mod2, null.deviance - deviance),
    `Pr(>Chisq)` = with(mod2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),
    AIC = mod2$aic
  )
 
  # Logit model, with controls, all
  mod3 = glm(response ~ condition + response_time + age + female + UK_citizen, family = binomial(link = "logit"), data = dat_w1, na.action = na.omit)
  #a = miceadds::glm.cluster(response ~ condition + age + female + UK_citizen, family = binomial(link = "logit"), data = dat2, cluster = dat2$PROLIFIC_PID)
  #logit_mod2_mfx = logitmfx(formula = response ~ condition + age + female + UK_citizen, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  mod3_mfx = margins_summary(mod3, data = dat_w1, vcov = vcovCL(mod3,cluster = dat_w1$PROLIFIC_PID))
  mod3_coef = coeftest(mod3, vcov = vcovCL(mod3,cluster = dat_w1$PROLIFIC_PID))
  mod3_stats = data.frame(
    number_of_obs = nobs(mod3),
    `Likl. Ratio` = with(mod3, null.deviance - deviance),
    `Pr(>Chisq)` = with(mod3, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),
    AIC = mod3$aic
  )
  
  # WEEK 2
  dat_w2 = dat[dat$threshold == survey_version & dat$week == 'week 2',]
  dat_w2f = dat[dat$threshold == survey_version & dat$week == 'week 2' & dat$response_time <= 60,]
  
  # Logit model, no controls
  mod4 = glm(response ~ condition, family = binomial(link = "logit"), data = dat_w2f, na.action = na.omit)
  #logit_mod3_mfx = logitmfx(formula = response ~ condition, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  mod4_mfx = margins_summary(mod4, data = dat_w2f, vcov = vcovCL(mod4,cluster = dat_w2f$PROLIFIC_PID))
  mod4_coef = coeftest(mod4, vcov = vcovCL(mod4,cluster = dat_w2f$PROLIFIC_PID))
  mod4_stats = data.frame(
    number_of_obs = nobs(mod4),
    `Likl. Ratio` = with(mod4, null.deviance - deviance),
    `Pr(>Chisq)` = with(mod4, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),
    AIC = mod4$aic
  )
  
  # Logit model, with controls
  mod5 = glm(response ~ condition + response_time  + age + female + UK_citizen, family = binomial(link = "logit"), data = dat_w2f, na.action = na.omit)
  #logit_mod4_mfx = logitmfx(formula = response ~ condition + age + female + UK_citizen, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  mod5_mfx = margins_summary(mod5, data = dat_w2f, vcov = vcovCL(mod5,cluster = dat_w2f$PROLIFIC_PID))
  mod5_coef = coeftest(mod5, vcov = vcovCL(mod5,cluster = dat_w2f$PROLIFIC_PID))
  mod5_stats = data.frame(
    number_of_obs = nobs(mod5),
    `Likl. Ratio` = with(mod5, null.deviance - deviance),
    `Pr(>Chisq)` = with(mod5, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),
    AIC = mod5$aic
  )
  
  # Logit model, with controls, all
  mod6 = glm(response ~ condition + response_time  + age + female + UK_citizen, family = binomial(link = "logit"), data = dat_w2, na.action = na.omit)
  #a = miceadds::glm.cluster(response ~ condition + age + female + UK_citizen, family = binomial(link = "logit"), data = dat2, cluster = dat2$PROLIFIC_PID)
  #logit_mod2_mfx = logitmfx(formula = response ~ condition + age + female + UK_citizen, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  mod6_mfx = margins_summary(mod6, data = dat_w1, vcov = vcovCL(mod6,cluster = dat_w2$PROLIFIC_PID))
  mod6_coef = coeftest(mod6, vcov = vcovCL(mod6,cluster = dat_w2$PROLIFIC_PID))
  mod6_stats = data.frame(
    number_of_obs = nobs(mod6),
    `Likl. Ratio` = with(mod6, null.deviance - deviance),
    `Pr(>Chisq)` = with(mod6, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),
    AIC = mod6$aic
  )
  
  
  # return results
  results_list = list(
    mod1_mfx, mod1_coef, mod1_stats, 
    mod2_mfx, mod2_coef, mod2_stats,
    mod3_mfx, mod3_coef, mod3_stats,
    
    mod4_mfx, mod4_coef, mod4_stats,
    mod5_mfx, mod5_coef, mod5_stats,
    mod6_mfx, mod6_coef, mod6_stats
  )
  
  return(results_list)
}
# Tabulate regression results and marginal effects
getMargins = function(regr_resp){
  
  mod1 = as.data.frame(regr_resp[[1]] %>% mutate_if(is.numeric, round, digits = 4))
  mod1$model = '(1)'
  mod2 = as.data.frame(regr_resp[[4]] %>% mutate_if(is.numeric, round, digits = 4))
  mod2$model = '(2)'
  mod3 = as.data.frame(regr_resp[[7]] %>% mutate_if(is.numeric, round, digits = 4))
  mod3$model = '(3)'
  mod4 = as.data.frame(regr_resp[[10]] %>% mutate_if(is.numeric, round, digits = 4))
  mod4$model = '(4)'
  mod5 = as.data.frame(regr_resp[[13]] %>% mutate_if(is.numeric, round, digits = 4))
  mod5$model = '(5)'
  mod6 = as.data.frame(regr_resp[[16]] %>% mutate_if(is.numeric, round, digits = 4))
  mod6$model = '(6)'
  
  res_table = rbind(mod1,mod2,mod3,mod4,mod5,mod6)
  #regr_results = list(mod1,mod2,mod3,mod4)
  #res_table = texreg(regr_results, include.ci = FALSE, stars = c(0.001,0.01, 0.05, 0.1))
  
  return(res_table)
}
tabulateEstimates = function(regr_resp){
  
  mod1 = regr_resp[[2]]
  mod2 = regr_resp[[5]]
  mod3 = regr_resp[[8]]
  mod4 = regr_resp[[11]]
  mod5 = regr_resp[[14]]
  mod6 = regr_resp[[17]]
  #mod7 = regr_resp[[14]]
  #mod8 = regr_resp[[16]]
  
  #regr_results = list(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)
  regr_results = list(mod1,mod2,mod3,mod4,mod5,mod6)
  res_table = texreg(regr_results, include.ci = FALSE, stars = c(0.001,0.01, 0.05, 0.1))
  
  return(res_table)
  
}
getRegrStats = function(regr_resp){
  
  mod1 = as.data.frame(regr_resp[[3]])
  mod1$model = '(1)'
  mod2 = as.data.frame(regr_resp[[6]])
  mod2$model = '(2)'
  mod3 = as.data.frame(regr_resp[[9]])
  mod3$model = '(3)'
  mod4 = as.data.frame(regr_resp[[12]])
  mod4$model = '(4)'
  mod5 = as.data.frame(regr_resp[[15]])
  mod5$model = '(5)'
  mod6 = as.data.frame(regr_resp[[18]])
  mod6$model = '(6)'
  
  res_table = rbind(mod1,mod2,mod3,mod4,mod5,mod6)
  #regr_results = list(mod1,mod2,mod3,mod4)
  #res_table = texreg(regr_results, include.ci = FALSE, stars = c(0.001,0.01, 0.05, 0.1))
  
  return(res_table)
}

# Run response time regressions
runResponseTimeRegressions = function(dat, survey_version){
  
  # WEEK 1
  dat_w1f = dat[dat$threshold == survey_version & dat$week == 'week 1' & dat$response_time <= 60,]
  dat_w1 = dat[dat$threshold == survey_version & dat$week == 'week 1',]
  
  resp_time_regr1 = lh_robust(formula = response_time ~ condition + response + condition*response, data = dat_w1f, clusters = dat_w1f$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditionFlat-PastRate = 0'))
  resp_time_regr2 = lh_robust(formula = response_time ~ condition + response + condition*response + age + female + UK_citizen, data = dat_w1f, clusters = dat_w1f$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditionFlat-PastRate = 0'))
  resp_time_regr3 = lh_robust(formula = response_time ~ condition + response + condition*response + age + female + UK_citizen, data = dat_w1, clusters = dat_w1$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditionFlat-PastRate = 0'))
  
  # WEEK 2
  dat_w2f = dat[dat$threshold == survey_version & dat$week == 'week 2' & dat$response_time <= 60,]
  dat_w2 = dat[dat$threshold == survey_version & dat$week == 'week 2',]
  
  resp_time_regr4 = lh_robust(formula = response_time ~ condition + response + condition*response, data = dat_w2f, clusters = dat_w2f$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditionFlat-PastRate = 0'))
  resp_time_regr5 = lh_robust(formula = response_time ~ condition + response + condition*response + age + female + UK_citizen, data = dat_w2f, clusters = dat_w2f$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditionFlat-PastRate = 0'))
  resp_time_regr6 = lh_robust(formula = response_time ~ condition + response + condition*response + age + female + UK_citizen, data = dat_w2, clusters = dat_w2$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditionFlat-PastRate = 0'))
  
  
  results_list = list(
    resp_time_regr1,
    resp_time_regr2,
    resp_time_regr3,
    resp_time_regr4,
    resp_time_regr5,
    resp_time_regr6
  )
  
  return(results_list)
  
}
# Tabulate response time regression results
tabulateRespTimeEstimates = function(regr_time){
  
  mod1 = regr_time[[1]]$lm_robust
  mod2 = regr_time[[2]]$lm_robust
  mod3 = regr_time[[3]]$lm_robust
  mod4 = regr_time[[4]]$lm_robust
  mod5 = regr_time[[5]]$lm_robust
  mod6 = regr_time[[6]]$lm_robust
  
  regr_results = list(mod1,mod2,mod3,mod4,mod5,mod6)
  #regr_results = list(mod5,mod6)
  res_table = texreg(regr_results, include.ci = FALSE, stars = c(0.001,0.01, 0.05, 0.1))
  
  return(res_table)
  
}


################-----------------READ DATA------------###############

dat = readData()

# save data
# write.csv(subset(dat,select = -c(exp_cond,nationality)),'s2_data.csv',row.names=FALSE)

# get summary statistics of experimental data and bonuses
summ_bonus = summaryBonus(dat)
summ_table = summaryStats(dat)


###############------------------MAIN----------------###############


## Figures in the main text
# Figure 6
pl_true = plotPercentageTrue(dat) 
# Figure 7
pl_resptime = plotResponseTime(dat)

## Figures in Appendix
# Figure C3
pl_clarity = plotClarityByTreatment(dat)
# percentage True by task - Figure D2
pl_true_bytask = plotPercentageTrueByTask(dat)
# response time distribution - Figure D3
pl_resptime_distr = plotResponseTimeDistr(dat)

### Response regressions
regr_resp_once_logit = runResponseRegressions(dat,'once')
regr_resp_twice_logit = runResponseRegressions(dat,'twice')

### Response time regressions
regr_time_once = runResponseTimeRegressions(dat,'once')
regr_time_twice = runResponseTimeRegressions(dat,'twice')

### Regression tables

# Main results
regrtable_once_logit = tabulateEstimates(regr_resp_once_logit)
margins_once_logit = getMargins(regr_resp_once_logit)
#margins_once_logit %>% mutate_if(is.numeric, round, digits = 2)
regrstats_once_logit = getRegrStats(regr_resp_once_logit)
#regrstats_once_logit %>% mutate_if(is.numeric, round, digits = 4)
regrtable_respTime_once = tabulateRespTimeEstimates(regr_time_once)

# at least twice analysis
regrtable_twice_logit = tabulateEstimates(regr_resp_twice_logit)
margins_twice_logit = getMargins(regr_resp_twice_logit)
#margins_twice_logit %>% mutate_if(is.numeric, round, digits = 2)
regrstats_twice_logit = getRegrStats(regr_resp_twice_logit)
#regrstats_twice_logit %>% mutate_if(is.numeric, round, digits = 4)
regrtable_respTime_twice = tabulateRespTimeEstimates(regr_time_twice)

