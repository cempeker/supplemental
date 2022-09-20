################## PPM Study 2 analysis

# libraries 
library(reshape2)
library(dplyr)
library(stringr)
library(ggplot2)
library(jcolors)
#library(lmtest)
#library(sandwich)
#library(miceadds)
library(estimatr)
library(mfx)
#library(stargazer)
library(texreg)


################----------------FUNCTIONS--------------##############

readData = function(){
  
  # Read choice data
  dat = read.csv('data/s2_qualtrics.csv', header = TRUE)
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
  dat_choice = dat[,c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.',response_cols)]
  rm(response_cols)
  dat_choice = melt(dat_choice,id.vars = c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.'))
  
  dat_choice$value = as.numeric(dat_choice$value)
  dat_choice = dat_choice[complete.cases(dat_choice),]
  dat_choice$value = (-1)*dat_choice$value + 2
  dat_choice$variable = as.character(dat_choice$variable)
  dat_choice$question = str_sub(dat_choice$variable,2,2)
  
  dat_choice = dat_choice[,c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.','question','value')]
  colnames(dat_choice) = c('PROLIFIC_PID','EndDate','exp_cond','Duration..in.seconds.','question','response')
  
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
  dat_prolific1 = read.csv('data/s2_prolific_week1.csv', header = TRUE)
  dat_prolific2 = read.csv('data/s2_prolific_week2.csv', header = TRUE)
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
  dat3[dat3$exp_cond %in% c('treatmentOnce','treatmentTwice'),'condition'] = 'PPM'
  
  dat3$week = paste0('week ',dat3$week)
  
  return(dat3)
}

# summarize bonuses
summaryBonus = function(dat){
  w1_bonus = read.csv('data/week1_bonuses.csv', header = FALSE)
  colnames(w1_bonus) = c('PROLIFIC_PID','bonus')
  w2_bonus = read.csv('data/week2_bonuses.csv', header = FALSE)
  colnames(w2_bonus) = c('PROLIFIC_PID','bonus')
  dat_bonus = rbind(w1_bonus,w2_bonus)
  rm(w1_bonus,w2_bonus)
  
  dat4 = merge(dat,dat_bonus,by='PROLIFIC_PID')
  #dat4 = dat4[dat4$condition == 'treatment']
  dat5 = as.data.frame(
    dat4 %>%
      group_by(week, condition, threshold) %>%
      summarize(
        avg_bonus = mean(bonus,na.rm=TRUE)
      )
  )
  
  return(dat5)
}

## generate summary statistics
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

## plot percentage true
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
  
  my_palette = as.vector(jcolors(palette = 'pal5')[c(1,2,3)])
  
  pl = ggplot(dat3, aes(x = week, y = perc_true_picks, fill = condition)) + 
    geom_bar(position="dodge", stat='identity',width=0.7) +
    scale_fill_manual(values=my_palette) +
    facet_wrap(vars(threshold)) +
    scale_y_continuous(name = '% of True') +
    theme_bw(base_size = 14)
  
  return(pl)
  
}
plotPercentageTrueByQuestion = function(dat){
  
  # Plot percentage of True vs exp cond for Once and Twice
  
  dat2 = dat[dat$response_time <= 60,]
  
  dat3 = as.data.frame(
    dat2 %>%
      group_by(question,condition,week,threshold) %>%
      summarize(n = n(),true_picks = sum(response))
  )
  
  dat3$perc_true_picks = 100*(dat3$true_picks / dat3$n)
  dat3$threshold = paste0('version: \'at least ',dat3$threshold,'\'')
  
  my_palette = as.vector(jcolors(palette = 'pal5')[c(1,2,3)])
  
  pl = ggplot(dat3, aes(x = question, y = perc_true_picks, fill = condition)) + 
    geom_bar(position="dodge", stat='identity',width=0.7) +
    scale_fill_manual(values=my_palette) +
    facet_grid(week ~ threshold) +
    scale_y_continuous(name = '% of True') +
    theme_bw(base_size = 14)
  
  return(pl)
  
}


## plot response times
plotResponseTime = function(dat){
  # Plot percentage of True vs exp cond for Once and Twice
  dat2 = dat
  dat2$threshold = paste0('version: \'at least ',dat2$threshold,'\'')
  dat3 = dat2[dat2$response_time <= 60,]
  #dat3 = dat2
  
  dat3$response = ifelse(dat3$response == 1, 'True', 'False')
  #dat3$response = paste0('response = ',dat3$response)
  
  my_palette = as.vector(jcolors(palette = 'pal5')[c(1,2,3)])
  
  pl = ggplot(dat3, aes(x = as.factor(response), y = response_time, fill = condition)) + 
    geom_boxplot(width=0.7, outlier.shape = NA) +
    scale_fill_manual(values=my_palette) +
    facet_grid(week ~ threshold) +
    scale_y_continuous(name = 'Response time (in seconds)',breaks = seq(0,14,2)) +
    scale_x_discrete(name = 'Response') +
    coord_cartesian(ylim = c(0,14)) +
    theme_bw(base_size = 14)
  
  return(pl)
  
}
plotResponseTimeBins = function(dat){
  
  dat2 = dat
  dat2$threshold = paste0('version: \'at least ',dat2$threshold,'\'')
  
  dat3 = as.data.frame(
    dat2 %>%
      group_by(condition) %>%
      summarize(
        resp_time_bin = cut(response_time,breaks = c(0,2,4,6,8,seq(10,90,10),300))
      )
  )
  
  dat3 = as.data.frame(
    dat3 %>%
      group_by(condition,resp_time_bin) %>%
      summarize(
        bin_count = n()
      )
  )
  
  my_palette = as.vector(jcolors(palette = 'pal5')[c(1,2,3)])
  
  pl = ggplot(dat3, aes(x = resp_time_bin, y = bin_count,fill = condition)) + 
    geom_bar(position="dodge", stat='identity',width=0.7) +
    scale_fill_manual(values=my_palette) +
    #facet_grid(.~condition) +
    coord_flip() +
    scale_x_discrete(name = 'Response Time') +
    scale_y_continuous(name = 'Number of observations') +
    theme_bw(base_size = 14)
  
  
  dat3 = dat2[dat2$response_time <= 60,]
  dat3$response = ifelse(dat3$response == 1, 'True', 'False')
  d = dat3[dat3$condition=='control2' & dat3$threshold == "version: 'at least once'",]
  d2 = as.data.frame(
    d%>%
      group_by(week,response) %>%
      summarize(
        resp_time_bin = cut(response_time,c(seq(0,30,2),60))
      )
  )
  
  d3 = as.data.frame(
    d2 %>%
      group_by(week,response,resp_time_bin) %>%
      summarize(
        bin_count = n()
      )
  )
  
  d3$response = paste0('Response = ', d3$response)
  
  my_palette2 = as.vector(jcolors(palette = 'pal9')[c(2,4)])
  
  pl2 = ggplot(d3, aes(x = resp_time_bin, y = bin_count,fill = week)) + 
    geom_bar(position="dodge", stat='identity',width=0.7) +
    facet_wrap(vars(response)) +
    scale_fill_manual(values=my_palette2) +
    #facet_grid(.~condition) +
    coord_flip() +
    scale_x_discrete(name = 'Response Time') +
    scale_y_continuous(name = 'Number of observations') +
    theme_bw(base_size = 14)
  
  return(list(pl,pl2))
  
}




plotResponseTimeByQuestion = function(dat){
  # Plot percentage of True vs exp cond for Once and Twice
  dat2 = dat
  dat2$threshold = paste0('version: \'at least ',dat2$threshold,'\'')
  dat3 = dat2[dat2$response_time < 60,]
  #dat3 = dat2
  
  dat3$response = ifelse(dat3$response == 1, 'True', 'False')
  #dat3$response = paste0('response = ',dat3$response)
  
  my_palette = as.vector(jcolors(palette = 'pal5')[c(1,2,3)])
  
  pl = ggplot(dat3, aes(x = as.factor(response), y = response_time, fill = condition)) +
    geom_boxplot(width=0.7, outlier.shape = NA) +
    scale_fill_manual(values=my_palette) +
    facet_grid(question ~ threshold + week) +
    scale_y_continuous(name = 'Response time (in seconds)') +
    scale_x_discrete(name = 'Response') +
    coord_cartesian(ylim = c(0,20)) +
    theme_bw(base_size = 14)
    
  return(pl)
  
}

runResponseRegressions = function(dat, survey_version){
  
  ##### WEEK 1
  dat2 = dat[dat$threshold == survey_version & dat$week == 'week 1' & dat$response_time <= 60,]
  
  # Logit model, no controls
  logit_mod1 = glm(response ~ condition, family = binomial(link = "logit"), data = dat2, na.action = na.omit)
  logit_mod1_mfx = logitmfx(formula = response ~ condition, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  logit_mod1_coef = coeftest(logit_mod1, vcov = vcovCL(logit_mod1,cluster = dat2$PROLIFIC_PID))
  
  # Logit model, with controls
  logit_mod2 = glm(response ~ condition + age + female + UK_citizen, family = binomial(link = "logit"), data = dat2, na.action = na.omit)
  logit_mod2_mfx = logitmfx(formula = response ~ condition + age + female + UK_citizen, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  logit_mod2_coef = coeftest(logit_mod2, vcov = vcovCL(logit_mod2,cluster = dat2$PROLIFIC_PID))
  
  # Probit model, no controls
  probit_mod1 = glm(response ~ condition, family = binomial(link = "probit"), data = dat2, na.action = na.omit)
  probit_mod1_mfx = probitmfx(formula = response ~ condition, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  probit_mod1_coef = coeftest(probit_mod1, vcov = vcovCL(probit_mod1,cluster = dat2$PROLIFIC_PID))
  
  # Probit model, with controls
  probit_mod2 = glm(response ~ condition + age + female + UK_citizen, family = binomial(link = "probit"), data = dat2, na.action = na.omit)
  probit_mod2_mfx = probitmfx(formula = response ~ condition + age + female + UK_citizen, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  probit_mod2_coef = coeftest(probit_mod2, vcov = vcovCL(probit_mod2,cluster = dat2$PROLIFIC_PID))
  
  
  
  # WEEK 2
  dat2 = dat[dat$threshold == survey_version & dat$week == 'week 2' & dat$response_time <= 60,]
  
  # Logit model, no controls
  logit_mod3 = glm(response ~ condition, family = binomial(link = "logit"), data = dat2, na.action = na.omit)
  logit_mod3_mfx = logitmfx(formula = response ~ condition, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  logit_mod3_coef = coeftest(logit_mod3, vcov = vcovCL(logit_mod3,cluster = dat2$PROLIFIC_PID))
  
  # Logit model, with controls
  logit_mod4 = glm(response ~ condition + age + female + UK_citizen, family = binomial(link = "logit"), data = dat2, na.action = na.omit)
  logit_mod4_mfx = logitmfx(formula = response ~ condition + age + female + UK_citizen, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  logit_mod4_coef = coeftest(logit_mod4, vcov = vcovCL(logit_mod4,cluster = dat2$PROLIFIC_PID))
  
  # Probit model, no controls
  probit_mod3 = glm(response ~ condition, family = binomial(link = "probit"), data = dat2, na.action = na.omit)
  probit_mod3_mfx = probitmfx(formula = response ~ condition, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  probit_mod3_coef = coeftest(probit_mod3, vcov = vcovCL(probit_mod3,cluster = dat2$PROLIFIC_PID))
  
  # Probit model, with controls
  probit_mod4 = glm(response ~ condition + age + female + UK_citizen, family = binomial(link = "probit"), data = dat2, na.action = na.omit)
  probit_mod4_mfx = probitmfx(formula = response ~ condition + age + female + UK_citizen, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  probit_mod4_coef = coeftest(probit_mod4, vcov = vcovCL(probit_mod4,cluster = dat2$PROLIFIC_PID))
  
  
  results_list = list(
    
    logit_mod1_mfx,logit_mod1_coef,
    logit_mod2_mfx,logit_mod2_coef,
    logit_mod3_mfx,logit_mod3_coef,
    logit_mod4_mfx,logit_mod4_coef,
    
    probit_mod1_mfx,probit_mod1_coef,
    probit_mod2_mfx,probit_mod2_coef,
    probit_mod3_mfx,probit_mod3_coef,
    probit_mod4_mfx,probit_mod4_coef
  )
  
  return(results_list)
}

runResponseTimeRegressions = function(dat, survey_version){
  
  # WEEK 1
  dat2 = dat[dat$threshold == survey_version & dat$week == 'week 1' & dat$response_time <= 60,]
  
  resp_time_regr1 = lh_robust(formula = response_time ~ condition, data = dat2, clusters = dat2$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditioncontrol2 = 0'))
  resp_time_regr2 = lh_robust(formula = response_time ~ condition + age + female + UK_citizen, data = dat2, clusters = dat2$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditioncontrol2 = 0'))
  
  # WEEK 2
  dat2 = dat[dat$threshold == survey_version & dat$week == 'week 2' & dat$response_time <= 60,]
  
  resp_time_regr3 = lh_robust(formula = response_time ~ condition, data = dat2, clusters = dat2$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditioncontrol2 = 0'))
  resp_time_regr4 = lh_robust(formula = response_time ~ condition + age + female + UK_citizen, data = dat2, clusters = dat2$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditioncontrol2 = 0'))
  
  
  results_list = list(
    resp_time_regr1,
    resp_time_regr2,
    resp_time_regr3,
    resp_time_regr4
  )
  
  return(results_list)
  
}

runResponseTimeRegressionsPooled = function(dat, survey_version){
  
  # Both weeks
  dat2 = dat[dat$threshold == survey_version & dat$response_time <= 60,]
  
  resp_time_regr1 = lh_robust(formula = response_time ~ condition, data = dat2, clusters = dat2$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditioncontrol2 = 0'))
  resp_time_regr2 = lh_robust(formula = response_time ~ condition + age + female + UK_citizen, data = dat2, clusters = dat2$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditioncontrol2 = 0'))
  
  dat2$condition = factor(dat2$condition, levels = c('control2','control1','PPM'))
  
  resp_time_regr3 = lh_robust(formula = response_time ~ condition, data = dat2, clusters = dat2$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditioncontrol1 = 0'))
  resp_time_regr4 = lh_robust(formula = response_time ~ condition + age + female + UK_citizen, data = dat2, clusters = dat2$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = c('conditionPPM - conditioncontrol1 = 0'))
  
  results_list = list(
    resp_time_regr1,
    resp_time_regr2,
    resp_time_regr3,
    resp_time_regr4
  )
  
  return(results_list)
}

tabulateLogitMfx = function(regr_resp){
  
  mod1 = regr_resp[[1]]
  mod2 = regr_resp[[3]]
  mod3 = regr_resp[[5]]
  mod4 = regr_resp[[7]]
  
  regr_results = list(mod1,mod2,mod3,mod4)
  res_table = texreg(regr_results, include.ci = FALSE, stars = c(0.001,0.01, 0.05, 0.1))
  
  return(res_table)
}

tabulateProbitMfx = function(regr_resp){
  
  mod1 = regr_resp[[9]]
  mod2 = regr_resp[[11]]
  mod3 = regr_resp[[13]]
  mod4 = regr_resp[[15]]
  
  regr_results = list(mod1,mod2,mod3,mod4)
  res_table = texreg(regr_results, include.ci = FALSE, stars = c(0.001,0.01, 0.05, 0.1))
  
  return(res_table)
  
}

tabulateLogitProbitEstimates = function(regr_resp){
  
  mod1 = regr_resp[[2]]
  mod2 = regr_resp[[4]]
  mod3 = regr_resp[[6]]
  mod4 = regr_resp[[8]]
  mod5 = regr_resp[[10]]
  mod6 = regr_resp[[12]]
  mod7 = regr_resp[[14]]
  mod8 = regr_resp[[16]]
  
  regr_results = list(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)
  res_table = texreg(regr_results, include.ci = FALSE, stars = c(0.001,0.01, 0.05, 0.1))
  
  return(res_table)
  
}

tabulateRespTimeEstimates = function(regr_time){
  
  mod5 = regr_time[[1]]$lm_robust
  mod6 = regr_time[[2]]$lm_robust
  mod7 = regr_time[[3]]$lm_robust
  mod8 = regr_time[[4]]$lm_robust
  
  regr_results = list(mod5,mod6,mod7,mod8)
  #regr_results = list(mod5,mod6)
  res_table = texreg(regr_results, include.ci = FALSE, stars = c(0.001,0.01, 0.05, 0.1))
  
  return(res_table)
  
}

tabulateRespTimePooledEstimates = function(regr_time){
  
  mod5 = regr_time[[1]]$lm_robust
  mod6 = regr_time[[2]]$lm_robust
  #mod7 = regr_time[[3]]$lm_robust
  #mod8 = regr_time[[4]]$lm_robust
  
  #regr_results = list(mod5,mod6,mod7,mod8)
  regr_results = list(mod5,mod6)
  res_table = texreg(regr_results, include.ci = FALSE, stars = c(0.001,0.01, 0.05, 0.1))
  
  return(res_table)
  
}

tabulateLogitMfxANDRespTimePooled = function(regr_resp,regr_time){
  
  mod1 = regr_resp[[1]]
  mod2 = regr_resp[[3]]
  mod3 = regr_resp[[5]]
  mod4 = regr_resp[[7]]
  mod5 = regr_time[[1]]$lm_robust
  mod6 = regr_time[[2]]$lm_robust
  
  regr_results = list(mod1,mod2,mod3,mod4,mod5,mod6)
  res_table = texreg(regr_results, include.ci = FALSE, stars = c(0.001,0.01, 0.05, 0.1))
  
  return(res_table)
}



################-----------------READ DATA------------###############

dat = readData()
#dat = readData2()

# get summary statistics of experimental data and bonuses
summ_bonus = summaryBonus(dat)
summ_table = summaryStats(dat)


###############------------------MAIN----------------###############


##### Plot percentage True in conditions and response times (figures in the main text)
pl_true = plotPercentageTrue(dat)
pdf(file = "./s2_trueperc.pdf",width=7,height=3.5)
pl_true
dev.off()

pl_resptime = plotResponseTime(dat)
pdf(file = "./s2_resptime.pdf",width=7,height=4)
pl_resptime
dev.off()


### Plot percentage True by question (Appendix)
pl_true_byquestion = plotPercentageTrueByQuestion(dat)
pdf(file = "./s2_trueperc_byquestion.pdf",width=7,height=4)
pl_true_byquestion
dev.off()

##### Plot response time bins (Appendix)
pl_resptime_bins = plotResponseTimeBins(dat)
pdf(file = "./s2_resptime_outliers.pdf",width=7,height=4)
pl_resptime_bins[[1]]
dev.off()
pdf(file = "./s2_resptime_control2.pdf",width=7.5,height=4)
pl_resptime_bins[[2]]
dev.off()

#### Response and response time regressions
regr_resp = runResponseRegressions(dat,'once')
regr_resp_twice = runResponseRegressions(dat,'twice')
regr_time = runResponseTimeRegressions(dat,'once')
regr_time_twice = runResponseTimeRegressions(dat,'twice')
regr_time_pooled = runResponseTimeRegressionsPooled(dat,'once')
regr_time_pooled_twice = runResponseTimeRegressionsPooled(dat,'twice')

#### Tables
regr_logitMfx = tabulateLogitMfx(regr_resp)
regr_probitMfx = tabulateProbitMfx(regr_resp)
regr_logitprobitEst = tabulateLogitProbitEstimates(regr_resp)

regr_respTime = tabulateRespTimeEstimates(regr_time)
regr_respTime_pooled = tabulateRespTimePooledEstimates(regr_time_pooled)

regr_maintext = tabulateLogitMfxANDRespTimePooled(regr_resp,regr_time_pooled)

# twice
regr_maintext_twice = tabulateLogitMfxANDRespTimePooled(regr_resp_twice,regr_time_twice)