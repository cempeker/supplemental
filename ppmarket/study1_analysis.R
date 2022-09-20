################## PPM Study 1 analysis

###### libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(mfx)
library(texreg)
library(xtable)
#library(miceadds)
library(jcolors)
#library(estimatr)
library(stringr)
#library(wmwpow)
#library(lubridate)
#library(lmtest)
#library(sandwich)
#library(stargazer)
#library(readODS)
#library(mfx)
#library(texreg)

################----------------FUNCTIONS--------------##############

## construct data
constructData = function(){
  
  # choice data
  dat_raw = read.csv('data/s1_qualtrics.csv', header = TRUE)
  
  ## Construct effort choice data (1 = effort, 0 = no effort)
  dat_temp = dat_raw[,c('PROLIFIC_PID','exp_cond', 'X1_task','X2_task','X3_task','X4_task', 'X5_task','X6_task','X7_task','X8_task','X9_task', 'X10_task')]
  dat_effort = melt(dat_temp, id.vars = c('PROLIFIC_PID','exp_cond'))
  dat_effort[,'value'] = (-1)*as.numeric(dat_effort[,'value']) + 2
  colnames(dat_effort)[3:4] = c('task','effort')
  levels(dat_effort$task) <- list(
    X1="X1_task",X2="X2_task",X3="X3_task",X4="X4_task",X5="X5_task",
    X6="X6_task",X7="X7_task",X8="X8_task",X9="X9_task",X10="X10_task"
  )
  rm(dat_temp)
  
  ## Construct effort time data
  dat_temp = dat_raw[,c('PROLIFIC_PID','exp_cond', 'X1_zero_count_time_Page.Submit','X2_zero_count_time_Page.Submit','X3_zero_count_time_Page.Submit','X4_zero_count_time_Page.Submit', 'X5_zero_count_time_Page.Submit','X6_zero_count_time_Page.Submit','X7_zero_count_time_Page.Submit','X8_zero_count_time_Page.Submit','X9_zero_count_time_Page.Submit', 'X10_zero_count_time_Page.Submit')]
  dat_time = melt(dat_temp, id.vars = c('PROLIFIC_PID','exp_cond'))
  colnames(dat_time)[3:4] = c('task','effort_time')
  levels(dat_time$task) <- list(
    X1="X1_zero_count_time_Page.Submit", X2="X2_zero_count_time_Page.Submit",
    X3="X3_zero_count_time_Page.Submit", X4="X4_zero_count_time_Page.Submit",
    X5="X5_zero_count_time_Page.Submit", X6="X6_zero_count_time_Page.Submit",
    X7="X7_zero_count_time_Page.Submit", X8="X8_zero_count_time_Page.Submit",
    X9="X9_zero_count_time_Page.Submit", X10="X10_zero_count_time_Page.Submit"
  )
  rm(dat_temp)
  
  ## Construct pick data (1 = yellow-heavy box, 0 = blue-heavy box)
  dat_temp = dat_raw[,c('PROLIFIC_PID','exp_cond', 'X1_pred','X2_pred','X3_pred','X4_pred', 'X5_pred','X6_pred','X7_pred','X8_pred','X9_pred', 'X10_pred')]
  dat_pick = melt(dat_temp, id.vars = c('PROLIFIC_PID','exp_cond'))
  #dat_pick[,'value'] = (-1)*as.numeric(dat_pick[,'value']) + 2
  dat_pick$value = ifelse(dat_pick$value==1,1,0)
  colnames(dat_pick)[3:4] = c('task','pick')
  levels(dat_pick$task) <- list(
    X1="X1_pred",X2="X2_pred",X3="X3_pred",X4="X4_pred",X5="X5_pred",
    X6="X6_pred",X7="X7_pred",X8="X8_pred",X9="X9_pred",X10="X10_pred"
  )
  rm(dat_temp)
  
  ## Construct draw data
  dat_temp = dat_raw[,c('PROLIFIC_PID','exp_cond', 'X1_draw','X2_draw','X3_draw','X4_draw', 'X5_draw','X6_draw','X7_draw','X8_draw','X9_draw', 'X10_draw')]
  dat_draw = melt(dat_temp, id.vars = c('PROLIFIC_PID','exp_cond'))
  dat_draw$value = ifelse(dat_draw$value==1,'yellow','blue')
  #dat_draw[,'value'] = (-1)*as.numeric(dat_pick[,'value']) + 2
  colnames(dat_draw)[3:4] = c('task','draw')
  levels(dat_draw$task) <- list(
    X1="X1_draw",X2="X2_draw",X3="X3_draw",X4="X4_draw",X5="X5_draw",
    X6="X6_draw",X7="X7_draw",X8="X8_draw",X9="X9_draw",X10="X10_draw"
  )
  rm(dat_temp)
  
  ## merge effort, effort time, pick and draw data
  dat = merge(dat_effort,dat_time,by=c('PROLIFIC_PID','exp_cond','task'))
  dat = merge(dat,dat_draw,by=c('PROLIFIC_PID','exp_cond','task'))
  dat = merge(dat,dat_pick,by=c('PROLIFIC_PID','exp_cond','task'))
  
  ## Get participant data from PROLIFIC and merge. Used to recover IDs, ages, gender
  dat_prol = read.csv('data/s1_prolific.csv', header = TRUE)
  dat_prol = dat_prol[dat_prol$status == 'APPROVED',c('participant_id','age','Current.Country.of.Residence','Sex')]
  colnames(dat_prol) = c('PROLIFIC_PID','age','Current.Country.of.Residence','gender')
  dat_prol$gender = ifelse(dat_prol$gender == 'Female', 1,0)
  dat_prol$US_resident = ifelse(dat_prol$Current.Country.of.Residence == 'United States',1,0)
  dat_prol$Current.Country.of.Residence = NULL
  dat = merge(dat, dat_prol,by ='PROLIFIC_PID')
  rm(dat_prol)
  
  # add duration in seconds
  dat_dur = unique(dat_raw[,c('PROLIFIC_PID','Duration..in.seconds.')])
  dat = merge(dat,dat_dur,by='PROLIFIC_PID')
  rm(dat_dur)
  
  # get quiz response data and merge
  dat_temp = dat_raw[,c('PROLIFIC_PID','exp_cond','quiz_peerincent','quiz_flat','quiz_accincent','QF.4','QF.5')]
  colnames(dat_temp) = c('PROLIFIC_PID','exp_cond','quiz_peerincent','quiz_flat','quiz_accincent','clarity','post_quiz')
  
  dat_temp$quiz = NA
  dat_temp[dat_temp$exp_cond == 'peerincent','quiz'] = dat_temp[dat_temp$exp_cond == 'peerincent','quiz_peerincent']
  dat_temp[dat_temp$exp_cond == 'accincent','quiz'] = dat_temp[dat_temp$exp_cond == 'accincent','quiz_accincent']
  dat_temp[dat_temp$exp_cond == 'flat','quiz'] = dat_temp[dat_temp$exp_cond == 'flat','quiz_flat']
  
  dat_temp = subset(dat_temp, select = -c(quiz_peerincent,quiz_flat, quiz_accincent))
  
  dat_temp$quiz_correct = ifelse(
    dat_temp$exp_cond == 'flat',
    as.numeric(dat_temp$quiz == 1),
    ifelse(
      dat_temp$exp_cond == 'accincent',
      as.numeric(dat_temp$quiz == 2),
      ifelse(
        dat_temp$exp_cond == 'peerincent',
        as.numeric(dat_temp$quiz == 3),
        NA
      )
    )
  )
  
  dat_temp$post_quiz_correct = ifelse(
    dat_temp$exp_cond == 'flat',
    as.numeric(dat_temp$post_quiz == 1),
    ifelse(
      dat_temp$exp_cond == 'accincent',
      as.numeric(dat_temp$post_quiz == 2),
      ifelse(
        dat_temp$exp_cond == 'peerincent',
        as.numeric(dat_temp$post_quiz == 3),
        NA
      )
    )
  )
  
  dat = merge(dat,dat_temp,by=c('PROLIFIC_PID','exp_cond'))
  rm(dat_temp)
  
  colnames(dat) = c('PROLIFIC_PID','condition','task','effort','effort_time','draw','pick','age','female','US_resident','duration','clarity','post_quiz','quiz','quiz_correct','post_quiz_correct')
  levels(dat$condition) <- list(
    Accuracy="accincent",
    PPM="peerincent",
    Flat="flat"
  )
  dat$condition = factor(dat$condition, levels = c('Flat','Accuracy','PPM'))
  
  return(dat)
}

## generate summary statistics
summaryStats = function(dat){
  dat2 = unique(dat[,c('PROLIFIC_PID','condition','age','female','US_resident','duration','clarity','quiz_correct','post_quiz_correct')])
  
  dat3 = as.data.frame(
    dat2 %>%
      group_by(condition) %>%
      summarize(
        avg_age = mean(age, na.rm=TRUE),
        num_US_resident = sum(US_resident, na.rm=TRUE),
        num_nonUS_resident= n()-num_US_resident,
        avg_duration = mean(duration, na.rm=TRUE),
        num_female = sum(female, na.rm=TRUE),
        num_male = n()-num_female,
        num_instr_unclear =  sum(length(clarity[clarity > 3]), na.rm=TRUE),
        num_instr_clear =  n()-num_instr_unclear,
        num_quiz_correct = sum(quiz_correct, na.rm=TRUE),
        num_quiz_incorrect = n() - num_quiz_correct,
        num_post_quiz_correct = sum(post_quiz_correct, na.rm=TRUE),
        num_post_quiz_incorrect = n() - num_post_quiz_correct
      )
  )
  
  return(dat3)
}

## plot effort task completion
plotEffortCompletion = function(dat){
  
  dat2 = as.data.frame(
    dat %>%
      group_by(condition,task) %>%
      summarize(
        avg_effort = mean(effort)
      )
  )
  
  my_palette = as.vector(jcolors(palette = 'pal8')[c(8,9,10)])
  
  pl = ggplot(dat2, aes(x = task, y = avg_effort, fill = condition)) + 
    geom_bar(position="dodge", stat='identity',width=0.7) +
    scale_fill_manual(name = 'Condition',values=my_palette) +
    scale_x_discrete(name='Task', labels = 1:10) +
    scale_y_continuous(name = 'Proportion',breaks = c(0,0.2,0.4,0.6,0.8,1)) +
    theme_bw(base_size = 14)
  
  return(pl)
}

## plot pick vs draw type
plotPickVsDraw = function(dat){

  dat2 = dat
  dat2$signal = ifelse(dat2$effort==0,'no effort',paste0('draw: ',dat2$draw))
  dat3 = as.data.frame(
    dat2 %>%
      group_by(condition,signal) %>%
      summarize(
        prop_left = mean(pick)
      )
  )
  
  my_palette = as.vector(jcolors(palette = 'pal8')[c(8,9,10)])
  
  pl = ggplot(dat3, aes(x = signal, y = prop_left, fill = condition)) + 
    geom_bar(position="dodge", stat='identity',width=0.7) +
    #facet_wrap(vars(signal)) +
    scale_fill_manual(name = 'Condition',values=my_palette) +
    scale_x_discrete(name='Subject\'s effort and draw') +
    scale_y_continuous(name = 'Proportion of left-box picks',breaks = c(0,0.2,0.4,0.6,0.8,1)) +
    theme_bw(base_size = 14)
  
  return(pl)
}


## plot pick vs draw type (effort only) by task
plotPickVsDrawByTask = function(dat){
  
  dat2 = dat
  dat2$signal = ifelse(dat2$effort==0,'no draw',paste0('draw: ',dat2$draw))
  #dat2 = dat2[dat2$effort == 1,]
  dat3 = as.data.frame(
    dat2 %>%
      group_by(condition,signal,task) %>%
      summarize(
        left = sum(pick),
        right = n() - left 
      )
  )
  
  dat4 = melt(dat3, id.vars = c('condition','signal','task'), variable.name = 'pick', value.name = 'total')
  #dat4$signal = factor(dat4$signal, levels=c('draw: yellow','draw: blue'))
  dat4$signal = factor(dat4$signal, levels=c('draw: yellow','draw: blue','no draw'))
  
  my_palette = as.vector(jcolors(palette = 'pal2')[c(2,1)])
  
  pl = ggplot(dat4, aes(x = task, y = total, fill = pick)) + 
    geom_bar(position=position_stack(reverse=TRUE), stat='identity',width=0.7) +
    facet_grid(condition ~ signal) +
    scale_fill_manual(name = 'Pick',values=my_palette) +
    scale_x_discrete(name='Task', labels = 1:10) +
    scale_y_continuous(name = 'Total number of picks') +
    theme_bw(base_size = 14) #+
    #theme(legend.position = 'none')
  
  return(pl)
}

## plot pick (percentage of all) vs draw type (effort only) by task
plotPickPercVsDrawByTask = function(dat){
  
  dat2 = dat
  dat2$signal = ifelse(dat2$effort==0,'no draw',paste0('draw: ',dat2$draw))
  #dat2 = dat2[dat2$effort == 1,]
  dat3 = as.data.frame(
    dat2 %>%
      group_by(condition,signal,task) %>%
      summarize(
        left = 100*sum(pick) / n(),
        right = 100*(n() - sum(pick)) / n() 
      )
  )
  
  # prior expectations by task 
  dat_prior = data.frame(paste0('X',1:10),c(30,35,40,45,50,50,55,60,65,70))
  colnames(dat_prior) = c('task','prior (yellow)')
  dat_prior = melt(dat_prior,id.vars = 'task')
  
  # construct
  dat4 = melt(dat3, id.vars = c('condition','signal','task'), variable.name = 'pick', value.name = 'total')
  dat5 = dat4
  #dat5 = merge(dat4, dat_prior, by='task')
  #dat5$signal = factor(dat5$signal, levels=c('draw: yellow','draw: blue'))
  dat5$signal = factor(dat5$signal, levels=c('draw: yellow','draw: blue','no draw'))
  
  my_palette = as.vector(jcolors(palette = 'pal2')[c(2,1)])
  
  pl = ggplot() + 
    geom_bar(data = dat5, mapping = aes(x = task, y = total, fill = pick), position=position_stack(reverse=TRUE), stat='identity',width=0.7) +
    geom_point(data = dat_prior, mapping = aes(x = task, y = value, color = str_wrap(variable,10))) + 
    facet_grid(condition ~ signal) +
    scale_fill_manual(name = 'Pick',values=my_palette) +
    scale_colour_manual(name='',values = 'black') +
    scale_x_discrete(name='Task', labels = 1:10) +
    scale_y_continuous(name = 'Percentage of picks') +
    theme_bw(base_size = 14) +
    guides(fill = guide_legend(order = 1),coolur = guide_legend(order = 2)) #+
  #theme(legend.position = 'none')
  
  return(pl)
}

## Tabulate pick and prior correlation
tabulatePicksAndPriors = function(dat){
  
  dat2 = dat
  dat2$signal = ifelse(dat2$effort==0,'no draw',paste0(dat$draw))
  
  dat3 = as.data.frame(
    dat2 %>%
      group_by(condition,signal,task) %>%
      summarize(
        left = 100*sum(pick) / n(),
        right = 100*(n() - sum(pick)) / n() 
      )
  )
  
  # prior expectations by task 
  dat_prior = data.frame(paste0('X',1:10),c(30,35,40,45,50,50,55,60,65,70))
  colnames(dat_prior) = c('task','prior (yellow)')
  dat_prior = melt(dat_prior,id.vars = 'task')
  
  # construct
  dat4 = melt(dat3, id.vars = c('condition','signal','task'), variable.name = 'pick', value.name = 'total')
  dat5 = dat4
  #dat5 = merge(dat4, dat_prior, by='task')
  #dat5$signal = factor(dat5$signal, levels=c('draw: yellow','draw: blue'))
  dat5$signal = factor(dat5$signal, levels=c('yellow','blue','no draw'))
  
  dat6 = dat5[dat5$pick == 'left',]
  dat6 = merge(dat6,dat_prior[,c('task','value')],by='task')
  colnames(dat6) = c('task','condition','signal','pick','perc_yellow','prior')
  
  dat7 = as.data.frame(
    dat6 %>%
      group_by(condition,signal) %>%
      summarize(
        pearson_est = cor.test(perc_yellow,prior,method = 'pearson')$estimate,
        pearson_pval = cor.test(perc_yellow,prior,method = 'pearson')$p.value,
        spearman_est = cor.test(perc_yellow,prior,method = 'spearman')$estimate,
        spearman_pval = cor.test(perc_yellow,prior,method = 'spearman')$p.value,
        t_stat = t.test(perc_yellow,prior)$statistic,
        t_pval = t.test(perc_yellow,prior)$p.value,
        w_stat = wilcox.test(perc_yellow,prior)$statistic,
        w_pval = wilcox.test(perc_yellow,prior)$p.value,
      )
  )
  
  dat7$pearson_pval_corrected = p.adjust(dat7$pearson_pval,method = 'fdr')
  dat7$spearman_pval_corrected = p.adjust(dat7$spearman_pval,method = 'fdr')
  dat7$t_pval_corrected = p.adjust(dat7$t_pval,method = 'fdr')
  dat7$w_pval_corrected = p.adjust(dat7$w_pval,method = 'fdr')
  
  #dat8 = melt(dat7,id.vars = c('condition','signal'),variable.name = 'stat_type')
  
  #dat7_p = dat8[dat8$stat_type %in% c('pearson_pval','spearman_pval','t_pval','w_pval'),]
  
  dat7$pearson_results = paste0('r = ',round(dat7$pearson_est,2),', p = ',round(dat7$pearson_pval,3),', q = ',round(dat7$pearson_pval_corrected,3))
  dat7$spearman_results = paste0('rho = ',round(dat7$spearman_est,2),', p = ',round(dat7$spearman_pval,3),', q = ',round(dat7$spearman_pval_corrected,3))
  dat7$t_results = paste0('t = ',round(dat7$t_stat,2),', p = ',round(dat7$t_pval,3),', q = ',round(dat7$t_pval_corrected,3))
  dat7$w_results = paste0('W = ',round(dat7$w_stat,2),', p = ',round(dat7$w_pval,3),', q = ',round(dat7$w_pval_corrected,3))
  
  dat_corr = dat7[,c('condition','signal','pearson_results','spearman_results')]
  colnames(dat_corr) = c('Condition','Draw',"Pearson's C.C.","Spearman's C.C.")
  
  dat_tests = dat7[,c('condition','signal','t_results','w_results')]
  colnames(dat_tests) = c('Condition','Draw',"T-test","Wilcoxon test")
  
  list_tables = list(
    print(xtable(dat_corr),include.rownames=FALSE),
    print(xtable(dat_tests),include.rownames=FALSE)
  )
  
  return(list_tables)
}

## Effort regression
runEffortRegressions = function(dat, benchmark){
  
  ### set benchmark group
  if(benchmark == 'Flat'){
    dat$condition = factor(dat$condition, levels = c('Flat','Accuracy','PPM'))
  } else if (benchmark == 'Accuracy'){
    dat$condition = factor(dat$condition, levels = c('Accuracy','Flat','PPM'))
  } else if (benchmark == 'PPM'){
    dat$condition = factor(dat$condition, levels = c('PPM','Flat','Accuracy'))
  }
  
  ##### Estimate logit models
  
  # whole sample
  logit_mod1 = glm(effort ~ condition, family = binomial(link = "logit"), data = dat, na.action = na.omit)
  logit_mod1_mfx = logitmfx(formula = effort ~ condition, data = dat, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  logit_mod1_coef = coeftest(logit_mod1, vcov = vcovCL(logit_mod1,cluster = dat$PROLIFIC_PID))
  
  logit_mod2 = glm(effort ~ condition + age + female + US_resident, family = binomial(link = "logit"), data = dat, na.action = na.omit)
  logit_mod2_mfx = logitmfx(formula = effort ~ condition + age + female + US_resident, data = dat, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  logit_mod2_coef = coeftest(logit_mod2, vcov = vcovCL(logit_mod2,cluster = dat$PROLIFIC_PID))
  
  # filtered sample
  dat2 = dat[dat$post_quiz_correct == 1,]
  
  logit_mod3 = glm(effort ~ condition, family = binomial(link = "logit"), data = dat2, na.action = na.omit)
  logit_mod3_mfx = logitmfx(formula = effort ~ condition, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  logit_mod3_coef = coeftest(logit_mod3, vcov = vcovCL(logit_mod3,cluster = dat2$PROLIFIC_PID))
  
  logit_mod4 = glm(effort ~ condition + age + female + US_resident, family = binomial(link = "logit"), data = dat2, na.action = na.omit)
  logit_mod4_mfx = logitmfx(formula = effort ~ condition + age + female + US_resident, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  logit_mod4_coef = coeftest(logit_mod4, vcov = vcovCL(logit_mod4,cluster = dat2$PROLIFIC_PID))
  
  
  
  
  ##### Estimate probit models
  
  # whole sample
  probit_mod1 = glm(effort ~ condition, family = binomial(link = "probit"), data = dat, na.action = na.omit)
  probit_mod1_mfx = probitmfx(formula = effort ~ condition, data = dat, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  probit_mod1_coef = coeftest(probit_mod1, vcov = vcovCL(probit_mod1,cluster = dat$PROLIFIC_PID))
  
  probit_mod2 = glm(effort ~ condition + age + female + US_resident, family = binomial(link = "probit"), data = dat, na.action = na.omit)
  probit_mod2_mfx = probitmfx(formula = effort ~ condition + age + female + US_resident, data = dat, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  probit_mod2_coef = coeftest(probit_mod2, vcov = vcovCL(probit_mod2,cluster = dat$PROLIFIC_PID))
  
  # filtered sample
  dat2 = dat[dat$post_quiz_correct == 1,]
  
  probit_mod3 = glm(effort ~ condition, family = binomial(link = "probit"), data = dat2, na.action = na.omit)
  probit_mod3_mfx = probitmfx(formula = effort ~ condition, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  probit_mod3_coef = coeftest(probit_mod3, vcov = vcovCL(probit_mod3,cluster = dat2$PROLIFIC_PID))
  
  probit_mod4 = glm(effort ~ condition + age + female + US_resident, family = binomial(link = "probit"), data = dat2, na.action = na.omit)
  probit_mod4_mfx = probitmfx(formula = effort ~ condition + age + female + US_resident, data = dat2, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
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

## tabulate regressions
tabulateMfxLogit = function(results_list){
  results_tab = texreg(results_list[c(1,3,5,7)], stars = c(0.001,0.01, 0.05, 0.1))
  return(results_tab)
}
tabulateMfxProbit = function(results_list){
  results_tab = texreg(results_list[c(9,11,13,15)], stars = c(0.001,0.01, 0.05, 0.1))
  return(results_tab)
}
tabulateEst = function(results_list){
  results_tab = texreg(results_list[c(2,4,6,8,10,12,14,16)], stars = c(0.001,0.01, 0.05, 0.1))
  return(results_tab)
}


################-----------------READ DATA------------###############

dat = constructData()
summ_table = summaryStats(dat)

###############------------------MAIN----------------###############

effort_plot = plotEffortCompletion(dat)
pdf(file = "./s1_effort.pdf",width=6.4,height=3.5)
effort_plot
dev.off()

pickvsdraw_plot = plotPickVsDraw(dat)
#pickvsdrawbytask_plot = plotPickVsDrawByTask(dat)
pickvsdrawbytask_plot = plotPickPercVsDrawByTask(dat)
pdf(file = "./s1_pick.pdf",width=7.5,height=4)
pickvsdrawbytask_plot
dev.off()
  
regr_results1 = runEffortRegressions(dat, 'Flat')
regr_results2 = runEffortRegressions(dat, 'Accuracy')
regr_results3 = runEffortRegressions(dat, 'PPM')

regr_tables1.1 = tabulateMfxLogit(regr_results1)
regr_tables1.2 = tabulateMfxProbit(regr_results1)
regr_tables1.3 = tabulateEst(regr_results1)

regr_tables2.1 = tabulateMfxLogit(regr_results2)
regr_tables2.2 = tabulateMfxProbit(regr_results2)
regr_tables2.3 = tabulateEst(regr_results2)

regr_tables3.1 = tabulateMfxLogit(regr_results3)
regr_tables3.2 = tabulateMfxProbit(regr_results3)
regr_tables3.3 = tabulateEst(regr_results3)
