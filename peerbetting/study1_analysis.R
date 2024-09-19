################## PPM Study 1 analysis

###### libraries
library(reshape2)
#library(dplyr)
#library(stringr)
library(tidyverse)
library(ggplot2)
#library(jcolors)
library(RColorBrewer)
library(lmtest)
library(sandwich)
library(estimatr)
library(margins)
library(texreg)
#library(mfx)
library(xtable)


################----------------FUNCTIONS--------------##############

# construct data
constructData = function(){
  
  # choice data
  dat_raw = read.csv('data/rawdata/s1_qualtrics.csv', header = TRUE)
  
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
  dat_prol = read.csv('data/rawdata/s1_prolific.csv', header = TRUE)
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
  dat_temp = dat_raw[,c('PROLIFIC_PID','exp_cond','quiz_peerincent','quiz_flat','quiz_accincent','QF.3.2','QF.4','QF.5')]
  colnames(dat_temp) = c('PROLIFIC_PID','exp_cond','quiz_peerincent','quiz_flat','quiz_accincent','training','clarity','post_quiz')
  
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
  
  colnames(dat) = c('PROLIFIC_PID','condition','task','effort','effort_time','draw','pick','age','female','US_resident','duration','training','clarity','post_quiz','quiz','quiz_correct','post_quiz_correct')
  dat$condition = as.factor(dat$condition)
  levels(dat$condition) = list(Flat = 'flat', Accuracy = 'accincent', Peerbet = 'peerincent')
  #dat$condition = factor(dat$condition, levels = c('Flat','Accuracy','PPM'))
  
  # read reward data and merge
  dat_rewards = read.csv('data/rawdata/s1_rewards.csv', header = TRUE)
  dat_rewards$exp_cond = as.factor(dat_rewards$exp_cond)
  levels(dat_rewards$exp_cond) = list(Flat = 'flat', Accuracy = 'accincent', Peerbet = 'peerincent')
  
  dat_rewards_summ = as.data.frame(
    dat_rewards %>%
      group_by(exp_cond) %>%
      summarize(
        min_rewad = min(total_reward),
        avg_reward = mean(total_reward),
        max_reward = max(total_reward)
      )
  )
  colnames(dat_rewards_summ) = c('condition','min_reward','avg_reward','max_reward')
  dat = merge(dat,dat_rewards_summ,by='condition')
  
  # recalibrate clarity variable, 5: highest, 1:lowest
  dat$clarity = (-1)*dat$clarity + 6
  
  # statistical training variable
  #d = dat
  dat$training = as.factor(dat$training)
  dat$training = recode(dat$training, `1` = 'no training', `2` = 'high-school level', `3` = "university level", `4`="research level", `5`="vocational training")
  
  # prior on yellow in each task (numerical)
  dat$prior_on_yellow = NA
  dat[dat$task == 'X1','prior_on_yellow'] = 30
  dat[dat$task == 'X2','prior_on_yellow'] = 35
  dat[dat$task == 'X3','prior_on_yellow'] = 40
  dat[dat$task == 'X4','prior_on_yellow'] = 45
  dat[dat$task == 'X5','prior_on_yellow'] = 50
  dat[dat$task == 'X6','prior_on_yellow'] = 50
  dat[dat$task == 'X7','prior_on_yellow'] = 55
  dat[dat$task == 'X8','prior_on_yellow'] = 60
  dat[dat$task == 'X9','prior_on_yellow'] = 65
  dat[dat$task == 'X10','prior_on_yellow'] = 70
  dat$prior_on_yellow_vs_50 = abs(dat$prior_on_yellow - 50)
  
  # order of task (numerical)
  dat = as.data.frame(
    dat %>%
      group_by(PROLIFIC_PID) %>%
      mutate(
        task_order = 1:10
      )
  )
  
  # append responses to "How clear were the instructions in this experiment"
  #dat_temp = dat_raw[,c('PROLIFIC_PID','QF.4')]
  #colnames(dat_temp) = c('PROLIFIC_PID','Clarity')
  #dat_temp$Clarity = recode(dat_temp$Clarity, `1` = 'Very clear', `2` = 'Mostly clear', `3` = "Understandable, but not very clear", `4`="Mostly unclear", `5`="Very Unclear")
  #dat = merge(dat,dat_temp, by='PROLIFIC_PID')
  
  return(dat)
}

# generate summary statistics
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

## Figures in the main text
# Figure 3 - effort task completion
plotEffortCompletion = function(dat){
  
  dat2 = as.data.frame(
    dat %>%
      group_by(condition,task) %>%
      summarize(
        avg_effort = mean(effort)
      )
  )
  
  #my_palette = as.vector(jcolors(palette = 'pal8')[c(8,9,10)])
  my_palette = c( brewer.pal(9,"YlOrBr")[8], brewer.pal(3,"YlOrBr")[2], brewer.pal(9,"Blues")[8] )
  
  pl = ggplot(dat2, aes(x = task, y = avg_effort, fill = condition)) + 
    geom_bar(position="dodge", stat='identity',width=0.7) +
    scale_fill_manual(name = 'Condition',values=my_palette) +
    scale_x_discrete(name='Task', labels = 1:10) +
    scale_y_continuous(name = 'Proportion',breaks = c(0,0.2,0.4,0.6,0.8,1)) +
    theme_bw(base_size = 14)
  
  ggsave(filename = "./s1_effort.pdf", plot = pl, width=6.4, height=3.5, device=cairo_pdf)
  
  return(pl)
}
# Figure 4 - participants' picks in each task for each draw type and treatment
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
  
  #my_palette = as.vector(jcolors(palette = 'pal2')[c(2,1)])
  my_palette = c( brewer.pal(6,"Set1")[6], brewer.pal(6,"Set1")[2] )
  
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
  
  ggsave(filename = "./s1_pick.pdf", plot = pl, width=7.5, height=4, device=cairo_pdf)
  
  return(pl)
}

## Figures in Appendix
# Figure C1 - "How clear were the instructions in this experiment" by treatment
plotClarityByTreatment = function(dat){
  
  #dat$clarity = (-1)*dat$clarity + 6
  dat$clarity = factor(dat$clarity)
  dat2 = unique(dat[,c('PROLIFIC_PID','condition','clarity')])
  
  dat2 = as.data.frame(
    dat2 %>%
      group_by(condition,clarity, .drop = FALSE) %>%
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
  
  #my_palette = as.vector(jcolors(palette = 'pal8')[c(8,9,10)])
  my_palette = c( brewer.pal(9,"YlOrBr")[8], brewer.pal(3,"YlOrBr")[2], brewer.pal(9,"Blues")[8] )
  
  pl = ggplot(dat3, aes(x = clarity, y = freq, fill = condition)) + 
    geom_bar(position="dodge", stat='identity',width=0.8) +
    #geom_label(aes(label = freq, color=condition), position = position_dodge()) +
    geom_text(data = dat3,aes(label=freq),position = position_dodge(0.9),vjust=-0.5) +
    scale_fill_manual(name = 'Condition',values=my_palette) +
    scale_x_discrete(name='Self-reported clarity of the instructions (5:Very clear, 1:Very unclear)', labels = 1:5) +
    #scale_y_continuous(name = 'Proportion',breaks = c(0,0.2,0.4,0.6,0.8,1)) +
    scale_y_continuous(name = 'Number of subjects', limits=c(0,35)) +
    theme_bw(base_size = 14)
  
  ggsave(filename = "./s1_clarity.pdf", plot = pl, width=7, height=3.5, device=cairo_pdf)
  
  return(pl)
  
}
# Figure C2 - "Did you receive a training in statistics?" by treatment
plotStatTrainingByTreatment = function(dat){
  
  #dat$clarity = (-1)*dat$clarity + 6
  #dat$clarity = factor(dat$clarity)
  dat2 = unique(dat[,c('PROLIFIC_PID','condition','training')])
  
  dat2 = as.data.frame(
    dat2 %>%
      group_by(condition,training, .drop = FALSE) %>%
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
  
  #my_palette = as.vector(jcolors(palette = 'pal8')[c(8,9,10)])
  my_palette = c( brewer.pal(9,"YlOrBr")[8], brewer.pal(3,"YlOrBr")[2], brewer.pal(9,"Blues")[8] )
  
  pl = ggplot(dat3, aes(x = training, y = freq, fill = condition)) + 
    geom_bar(position="dodge", stat='identity',width=0.8) +
    #geom_label(aes(label = freq, color=condition), position = position_dodge()) +
    geom_text(data = dat3,aes(label=freq),position = position_dodge(0.9),vjust=-0.5) +
    scale_fill_manual(name = 'Condition',values=my_palette) +
    scale_x_discrete(name='Self-reported training in statistics') +
    #scale_y_continuous(name = 'Proportion',breaks = c(0,0.2,0.4,0.6,0.8,1)) +
    scale_y_continuous(name = 'Number of subjects', limits=c(0,35)) +
    theme_bw(base_size = 14) +
    theme(
      axis.text.x = element_text(angle=30,hjust=1)
    )
  
  ggsave(filename = "./s1_training.pdf", plot = pl, width=7, height=3.5, device=cairo_pdf)
  
  return(pl)
  
}

## Table D1 - Tabulate pick and prior correlation
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
        pearson_est = cor.test(perc_yellow,prior,method = 'pearson',exact = FALSE)$estimate,
        pearson_pval = cor.test(perc_yellow,prior,method = 'pearson',exact = FALSE)$p.value,
        spearman_est = cor.test(perc_yellow,prior,method = 'spearman',exact = FALSE)$estimate,
        spearman_pval = cor.test(perc_yellow,prior,method = 'spearman',exact = FALSE)$p.value,
        t_stat = t.test(perc_yellow,prior)$statistic,
        t_pval = t.test(perc_yellow,prior)$p.value,
        w_stat = wilcox.test(perc_yellow,prior,exact = FALSE)$statistic,
        w_pval = wilcox.test(perc_yellow,prior,exact = FALSE)$p.value,
      )
  )
  
  #dat7$pearson_pval_corrected = p.adjust(dat7$pearson_pval,method = 'fdr')
  #dat7$spearman_pval_corrected = p.adjust(dat7$spearman_pval,method = 'fdr')
  #dat7$t_pval_corrected = p.adjust(dat7$t_pval,method = 'fdr')
  #dat7$w_pval_corrected = p.adjust(dat7$w_pval,method = 'fdr')
  
  #dat8 = melt(dat7,id.vars = c('condition','signal'),variable.name = 'stat_type')
  
  #dat7_p = dat8[dat8$stat_type %in% c('pearson_pval','spearman_pval','t_pval','w_pval'),]
  
  #dat7$pearson_results = paste0('r = ',round(dat7$pearson_est,2),', p = ',round(dat7$pearson_pval,3),', q = ',round(dat7$pearson_pval_corrected,3))
  #dat7$spearman_results = paste0('rho = ',round(dat7$spearman_est,2),', p = ',round(dat7$spearman_pval,3),', q = ',round(dat7$spearman_pval_corrected,3))
  #dat7$t_results = paste0('t = ',round(dat7$t_stat,2),', p = ',round(dat7$t_pval,3),', q = ',round(dat7$t_pval_corrected,3))
  #dat7$w_results = paste0('W = ',round(dat7$w_stat,2),', p = ',round(dat7$w_pval,3),', q = ',round(dat7$w_pval_corrected,3))
  
  dat7$pearson_results = paste0('r = ',round(dat7$pearson_est,2),', p = ',round(dat7$pearson_pval,3))
  dat7$spearman_results = paste0('rho = ',round(dat7$spearman_est,2),', p = ',round(dat7$spearman_pval,3))
  dat7$t_results = paste0('t = ',round(dat7$t_stat,2),', p = ',round(dat7$t_pval,3))
  dat7$w_results = paste0('W = ',round(dat7$w_stat,2),', p = ',round(dat7$w_pval,3))
  
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

## Plot effort for different levels of E[bonus | pick = accurate]
runBootstrap = function(i,dat2){
  
  d = as.data.frame(
    dat2 %>% 
      group_by(Ebonus) %>% 
      sample_n(n(), replace = TRUE) 
  )
  
  d2 = as.data.frame(
    d %>%
      group_by(Ebonus) %>%
      summarize(
        perc_effort = mean(effort)
      )
  )
  
  d2$iteration = i
  return(d2)
  
}
plotEffortAndEbonus = function(dat){
  
  d_bonus = data.frame(sort(unique(dat$task)),c(15,17.5,20,22.5,25,25,22.5,20,17.5,15))
  colnames(d_bonus) = c('task','Ebonus')
  dat2 = merge(dat,d_bonus,by='task')
  
  dat2 = dat2[dat2$condition == 'Peerbet',]
  dat3 = as.data.frame(
    dat2 %>%
      group_by(Ebonus) %>%
      summarize(
        perc_effort = mean(effort)
      )
  )
  
  iter = 1:1000
  d = lapply(iter, runBootstrap, dat = dat2)
  d = bind_rows(d)
  
  d2 = as.data.frame(
    d %>%
      group_by(Ebonus) %>%
      summarize(
        perc_effort_sd = sd(perc_effort)
      )
  )
  
  dat4 = merge(dat3,d2,by='Ebonus')
  dat4$lb = dat4$perc_effort - 2*dat4$perc_effort_sd
  dat4$ub = dat4$perc_effort + 2*dat4$perc_effort_sd
  
  pl = ggplot(dat4, aes(x=as.factor(Ebonus),y=perc_effort)) +
    geom_bar(stat='identity',position = 'dodge', width=0.6,color='black',fill='lightgreen') +
    geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge(0.9),width=0.3) +
    #scale_fill_manual(values=my_palette) +
    scale_x_discrete(name = "Expected bonus from an accurate pick, Peerbet condition") +
    scale_y_continuous(name = 'Proportion (effort)',limit = c(0,1),breaks = seq(0,1,by=0.1),labels = seq(0,1,by=0.1)) +
    theme_bw(base_size = 14)
  
  ggsave(filename = "./s1_effbonus.pdf", plot = pl, width=6.4, height=3.5, device=cairo_pdf)
  
  #pdf(file = "./s1_effbonus.pdf",width=6.4,height=3.5)
  #effortvsEbonus_plot
  #dev.off()
  
  return(pl)
}

## Effort regression
runEffortRegressions = function(dat, benchmark){
  
  ### set benchmark group
  if(benchmark == 'Flat'){
    dat$condition = factor(dat$condition, levels = c('Flat','Accuracy','Peerbet'))
  } else if (benchmark == 'Accuracy'){
    dat$condition = factor(dat$condition, levels = c('Accuracy','Flat','Peerbet'))
  } else if (benchmark == 'Peerbet'){
    dat$condition = factor(dat$condition, levels = c('Peerbet','Flat','Accuracy'))
  }
  
  #d_bonus = data.frame(sort(unique(dat$task)),c(15,17.5,20,22.5,25,25,22.5,20,17.5,15))
  #colnames(d_bonus) = c('task','Ebonus')
  #dat = merge(dat,d_bonus,by='task')
  
  ##### Estimate models
  
  # whole sample
  mod1 = glm(effort ~ condition, family = binomial(link = "logit"), data = dat, na.action = na.omit)
  #mod1_mfx = logitmfx(formula = effort ~ condition, data = dat, atmean = TRUE, clustervar1 ='PROLIFIC_PID')
  mod1_mfx = margins_summary(mod1, data = dat, vcov = vcovCL(mod1,cluster = dat$PROLIFIC_PID))
  mod1_coef = coeftest(mod1, vcov = vcovCL(mod1,cluster = dat$PROLIFIC_PID))
  mod1_stats = data.frame(
    number_of_obs = nobs(mod1),
    `Likl. Ratio` = with(mod1, null.deviance - deviance),
    `Pr(>Chisq)` = with(mod1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),
    AIC = mod1$aic
  )
  
  mod2 = glm(effort ~ condition + age + female + US_resident, family = binomial(link = "logit"), data = dat, na.action = na.omit)
  mod2_mfx = margins_summary(mod2, data = dat, vcov = vcovCL(mod2,cluster = dat$PROLIFIC_PID))
  mod2_coef = coeftest(mod2, vcov = vcovCL(mod2,cluster = dat$PROLIFIC_PID))
  mod2_stats = data.frame(
    number_of_obs = nobs(mod2),
    `Likl. Ratio` = with(mod2, null.deviance - deviance),
    `Pr(>Chisq)` = with(mod2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),
    AIC = mod2$aic
  )
  
  mod3 = glm(effort ~ condition + age + female + US_resident + prior_on_yellow_vs_50 + task_order, family = binomial(link = "logit"), data = dat, na.action = na.omit)
  mod3_mfx = margins_summary(mod3, data = dat, vcov = vcovCL(mod3,cluster = dat$PROLIFIC_PID))
  mod3_coef = coeftest(mod3, vcov = vcovCL(mod3,cluster = dat$PROLIFIC_PID))
  mod3_stats = data.frame(
    number_of_obs = nobs(mod3),
    `Likl. Ratio` = with(mod3, null.deviance - deviance),
    `Pr(>Chisq)` = with(mod3, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),
    AIC = mod3$aic
  )
  
  # filtered sample
  dat_f = dat[dat$post_quiz_correct == 1,]
  
  mod4 = glm(effort ~ condition, family = binomial(link = "logit"), data = dat_f, na.action = na.omit)
  mod4_mfx = margins_summary(mod4, data = dat_f, vcov = vcovCL(mod4,cluster = dat_f$PROLIFIC_PID))
  mod4_coef = coeftest(mod4, vcov = vcovCL(mod4,cluster = dat_f$PROLIFIC_PID))
  mod4_stats = data.frame(
    number_of_obs = nobs(mod4),
    `Likl. Ratio` = with(mod4, null.deviance - deviance),
    `Pr(>Chisq)` = with(mod4, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),
    AIC = mod4$aic
  )
  
  mod5 = glm(effort ~ condition + age + female + US_resident, family = binomial(link = "logit"), data = dat_f, na.action = na.omit)
  mod5_mfx = margins_summary(mod5, data = dat_f, vcov = vcovCL(mod5,cluster = dat_f$PROLIFIC_PID))
  mod5_coef = coeftest(mod5, vcov = vcovCL(mod5,cluster = dat_f$PROLIFIC_PID))
  mod5_stats = data.frame(
    number_of_obs = nobs(mod5),
    `Likl. Ratio` = with(mod5, null.deviance - deviance),
    `Pr(>Chisq)` = with(mod5, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),
    AIC = mod5$aic
  )
  
  mod6 = glm(effort ~ condition + age + female + US_resident + prior_on_yellow_vs_50 + task_order, family = binomial(link = "logit"), data = dat_f, na.action = na.omit)
  mod6_mfx = margins_summary(mod6, data = dat, vcov = vcovCL(mod6,cluster = dat_f$PROLIFIC_PID))
  mod6_coef = coeftest(mod6, vcov = vcovCL(mod6,cluster = dat_f$PROLIFIC_PID))
  mod6_stats = data.frame(
    number_of_obs = nobs(mod6),
    `Likl. Ratio` = with(mod6, null.deviance - deviance),
    `Pr(>Chisq)` = with(mod6, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),
    AIC = mod6$aic
  )
 
  ## Return results
  results_list = list(
    
    mod1_mfx,mod1_coef,mod1_stats,
    mod2_mfx,mod2_coef,mod2_stats,
    mod3_mfx,mod3_coef,mod3_stats,
    mod4_mfx,mod4_coef,mod4_stats, 
    mod5_mfx,mod5_coef,mod5_stats,
    mod6_mfx,mod6_coef,mod6_stats
  )
  
  return(results_list)
}

## tabulate regressions
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

  return(res_table)
}


################-----------------READ DATA------------###############

# read raw data and construct data for the analysis
dat = constructData()
# save data
write.csv(dat,'s1_data.csv',row.names=FALSE)
# summary statistics - Table C1
summ_table = summaryStats(dat)


###############------------------MAIN----------------###############


## Figures in Main text
# Figure 3
effort_plot = plotEffortCompletion(dat)
# Figure 4
pickvsdrawbytask_plot = plotPickPercVsDrawByTask(dat)


## Figures in Appendix
#Figure C1
clarity_plot = plotClarityByTreatment(dat)
# Figure C2
training_plot = plotStatTrainingByTreatment(dat)
# Figure D1
effortvsEbonus_plot = plotEffortAndEbonus(dat)
# Table D1
pickvsprior_tables = tabulatePicksAndPriors(dat)


## Regression results
# Estimate logistic regression, baseline category is "Flat"
regr_results1 = runEffortRegressions(dat, 'Flat')
# Table D3 - Logistic regression estimates
regrtable1 = tabulateEstimates(regr_results1)
# Table 1 - marginal effects
margins1 = getMargins(regr_results1)
margins1 %>% mutate_if(is.numeric, round, digits = 2)
# Table 1 - stats in the bottom half
regrstats1 = getRegrStats(regr_results1)
# Estimate logistic regression, baseline category is "Peerbet"
regr_results2 = runEffortRegressions(dat, 'Peerbet')
# Logistic regression estimates
regrtable2 = tabulateEstimates(regr_results2)
# Table D2 - marginal effects
margins2 = getMargins(regr_results2)
margins2 %>% mutate_if(is.numeric, round, digits = 2)
# Table D2 - stats in the bottom half
regrstats2 = getRegrStats(regr_results2)
