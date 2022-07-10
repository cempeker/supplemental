########### Plot the dispersion of predictions across tasks in all data sets, generates Figure C1
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

constructCoinData = function(){
  
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
  
  # exclude cases where private signals are identical ("Nested" structure in Palley and Soll (2019))
  #dat_coin1 = dat_coin1[dat_coin1$Condition != 3,]
  
  # return data
  return(dat_coin1)
}

constructGeneralKnowledgeData = function(){
  
  # read raw data
  dataset_names = c('r2a_rawdata','r2b_rawdata','r2c_rawdata','r2d_rawdata','r2e_rawdata')
  dataset_links = paste0('./data_binary/',dataset_names,'.csv')
  dat_gk = NULL
  subject_count = 0
  for(i in 1:5){
    dat_gk_temp = read.csv(dataset_links[i], header = FALSE)
    dat_gk_temp$Dataset = i
    dat_gk_temp$subject = subject_count + 1:nrow(dat_gk_temp)
    subject_count = subject_count + nrow(dat_gk_temp)
    
    # get forecasts and name columns
    # f: probabilistic pred, g: probabilistic meta-pred, votef: binary vote (0,1), voteg: metavote (estimates on percentage endorsements) 
    colnames(dat_gk_temp) = c(1:400,'Dataset','subject')
    colnames(dat_gk_temp)[seq(1,397,by=4)]= paste0('votef.',100*(i-1) + 1:100)
    colnames(dat_gk_temp)[seq(2,398,by=4)]= paste0('voteg.',100*(i-1) + 1:100)
    colnames(dat_gk_temp)[seq(3,399,by=4)]= paste0('f.',100*(i-1) + 1:100)
    colnames(dat_gk_temp)[seq(4,400,by=4)]= paste0('g.',100*(i-1) + 1:100)
    
    # wide to long
    dat_gk_temp = melt(dat_gk_temp,id.vars=c('Dataset','subject'))
    dat_gk_temp = dat_gk_temp %>% separate(variable,into=c('report','item'))
    dat_gk_temp$item = paste0('gk',dat_gk_temp$item)
    dat_gk_temp$task_type = 'General Knowledge'
    
    # bind to whole data
    dat_gk = rbind(dat_gk,dat_gk_temp)
    
    # remove temp vars
    rm(dat_gk_temp)
  }
  
  # get question list and outcomes
  dat_gk_questions = read.csv('./data_binary/r2_outcomes.csv',header = FALSE)
  colnames(dat_gk_questions) = c('item','Actual','Difficulty','Text')
  dat_gk_questions$item = paste0('gk',dat_gk_questions$item) 
  
  # merge outcomes with prediction data
  dat_gk = merge(dat_gk,dat_gk_questions[,c('item','Difficulty','Actual')],by=c('item'))
  colnames(dat_gk)[which(colnames(dat_gk) == 'Actual')] = 'binary_outcome'
  
  rm(dataset_links,dataset_names,dat_gk_questions)
  
  # cast data so that predictions and meta-predictions are in different columns
  dat_gk = dcast(dat_gk, item + Dataset + subject + binary_outcome + task_type + Difficulty ~  report, value.var = 'value')
  dat_gk$votef = dat_gk$votef - 1
  
  # remove cases with NA entry (no such rows are found in data)
  dat_gk = dat_gk[complete.cases(dat_gk),]
  
  # return data
  return(dat_gk)
}

constructStateCapitalData = function(){
  
  # read raw data
  dataset_link = './data_binary/m4data.csv'
  dat_raw <- read.csv(dataset_link, header = FALSE)
  dat_raw$subject = 1:89
  
  # get forecasts and name columns
  # f: probabilistic pred, g: probabilistic meta-pred, votef: binary vote (0,1), voteg: metavote (estimates on percentage endorsements)
  colnames(dat_raw) = c(1:200,'subject')
  colnames(dat_raw)[seq(1,197,by=4)]= paste0('votef.',1:50)
  colnames(dat_raw)[seq(2,198,by=4)]= paste0('voteg.',1:50)
  colnames(dat_raw)[seq(3,199,by=4)]= paste0('f.',1:50)
  colnames(dat_raw)[seq(4,200,by=4)]= paste0('g.',1:50)
  
  # wide to long
  dat_s = melt(dat_raw,id.vars=c('subject'))
  dat_s = dat_s %>% separate(variable,into=c('report','item'))
  dat_s$item = paste0('state',dat_s$item)
  dat_s$task_type = 'State Capital'
  
  
  # get question list and outcomes
  dat_s_questions = read.csv('./data_binary/m4data_details.csv',header = TRUE)
  colnames(dat_s_questions)[which(colnames(dat_s_questions) == 'Question')] = 'item'
  dat_s_questions$item = paste0('state',dat_s_questions$item) 
  
  # merge outcomes with prediction data
  dat_s = merge(dat_s,dat_s_questions[,c('item','Outcome')],by=c('item'))
  colnames(dat_s)[which(colnames(dat_s) == 'Outcome')] = 'binary_outcome'
  
  # cast data so that predictions and meta-predictions are in different columns
  dat_s = dcast(dat_s, item + subject + binary_outcome + task_type ~  report, value.var = 'value')
  
  # transform vote data so that 1 corresponds to reporting TRUE, 0 to reporting FALSE
  dat_s$votef = dat_s$votef - 1
  
  # remove cases with NA entry (no such rows are found in data)
  dat_s = dat_s[complete.cases(dat_s),]
  
  # return data
  return(dat_s)
}


### plot the distribution of inter-quartile ranges
plotSD = function(dat_coin,dat_gk,dat_sc){
  
  cnames = c('item','subject','task_type','f','g')
  d = rbind(dat_coin[,cnames],dat_gk[,cnames],dat_sc[,cnames])
  
  d2 = as.data.frame(
    d %>%
      group_by(item,task_type) %>%
      summarize(
        f_iqr = IQR(f)
      )
  ) 
  
  pl = ggplot(d2, aes(x=f_iqr,fill=task_type)) +
    geom_histogram(aes(y=..density..),breaks = seq(0,100,by=5)) +
    #geom_histogram(aes(y=..density..)) +
    facet_wrap(vars(task_type)) +
    scale_y_continuous(name = 'Density') +
    scale_x_continuous(name = "Inter-quartile range of predictions in a task",breaks = seq(0,100,by=20)) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = 'none'
    )
  
  return(pl)
}


######################------------------MAIN--------------################

### Read data
dat_coin = constructCoinData()
dat_coin$task_type = 'Coin Flips'
dat_gk = constructGeneralKnowledgeData()
dat_sc = constructStateCapitalData()

### plot the distribution of inter-quartile ranges
plotSD(dat_coin,dat_gk,dat_sc)

