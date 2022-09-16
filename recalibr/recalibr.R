
#### libraries
library(ggplot2)
#library(ggpubr)
library(readxl)
#library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
#library(latex2exp)
#library(MASS)
#library(caret)
#library(RColorBrewer)
library(jcolors)
#library(mltools)
library(metaggR)
#library(entropy)
#library(moments)
#library(ggdark)


################----------------FUNCTIONS--------------##############

# read GK and SC data
constructGeneralKnowledgeData = function(){
  # read data
  dataset_names = c('r2a_rawdata','r2b_rawdata','r2c_rawdata','r2d_rawdata','r2e_rawdata')
  dataset_links = paste0('./sca_data/Files/',dataset_names,'.csv')
  dat_gk = NULL
  subject_count = 0
  for(i in 1:5){
    # read raw data
    dat_gk_temp = as.data.frame(read.csv(dataset_links[i], header = FALSE))
    dat_gk_temp$Dataset = i
    dat_gk_temp$subject = subject_count + 1:nrow(dat_gk_temp)
    subject_count = subject_count + nrow(dat_gk_temp)
    
    # select probabilistic forecasts only
    colnames(dat_gk_temp) = c(1:400,'Dataset','subject')
    #ind_forecast = seq(3,399,by=4)
    #ind_meta = ind_forecast + 1
    #ind_binary_f = seq(1,397,by=4)
    #ind_binary_g = ind_binary_f + 1
    #ind_all = c(sort(c(ind_forecast,ind_meta)),'Dataset','subject')
    #dat_gk_temp = dat_gk_temp[,ind_all]
    
    colnames(dat_gk_temp)[seq(1,397,by=4)]= paste0('vote.',100*(i-1) + 1:100)
    colnames(dat_gk_temp)[seq(2,398,by=4)]= paste0('metavote.',100*(i-1) + 1:100)
    colnames(dat_gk_temp)[seq(3,399,by=4)]= paste0('prob.',100*(i-1) + 1:100)
    colnames(dat_gk_temp)[seq(4,400,by=4)]= paste0('metaprob.',100*(i-1) + 1:100)
    
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
  
  
  # question list and outcomes
  dat_gk_questions = read.csv('./sca_data/Files/r2_outcomes.csv',header = FALSE)
  colnames(dat_gk_questions) = c('item','Actual','Difficulty','Question')
  dat_gk_questions$item = paste0('gk',dat_gk_questions$item)
  
  # merge outcomes with subject reports
  dat_gk = merge(dat_gk,dat_gk_questions)
  colnames(dat_gk)[which(colnames(dat_gk) == 'Actual')] = 'binary_outcome'
  
  rm(dataset_links,dataset_names,dat_gk_questions)
  
  # cast to f and g being in different columns
  dat_gk = dcast(dat_gk, item + Dataset + subject + binary_outcome + task_type + Difficulty ~  report, value.var = 'value')
  dat_gk$vote = dat_gk$vote - 1
  
  # remove subjects with NA in one of the reports
  dat_gk = dat_gk[complete.cases(dat_gk),]
  
  
  dat_gk$prob = 0.01*dat_gk$prob
  dat_gk$metaprob = 0.01*dat_gk$metaprob
  
  # return data
  return(dat_gk)
}
constructStateCapitalData = function(){
  
  dataset_link = './sca_data/Files/m4data.csv'
  dat_raw <- read.csv(dataset_link, header = FALSE)
  dat_raw$subject = 1:89
  
  colnames(dat_raw) = c(1:200,'subject')
  #ind_forecast = seq(3,199,by=4)
  #ind_meta = ind_forecast + 1
  
  #ind_all = c(sort(c(ind_forecast,ind_meta)),'subject')
  #dat_raw = dat_raw[,ind_all]
  #colnames(dat_raw)[seq(1,99,by = 2)] = paste0('f', 1:50)
  #colnames(dat_raw)[seq(2,100,by = 2)]= paste0('g', 1:50)
  
  colnames(dat_raw)[seq(1,197,by=4)]= paste0('vote.',1:50)
  colnames(dat_raw)[seq(2,198,by=4)]= paste0('metavote.',1:50)
  colnames(dat_raw)[seq(3,199,by=4)]= paste0('prob.',1:50)
  colnames(dat_raw)[seq(4,200,by=4)]= paste0('metaprob.',1:50)
  
  # wide to long
  dat_s = melt(dat_raw,id.vars=c('subject'))
  dat_s = dat_s %>% separate(variable,into=c('report','item'))
  dat_s$item = paste0('state',dat_s$item)
  dat_s$task_type = 'State Capital'
  
  
  # question list and outcomes
  dat_s_questions = read.csv('./sca_data/Files/m4data_details.csv',header = TRUE)
  colnames(dat_s_questions)[which(colnames(dat_s_questions) == 'Question')] = 'item'
  dat_s_questions$item = paste0('state',dat_s_questions$item) 
  
  # merge outcomes with subject reports
  dat_s = merge(dat_s,dat_s_questions[,c('item','Outcome')],by=c('item'))
  colnames(dat_s)[which(colnames(dat_s) == 'Outcome')] = 'binary_outcome'
  
  # cast to f and g being in differen columns
  dat_s = dcast(dat_s, item + subject + binary_outcome + task_type ~  report, value.var = 'value')
  
  dat_s$vote = dat_s$vote - 1
  
  # remove subjects with NA in one of the reports
  #na_subjects = unique(dat_gk[is.na(dat_gk$value),'subject'])
  #dat_gk = dat_gk[!(dat_gk$subject %in% na_subjects),]
  dat_s = dat_s[complete.cases(dat_s),]
  
  
  dat_s$prob = 0.01*dat_s$prob
  dat_s$metaprob = 0.01*dat_s$metaprob
  
  return(dat_s)
}

# read new data
getNewData = function(){
  # read data
  dataset_link = './new_data/MWH2021_data.csv'
  dat_raw <- read.csv(dataset_link, header = TRUE)
  
  #dat_raw$QuestionID = paste0('E',dat_raw$Experiment,'Q',dat_raw$QuestionNumber)
  
  dat_raw$QuestionType = NA
  dat_raw[dat_raw$Experiment == '1' & dat_raw$QuestionNumber <= 50,'QuestionType'] = 'NFL'
  #dat_raw[dat_raw$Experiment == '1' & dat_raw$QuestionNumber > 50,'QuestionType'] = 'Science 2'
  dat_raw[dat_raw$Experiment == '1' & dat_raw$QuestionNumber > 50,'QuestionType'] = 'Science'
  
  dat_raw[dat_raw$Experiment == '2' & dat_raw$QuestionNumber <= 50,'QuestionType'] = 'NFL'
  #dat_raw[dat_raw$Experiment == '2' & dat_raw$QuestionNumber > 50 & dat_raw$QuestionNumber <= 100,'QuestionType'] = 'Science 2'
  dat_raw[dat_raw$Experiment == '2' & dat_raw$QuestionNumber > 50 & dat_raw$QuestionNumber <= 100,'QuestionType'] = 'Science'
  dat_raw[dat_raw$Experiment == '2' & dat_raw$QuestionNumber > 100,'QuestionType'] = 'Scenario'
  
  #dat_raw$QuestionID = NA
  #dat_raw[dat_raw$Experiment == '1' | dat_raw$Experiment == '2','QuestionID'] = paste0('12_',dat_raw[dat_raw$Experiment == '1' | dat_raw$Experiment == '2','QuestionType'])
  
  dat_raw[dat_raw$Experiment == '3a' & dat_raw$QuestionNumber <= 40,'QuestionType'] = 'Artwork'
  #dat_raw[dat_raw$Experiment == '3a' & dat_raw$QuestionNumber > 40 & dat_raw$QuestionNumber <= 80,'QuestionType'] = 'Science'
  dat_raw[dat_raw$Experiment == '3a' & dat_raw$QuestionNumber > 40 & dat_raw$QuestionNumber <= 80,'QuestionType'] = 'Science'
  dat_raw[dat_raw$Experiment == '3a' & dat_raw$QuestionNumber > 80,'QuestionType'] = 'Scenario'
  
  dat_raw[dat_raw$Experiment == '3b' & dat_raw$QuestionNumber <= 40,'QuestionType'] = 'Artwork'
  #dat_raw[dat_raw$Experiment == '3b' & dat_raw$QuestionNumber > 40 & dat_raw$QuestionNumber <= 80,'QuestionType'] = 'Science'
  dat_raw[dat_raw$Experiment == '3b' & dat_raw$QuestionNumber > 40 & dat_raw$QuestionNumber <= 80,'QuestionType'] = 'Science'
  dat_raw[dat_raw$Experiment == '3b' & dat_raw$QuestionNumber > 80,'QuestionType'] = 'Scenario'
  
  #dat_raw$DataType = paste0(dat_raw$Experiment,'.',dat_raw$QuestionType)
  #dat_raw = dat_raw[dat_raw$Experiment %in% c('1','2'),]
  
  return(dat_raw)
  
}

# combine data
combineDatasets = function(dat_gk,dat_sc,dat_new){
  
  dat_gk$Dataset = NULL
  #dat_gk$metavote = NULL
  dat_gk$task_type = NA
  #dat_gk[dat_gk$Difficulty == 1,'task_type'] = 'Science 1, G1'
  #dat_gk[dat_gk$Difficulty == 2,'task_type'] = 'Science 1, G2'
  #dat_gk[dat_gk$Difficulty == 3,'task_type'] = 'Science 1, G3'
  #dat_gk[dat_gk$Difficulty == 4,'task_type'] = 'Science 1, G4'
  #dat_gk[dat_gk$Difficulty == 5,'task_type'] = 'Science 1, G5'
  #dat_gk$task_type = 'Science 1'
  dat_gk$task_type = 'Science'
  dat_gk$Difficulty = NULL
  
  dat_sc$task_type = 'States'
  #dat_sc$metavote = NULL
  dat = rbind(dat_gk,dat_sc)
  
  # remove inconsistent observations (Science data)
  ind = which(dat$vote == 1 & dat$prob < 0.50)
  ind2 = which(dat$vote == 0 & dat$prob > 0.50)
  ind = sort(c(ind,ind2));rm(ind2)
  dat = dat[-ind,]
  
  #dat_new$item = paste0(dat_new$QuestionType,dat_new$QuestionNumber)
  dat_new$item = paste0(dat_new$QuestionType,'.',dat_new$Experiment,'.',dat_new$QuestionNumber)
  dat_new$subject = paste0(dat_new$Experiment,'.',dat_new$SubjectID)
  colnames(dat_new)[which(colnames(dat_new) == 'Vote')] = 'vote'
  colnames(dat_new)[which(colnames(dat_new) == 'Pr_Forecast')] = 'prob'
  colnames(dat_new)[which(colnames(dat_new) == 'Meta.Prediction')] = 'metaprob'
  colnames(dat_new)[which(colnames(dat_new) == 'ActualOutcome')] = 'binary_outcome'
  colnames(dat_new)[which(colnames(dat_new) == 'QuestionType')] = 'task_type'
  dat_new$Experiment=NULL
  dat_new$QuestionNumber=NULL
  dat_new$SubjectID=NULL
  dat_new$metavote = NA
  
  # remove Science data
 #  dat_new = dat_new[dat_new$task_type %in% c('Scenario','NFL','Artwork'),]
  
  dat = rbind(dat,dat_new)
  
  dat =  dat[dat$task_type %in% c('Science','States','NFL','Artwork'),]
  
  return(dat)
}

# get simple aggregates and binary preds
getSimpleAggregates = function(dat){
  
  dat = as.data.frame(
    dat %>%
      group_by(item) %>%
      mutate(
        avgprob = mean(prob, na.rm = TRUE),
        avgmetaprob = mean(metaprob,na.rm = TRUE),
        avgmetavote = mean(metavote,na.rm = TRUE),
        #count_F = length(prob[prob<=0.5]),
        #count_T = length(prob[prob>0.5]),
        avg_conf_F = 1-mean(prob[vote==0], na.rm = TRUE),
        avg_conf_T = mean(prob[vote==1], na.rm = TRUE),
        count_F = sum(vote == 0),
        count_T = sum(vote == 1),
        perc_T = 100*(count_T / (count_T + count_F)),
        perc_F = 100 - perc_T,
        conf_weight = ifelse(avgprob > 0.5,1,0),
        max_conf = ifelse(avg_conf_T > avg_conf_F,1,0),
        majority_vote = ifelse(count_T > count_F,1,0),
        SP_pred = ifelse(perc_T > avgmetavote,1,0),
        SC_pred = ifelse(avgprob > avgmetaprob, 1, 0)
      )
  )
  
  return(dat)
}

# run continuous-porbability algorithms
runSOAlgorithm = function(dat){
  # function to implement the SO algorithm  
  
  dat$forecast_overshoot = ifelse(dat$prob > dat$avgprob,1,0)
  dat$meta_overshoot = ifelse(dat$metaprob > dat$avgprob,1,0)
  
  dat = as.data.frame(
    dat %>%
      group_by(item) %>%
      mutate(
        px = mean(forecast_overshoot),  # overshoot rate px
        pz = mean(meta_overshoot), # overshoot rate pz
        q = 1-pz,
        SOA = as.numeric(quantile(prob,q,type=1)), # quantile type 1
        SOA2 = as.numeric(quantile(prob,q,type=4)) # quantile type 4, interpolation
      )
  )
  
  return(dat)
}
runMinimalPivoting = function(dat){
  ##### get minimal pivoting estimate
  #dat = as.data.frame(
  #  dat %>%
  #    group_by(item) %>%
  #    mutate(zbar = mean(g,na.rm = TRUE))
  #)
  dat$MP = 2*dat$avgprob - dat$avgmetaprob
  dat$MP = ifelse(dat$MP < 0, 0, dat$MP)
  dat$MP = ifelse(dat$MP > 1, 1, dat$MP)
  
  return(dat)
}
runKnowledgeWeighted = function(dat){
  #### Knowledge weighted estimate of Palley and Satopaa 2022
  # if all predictions in a given Bootstrap sample are the same, knowledge_weighted_estimate throws an error. In such cases, KW estimate is simply set as the consensus estimate.
  
  dat = as.data.frame(
    dat %>%
      group_by(item) %>%
      mutate(
        not_all_E_same = ifelse(length(unique(prob)) > 1,1,0), 
        KW = ifelse(not_all_E_same, knowledge_weighted_estimate(E = prob,P = metaprob, no_inf_check=TRUE), unique(E))
      )
  )
  
  dat$KW = ifelse(dat$KW < 0, 0, dat$KW)
  dat$KW = ifelse(dat$KW > 1, 1, dat$KW)
  
  # return
  return(dat)
}
runMPW = function(dat){
  #### Meta-probability weighting (MPW) algorithm of Martinie et al (2020)
  
  # get forecast - metaprediction for each item and subject
  dat$f_min_g = abs(dat$prob - dat$metaprob)
  
  # compute weights and MPW estimate
  dat = as.data.frame(
    dat %>% 
      group_by(item) %>%
      mutate(
        weight = f_min_g / sum(f_min_g),
        MPW = as.vector(weight %*% prob)
      )
  )
  
  return(dat)
}

# run linear regression for estimating priors and plot estimated priors
runItemRegression = function(i, dat){
  
  dat = dat[dat$item == i,]
  linmod1 = lm(metaprob ~ prob,data = dat)
  
  EavgF = as.numeric(linmod1$coefficients[1])
  EavgT = as.numeric(linmod1$coefficients[1]) + as.numeric(linmod1$coefficients[2])
  #midFT = 0.5*(EavgF + EavgT)
  
  df = data.frame(i,EavgF,EavgT)
  colnames(df) = c('item','EavgF','EavgT')
  #df = data.frame(i,EavgF,EavgT)
  #colnames(df) = c('item','EavgF','EavgT')
  
  return(df)
}
getEstimatedPrior = function(dat2){
  
  item_list = unique(dat$item)
  dat_est = lapply(item_list, runItemRegression, dat = dat)
  dat_est = bind_rows(dat_est)
  dat = merge(dat,dat_est,by='item')
  
  #dat2 = unique( subset(dat,select=-c(subject,vote,prob,metaprob)) )
  dat2 = dat
  
  dat2$estPrior = dat2$EavgF / (1-(dat2$EavgT - dat2$EavgF))
  #dat2$estPrior = ifelse(dat2$estPrior > 1, 0.5, dat2$estPrior)
  #dat2$estPrior = ifelse(dat2$estPrior < 0, 0.5, dat2$estPrior)
  
  return(dat2)
}
plotEstPriors = function(dat2){
  
  d = unique(dat2[,c('item','task_type','estPrior')])
  d = as.data.frame(
    d %>%
      mutate(
        estPriorbin = cut(estPrior,breaks=c(-5,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,5),include.lowest=TRUE)
      )
  )
  
  d2 = as.data.frame(
    d %>%
      group_by(task_type,estPriorbin,.drop = FALSE) %>%
      tally()
  )
  
  pl_estpriors = ggplot(d2,aes(x=estPriorbin,y=n)) +
    geom_bar(stat='identity') +
    geom_text(data = d2 %>% filter(n>0),aes(label=n),vjust=-0.25) +
    facet_wrap(~task_type) +
    scale_y_continuous(name='Count',limits = c(0,335)) +
    scale_x_discrete(name='Estimated Prior',labels=c('<0','[0,0.1]','(0.1,0.2]','(0.2,0.3]','(0.3,0.4]','(0.4,0.5]','(0.5,0.6]','(0.6,0.7]','(0.7,0.8]','(0.8,0.9]','[0.9,1]','>1')) +
    theme_bw(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust=1)
    )
  
  return(pl_estpriors)
}
updateEstimatedPrior = function(dat2){
  
  dat2$estPrior = ifelse(dat2$estPrior > 1, 0.5, dat2$estPrior)
  dat2$estPrior = ifelse(dat2$estPrior < 0, 0.5, dat2$estPrior)
  
  return(dat2)
}

# plot average vs est prior by state
tabulateAvgVsPrior = function(dat3){
  
  d = unique(dat3[,c('item','task_type','avgprob','estPrior','binary_outcome')])
  d$avg_higher_estprior = ifelse(d$avgprob > d$estPrior, 1, 0)
  d$avg_higher_0.5 = ifelse(d$avgprob > 0.5, 1, 0)
  
  d_estprior = as.data.frame(
    d %>%
      group_by(binary_outcome,avg_higher_estprior) %>%
      summarize(
        counts = n()
      )
  )
  
  d_0.5= as.data.frame(
    d %>%
      group_by(binary_outcome,avg_higher_0.5) %>%
      summarize(
        counts = n()
      )
  )
  
  return(list(d_estprior,d_0.5))
}
plotAverages = function(dat3){
  
  d = unique(dat3[,c('item','binary_outcome','task_type','avgprob')])
  d$wrongsided = ifelse(
    (d$avgprob > 0.5 & d$binary_outcome==0) | (d$avgprob < 0.5 & d$binary_outcome==1),
  1,
  0)
  
  d2 = d
  d2$binary_outcome = ifelse(d2$binary_outcome == 1, "Answer = \"True\"","Answer = \"False\"") 
  
  pl_avgpred = ggplot(d2,aes(x=avgprob)) +
    geom_histogram() +
    facet_grid(task_type~binary_outcome) +
    scale_x_continuous(name='Average Prediction',labels=c('0','0.25','0.50','0.75','1')) +
    theme_bw(base_size = 14)
  
  d2 = as.data.frame(
    d %>%
      group_by(task_type) %>%
      summarize(
        num_wrongsided = sum(wrongsided),
        num_total = n()
      )
  )
  colnames(d2) = c('task_type','Wrong-sided','All')
  d2 = melt(d2,id.vars = 'task_type')
  colnames(d2) = c('task_type','Average prediction','value')
  
  my_palette = as.vector(jcolors(palette = 'pal7'))[c(1,2)]
  
  pl_wrongsided = ggplot(d2, aes(x = task_type, y=value, fill=`Average prediction`)) +
    geom_bar(stat='identity',position = 'dodge') +
    scale_x_discrete(name='Task type') +
    scale_y_continuous(name='Number of tasks',breaks = seq(0,700,50)) +
    scale_fill_manual(values=my_palette) +
    theme_bw(base_size = 14)
  
  
  return(list(pl_wrongsided,pl_avgpred))
}

# Transformations
extremFunction = function(prob,gamma,prior){
  
  delta = ((1-prior)/prior)^gamma
  prob_ext = (delta*(prob^(1+gamma))) / ( (delta*(prob^(1+gamma))) + (1-prob)^(1+gamma) )
  
  #prob_ext = (prob^gamma) / ( prob^gamma + (1-prob)^gamma )
  
  return(prob_ext)
}
transformAvgProb = function(dat3){
  
  d = as.data.frame(
    dat3 %>%
      mutate(
        #t1 = ifelse(avg_prob > estPrior,1,0),
        avgextrem_1 = extremFunction(avgprob,1,0.5),
        avgextrem_1.5 = extremFunction(avgprob,1.5,0.5),
        avgextrem_2 = extremFunction(avgprob,2,0.5),
        avgextrem_2.5 = extremFunction(avgprob,2.5,0.5),
        t_1 = extremFunction(avgprob,1,estPrior),
        t_1.5 = extremFunction(avgprob,1.5,estPrior),
        t_2 = extremFunction(avgprob,2,estPrior),
        t_2.5 = extremFunction(avgprob,2.5,estPrior)
      )
  )
  
  d2 = unique(d[,c('item','binary_outcome','task_type','avgprob','conf_weight', 'max_conf', 'majority_vote','SP_pred','SC_pred','MP','KW','SOA','SOA2','MPW','avgextrem_1','avgextrem_1.5','avgextrem_2','avgextrem_2.5','t_1','t_1.5','t_2','t_2.5')])
  
  return(d2)
}

# Calibration curves
calibrationPlotT = function(dat_transformed){
  
  # Simple methods
  d1 = dat_transformed[,c('item', 'binary_outcome','task_type','avgprob','avgextrem_1','avgextrem_1.5','avgextrem_2', 'avgextrem_2.5','t_1','t_1.5','t_2','t_2.5')]
  d1 = melt(d1, id.vars = c('item', 'binary_outcome','task_type'))
  
  d_avg1 = d1[d1$variable == 'avgprob',]; d_avg1$gamma = "1";
  d_avg1.5 = d1[d1$variable == 'avgprob',]; d_avg1.5$gamma = "1.5";
  d_avg2 = d1[d1$variable == 'avgprob',]; d_avg2$gamma = "2";
  d_avg2.5 = d1[d1$variable == 'avgprob',]; d_avg2.5$gamma = "2.5";
  d_avg = do.call(rbind,list(d_avg1,d_avg1.5,d_avg2,d_avg2.5))
  rm(d_avg1,d_avg1.5,d_avg2,d_avg2.5)
  
  d_other = d1[d1$variable != 'avgprob',]
  d_other = d_other %>% separate(variable,into=c('variable','gamma'),sep='_')
  
  d1 = rbind(d_avg,d_other)
  colnames(d1) = c('item','binary_outcome','task_type','method','value','gamma')
  d1$method = recode(d1$method, 'avgprob' = 'average','avgextrem'='extrem.average','t' = 'robust.recalibr')
  d1$method = factor(d1$method,levels=c('average','extrem.average','robust.recalibr'))
  
  d1 = as.data.frame(
    d1 %>%
      mutate(
        prob_cut = cut(value,breaks=seq(0,1,by=0.1),include.lowest = TRUE)
      )
  )
  
  dplot1 = as.data.frame(
    d1 %>%
      group_by(prob_cut,method,gamma) %>%
      summarize(
        true_rate = mean(binary_outcome),
        num_tasks = n() 
      )
  )
  dplot1 = dplot1[dplot1$num_tasks >= 10,]
  dplot1$gamma = paste0("gamma = ",dplot1$gamma)
  
  my_palette1 = as.vector(c(
    jcolors(palette = 'pal3')[5],
    jcolors(palette = 'pal2')[3],
    jcolors(palette = 'pal2')[4]
  ))
  my_shapes1 = c(17,18,16)
  
  #my_palette = c( as.vector(jcolors(palette = 'pal2')), as.vector(jcolors(palette = 'pal5')[2:3]) )[2:4]
  
  pl1 = ggplot(data = dplot1, aes(x= as.numeric(prob_cut), y=true_rate, color=method, shape=method)) +
    geom_line() +
    geom_point(size = 3) +
    facet_wrap(~gamma) +
    scale_color_manual(values=my_palette1) +
    scale_shape_manual(values=my_shapes1) +
    annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 0.1, alpha = 0.2) +
    annotate("rect", xmin = 1.5, xmax = 2.5, ymin = 0.1, ymax = 0.2, alpha = 0.2) +
    annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 0.2, ymax = 0.3, alpha = 0.2) +
    annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 0.3, ymax = 0.4, alpha = 0.2) +
    annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 0.4, ymax = 0.5, alpha = 0.2) +
    annotate("rect", xmin = 5.5, xmax = 6.5, ymin = 0.5, ymax = 0.6, alpha = 0.2) +
    annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 0.6, ymax = 0.7, alpha = 0.2) +
    annotate("rect", xmin = 7.5, xmax = 8.5, ymin = 0.7, ymax = 0.8, alpha = 0.2) +
    annotate("rect", xmin = 8.5, xmax = 9.5, ymin = 0.8, ymax = 0.9, alpha = 0.2) +
    annotate("rect", xmin = 9.5, xmax = 10.5, ymin = 0.9, ymax = 1, alpha = 0.2) +
    scale_x_continuous(name='Predicted probability of True',breaks = seq(1,10,by=1), labels = levels(dplot1$prob_cut)) +
    scale_y_continuous(name='Proportion True',breaks = seq(0,1,by=0.1)) +
    theme_bw(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45,hjust=1),
      legend.position = 'top',
      legend.direction = 'horizontal'
    )
  
  
  ### Advanced benchmarks
  
  d2 = dat_transformed[,c('item', 'binary_outcome','task_type','MP', 'KW','MPW','SOA','t_2')]
  d2 = melt(d2, id.vars = c('item', 'binary_outcome','task_type'))
  
  colnames(d2) = c('item','binary_outcome','task_type','method','value')
  d2$method = recode(d2$method, 'MP' = 'min.pivot', 'KW' = 'know.weight','MPW' = 'meta.prob.weight','SOA' = 'surp.overshoot', 't_2' = 'robust.recalibr')
  d2$method = factor(d2$method,c('min.pivot','know.weight','meta.prob.weight','surp.overshoot','robust.recalibr'))
  
  d2 = as.data.frame(
    d2 %>%
      mutate(
        prob_cut = cut(value,breaks=seq(0,1,by=0.1),include.lowest=TRUE)
      )
  )
  
  dplot2 = as.data.frame(
    d2 %>%
      group_by(prob_cut,method) %>%
      summarize(
        true_rate = mean(binary_outcome),
        num_tasks = n() 
      )
  )
  dplot2 = dplot2[dplot2$num_tasks >= 10,]
  
  my_palette2 = as.vector(c(
    jcolors(palette = 'pal8')[3],
    jcolors(palette = 'pal6')[6],
    jcolors(palette = 'pal7')[1],
    jcolors(palette = 'pal6')[8],
    jcolors(palette = 'pal2')[4]
  ))
  my_shapes2 = c(3,4,7,8,16)
  
  #my_palette = c( as.vector(jcolors(palette = 'pal2')), as.vector(jcolors(palette = 'pal5')[2:3]) )
  
  pl2 = ggplot(data = dplot2, aes(x= as.numeric(prob_cut), y=true_rate, color=method, shape=method)) +
    geom_line() +
    geom_point(size = 3) +
    scale_color_manual(values=my_palette2) +
    scale_shape_manual(values=my_shapes2) +
    annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 0.1, alpha = 0.2) +
    annotate("rect", xmin = 1.5, xmax = 2.5, ymin = 0.1, ymax = 0.2, alpha = 0.2) +
    annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 0.2, ymax = 0.3, alpha = 0.2) +
    annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 0.3, ymax = 0.4, alpha = 0.2) +
    annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 0.4, ymax = 0.5, alpha = 0.2) +
    annotate("rect", xmin = 5.5, xmax = 6.5, ymin = 0.5, ymax = 0.6, alpha = 0.2) +
    annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 0.6, ymax = 0.7, alpha = 0.2) +
    annotate("rect", xmin = 7.5, xmax = 8.5, ymin = 0.7, ymax = 0.8, alpha = 0.2) +
    annotate("rect", xmin = 8.5, xmax = 9.5, ymin = 0.8, ymax = 0.9, alpha = 0.2) +
    annotate("rect", xmin = 9.5, xmax = 10.5, ymin = 0.9, ymax = 1, alpha = 0.2) +
    scale_x_continuous(name='Predicted probability of True',breaks = seq(1,10,by=1), labels = levels(dplot1$prob_cut)) +
    scale_y_continuous(name='Proportion True',breaks = seq(0,1,by=0.1)) +
    theme_bw(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45,hjust=1)
    )
  
  return(list(pl1,pl2))
}

# get Brier scores
calculateBrierScoresOLD = function(dat_transformed){
  
  d = dat_transformed[,c('item', 'binary_outcome','task_type','avgprob','avgextrem_1','MP', 'KW','MPW','SOA','t_2')]
  d = melt(d, id.vars = c('item', 'binary_outcome','task_type'))
  
  colnames(d) = c('item','binary_outcome','task_type','method','value')
  d$method = recode(d$method, 'avgprob' = 'average','avgextrem_1'='extrem.average','MP' = 'min.pivot', 'KW' = 'know.weight','MPW' = 'meta.prob.weight','SOA' = 'surp.overshoot', 't_2' = 'robust.recalibr')
  d$method = factor(d$method,c('average','extrem.average','min.pivot','know.weight','meta.prob.weight','surp.overshoot','robust.recalibr'))
 
  d$brierscore = (d$binary_outcome - d$value)^2 
  
  pl_brier = ggplot(d,aes(x=brierscore)) +
    geom_histogram() +
    #geom_vline(xintercept = 0, linetype = 2, color = 'blue') +
    facet_wrap(vars(method),ncol=2) +
    scale_x_continuous(name = 'Brier score',breaks = seq(0,1,by=0.2),labels = seq(0,1,by=0.2)) +
    coord_cartesian(xlim = c(0,1)) +
    #scale_y_discrete(limits = c(0,70)) +
    theme_bw(base_size = 14)
  
}
calculateBrierScores = function(dat_transformed){
  
  d = dat_transformed[,c('item', 'binary_outcome','task_type','MP', 'KW','MPW','SOA','t_2')]
  d = melt(d, id.vars = c('item', 'binary_outcome','task_type'))
  
  colnames(d) = c('item','binary_outcome','task_type','method','value')
  d$method = recode(d$method, 'MP' = 'min.pivot', 'KW' = 'know.weight','MPW' = 'meta.prob.weight','SOA' = 'surp.overshoot', 't_2' = 'robust.recalibr')
  d$method = factor(d$method,c('min.pivot','know.weight','meta.prob.weight','surp.overshoot','robust.recalibr'))
  
  d$brierscore = (d$binary_outcome - d$value)^2 
  
  pl_brier = ggplot(d,aes(x=brierscore)) +
    geom_histogram() +
    #geom_vline(xintercept = 0, linetype = 2, color = 'blue') +
    facet_wrap(vars(method),ncol=2) +
    scale_x_continuous(name = 'Brier score',breaks = seq(0,1,by=0.2),labels = seq(0,1,by=0.2)) +
    coord_cartesian(xlim = c(0,1)) +
    #scale_y_discrete(limits = c(0,70)) +
    theme_bw(base_size = 14)
  
  return(pl_brier)
}


################----------READ RAW DATA & CONSTRUCT DATA SET--------------################

##### General knowledge data
dat_gk = constructGeneralKnowledgeData()

#### State capital data
dat_sc = constructStateCapitalData()

#### New data
dat_new = getNewData()

#### Combine data sets
dat = combineDatasets(dat_gk,dat_sc,dat_new)
rm(dat_gk,dat_sc,dat_new)

####################------------------MAIN--------------################

# calculate prob aggregates
dat = getSimpleAggregates(dat)
dat = runMinimalPivoting(dat)
dat = runKnowledgeWeighted(dat)
dat = runSOAlgorithm(dat)
dat = runMPW(dat)

# get estimated priors
dat2 = getEstimatedPrior(dat)
pdf(file = "./estprior.pdf",width=7.5,height=5)
plotEstPriors(dat2)
dev.off()
dat3 = updateEstimatedPrior(dat2)

# table average vs est prior by state (Table 1)
table_avgvsprior = tabulateAvgVsPrior(dat3)

# plot average forecasts in each state'
pl_avgpred = plotAverages(dat3)
pdf(file = "./wrongsided.pdf",width=6.4,height=4)
pl_avgpred[[1]]
dev.off()
pdf(file = "./avgpred.pdf",width=6,height=6)
pl_avgpred[[2]]
dev.off()

# Transformations t1 and t2
dat_transformed = transformAvgProb(dat3)

# Calibration curve
pl_calibr = calibrationPlotT(dat_transformed)
pdf(file = "./calibr1.pdf",width=7,height=6.4)
pl_calibr[[1]]
dev.off()
pdf(file = "./calibr2.pdf",width=6.4,height=4)
pl_calibr[[2]]
dev.off()

# Brier scores 
pdf(file = "./brier.pdf",width=6,height=6)
calculateBrierScores(dat_transformed)
dev.off()

