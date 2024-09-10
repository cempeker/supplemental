
#### construct_dataset.R

# INPUT: folder "data/rawdata" and its contents
# OUTPUT: data/recalibr_dataset.csv

# Execute the commands in order to construct recalibr_dataset.csv
# recalibr_analysis.R uses.the output of this script to produce the figures & tables in the "Robust recalibration of aggregate probability forecasts using meta-beliefs" by Peker and Wilkening.

# Seting working directory: Check getwd(), the working director should be .../supplemental (it is by default if construct_dataset.R is launched in RStudio). If not, copy the path of the folder and use setwd(path).


#----------LIBRARIES

library(tidyverse)
library(reshape2)


#----------FUNCTIONS

# read Wilkening et al. (2022) data 
getGradesData = function(){
  
  # read raw data, responses to Grades questions
  dataset_names = c('r2a_rawdata','r2b_rawdata','r2c_rawdata','r2d_rawdata','r2e_rawdata')
  dataset_links = paste0('./data/rawdata/wilkening2022/',dataset_names,'.csv')
  dat_gr = NULL
  subject_count = 0
  for(i in 1:5){
    # read raw data, part i
    dat_gr_temp = as.data.frame(read.csv(dataset_links[i], header = FALSE))
    dat_gr_temp$Dataset = i
    dat_gr_temp$subject = subject_count + 1:nrow(dat_gr_temp)
    subject_count = subject_count + nrow(dat_gr_temp)
    
    # select forecast data, rename columns
    colnames(dat_gr_temp) = c(1:400,'Dataset','subject')
    colnames(dat_gr_temp)[seq(1,397,by=4)]= paste0('vote.',100*(i-1) + 1:100)
    colnames(dat_gr_temp)[seq(2,398,by=4)]= paste0('metavote.',100*(i-1) + 1:100)
    colnames(dat_gr_temp)[seq(3,399,by=4)]= paste0('prob.',100*(i-1) + 1:100)
    colnames(dat_gr_temp)[seq(4,400,by=4)]= paste0('metaprob.',100*(i-1) + 1:100)
    
    # wide to long
    dat_gr_temp = melt(dat_gr_temp,id.vars=c('Dataset','subject'))
    dat_gr_temp = dat_gr_temp %>% separate(variable,into=c('report','item'))
    dat_gr_temp$item = paste0('gr',dat_gr_temp$item)
    dat_gr_temp$task_type = 'Grades'
    
    # bind to whole data set(=all 5 parts)
    dat_gr = rbind(dat_gr,dat_gr_temp)
    
    # remove temp vars
    rm(dat_gr_temp)
  }
  
  # question list and outcomes
  dat_gr_questions = read.csv('./data/rawdata/wilkening2022/r2_outcomes.csv',header = FALSE)
  colnames(dat_gr_questions) = c('item','Actual','Difficulty','Question')
  dat_gr_questions$item = paste0('gr',dat_gr_questions$item)
  
  # merge outcome data with response data
  dat_gr = merge(dat_gr,dat_gr_questions)
  colnames(dat_gr)[which(colnames(dat_gr) == 'Actual')] = 'binary_outcome'
  
  rm(dataset_links,dataset_names,dat_gr_questions)
  
  # reshape data, pred and meta-pred in different columns for subsequent analyses
  dat_gr = dcast(dat_gr, item + Dataset + subject + binary_outcome + task_type + Difficulty ~  report, value.var = 'value')
  # make vote=1 for True/Yes, vote=0 for False/No. These non-prob forecasts are not used in the analysis, only to filter out insconsistent forecasts.
  dat_gr$vote = dat_gr$vote - 1
  
  # remove subjects with missing reports if there are any
  dat_gr = dat_gr[complete.cases(dat_gr),]
  
  # prob. forecasts were elicited in %, convert % to probabilities
  dat_gr$prob = 0.01*dat_gr$prob
  dat_gr$metaprob = 0.01*dat_gr$metaprob
  
  # return data
  return(dat_gr)
}
getStateCapitalData = function(){
  
  # read raw data, responses to State Capital questions
  dataset_link = './data/rawdata/wilkening2022/m4data.csv'
  dat_sc <- read.csv(dataset_link, header = FALSE)
  dat_sc$subject = 1:89
  
  # select forecast data, rename columns
  colnames(dat_sc) = c(1:200,'subject')
  colnames(dat_sc)[seq(1,197,by=4)]= paste0('vote.',1:50)
  colnames(dat_sc)[seq(2,198,by=4)]= paste0('metavote.',1:50)
  colnames(dat_sc)[seq(3,199,by=4)]= paste0('prob.',1:50)
  colnames(dat_sc)[seq(4,200,by=4)]= paste0('metaprob.',1:50)
  
  # wide to long
  dat_sc = melt(dat_sc,id.vars=c('subject'))
  dat_sc = dat_sc %>% separate(variable,into=c('report','item'))
  dat_sc$item = paste0('state',dat_sc$item)
  dat_sc$task_type = 'State Capital'
  
  
  # question list and outcomes
  dat_sc_questions = read.csv('./data/rawdata/wilkening2022/m4data_details.csv',header = TRUE)
  colnames(dat_sc_questions)[which(colnames(dat_sc_questions) == 'Question')] = 'item'
  dat_sc_questions$item = paste0('state',dat_sc_questions$item) 
  
  # merge outcome data with response data
  dat_sc = merge(dat_sc,dat_sc_questions[,c('item','Outcome')],by=c('item'))
  colnames(dat_sc)[which(colnames(dat_sc) == 'Outcome')] = 'binary_outcome'
  
  rm(dataset_link, dat_sc_questions)
  
  # reshape data, pred and meta-pred in different columns for subsequent analyses
  dat_sc = dcast(dat_sc, item + subject + binary_outcome + task_type ~  report, value.var = 'value')
  # make vote=1 for True/Yes, vote=0 for False/No. These non-prob forecasts are not used in the analysis, only to filter out insconsistent forecasts.
  dat_sc$vote = dat_sc$vote - 1
  
  # remove subjects with missing reports if there are any
  dat_sc = dat_sc[complete.cases(dat_sc),]
  
  # prob. forecasts were elicited in %, convert % to probabilities
  dat_sc$prob = 0.01*dat_sc$prob
  dat_sc$metaprob = 0.01*dat_sc$metaprob
  
  return(dat_sc)
}

# read Howe et al. (2023) data
getMWHData = function(){
  
  # read raw data
  dataset_link = './data/rawdata/howe2024/mwh_data.csv'
  dat_mwh <- read.csv(dataset_link, header = TRUE)
  
  # Classify questions (tasks)
  dat_mwh$QuestionType = NA
  dat_mwh[dat_mwh$Experiment == '1' & dat_mwh$QuestionNumber <= 50,'QuestionType'] = 'NFL'
  dat_mwh[dat_mwh$Experiment == '1' & dat_mwh$QuestionNumber > 50,'QuestionType'] = 'Science'
  
  dat_mwh[dat_mwh$Experiment == '2' & dat_mwh$QuestionNumber <= 50,'QuestionType'] = 'NFL'
  dat_mwh[dat_mwh$Experiment == '2' & dat_mwh$QuestionNumber > 50 & dat_mwh$QuestionNumber <= 100,'QuestionType'] = 'Science'
  dat_mwh[dat_mwh$Experiment == '2' & dat_mwh$QuestionNumber > 100,'QuestionType'] = 'Scenario'

  dat_mwh[dat_mwh$Experiment == '3a' & dat_mwh$QuestionNumber <= 40,'QuestionType'] = 'Artwork'
  dat_mwh[dat_mwh$Experiment == '3a' & dat_mwh$QuestionNumber > 40 & dat_mwh$QuestionNumber <= 80,'QuestionType'] = 'Science'
  dat_mwh[dat_mwh$Experiment == '3a' & dat_mwh$QuestionNumber > 80,'QuestionType'] = 'Scenario'
  
  dat_mwh[dat_mwh$Experiment == '3b' & dat_mwh$QuestionNumber <= 40,'QuestionType'] = 'Artwork'
  dat_mwh[dat_mwh$Experiment == '3b' & dat_mwh$QuestionNumber > 40 & dat_mwh$QuestionNumber <= 80,'QuestionType'] = 'Science'
  dat_mwh[dat_mwh$Experiment == '3b' & dat_mwh$QuestionNumber > 80,'QuestionType'] = 'Scenario'
  
  # return data
  return(dat_mwh)
  
}

# combine all data sets
combineDatasets = function(dat_gr,dat_sc,dat_mwh){
  
  # combine Grades and States data, rename Grades as "Science" tasks
  dat_gr$Dataset = NULL
  dat_gr$task_type = NA
  dat_gr$task_type = 'Science'
  dat_gr$Difficulty = NULL
  dat_sc$task_type = 'States'
  dat = rbind(dat_gr,dat_sc)
  
  # remove inconsistent responses
  ind = which(dat$vote == 1 & dat$prob < 0.50)
  ind2 = which(dat$vote == 0 & dat$prob > 0.50)
  ind = sort(c(ind,ind2));rm(ind2)
  dat = dat[-ind,]
  rm(ind)
  
  # rename the columns of Howe et al (2023) data and combine all data sets
  dat_mwh$item = paste0(dat_mwh$QuestionType,'.',dat_mwh$Experiment,'.',dat_mwh$QuestionNumber)
  dat_mwh$subject = paste0(dat_mwh$Experiment,'.',dat_mwh$SubjectID)
  colnames(dat_mwh)[which(colnames(dat_mwh) == 'Vote')] = 'vote'
  colnames(dat_mwh)[which(colnames(dat_mwh) == 'Pr_Forecast')] = 'prob'
  colnames(dat_mwh)[which(colnames(dat_mwh) == 'Meta.Prediction')] = 'metaprob'
  colnames(dat_mwh)[which(colnames(dat_mwh) == 'ActualOutcome')] = 'binary_outcome'
  colnames(dat_mwh)[which(colnames(dat_mwh) == 'QuestionType')] = 'task_type'
  dat_mwh$Experiment=NULL
  dat_mwh$QuestionNumber=NULL
  dat_mwh$SubjectID=NULL
  dat_mwh$metavote = NA
  dat = rbind(dat,dat_mwh)
  
  # select probability judgments 
  dat =  dat[dat$task_type %in% c('Science','States','NFL','Artwork'),]
  dat = dat[,c('task_type','item','subject','prob','metaprob','binary_outcome')]
  
  # return data
  return(dat)
}


#----------MAIN

# Wilkening et al. (2022) data 
dat_gr = getGradesData()
dat_sc = getStateCapitalData()

# Howe et al. (2023) data
dat_mwh = getMWHData()

# Combine data sets
dat = combineDatasets(dat_gr,dat_sc,dat_mwh)
rm(dat_gr,dat_sc,dat_mwh)

# Save combined data set in a csv file
write.csv(dat,"./data/recalibr_dataset.csv", row.names=FALSE)


