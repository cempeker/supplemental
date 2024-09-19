########### PPM week 0 - calculates last week's % True and False for week 1 survey


# libraries 
library(reshape2)
library(dplyr)
library(stringr)

# Read data
dat = read.csv('data/rawdata/s2_qualtrics_week0.csv', header = TRUE)
dat = dat[-(1:2),]

# get choice data columns. Column names in the raw data represent questions (X1,X2,...,X9) and alternatives (once or more, twice or more, ... 5 times or more). To illustrate, x1_q_plain_1 = question 1 & "once or more", x1_q_plain_2 = question 1 & "twice or more", x1_q_plain_3 = question 1 & "3 times or more" etc. Columns will be renamed below for clarification
question_cols = paste0('X',1:9,'_q_plain_')
alternatives = sort(paste0(question_cols,rep(1:5,9)))
dat = dat[,c('PROLIFIC_PID','exp_cond',alternatives)]
rm(question_cols,alternatives)


# recode responses so that, for each alternative (among once or more, twice or more,...,5 times or more) in each question, 1 if True is picked, 0 if False is picked. 
dat[,3:47] = as.numeric(as.character(unlist(dat[,3:47])))
dat[,3:47] = (-1)*dat[,3:47] + 2

# calculate # of True for each alternative in each question
dat2 = melt(dat,id.vars = c('PROLIFIC_PID','exp_cond'))
dat3 = as.data.frame(
    dat2 %>%
      group_by(variable) %>%
      summarize(true_picks = sum(value))
)
colnames(dat3) = c('question','true_picks')

# Rename columns to clarify
questions = rep(paste0('X',1:9),each=5)
questions = paste0(questions,',',c('once or more','twice or more','3 times or more','4 times or more','5 times or more'))
dat3$question = questions

# calculate % True for each alternative in each question (to be displayed in the Flat-PastRate and PPM conditions in week 1). % False will be 100 - %True
sample_size = nrow(dat)
dat3$percentage_True = round(100*dat3$true_picks/sample_size)
dat3


