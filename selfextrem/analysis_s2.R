########### Study 2

### libraries
library(ggplot2)
library(dplyr)
library(reshape2) 
library(mfx)
library(texreg)
library(miceadds)
library(jcolors)
library(estimatr)

############## Functions...

## ...to read and prepare raw data for analysis
readData = function(){
  dat = read.csv(paste0(getwd(),'/data/study2.csv'), header = TRUE)
  dat_nonexp = read.csv(paste0(getwd(),'/data/study2_nonexp.csv'), header = TRUE)
  return(list(dat,dat_nonexp))
}
calculateMeasures = function(dat_temp){
  
  dat_temp$exp_cond = factor(dat_temp$exp_cond , levels = c('individual','crowd10','contest10'))
  
  dat_temp = dat_temp[!is.na(dat_temp$imputed_omega),]
  
  dat_temp$theta = 100*dat_temp$prob_heads
  dat_temp$US_citizen = ifelse(dat_temp$Nationality == 'United States',1,0)
  
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
  
  return(dat_temp)
} 
calculateMeasuresNonExp= function(dat_temp){
  # preparing non-expert data
  dat_temp$theta = 100*dat_temp$prob_heads
  dat_temp$US_citizen = ifelse(dat_temp$Nationality == 'United States',1,0)
  return(dat_temp)
} 

## ...for the analysis on extremizing adjustments
calculateSharedInfoDiff = function(dat2){
  # calculate shared-information gap
  dat2$sharedinfo_diff = abs(dat2$xi_shared - dat2$theta)
  return(dat2)
}
generateNonExpPlot = function(dat){
  # generate Figure B1
  
  dat2 = dat[,c('xi','xi_shared')]
  dat2$xi_diff = dat2$xi - dat2$xi_shared
  
  # plot
  plot1 = ggplot(dat=dat2, aes(x=xi_diff, y=..density..)) +
    geom_histogram(color='black',fill='lightgreen') +
    geom_vline(aes(xintercept=0),linetype='dashed') +
    scale_x_continuous(name = 'Difference between layperson prediction and shared signal') +
    scale_y_continuous(name = 'density') +
    theme_bw(base_size = 14)
  
  return(plot1)
}
generateExtremAdjPlot = function(dat2){
  # function to generate Figure 4
  # determine shared-info gap bins
  dat3 = as.data.frame(
    dat2 %>%
      mutate(
        sharedinfo_diff_bin = cut(sharedinfo_diff, breaks = c(seq(0,30,by=5),45), include.lowest = TRUE)
      )
  )
  
  # calculate average extremizing adjustment per bin
  dat4 = as.data.frame(
    dat3 %>%
      group_by(sharedinfo_diff_bin, exp_cond) %>%
      summarize(avg_extrem_adj = mean(extrem_adj))
  )
  colnames(dat4) = c('sharedinfo_diff_bin','condition','avg_extrem_adj')
  
  # Figure 4
  my_palette = as.vector(c(jcolors(palette = 'pal2')[1],jcolors(palette = 'pal9')[c(2,4)]))
  plot1 = ggplot(dat4, aes(x=as.numeric(sharedinfo_diff_bin),y=avg_extrem_adj,color = condition, shape = condition)) +
    geom_point(size=3) +
    geom_line(size=1) +
    scale_color_manual(values=my_palette) +
    scale_shape_manual(values=c(15,19,17)) +
    scale_x_continuous(name = 'Range of shared-information gap', breaks=1:7, labels = levels(dat4$sharedinfo_diff_bin)) +
    scale_y_continuous(name = 'Average extremizing adjustment') +
    theme_bw(base_size = 14)
  
  return(plot1)
}
runExtremAdjRegression = function(dat2){
  
  # run regressions in Table 2 (models 1,2,3 and 4)
  # also conduct pairwise tests (linear hypotheses)
  
  dat3 = dat2
  linmod1 = lh_robust(extrem_adj ~ exp_cond, data = dat3, clusters = dat3$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = 'exp_condcrowd10 - exp_condcontest10 = 0')
  linmod2 = lh_robust(extrem_adj ~ exp_cond + Gender + age + US_citizen, data = dat3, clusters = dat3$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = 'exp_condcrowd10 - exp_condcontest10 = 0')
  
  
  dat4 = dat3[dat3$quiz_correct == 1 & dat3$clarity <= 3,]
  linmod3 = lh_robust(extrem_adj ~ exp_cond, data = dat4, clusters = dat4$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = 'exp_condcrowd10 - exp_condcontest10 = 0')
  linmod4 = lh_robust(extrem_adj ~ exp_cond + Gender + age + US_citizen, data = dat4, clusters = dat4$PROLIFIC_PID, se_type = 'stata', linear_hypothesis = 'exp_condcrowd10 - exp_condcontest10 = 0')
  
  return(list(linmod1,linmod2,linmod3,linmod4))
  
}
tabulateRegResults = function(regr_results){
  # constructs Table 2 from regression results
  for(i in 1:length(regr_results)){
    regr_results[[i]] = regr_results[[i]]$lm_robust
  }
  res_table = texreg(regr_results, include.ci = FALSE, stars = c(0.01,0.05,0.1))
  return(res_table)
  
}


################# MAIN

###### Read and prepare data
dat_all = readData()
# expert and non-expert data
dat = dat_all[[1]]
dat_nonexp = dat_all[[2]]
rm(dat_all)

dat = calculateMeasures(dat) 
dat_nonexp = calculateMeasuresNonExp(dat_nonexp)

#### generate non-expert plot (Figure B1)
plt_nonexpert = generateNonExpPlot(dat_nonexp)

######### Analysis on extremizing adjustment
dat2 = calculateSharedInfoDiff(dat)
#### Generate Figure 4
plt_extremadj = generateExtremAdjPlot(dat2)
#### obtain regression results and generate Table 2
reg_extremeadj = runExtremAdjRegression(dat2)
table_reg_extremeadj = tabulateRegResults(reg_extremeadj)


