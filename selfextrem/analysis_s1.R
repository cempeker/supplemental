########### Study 1

### libraries
library(ggplot2)
library(dplyr)
library(reshape2) 
library(mfx)
library(texreg)
library(miceadds)
library(jcolors)
library(estimatr)

######### Functions...

## ...to read and prepare raw data for analysis
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
  
  return(dat_temp)
}

## ...to generate imputed weight plot (Figure 2 in the manuscript)
generateImputedWeightPlot = function(dat){
  
  dat2 = dat[,c('exp_cond','imputed_omega')]
  
  # calculate frequency by bins of imputed weight (omega)
  dat2 = as.data.frame(
    dat2 %>%
      mutate(
        omega_bin = cut(imputed_omega, breaks = c(-0.0001,0.0001,0.25,0.4999,0.5001,0.75,0.9999,1.0001),labels = c('0','(0,0.25]','[0.25,0.5)','0.5','(0.5,0.75]','(0.75,1)','1')) )
  )
  
  dat2 = as.data.frame(
    dat2 %>%
      group_by(exp_cond) %>%
      mutate(
        omega_count_all = length(imputed_omega)
      )
  )
  
  dat3 = as.data.frame(
    dat2 %>%
      group_by(exp_cond,omega_bin) %>%
      summarize(
        omega_perc = 100*length(imputed_omega)/omega_count_all,
      )
  )
  dat3 = unique(dat3)
  
  
  # generate imputed weight plot
  my_palette = as.vector(jcolors(palette = 'pal2')[1:4])
  pl1 = ggplot(dat3, aes(x=omega_bin,y=omega_perc,fill = exp_cond)) +
    geom_bar(stat='identity',position = 'dodge') +
    scale_fill_manual(values=my_palette) +
    scale_x_discrete(name = 'imputed weight') +
    scale_y_continuous(name = 'percentage of reports') +
    theme_bw(base_size = 14) + 
    labs(fill = "condition")
  
  return(pl1)
  
}

## ...for the analysis on extremizing adjustments 
calculateSharedInfoGap = function(dat2){
  # calculate shared-information gap
  dat2$sharedinfo_diff = abs(dat2$xi_shared - dat2$theta)
  return(dat2)
}
generateExtremAdjPlot = function(dat2){
  # function to generate Figure 3
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
  
  # Figure 3
  my_palette = as.vector(jcolors(palette = 'pal2')[1:4])
  plot1 = ggplot(dat4, aes(x=as.numeric(sharedinfo_diff_bin),y=avg_extrem_adj,color = condition, shape = condition)) +
    geom_point(size=3) +
    geom_line(size=1) +
    scale_color_manual(values=my_palette) +
    scale_shape_manual(values=c(15,16,17,8)) +
    scale_x_continuous(name = 'Range of shared-information gap', breaks=1:7, labels = levels(dat4$sharedinfo_diff_bin)) +
    scale_y_continuous(name = 'Average extremizing adjustment') +
    theme_bw(base_size = 14)
  
  return(plot1)
}
runExtremAdjRegression = function(dat2){
  
  # run regressions in Table 1 (models 1,2,3 and 4)
  # also conduct pairwise tests (linear hypotheses)
  
  dat3 = dat2
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

#### Generate Figure 2 in the manuscript
plt_imputedweight = generateImputedWeightPlot(dat)

########## Analysis on extremizing adjustment
dat2 = calculateSharedInfoGap(dat)
#### Generate Figure 3
plt_extremadj = generateExtremAdjPlot(dat2)
#### obtain regression results and generate Table 1
reg_extremeadj = runExtremAdjRegression(dat2)
table_reg_extremeadj = tabulateRegResults(reg_extremeadj)







