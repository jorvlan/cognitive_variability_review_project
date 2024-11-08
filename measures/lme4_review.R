# Estimate residual SD from lm
#####################################
############# Appendix ##############
# 0. Load packages & data
# 1. Estimate residual SD from lm
#####################################

# load packages
library(lme4)
library(tidyr)
library(effects)
library(broom)

# load simulated data
df.LSME = read.csv("~\\VarReview\\Code\\sim_data.csv")

####################################
# 1. Estimate residual SD from lm
####################################

#lmer model
# Create a vector with unique subject IDs
sub_id <- unique(df.LSME$subject)
twolevel = list() # create list to store estimates

# run single-case DSEM for each subject
for (subj in sub_id){
  
  # filter rows for each subject
  subject_data <- df.LSME %>% filter(subject == subj)
  
  # estimate linear regression
  twolevel[[subj]] = lm(y ~ 1 + ctime + Y_tm1, 
                           data=subject_data)
  print(sub_id[subj])
}

# put effects into list
effects = list()
for (subj in sub_id){
  # extract variance/std components from random effects table for each ID3
  effects[[subj]] = as.data.frame(tidy(twolevel[[subj]]))
}

#create a dataframe for each estimate
intercept = data.frame()
trend = data.frame()
ar1 = data.frame()
trialsd = data.frame()

for (subj in sub_id){
  # standard deviation
  intercept[subj,1] = effects[[subj]]$estimate[1]      
  trend[subj,1] = effects[[subj]]$estimate[2]
  ar1[subj,1] = effects[[subj]]$estimate[3]
  trialsd[subj,1] = summary(twolevel[[subj]])$sigma
}
