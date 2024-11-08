# Estimate iCV
#####################################
############# Appendix ##############
# 0. Load packages & data
# 2. Estimate iCV
#####################################

#################################
# 0. Load packages & data
#################################

# load packages
library(dplyr)

# load simulated data
df.LSME = read.csv("~\\VarReview\\Code\\sim_data.csv")

################################
# 1. Estimate iCV
################################

# Calculate CV for each subject
df_iCV <- df.LSME %>%
  group_by(subject) %>%
  summarize(
    iMU = mean(y, na.rm = TRUE),
    iSD = sd(y, na.rm = TRUE),
    iCV = (iSD/iMU) # Calculate CV as a percentage
  )
