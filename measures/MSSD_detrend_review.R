# Estimate rMSSD
#####################################
############# Appendix ##############
# 0. Load packages & data
# 1. Detrend data
# 2. Estimate rMSSD
#####################################

#################################
# 0. Load packages & data
#################################

# load packages
library(psych)
library(dplyr)

# load simulated data
df.LSME = read.csv("~\\VarReview\\Code\\sim_data.csv")

################################
# 1. Detrend data
################################

# Detrend the data
df.LSME <- df.LSME %>%
  group_by(subject) %>%
  mutate(
    trend = predict(lm(y ~ time)),      # Fit a linear model and get the trend
    y_detrend = y - trend             # Subtract the trend from the observed y
  ) %>%
  ungroup()                             

################################
# 2. Estimate MSDD
################################

# Create data frame to store MSSD for each subject
subject_MSSD_detrended <- data.frame(subject = factor(), MSSD_detrend = numeric())

# Create vector with subject ids
sub_id <- unique(df.LSME$subject)

# Loop over each subject
for (subj in sub_id) {
  # Filter the rows for current subject
  subject_data <- df.LSME[df.LSME$subject == subj,]
  
  # Calculate the MSSD of y_detrend for current subject
  MSSD_value <- psych::mssd(subject_data$y_detrend, na.rm = TRUE)
  
  # Append the subject and its MSSD to the results data frame
  subject_MSSD_detrended <- rbind(subject_MSSD_detrended, data.frame(subject = subj, MSSD_detrend = MSSD_value))
}
