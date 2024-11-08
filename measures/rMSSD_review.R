# Estimate rMSSD
#####################################
############# Appendix ##############
# 0. Load packages & data
# 2. Estimate rMSSD
#####################################

#################################
# 0. Load packages & data
#################################

# load packages
library(psych)

# load simulated data
df.LSME = read.csv("~\\VarReview\\Code\\sim_data.csv")

################################
# 1. Estimate MSDD
################################

# Create data frame to store rMSSD for each subject
subject_rMSSD <- data.frame(subject = factor(), rMSSD = numeric())

# Create vector with subject ids
sub_id <- unique(df.LSME$subject)

# Loop over each subject
for (subj in sub_id) {
  # Filter the rows for the current subject
  subject_data <- df.LSME[df.LSME$subject == subj,]
  
  # Calculate the standard deviation of y for the current subject
  rMSSD_value <- psych::rmssd(subject_data$y, na.rm = TRUE)
  
  # Append the subject and its iSD to the results data frame
  subject_rMSSD <- rbind(subject_rMSSD, data.frame(subject = subj, rMSSD = rMSSD_value))
}
