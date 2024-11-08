# Estimate MSSD
#####################################
############# Appendix ##############
# 0. Load packages & data
# 1. Estimate MSSD
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

# Create data frame to store MSSD for each subject
subject_MSSD <- data.frame(subject = factor(), MSSD = numeric())

# Create vector with subject ids
sub_id <- unique(df.LSME$subject)

# Loop over each subject
for (subj in sub_id) {
  # Filter the rows for the current subject
  subject_data <- df.LSME[df.LSME$subject == subj,]
  
  # Calculate the standard deviation of y for the current subject
  MSSD_value <- psych::mssd(subject_data$y, na.rm = TRUE)
  
  # Append the subject and its iSD to the results data frame
  subject_MSSD <- rbind(subject_MSSD, data.frame(subject = subj, MSSD = MSSD_value))
}
