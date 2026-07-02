# Estimate iSD
#####################################
############# Appendix ##############
# 0. Load packages & data
# 2. Estimate iSD
#####################################

#################################
# 0. Load packages & data
#################################

# load simulated data
df.LSME = read.csv("~\\VarReview\\Code\\sim_data.csv")

################################
# 1. Estimate iSD
################################

# Create data frame to store iSD for each subject
subject_iSD <- data.frame(subject = factor(), iSD = numeric())

# Create vector with subject ids
sub_id <- unique(df.LSME$subject)

# Loop over each subject
for (subj in sub_id) {
  # Filter the rows for the current subject
  subject_data <- df.LSME[df.LSME$subject == subj,]
  
  # Calculate the standard deviation of y for the current subject
  iSD_value <- sd(subject_data$y, na.rm = TRUE)
  
  # Append the subject and its iSD to the results data frame
  subject_iSD <- rbind(subject_iSD, data.frame(subject = subj, iSD = iSD_value))
}
