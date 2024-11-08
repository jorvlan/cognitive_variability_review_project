# Ex-Gaussian Tau
####################

library(dplyr)
library(ExGaussEstim)

# load simulated data
df.LSME = read.csv("~\\VarReview\\Code\\sim_data.csv")
df.LSME$y = df.LSME$y + 20
################################
# 1. Estimate exGauss tau
################################

# Create a data frame to store the ex-Gaussian parameters for each subject
subject_exGauss <- data.frame(subject = character(),
                              mu = numeric(),
                              sigma = numeric(),
                              tau = numeric(),
                              stringsAsFactors = FALSE)

# Create a vector with unique subject IDs
sub_id <- unique(df.LSME$subject)

# Loop over each subject
for (subj in sub_id) {
  tryCatch({
  # Filter the rows for the current subject
  subject_data <- df.LSME %>% filter(subject == subj)
  
  # Estimate ex-Gaussian parameters using BayesianExgaussian
  fit <- BayesianExgaussian(n = 50, x = subject_data$y, nSamples = 1000, Ti = 500)
  
  # Extract the estimated parameters (mu, sigma, tau)
  mu <- fit$mu
  sigma <- fit$sigma
  tau <- fit$tau
  
  # Store the results in the data frame
  subject_exGauss <- rbind(subject_exGauss, data.frame(subject = subj, mu = mu, sigma = sigma, tau = tau))
  
}, error = function(e) {
  # Display a message if there is an error
  message("Skipping subject ", subj, ": ", e$message)
})
}


