####################################################################################
#0. Install & load packages
####################################################################################

#Load packages
library(MASS)
library(dplyr)
library(cmdstanr)
library(beepr)
library(lme4)
library(posterior)
library(bayesplot)
library(ggplot2)
library(kableExtra)
library(tidybayes)

###################################################################################
#0.1 Top level changes
###################################################################################

# DSEM characteristics
npeople = 100 # number of subject
tpoints = 100 # number of trials #was 50
ndays = 1 # number of days

# TREND VALUES DEPEND ON tpoints. ctime = time/tpoints, so a coefficient on
# centred ctime equals beta2 * tpoints. Under RDSEM there is no 1/(1-phi)
# inflation any more, so these are the marginal trend directly:
#
#   tpoints  50 :  beta2 = 0.01000   sd.subj_tau[2] = 0.00800
#   tpoints 100 :  beta2 = 0.00500   sd.subj_tau[2] = 0.00400   <- used below
#   tpoints 150 :  beta2 = 0.00333   sd.subj_tau[2] = 0.00267
#
# All three give a trend of about 0.6 process SDs across the block.

####################################################################################
#1. Data generating model (RDSEM: AR on the residuals)
####################################################################################

DataGen <- function(nsubj, nwave, nobs,
                    beta1, beta2, beta3,
                    alpha0, alpha1,
                    sd.subj_tau,
                    sd.wave_loc, sd.wave_scale,
                    cor.subj_tau = matrix(c(1.000, 0.662, 0.445, 0.609, 0.000,
                                            0.662, 1.000, 0.396, 0.563, 0.000,
                                            0.445, 0.396, 1.000, 0.305, 0.000,
                                            0.609, 0.563, 0.305, 1.000, 0.000,
                                            0.000, 0.000, 0.000, 0.000, 0.000),
                                          nrow = 5,
                                          byrow = TRUE)){
  
  ## Generate subject wave and time indicators
  N <- nsubj * nwave * nobs
  subject <- gl(n = nsubj, k = nwave * nobs)
  wave <- rep(gl(n = nwave,k = nobs), nsubj)
  time <- rep(seq_len(nwave*nobs), nsubj)
  mtime <- mean(time)
  
  ## Generate subject, wave level random effects
  # Define correlation matrix
  chol.subj_tau <- t(chol(cor.subj_tau))
  sigma.sd.subj_tau <- diag(sd.subj_tau) %*% chol.subj_tau
  subj_tau <- mvrnorm(nsubj, mu = c(0, 0, 0, 0, 0), 
                      Sigma = sigma.sd.subj_tau %*% t(sigma.sd.subj_tau))
  # Define subject-level random effects
  subj1.tau <- rep(subj_tau[, 1], each = nwave * nobs)
  subj2.tau <- rep(subj_tau[, 2], each = nwave * nobs)
  subj3.tau <- rep(subj_tau[, 3], each = nwave * nobs)
  subj4.tau <- rep(subj_tau[, 4], each = nwave * nobs)
  subj5.tau <- rep(subj_tau[, 5], each = nwave * nobs)
  
  # Define wave level random effects
  wave_loc <- rnorm(nwave, mean = 0, sd = sd.wave_loc)
  wave_scale <- rnorm(nwave, mean = 0, sd = sd.wave_scale)
  wave.loc <- rep(rep(wave_loc, each = nobs), nsubj)
  wave.scale <- rep(rep(wave_scale, each = nobs), nsubj)
  
  # Specify model (restart AR for each subject)
  # RDSEM: the carry-over acts on the deviation from the subject's OWN trend
  # line, so the lag is measured from mu at the PREVIOUS occasion.
  first <- c(TRUE, subject[-1] != subject[-N])
  y <- numeric(N)
  for (i in 1:N) {
    mu_i  <- beta1 + subj1.tau[i] + wave.loc[i]
    b_i   <- beta2 + subj2.tau[i]
    phi_i <- beta3 + subj3.tau[i]
    sd_i  <- exp(alpha0 + subj4.tau[i] + (time[i] - mtime) * (alpha1 + subj5.tau[i]) + wave.scale[i])
    
    mu_now <- mu_i + (time[i] - mtime) * b_i
    
    if (first[i]) {
      # stationary start for each subject
      y[i] <- rnorm(1, mu_now, sd_i / sqrt(1 - phi_i^2))
    } else {
      # predicted value at the previous occasion, same subject
      mu_prev <- beta1 + subj1.tau[i] + wave.loc[i - 1] + (time[i - 1] - mtime) * b_i
      y[i] <- rnorm(1, mu_now + (y[i - 1] - mu_prev) * phi_i, sd_i)
    }
  }
  
  
  # Create data frame
  df.LSME <- data.frame(subject = subject, wave = wave, time = time,
                        y = y)
  
  # Save true values in a vector
  
  gtruth <- c(beta1, beta2, beta3, 
              cor.subj_tau[1:4,1:4],  alpha0,
              sd.subj_tau[1:4])
  
  return(list(df.LSME = df.LSME, gtruth = gtruth, subj_tau = subj_tau))
}

#########################################################################
# Simulate data for n repetitions
#########################################################################

set.seed(2094)

# Run function
output <- DataGen(nsubj = npeople, nwave = ndays, nobs = tpoints, # subjects, waves, trials
                  # Fixed effects
                  beta1 = 0.5, # group mean
                  beta2 = 0.005,# group trend   (rescaled for tpoints = 100)
                  beta3 = 0.5, # group ar-1
                  alpha0 = -0.2, # group sigma (residual SD (innovations))
                  alpha1 = 0, # group trend on residuals
                  # Random effects
                  sd.subj_tau = c(0.26, 0.004, 0.11, 0.141, 0.0001), # [2] rescaled for tpoints = 100
                  sd.wave_loc = 0, # day-to-day sd in means
                  sd.wave_scale = 0, # day-to-daysd in sigma (trialvar)
                  cor.subj_tau = matrix(c(1.000, 0.662, 0.445, 0.609, 0.000,
                                          0.662, 1.000, 0.396, 0.563, 0.000,
                                          0.445, 0.396, 1.000, 0.305, 0.000,
                                          0.609, 0.563, 0.305, 1.000, 0.000,
                                          0.000, 0.000, 0.000, 0.000, 1.000),
                                        nrow = 5,
                                        byrow = TRUE)) # correlation matrix for scale trial-level estimates


df.LSME  = output$df.LSME
subj_tau = output$subj_tau                                         

# Stop if a subject drew |phi_i| >= 1 (stationary start becomes NaN) 
if (any(!is.finite(df.LSME$y)))
  stop(sum(!is.finite(df.LSME$y)), " non-finite values in y: a subject drew |phi_i| >= 1. ",
       "Use another seed or lower sd.subj_tau[3].", call. = FALSE)

###########################################
# Add measurement error
###########################################

sd_meas <- 0.5
df.LSME$y_true <- df.LSME$y
df.LSME$y <- df.LSME$y + rnorm(n = nrow(df.LSME), mean = 0, sd = sd_meas)

###########################################
# True values
###########################################

gtruth = output$gtruth
name_vars <- c(
  "alpha", "beta", "phi",
  with(expand.grid(i = 1:4, j = 1:4), paste0("rho_u[", j, ",", i, "]")),
  "sigma",
  paste0("tau_u[", 1:4, "]")
)
# rescale trend and trend SD to the ctime scale (ctime = time / tpoints)
gtruth[c(2, 22)] <- gtruth[c(2, 22)] * length(unique(df.LSME$time))

gtruth    <- c(gtruth, log(sd_meas))
name_vars <- c(name_vars, "log_sd_meas")

gtruth = tibble(.variable = name_vars, Value = gtruth)

###########################################
# Preprocessing
###########################################

# Lagged outcome (only needed for the standard DSEM comparison)
df.LSME = df.LSME %>%
  group_by(subject, wave) %>%
  mutate(Y_tm1 = lag(y)) %>%
  ungroup() %>%
  group_by(subject) %>%
  mutate(Y_tm1 = if_else(is.na(Y_tm1), mean(Y_tm1, na.rm = TRUE), Y_tm1)) %>%
  ungroup()

# Rescaled time and each subject's mean time
df.LSME = df.LSME %>%
  group_by(subject) %>%
  mutate(ctime = time / length(unique(df.LSME$time))) %>%
  mutate(mtime = mean(ctime)) %>%
  ungroup()

# First trial per subject
df.LSME = df.LSME %>%
  group_by(subject) %>%
  mutate(first = as.integer(row_number() == 1)) %>%
  ungroup()

###########################################
# Save
###########################################

write.csv(df.LSME, file.path("sim_data.csv"), row.names = FALSE)
saveRDS(gtruth,   file.path("gtruth.rds"))                 
saveRDS(subj_tau, file.path("subj_tau.rds")) 
