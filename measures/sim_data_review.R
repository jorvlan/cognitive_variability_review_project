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
tpoints = 50 # number of trials
ndays = 1 # number of days

####################################################################################
#1. Data generating model (Standard DSEM)
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
  
  # Specify model
  y = rep(0,N)
  y[1] = beta1 
  for(i in 2:N){
    y[i] = rnorm(1, mean = beta1 + subj1.tau[i] + (time[i] - mtime)*(beta2 + subj2.tau[i]) + (y[i-1] - (wave.loc[i] + beta1 + subj1.tau[i]))*(beta3 + subj3.tau[i]) + wave.loc[i], 
                 exp(alpha0 + subj4.tau[i] + (time[i] - mtime)*(alpha1 + subj5.tau[i]) + wave.scale[i]))
  }
  
  # Create data frame
  df.LSME <- data.frame(subject = subject, wave = wave, time = time,
                        y = y)
  
  # Save true values in a vector

    gtruth <- c(beta1, beta2, beta3, 
                cor.subj_tau[1:4,1:4],  alpha0,
                sd.subj_tau[1:4])
  
  return(list(df.LSME = df.LSME, gtruth = gtruth))
}

#########################################################################
# Simulate data for n repetitions
#########################################################################

  # Run function
  output <- DataGen(nsubj = npeople, nwave = ndays, nobs = tpoints, # subjects, waves, trials
                    # Fixed effects
                    beta1 = 0.5, # group mean
                    beta2 = 0.00056,# group trend 0.000475
                    beta3 = 0.2, # group ar-1
                    alpha0 = -0.2, # group sigma (residual SD (innovations))
                    alpha1 = 0, # group trend on residuals
                    # Random effects
                    sd.subj_tau = c(0.26, 0.00081, 0.2, 0.141, 0.0001), # mean sd, trend sd, ar1 sd trend(0.0841)
                    sd.wave_loc = 0, # day-to-day sd in means
                    sd.wave_scale = 0, # day-to-daysd in sigma (trialvar)
                    cor.subj_tau = matrix(c(1.000, 0.662, 0.445, 0.609, 0.000,
                                            0.662, 1.000, 0.396, 0.563, 0.000,
                                            0.445, 0.396, 1.000, 0.305, 0.000,
                                            0.609, 0.563, 0.305, 1.000, 0.000,
                                            0.000, 0.000, 0.000, 0.000, 1.000),
                                          nrow = 5,
                                          byrow = TRUE)) # correlation matrix for scale trial-level estimates
  
  
  df.LSME = output$df.LSME
  gtruth = output$gtruth
  # add name labels to gtruth values
    name_vars <- c(
      "alpha", "beta", "phi",
      # Generate rho_u[1,1:10] to rho_u[10,1:10] labels
      with(expand.grid(i = 1:4, j = 1:4), paste0("rho_u[", j, ",", i, "]")),
      "sigma",
      # Generate tau_u[1:10] labels
      paste0("tau_u[", 1:4, "]")
    )
    gtruth[c(2,22)] <- gtruth[c(2,22)] * length(unique(df.LSME$time)) 
  
  # create dataframe for gtruth
  gtruth = tibble(
    Parameter = name_vars,
    Value = gtruth
  )
  
  gtruth = gtruth %>% dplyr::rename(".variable" = "Parameter")
  
  ###########################################
  # Preprocessing
  ###########################################
  
  #2-level model
  # Create lagged outcome for AR1
  df.LSME = df.LSME %>% 
    group_by(subject, wave) %>% 
    mutate(Y_tm1 = lag(y)) %>%
    ungroup %>%
    group_by(subject) %>%
    mutate(Y_tm1 = if_else(is.na(Y_tm1), mean(Y_tm1, na.rm=TRUE), Y_tm1)) %>%
    # mutate(Y_tm1 = Y_tm1 - mean(Y_tm1)) %>%
    ungroup()
  
  #3-level model
  df.LSME = df.LSME %>% 
    group_by(subject, wave) %>% 
    mutate(Y_tm1d = lag(y)) %>%
    mutate(Y_tm1d = if_else(is.na(Y_tm1d), mean(Y_tm1d, na.rm=TRUE), Y_tm1d)) %>%
    #mutate(Y_tm1d = Y_tm1d - mean(na.omit(Y_tm1d))) %>%
    ungroup()
  
  # Create within-person centered time
  df.LSME = df.LSME %>%
    group_by(subject) %>%
    mutate(ctime = time/100) %>%
    mutate(mtime = mean(ctime)) %>%
    ungroup()
  
  l2time = df.LSME$mtime[which(df.LSME$time == "1")]  
  
  write.csv(df.LSME, "~\\VarReview\\Code\\sim_data.csv", row.names = FALSE) # write data file
