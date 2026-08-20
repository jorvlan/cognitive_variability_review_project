#---------------------------------------------------------------------------------
### SCRIPT WITH BINARY VARIABILITY FUNCTIONS ###
## ALINE KORVER, 19 JAN 2026

# This is an updated script which includes the functions for the following 
# binary variability measures:

## Autocorrelation;
## Hurst exponent;
## 

# load packages
library(psych)
library(pracma)
library(stats)

#---------------------------------------------------------------------------------
### AUTOCORRELATION ###

# The bi_ac function computes the autocorrelation using ACF(); it returns NA when the 
# time series is shorter than 4, or includes only 1's or 0's

# NOTE that the current functions simply excludes NAs if present.

bi_ac = function(ts) { 
  # define vector to save ACs
  AC = vector()
  
  # loop through rows
  for (row in 1:nrow(ts)) {
    
    # get vectors of values
    vector = na.exclude(as.numeric(ts[row,]))
    
    # check whether vector is not empty and contains both 0's and 1's
    {if (length(vector) > 3 & 1 %in% vector & 0 %in% vector) 
      {AC[row] = acf(vector, plot = F)$acf[2]}
    
    else {AC[row] = NA}}}
  return(AC)}


#---------------------------------------------------------------------------------
### HURST ### 

# The bi_Hu function computes the Hurst exponent using hurstexp, the Hrs variant
# (see ? hurstexp).
# It returns NA when the time series 

# NOTE that the current functions simply excludes NAs if present.

bi_Hu = function(ts) {
  Hu = vector()
  for (row in 1:nrow(ts)) {vector = na.exclude(as.numeric(ts[row,]))
    if (length(vector) > 3 & 1 %in% vector & 0 %in% vector)
  {Hurst = hurstexp(vector, display = F, d=10)
  Hu[row] = Hurst$Hrs} 
    else {Hu[row] = NA}}
  return(Hu)}


#---------------------------------------------------------------------------------
### CORRECT AND ERROR STREAKINESS ### 

# The cor_err_streak function computes the streakiness of correct answers.

# This measure includes a "standardization procedure"; the default of the 
# function is to output the standardized value, but the raw, extra, or 
# range-standardized value can also be outputted using the second argument.

# The correct and error streakiness are computed in the same function, because 
# this makes the computation more efficient. Instead of a vector, this function
# outputs a data frame with two columns for the cor and err streakiness value.

# The default standardized value is both range-standardized and residualized.
# Other options for output are: "raw", "extra", "rangestd"

cor_err_streak <- function(ts, output="standardized") {
  
  # create dataframe with all 4 corstreak versions
  asym_streaks <- data.frame(
    corstreak_raw = rep(NA, nrow(ts)),
    corstreakextra = rep(NA, nrow(ts)),
    corstreak_rangestd = rep(NA, nrow(ts)),
    corstreak = rep(NA, nrow(ts)),
    errstreak_raw = rep(NA, nrow(ts)),
    errstreakextra = rep(NA, nrow(ts)),
    errstreak_rangestd = rep(NA, nrow(ts)),
    errstreak = rep(NA, nrow(ts)))
  
  # compute matrix with maximum probextra values for later
  # standardization
  max_probextra = matrix(ncol = ncol(ts), nrow = ncol(ts))
  colnames(max_probextra) = paste("n_trials", 1:ncol(ts), sep = "_")
  for (n_trials in 1:ncol(ts)) {
    for (n_true in (1:n_trials)) {
      max_probextra[n_true, n_trials] = 1 - (n_true/n_trials)}}
  
  # compute minimum probextra values
  min_probextra = matrix(ncol = ncol(ts), nrow = ncol(ts))
  colnames(min_probextra) = paste("n_trials", 1:ncol(ts), sep = "_")
  for (n_trials in 1:ncol(ts)) {
    for (n_true in (1:n_trials)) {
      if (n_true < (n_trials-n_true)) {
        min_probextra[n_true, n_trials] = 0 - (n_true/n_trials)}
      else { maxblock = min(n_true, n_trials - n_true + 1)
      adj =  n_true - maxblock
      min_probextra[n_true, n_trials] = adj/(n_true-1)-(n_true/n_trials)} }}
  
  max_probextra = as.data.frame(max_probextra)
  min_probextra = as.data.frame(min_probextra)
  
  max_probextra$n_cor = 1:ncol(ts)
  min_probextra$n_cor = 1:ncol(ts)
  
  # compute stuff per row in ts
  for (row in 1:nrow(ts)) {
    vector = na.exclude(as.numeric(ts[row,]))
    
    # compute n_correct and n_incorrect
    n_correct = sum(vector == 1, na.rm = T)
    n_trials = length(vector)
    n_incorrect = n_trials - n_correct
    
    if (length(vector) > 1 & n_correct > 0 & n_incorrect > 1)
      # compare current to next
    {same_next <- vector[-length(vector)] == vector[-1]  
    
    # count nr of succeeding same trials
    nexcor <- sum(vector[-length(vector)] == 1 & same_next)
    nexerr <- sum(vector[-length(vector)] == 0 & same_next)
    
    # compute corstreak_raw if not all correct or all wrong
    if (vector[n_trials] == 1)
    {asym_streaks$corstreak_raw[row] <- if (n_correct > 0) nexcor / (n_correct-1) else NA
    asym_streaks$errstreak_raw[row] <- if (n_incorrect > 0) nexerr / n_incorrect else NA}
    
    if (vector[n_trials] == 0) {asym_streaks$corstreak_raw[row] <- if (n_correct > 0) nexcor / n_correct else NA
    asym_streaks$errstreak_raw[row] <- if (n_incorrect > 0) nexerr / (n_incorrect-1) else NA}
    
    # asymmetric extra probability
    asym_streaks$corstreakextra[row] <- asym_streaks$corstreak_raw[row] - (n_correct / n_trials)
    asym_streaks$errstreakextra[row]  <- asym_streaks$errstreak_raw[row] - (n_trials-n_correct) / n_trials
    
    # compute range-standardized extra probability
    minmax_corrange = max_probextra[n_correct, n_trials] - min_probextra[n_correct, n_trials]
    asym_streaks$corstreak_rangestd[row] = (asym_streaks$corstreakextra[row] - min_probextra[n_correct, n_trials]) / minmax_corrange  
    
    minmax_errrange = max_probextra[n_incorrect, n_trials] - min_probextra[n_incorrect, n_trials]
    asym_streaks$errstreak_rangestd[row] = (asym_streaks$errstreakextra[row] - min_probextra[n_incorrect, n_trials]) / minmax_errrange 
    
    # compute standardized extra probability by adapting rangestd based on distance from mean
    asym_streaks$corstreak[row] = asym_streaks$corstreak_rangestd[row] - ((0 - min_probextra[n_correct, n_trials]) / minmax_corrange)
    asym_streaks$errstreak[row] = asym_streaks$errstreak_rangestd[row] - ((0 - min_probextra[n_incorrect, n_trials]) / minmax_errrange)} 
    
    
    else {asym_streaks$corstreak_raw[row] = NA
    asym_streaks$errstreak_raw[row] = NA
    asym_streaks$corstreakextra[row] = NA
    asym_streaks$errstreakextra[row] = NA
    asym_streaks$corstreak_rangestd[row] = NA
    asym_streaks$errstreak_rangestd[row] = NA
    asym_streaks$corstreak[row] = NA
    asym_streaks$errstreak[row] = NA}}
  
  # make a sel_streaks output data frame based on second argument
  if (output == "standardized")
  {sel_streaks = data.frame(corstreak = asym_streaks$corstreak, errstreak = asym_streaks$errstreak)}
  
  else if (output == "raw")
  {sel_streaks = data.frame(corstreak_raw = asym_streaks$corstreak_raw, errstreak_raw = asym_streaks$errstreak_raw)}
  
  else if (output == "extra")
  {sel_streaks = data.frame(corstreakextra = asym_streaks$corstreakextra, errstreakextra = asym_streaks$errstreakextra)}
  
  else if (output == "rangestd")
  {sel_streaks = data.frame(corstreak_rangestd = asym_streaks$corstreak_rangestd, errstreak_rangestd = asym_streaks$errstreak_rangestd)}
  
  return(sel_streaks)} 


#---------------------------------------------------------------------------------
### MEAN SQUARE SUCCESSIVE DIFFERENCE  ### 

# The bi_mssd function computes the mssd value in binary data using mssd().

# This measure includes a "standardization procedure"; the default of the 
# function is to output the standardized value, but the raw, residualized, or 
# range-standardized value can also be outputted using the second argument.

# The default standardized value is both range-standardized and residualized.
# Other options for output are: "raw", "residualized", "rangestd"

bi_mssd = function(ts, output="standardized") {
  mssd_measures = data.frame(
    mssd_raw = rep(NA, nrow(ts)),
    mssd_rangestd = rep(NA, nrow(ts)),
    mssd_res = rep(NA, nrow(ts)),
    mssd = rep(NA, nrow(ts)))
  
  # compute raw mssd
  for (row in 1:nrow(ts)) {
    mssd_measures$mssd_raw[row] = mssd(as.numeric(ts[row,]))
    
    # compute n_correct, p_correct,, total_items, n_incorrect, max and min
    n_correct = sum(ts[row,] == 1, na.rm = T)
    total_items = length(which(!is.na(ts[row,])))
    p_correct = n_correct/total_items
    n_incorrect = total_items - n_correct
    mssd_max = min(2*min(n_correct, n_incorrect), (total_items-1)) / (total_items-1)
    mssd_min = ifelse(n_correct < (total_items-1) & n_correct > 1, 1/(total_items-1), 0)
    
    # compute mssd_rangestd
    mssd_measures$mssd_rangestd[row]  = (mssd_measures$mssd_raw[row] - mssd_min) / (mssd_max - mssd_min)
    
    # compute residualized mssd_rangestd based on expected
    exp_mssd = 2*p_correct*(1-p_correct)
    mssd_measures$mssd_res[row] = mssd_measures$mssd_raw[row] - exp_mssd
    
    # compute rangestandardized residualized mssd_rangestd based on expected and min max
    mssd_rangestd_exp = (exp_mssd - mssd_min) / (mssd_max - mssd_min)
    mssd_measures$mssd[row] = mssd_measures$mssd_rangestd[row] - mssd_rangestd_exp }
  
  # make a sel_mssd output data frame based on second argument
  if (output == "standardized")
  {sel_mssd = mssd_measures$mssd}
  
  else if (output == "raw")
  {sel_mssd = mssd_measures$mssd_raw}
  
  else if (output == "residualized")
  {sel_mssd = mssd_measures$mssd_res}
  
  else if (output == "rangestd")
  {sel_mssd = mssd_measures$mssd_rangestd}
  
  
  return(sel_mssd)}

#---------------------------------------------------------------------------------
### BURSTINESS  ### 

# Burstiness is calculated using three separate functions: the first function
# computes the burstiness itself, the second computes expected burstiness given 
# some proportion correct p and ts length n, and the final function computes the
# actual burstiness values: raw correct and error burstiness values, standardized
# values, and a combined burstiness value for both correct and error values which
# shows the lowest bias.

# Note that all burstiness measures show bias, i.e. there is a dependency on 
# proportion correct in unstructured data.

# The default output is the combined standardized burstiness
# Other options for output are: "raw", "separate", which output separate values
# for the correct and error burstiness

# actual burstiness
burst_from_intervals = function(intervals) {
  mean_int = mean(intervals, na.rm = TRUE)
  sd_int = sd(intervals, na.rm = TRUE)
  burst = (sd_int - mean_int) / (sd_int + mean_int)
  return(burst)
}

# expected burstiness
exp_burst = function(p, n) {
  k = 1:n
  ev = n*p
  pmf = (1-p)^(k-1)*p
  mean = sum(pmf*k) 
  var = sum((k-mean)^2 * pmf) 
  sd = sqrt(var)
  burst = (sd - mean) / (sd + mean)
  return(burst)
}

# compute error and correct burstiness, and standardized values
burst = function(ts, output = "standardized") { 
  
  # create df
  burstiness_meas = data.frame(
    burst_errs = rep(NA, nrow(ts)),
    burst_cors = rep(NA, nrow(ts)),
    burst_errs_std = rep(NA, nrow(ts)),
    burst_cors_std = rep(NA, nrow(ts)),
    burst_std = rep(NA, nrow(ts)))
  
  # loop over rows, extract time series vector
  for (row in 1:nrow(ts)) {
    vector = na.exclude(as.numeric(ts[row,])) 
    
    # make sure the time series contains at least two 1's and two 0's
    if (length(which(vector == 1)) > 1 & length(which(vector == 0)) > 1) {
      
      # compute p_cor and p_incor for later standardization
      n_cor = length(which(vector == 1))
      n_total = length(vector)
      p = n_cor/n_total
      q = 1 - p
      
      # compute the differences in indices between 0's / 1's
      err_intervals = diff(which(vector == 0))
      cor_intervals = diff(which(vector == 1))
      
      # compute differences between start and first 1/0
      if (vector[1] == 1) {start_err = which(vector == 0)[1]} else {start_err = NA}
      if (vector[1] == 0) {start_cor = which(vector == 1)[1]} else {start_cor = NA}
      
      # compute differences between last 1/0 and end
      if (vector[length(vector)] == 1) {end_err = (length(vector)+1) - (tail(which(vector == 0), 1))} else {end_err = NA}
      if (vector[length(vector)] == 0) {end_cor = (length(vector)+1) - (tail(which(vector == 1), 1))} else {end_cor = NA}
      
      # add up the start-first and last-end intervals as a single interval
      full_interval_err = c(start_err, err_intervals, end_err)
      full_interval_cor = c(start_cor, cor_intervals, end_cor)
      
      # compute burst_errs and burst_cors
      burstiness_meas$burst_errs[row] = burst_from_intervals(full_interval_err)
      burstiness_meas$burst_cors[row] = burst_from_intervals(full_interval_cor)
      
      # compute burst_errs_std and burst_cors_std
      exp_cor = exp_burst(p, n_total)
      burstiness_meas$burst_cors_std[row] = burstiness_meas$burst_cors[row] - exp_cor
      exp_err = exp_burst(q, n_total)
      burstiness_meas$burst_errs_std[row] = burstiness_meas$burst_errs[row] - exp_err
      if (!is.na(burstiness_meas$burst_cors_std[row]) & !is.na(burstiness_meas$burst_errs_std[row])) 
      {burstiness_meas$burst_std[row] = burstiness_meas$burst_cors_std[row]+burstiness_meas$burst_errs_std[row]}}}
  
  # make a sel_mssd output data frame based on second argument
  if (output == "standardized")
  {sel_burst = burstiness_meas$burst_std}
  
  if (output == "raw")
  {sel_burst = data.frame(cor_burst_raw = burstiness_meas$burst_cors, err_burst_raw = burstiness_meas$burst_errs)}
  
  if (output == "separate")
  {sel_burst = data.frame(cor_burst_std = burstiness_meas$burst_cors_std, err_burst_std = burstiness_meas$burst_errs_std)}
  
  return(sel_burst)
}


#---------------------------------------------------------------------------------
### WINDOWED SD  ### 

# The windowed SD is calculated using four separate functions for computing the 
# raw, max, and min windowed SD, and a main function which computes a raw, 
# residualized, and standardized version of the windowed SD.

# The default output is the standardized windowed SD.
# Other options for output are: "raw", "residualized".

# raw windowed SD
raw_wind_sd = function(ts_vec, w) {
  vec = na.exclude(as.numeric(ts_vec))
  means = vector()
  nr_windows = length(vec)
  vector_double = c(vec, vec)
  for (win in 1:nr_windows) {means[win] = mean(vector_double[win:(win+w-1)])}
  raw_sd = sd(means, na.rm=T)
  return(raw_sd)}

# max possible sd for proportion correct p, length n, and window w
max_ts = function(p,n,w) {
  if (p > 0) {q = 1 - p
  ts_v = c(rep(1,p*n), rep(0, (q*n)))
  max_sd = raw_wind_sd(ts_v, w)
  } else {max_sd = 0}
  return(max_sd)
}

# min possible sd for proportion correct p, length n and window w
min_ts = function(p,n,w) {
  ts_v = rep(0, n)
  k = round(p * n)             
  if (k > 0) {positions = floor(seq(1, n, length.out = k))
  ts_v[positions] = 1}
  min_sd = raw_wind_sd(ts_v, w)
  return(min_sd)
}

# main function for computing raw, residualized, and standardized windowed sd
window_SD = function(ts, output = "standardized", window = 10) {
  
  # generate df for windowed measures
  windowed = data.frame(sd = rep(NA, nrow(ts)), 
                        sd_res = rep(NA, nrow(ts)), 
                        sd_rangestd_res = rep(NA, nrow(ts)))
  
  # compute raw windowed sd 
  for (row in 1:nrow(windowed)) {windowed$sd[row] = raw_wind_sd(ts[row,], window)}
  
  # compute mean proportion correct; predicted sd; residualized sd
  means = rowMeans(ts, na.rm = T)
  pred_sd = vector()
  for (row in 1:nrow(ts)) {pred_sd[row] = sqrt(means[row] *(1-means[row] )/window)}
  windowed$sd_res = windowed$sd - pred_sd
  
  # compute min and max; compute rangestd_res measure
  for (row in 1:nrow(windowed)) {
    vec = na.exclude(ts[row,])
    max_sd = max_ts(means[row], length(vec), window)
    min_sd =  min_ts(means[row], length(vec), window)
    windowed$sd_rangestd_res[row] = (windowed$sd[row] / max_sd) - (pred_sd[row] / max_sd)}
  
  # make a sel_windowed output data frame based on second argument
  if (output == "standardized")
  {sel_windowed = windowed$sd_rangestd_res}
  
  if (output == "raw")
  {sel_windowed = windowed$sd}
  
  if (output == "residualized")
  {sel_windowed = windowed$sd_res}
  
  return(sel_windowed)
}

