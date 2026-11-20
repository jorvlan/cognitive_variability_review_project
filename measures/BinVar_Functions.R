
### SCRIPT INCLUDING FUNCTIONS FOR COMPUTING THE BINARY VARIABILITY MEASURES
# ALINE KORVER
# LAST UPDATED: 1 SEPTEMBER 2026


### BINARY AUTOCORRELATION (binAC)

binAC = 
  # input time series, specify lag k (optional)
  function(ts, k=1) {
    n = length(ts)
    
    # center ts
    ts_c = ts - mean(ts)
    
    # compute covariance between the main and lagged ts; 
    # note we do not divide the cov and var by n as they cancel each other out 
    cov = sum(ts_c[(1+k):n]*ts_c[1:(n-k)])
    
    # compute variance of the complete ts
    var = sum(ts_c^2)
    
    # compute autocorrelation and return
    AC = cov / (var)
    return(AC)
  }

###


### BINARY MSSD (binMSSD)

# Input time series, specify output
# The default output is "standardized"
# Other options for output are: "raw", "rangestd"
binMSSD = function(ts, output="standardized") {
  
  # checks whether the ts includes missing data and returns error message if so
  if (sum(is.na(ts)) > 0) 
  {stop("Data includes missingness; please input a data set without missing values")}
  
  # compute raw binMSSD, 
  binMSSD_raw = mssd(ts)
  
  # return if output="raw"
  if (output == "raw")
  {return(binMSSD_raw)}
  
  # compute n correct, p correct, total n trials, n incorrect
  n_cor = sum(ts)
  n = length(ts)
  p = n_cor/n
  n_err = n - n_cor
  
  # max binMSSD: max number of transitions
  binMSSD_max = min(2*min(n_cor, n_err), (n-1)) / (n-1)
  
  # min binMSSD: either 1 or 0 transitions
  binMSSD_min = ifelse(n_cor < (n-1) & n_cor > 1, 1/(n-1), 0)
  
  # compute binMSSD_rangestd
  binMSSD_rangestd  = (binMSSD_raw - binMSSD_min) / (binMSSD_max - binMSSD_min)
  
  # return if output = "rangestd"
  if (output == "rangestd")
  {return(binMSSD_rangestd)}
  
  # compute expected binMSSD
  exp_binMSSD = 2*var(ts)
  
  # compute rangestandardized expected binMSSD
  binMSSD_rangestd_exp = (exp_binMSSD - binMSSD_min) / (binMSSD_max - binMSSD_min)
  
  # compute standardized binMSSD
  binMSSD_std = binMSSD_rangestd - binMSSD_rangestd_exp 
  return(binMSSD_std)}

###


### SEQUENTIAL ERROR DEPENDENCY (SED)

# Input time series, specify output
# The default output is "standardized"
# Other options for output are: "raw", "extra", "rangestd"
SED <- function(ts, output="standardized") {
  
  # checks whether the ts includes missing data and returns error message if so
  if (sum(is.na(ts)) > 0) 
  {stop("Data includes missingness; please input a data set without missing values")}
  
  # compute n correct, total n trials, p correct,  and n errors
  n_cor = sum(ts)
  n = length(ts)
  p = n_cor / n 
  n_err = n - n_cor
  
  # T/F ts denoting succeeding same trials
  same_next <- ts[-length(ts)] == ts[-1]  
  
  # count nr of succeeding errors
  suc_err <- sum(ts[-length(ts)] == 0 & same_next)
  
  # compute raw SED; returns NA if n errors = 0
  SED_raw <- if (n_err > 0) {suc_err / (n_err-1)} else NA
  if (output=="raw") {return(SED_raw)}
  
  # compute extra SED: probability of succeeding errors beyond base rate
  SED_ex <- SED_raw - (1-p)
  if (output=="extra") {return(SED_ex)}
  
  # compute maximum extra SED
  maxSED_ex = 1 - (1-p)
  
  # compute minimum extra SED
  if (n_err < ((n+1)/2) ) {minSED_ex = 0 - (1-p)}
  minSED_ex = ((n_err - n_cor - 1) / (n_err - 1)) - (1-p)
  
  # compute range-standardized SED
  SED_rangestd = (SED_ex - minSED_ex) / (maxSED_ex - minSED_ex)
  if (output=="rangestd") {return(SED_rangestd)}
  
  #compute standardized SED and return
  SED_std = SED_rangestd - ((0 - minSED_ex) / (maxSED_ex - minSED_ex))
  return(SED_std)} 

###


### SEQUENTIAL CORRECT DEPENDENCY (SCD)

# The SCD is exactly the same function, with correct responses replacing errors
SCD <- function(ts, output="standardized") {
  
  # checks whether the ts includes missing data and returns error message if so
  if (sum(is.na(ts)) > 0) 
  {stop("Data includes missingness; please input a data set without missing values")}
  
  # compute n correct, total n trials, p correct,  and n errors
  n_cor = sum(ts)
  n = length(ts)
  p = n_cor / n 
  n_err = n - n_cor
  
  # T/F ts denoting succeeding same trials
  same_next <- ts[-length(ts)] == ts[-1]  
  
  # count nr of succeeding errors
  suc_cor <- sum(ts[-length(ts)] == 1 & same_next)
  
  # compute raw SCD; returns NA if n errors = 0
  SCD_raw <- if (n_cor > 0) {suc_cor / (n_cor-1)} else NA
  if (output=="raw") {return(SCD_raw)}
  
  # compute extra SCD: probability of succeeding errors beyond base rate
  SCD_ex <- SCD_raw - p
  if (output=="extra") {return(SCD_ex)}
  
  # compute maximum extra SCD
  maxSCD_ex = 1 - p
  
  # compute minimum extra SCD
  if (n_cor < ((n+1)/2) ) {minSCD_ex = 0 - p}
  minSCD_ex = ((n_cor - n_err - 1) / (n_cor - 1)) - p
  
  # compute range-standardized SCD
  SCD_rangestd = (SCD_ex - minSCD_ex) / (maxSCD_ex - minSCD_ex)
  if (output=="rangestd") {return(SCD_rangestd)}
  
  #compute standardized SCD and return
  SCD_std = SCD_rangestd - ((0 - minSCD_ex) / (maxSCD_ex - minSCD_ex))
  return(SCD_std)} 

###


# BINARY WINDOWED VARIANCE (winVar)

# Main function
# Input time series, specify output
# The default output is "standardized"
# Other options for output are: "raw", "range-std"
# main function for computing raw, range-std, and standardized windowed variance
winVar = function(ts, output = "standardized", w = 10) {
  
  # deletes NAs at the end of the ts
  ts <- if (all(is.na(ts))) NA else ts[seq_len(max(which(!is.na(ts))))]
  
  # checks whether the ts include missing data and returns error message if so
  if (sum(is.na(ts)) > 0) 
  {stop("Data includes missingness; please input a data set without missing values")}
  
  # compute number of full windows that fit into the ts, cut off remaining trials
  f = length(ts) %/% w # nr of full windows of length w
  ts_full = ts[1:(w*f)] # ts with full windows
  k = sum(ts_full) # nr of 1s
  n_full = length(ts_full)
  
  # compute raw windowed variance; return if output == "raw"
  raw_winVar = winVar_raw(ts_full, w, f)
  if (output == "raw") {return(raw_winVar)}
  
  # compute predicted variance
  pred_Var = var(ts_full) / w
  
  # compute min and max; compute range-standardized measure
  max_var = (winVar_max(ts_full, k, w, f))
  min_var = (winVar_min(ts_full, f, k, w))
  range_var = round(max_var - min_var, digits = 10)
  winVar_std = ((raw_winVar - min_var) / range_var) 
  if (output == "range-std") return(winVar_std)
  
  # compute standardized variance
  winVar_res = winVar_std - ((pred_Var - min_var) / range_var)
  return(winVar_res)
}

# helper function to compute raw windowed variance
winVar_raw = function(ts_full, w, f) {
  
  # create vector for saving window means, compute means for the full windows
  w_means = vector() 
  for (win in 1:f) { ind = (win-1)*w+1
  w_means[win] = mean(ts_full[ind:(ind+w-1)])}
  # compute the variance over the means and return
  raw_var = var(w_means)
  return(raw_var)
}

# helper function for computing the minimum winVar
winVar_min <- function(ts_full, f, k, w) {
  spr = k %/% f
  rem = k %% f
  if (rem == 0) {return(0)}
  else {
    counts = c(rep(spr+1, rem), rep(spr, (f-rem)))
    min_ts = unlist(lapply(counts, function(cn) c(rep(1, cn), rep(0, w - cn))))
    return(winVar_raw(min_ts, w, f))  
  }
}

# helper function for computing the maximum winVar
winVar_max <- function(ts_full, k, w, f) {
  max_ts = c(rep(1,k), rep(0, length(ts_full)-k))
  return(winVar_raw(max_ts, w, f))
}



