#---------------------------------------------------------------------------------
### TUTORIAL FOR COMPUTING BINARY VARIABILITY ###
## ALINE KORVER, 19 JAN 2026

# This script serves as a tutorial for using the binary variability functions
# in the other accompanying bivar_functions script


#---------------------------------------------------------------------------------
### SETUP ###

# First, load the following packages, set your wd() to the bivar folder, and 
# "source" the functions.

# You can also load your data set here on which you want to use the functions,
# or first use the simulated data set below.

# The "source" section also loads a convenient plot_meas function, which allows
# you to plot 9 time series with the the 3 lowest, mid, and highest scores on a
# certain binary measure.

## load packages
library(psych)
library(pracma)
library(stats)
library(ggplot2)
library(reshape2)

## set wd()    
setwd('/Users/alinekorver/Documents /Binary variability project/Binary_variability_tutorial')

## source functions
source("Bivar_functions.R")
source("PlotMeasures_Function.R")

# load your own data set
#
#
#


#---------------------------------------------------------------------------------
### SIMULATE DATA ###

# We'll simulate a small data set which can be used to try out the functions.

# The data is simulated as follows: I picked a set of normally distributed,
# moderately positive autocorrelations and means, and generated continuous
# time series, which were then dichotomized (i.e. turned into 1/0 based on
# the continuous value being below or above 0).

# set seed & create empty time series df
set.seed(1108)
ts <- matrix(nrow = 100, ncol = 100)
ts <- as.data.frame(ts)

# create range of autocorrelations
sim_acs = rnorm(100, mean=0.3, sd=0.15)
sim_acs = ifelse(acs>.99, .99, acs)

# simulate autocorrelated time series; dichotomize
for (row in 1:100) {sim = arima.sim(list(ar=sim_acs[row], ma=0), n=100) + rnorm(1, mean=.5, sd=.3)
ts[row,1:100] <- ifelse(sim<0, 0, 1)}

# create df and add pcor and ID variable
df = ts
df$pcor = rowMeans(ts)
df$ID = as.factor(1:nrow(ts))

# get some idea of the distribution of the autocorrelation and mean values
hist(sim_acs, breaks=20)
hist(df$pcor, breaks=20) 


#---------------------------------------------------------------------------------
### COMPUTE BINARY VARIABILITY MEASURES ###

# Here we'll compute the binary measures on the data.

# For more info on the background and computation of each of these measures,
# see the BinaryVariabilityThesis.pdf document in the tutorial folder.

# If the "sourcing" of functions went well, you now have the following set of 
# functions in your R environment:

## bi_ac --> computes binary autocorrelation
## bi_Hu --> computes binary Hurst exponent
## bi_mssd --> computes binary MSSD value
## burst --> computes burstiness
## cor_err_streak --> computes correct and error streakiness
## window_SD --> computes windowed standard deviation

## The plot_meas function plots time series for the 3 lowest, middle, and highest 
## values for a certain measure. You input a time series data frame, the 
## corresponding vector of values on a binary variability measure, and the
## name of the measure. It outputs a ggplot with time series and their
## binary variability measure values.

# (There's also a few helper functions which are used inside of these functions,
# which you can ignore.)

# For the MSSD, burstiness, correct/error streakiness, and windowed SD, I 
# performed a "standardization procedure" (again see BinaryVariabilityThesis.pdf)
# The functions will output the standardized value by default; it is also 
# possible to specify that you want the "raw" or partly standardized values
# (see the Bivar_Functions.R script for more information).

# The functions accept data frames with a time series per row as input (i.e. like
# the 'ts' data frame that we simulated above). 
# The output is a vector or a data frame with values.


# If you want to use your own data set, assign it to the 'ts' object here 
# (otherwise you'll be using the simulated data set):
ts = ts 


#########################
#### Autocorrelation ####

# We'll compute the binary autocorrelation and assign the output to an 
# "autocorrelation" object
autocorrelation = bi_ac(ts)

# Look at the distribution of values
hist(autocorrelation)

# We can look at how well the binary autocorrelation detects the 'latent'
# autocorrelation we used to simulate the continuous data:
plot(sim_acs, autocorrelation)
cor.test(sim_acs, autocorrelation)
  # quite a strong correlation!

# plot the time series with the 3 lowest, middle, and highest values:
plot_meas(ts, autocorrelation, "Autocorrelation")
    # note that the low values go up and down a lot, while the highe values
    # are more stable.


########################
#### Hurst exponent ####

# same steps: compute Hurst, look at distribution
Hurst_exponent = bi_Hu(ts)
hist(Hurst_exponent)

# How is it related to the binary autocorrelation?
plot(Hurst_exponent, autocorrelation)
cor.test(Hurst_exponent, autocorrelation)
    # no strong relation

# plot the time series with the 3 lowest, middle, and highest values:
plot_meas(ts, Hurst_exponent, "Hurst_exponent")


#######################################
#### Correct and error streakiness ####

# compute correct and error streakiness; note that here the output is a 
# data frame with one column for correct and one for error streakiness
streaks = cor_err_streak(ts)

# how are they related to each other?
plot(streaks$corstreak, streaks$errstreak)
cor.test(streaks$corstreak, streaks$errstreak)
    # extremely high correlation; correct and error streakiness are almost
    # interchangeable

# an example of computing the raw instead of the standardized streakiness
streaks_raw = cor_err_streak(ts, output = "raw")

# how is the raw correct streakiness related to the standardized value?
plot(streaks$corstreak, streaks_raw$corstreak_raw)
cor.test(streaks$corstreak, streaks_raw$corstreak_raw)
    # there's a positive correlation, but it is also clear that they are
    # very different.

# plot the time series with the 3 lowest, middle, and highest values:
plot_meas(ts, streaks$corstreak, "Correct streakiness")
plot_meas(ts, streaks$errstreak, "Error streakiness")


##############
#### MSSD ####

# compute MSSD & compare to autocorrelation
mssd = bi_mssd(ts)
plot(mssd, autocorrelation)
cor.test(mssd, autocorrelation)
    # MSSD and autocorrelation are also almost exchangeable

# plot the time series with the 3 lowest, middle, and highest values:
plot_meas(ts, mssd, "MSSD")


####################
#### Burstiness ####

# compute burstiness and compare to mssd
burstiness = burst(ts)
plot(mssd, burstiness)
cor.test(mssd, burstiness)

# plot the time series with the 3 lowest, middle, and highest values:
plot_meas(ts, burstiness, "Burstiness")


#####################
#### Windowed SD ####

# compute and compare to autocorrelation
win_SD = window_SD(ts)
plot(autocorrelation, win_SD)
cor.test(autocorrelation, win_SD)

# plot the time series with the 3 lowest, middle, and highest values:
plot_meas(ts, win_SD, "Windowed standard deviation")





