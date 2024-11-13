##################################################################################
# Code to estimate Drift Diffusion models
##################################################################################

# This code is from the easyRT tutorial
# Credit to: https://dominiquemakowski.github.io/easyRT/articles/ddm.html
# Cite: Makowski D (2023). easyRT: Tools and Examples for Fitting (Hierarchical)
#       Drift Diffusion Models in R. R package version 0.1.0.

##################################################
#################    Appendix    #################
# 1. Install and load packages
# 2. Simulate data
# 3. Specify and fit DDM
##################################################

###############################
#1. Install and load packages
###############################

# Install easyRT package
remotes::install_github("DominiqueMakowski/easyRT")

#load required packagies
library(tidyverse)
library(easystats)
library(patchwork)
library(brms)
library(easyRT)

#############################
# 2. Simulate data
#############################

# Simulate data from DDM
sim <- ddm_data(n = c(200, 200, 200, 200),
                drift = c(-1, 0, 1, 2),
                bs = 1,
                bias = c(0.4, 0.5, 0.6, 0.7),
                ndt = 0.2)

ddm_plot(sim)

df <- sim$data
head(df)

#################################
# 3. Specify and fit DDM
#################################

# Specify model
df$condition <- as.numeric(df$condition) - 1

formula <- bf(rt | dec(response) ~ 0 + Intercept + condition,
              bias ~ 0 + Intercept + condition)

family <- wiener(link_bs = "identity",
                 link_ndt = "identity",
                 link_bias = "identity")

# get_prior(formula, data = df, family = family)
prior <- c(
  prior("student_t(3, 0, 1)", class = "b", coef="condition", dpar = ""),
  set_prior("student_t(3, 0, 1)", class = "b", coef="Intercept", dpar = ""),
  set_prior("student_t(3, 0, 0.05)", class = "b", coef="condition", dpar = "bias"),
  set_prior("normal(0.5, 0.15)", class = "b", coef = "Intercept", dpar = "bias"),
  set_prior("gamma(1.5, 3)", class = "ndt", ub = "min_Y"),
  set_prior("gamma(3, 2)", class = "bs")
) |>  brms::validate_prior(formula,
                           family = family,
                           data = df)


# Sample from model
# stancode(m)
init_func <- function(chain_id=1) {
  list(b = rep(0, 2),
       b_bias = c(0.5, 0),
       bs = 1, ndt = 0.2)
}

# m$fit@inits

m <- brm(formula,
         data = df,
         family = family,
         prior = prior,
         cores = 4,
         algorithm = "sampling",
         init = init_func)

print(m)

# # If using `algorithm = "fullrank"`, add the following:
# iter = 10000, # N of convergence iterations, defaults to 2000.
# output_samples = 4000, # N of posterior samples to draw and save, defaults to 1000.
# elbo_samples = 100, # N of samples for Monte Carlo estimate of ELBO (objective function), defaults to 100.
# tol_rel_obj = 0.01, # convergence tolerance on the relative norm of the objective, defaults to 0.01.
# importance_resampling = TRUE # adjust the draws at the optimum to be more like draws from the posterior distribution
