# Sample lavaan code for the CLPM, RI-CLPM, and DPM

# The code includes manifest-variable and latent-variable versions of the  
# models, with four waves of data and two constructs (denoted as X and Y).

# Wide-format data: one row per participant
# Manifest models: one observed variable per construct and wave (e.g., x1)
# Latent models: three indicators per construct and wave (e.g., x1a, x1b, x1c)

# In the sample code, autoregressive and cross-lagged coefficients are 
# constrained to be equal across waves. These constraints are appropriate when 
# the time intervals between assessments are equal or approximately equal and 
# effects can be assumed to be constant over time. The constraints can be 
# tested by comparing the fit of constrained and unconstrained models. If 
# supported, the constraints may facilitate model convergence, increase the 
# precision of estimates, and simplify the interpretation of the effects.


# load packages
library(lavaan)


# CLPM with manifest variables

clpm.1 <- '
  # autoregressive effects
    x2 ~ e*x1
    x3 ~ e*x2
    x4 ~ e*x3
    y2 ~ f*y1
    y3 ~ f*y2
    y4 ~ f*y3
  # cross-lagged effects
    x2 ~ g*y1
    x3 ~ g*y2
    x4 ~ g*y3
    y2 ~ h*x1
    y3 ~ h*x2
    y4 ~ h*x3
  # covariances
    x1 ~~ y1
    x2 ~~ y2
    x3 ~~ y3
    x4 ~~ y4
'

fit.clpm.1 <- sem(clpm.1, data = dat, missing = "FIML")
summary(fit.clpm.1, fit_measures = TRUE, standardized = TRUE)


# RI-CLPM with manifest variables

riclpm.1 <- '
  # random intercepts
    x =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
    y =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
  # wave-specific residual factors
    xr1 =~ 1*x1
    xr2 =~ 1*x2
    xr3 =~ 1*x3
    xr4 =~ 1*x4
    yr1 =~ 1*y1
    yr2 =~ 1*y2
    yr3 =~ 1*y3
    yr4 =~ 1*y4
  # set residual variances of manifest variables to zero
    x1 ~~ 0*x1
    x2 ~~ 0*x2
    x3 ~~ 0*x3
    x4 ~~ 0*x4
    y1 ~~ 0*y1
    y2 ~~ 0*y2
    y3 ~~ 0*y3
    y4 ~~ 0*y4
  # autoregressive effects
    xr2 ~ e*xr1
    xr3 ~ e*xr2
    xr4 ~ e*xr3
    yr2 ~ f*yr1
    yr3 ~ f*yr2
    yr4 ~ f*yr3
  # cross-lagged effects
    xr2 ~ g*yr1
    xr3 ~ g*yr2
    xr4 ~ g*yr3
    yr2 ~ h*xr1
    yr3 ~ h*xr2
    yr4 ~ h*xr3
  # covariances
    xr1 ~~ yr1
    xr2 ~~ yr2
    xr3 ~~ yr3
    xr4 ~~ yr4
  # covariances of random intercepts
    x ~~ y
    x ~~ 0*xr1
    x ~~ 0*yr1
    y ~~ 0*xr1
    y ~~ 0*yr1
'

fit.riclpm.1 <- sem(riclpm.1, data = dat, missing = "FIML")
summary(fit.riclpm.1, fit_measures = TRUE, standardized = TRUE)


# DPM with manifest variables

dpm.1 <- '
  # autoregressive effects
    x2 ~ e*x1
    x3 ~ e*x2
    x4 ~ e*x3
    y2 ~ f*y1
    y3 ~ f*y2
    y4 ~ f*y3
  # cross-lagged effects
    x2 ~ g*y1
    x3 ~ g*y2
    x4 ~ g*y3
    y2 ~ h*x1
    y3 ~ h*x2
    y4 ~ h*x3
  # covariances
    x1 ~~ y1
    x2 ~~ y2
    x3 ~~ y3
    x4 ~~ y4
  # unit effects
    x =~ 1*x2 + 1*x3 + 1*x4
    y =~ 1*y2 + 1*y3 + 1*y4
  # covariances of unit effects
    x ~~ y
    x ~~ x1
    x ~~ y1
    y ~~ x1
    y ~~ y1
'

fit.dpm.1 <- sem(dpm.1, data = dat, missing = "FIML")
summary(fit.dpm.1, fit_measures = TRUE, standardized = TRUE)


# CLPM with latent constructs

clpm.2 <- '
  # measurement model (assuming metric measurement invariance across waves)
    fx1 =~ x1a + a*x1b + b*x1c
    fx2 =~ x2a + a*x2b + b*x2c
    fx3 =~ x3a + a*x3b + b*x3c
    fx4 =~ x4a + a*x4b + b*x4c
    fy1 =~ y1a + c*y1b + d*y1c
    fy2 =~ y2a + c*y2b + d*y2c
    fy3 =~ y3a + c*y3b + d*y3c
    fy4 =~ y4a + c*y4b + d*y4c
  # correlated residuals
    x1a ~~ x2a + x3a + x4a
    x2a ~~ x3a + x4a
    x3a ~~ x4a
    x1b ~~ x2b + x3b + x4b
    x2b ~~ x3b + x4b
    x3b ~~ x4b
    x1c ~~ x2c + x3c + x4c
    x2c ~~ x3c + x4c
    x3c ~~ x4c
    y1a ~~ y2a + y3a + y4a
    y2a ~~ y3a + y4a
    y3a ~~ y4a
    y1b ~~ y2b + y3b + y4b
    y2b ~~ y3b + y4b
    y3b ~~ y4b
    y1c ~~ y2c + y3c + y4c
    y2c ~~ y3c + y4c
    y3c ~~ y4c
  # autoregressive effects
    fx2 ~ e*fx1
    fx3 ~ e*fx2
    fx4 ~ e*fx3
    fy2 ~ f*fy1
    fy3 ~ f*fy2
    fy4 ~ f*fy3
  # cross-lagged effects
    fx2 ~ g*fy1
    fx3 ~ g*fy2
    fx4 ~ g*fy3
    fy2 ~ h*fx1
    fy3 ~ h*fx2
    fy4 ~ h*fx3
  # covariances
    fx1 ~~ fy1
    fx2 ~~ fy2
    fx3 ~~ fy3
    fx4 ~~ fy4
'

fit.clpm.2 <- sem(clpm.2, data = dat, missing = "FIML")
summary(fit.clpm.2, fit_measures = TRUE, standardized = TRUE)


# RI-CLPM with latent constructs

riclpm.2 <- '
  # measurement model (assuming metric measurement invariance across waves)
    fx1 =~ x1a + a*x1b + b*x1c
    fx2 =~ x2a + a*x2b + b*x2c
    fx3 =~ x3a + a*x3b + b*x3c
    fx4 =~ x4a + a*x4b + b*x4c
    fy1 =~ y1a + c*y1b + d*y1c
    fy2 =~ y2a + c*y2b + d*y2c
    fy3 =~ y3a + c*y3b + d*y3c
    fy4 =~ y4a + c*y4b + d*y4c
  # correlated residuals
    x1a ~~ x2a + x3a + x4a
    x2a ~~ x3a + x4a
    x3a ~~ x4a
    x1b ~~ x2b + x3b + x4b
    x2b ~~ x3b + x4b
    x3b ~~ x4b
    x1c ~~ x2c + x3c + x4c
    x2c ~~ x3c + x4c
    x3c ~~ x4c
    y1a ~~ y2a + y3a + y4a
    y2a ~~ y3a + y4a
    y3a ~~ y4a
    y1b ~~ y2b + y3b + y4b
    y2b ~~ y3b + y4b
    y3b ~~ y4b
    y1c ~~ y2c + y3c + y4c
    y2c ~~ y3c + y4c
    y3c ~~ y4c
  # random intercepts
    x =~ 1*fx1 + 1*fx2 + 1*fx3 + 1*fx4
    y =~ 1*fy1 + 1*fy2 + 1*fy3 + 1*fy4
  # wave-specific residual factors
    xr1 =~ 1*fx1
    xr2 =~ 1*fx2
    xr3 =~ 1*fx3
    xr4 =~ 1*fx4
    yr1 =~ 1*fy1
    yr2 =~ 1*fy2
    yr3 =~ 1*fy3
    yr4 =~ 1*fy4
  # set residual variances of construct factors to zero
    fx1 ~~ 0*fx1
    fx2 ~~ 0*fx2
    fx3 ~~ 0*fx3
    fx4 ~~ 0*fx4
    fy1 ~~ 0*fy1
    fy2 ~~ 0*fy2
    fy3 ~~ 0*fy3
    fy4 ~~ 0*fy4
  # autoregressive effects
    xr2 ~ e*xr1
    xr3 ~ e*xr2
    xr4 ~ e*xr3
    yr2 ~ f*yr1
    yr3 ~ f*yr2
    yr4 ~ f*yr3
  # cross-lagged effects
    xr2 ~ g*yr1
    xr3 ~ g*yr2
    xr4 ~ g*yr3
    yr2 ~ h*xr1
    yr3 ~ h*xr2
    yr4 ~ h*xr3
  # covariances
    xr1 ~~ yr1
    xr2 ~~ yr2
    xr3 ~~ yr3
    xr4 ~~ yr4
  # covariances of random intercepts
    x ~~ y
    x ~~ 0*xr1
    x ~~ 0*yr1
    y ~~ 0*xr1
    y ~~ 0*yr1
'

fit.riclpm.2 <- sem(riclpm.2, data = dat, missing = "FIML")
summary(fit.riclpm.2, fit_measures = TRUE, standardized = TRUE)


# DPM with latent constructs

dpm.2 <- '
  # measurement model (assuming metric measurement invariance across waves)
    fx1 =~ x1a + a*x1b + b*x1c
    fx2 =~ x2a + a*x2b + b*x2c
    fx3 =~ x3a + a*x3b + b*x3c
    fx4 =~ x4a + a*x4b + b*x4c
    fy1 =~ y1a + c*y1b + d*y1c
    fy2 =~ y2a + c*y2b + d*y2c
    fy3 =~ y3a + c*y3b + d*y3c
    fy4 =~ y4a + c*y4b + d*y4c
  # correlated residuals
    x1a ~~ x2a + x3a + x4a
    x2a ~~ x3a + x4a
    x3a ~~ x4a
    x1b ~~ x2b + x3b + x4b
    x2b ~~ x3b + x4b
    x3b ~~ x4b
    x1c ~~ x2c + x3c + x4c
    x2c ~~ x3c + x4c
    x3c ~~ x4c
    y1a ~~ y2a + y3a + y4a
    y2a ~~ y3a + y4a
    y3a ~~ y4a
    y1b ~~ y2b + y3b + y4b
    y2b ~~ y3b + y4b
    y3b ~~ y4b
    y1c ~~ y2c + y3c + y4c
    y2c ~~ y3c + y4c
    y3c ~~ y4c
  # autoregressive effects
    fx2 ~ e*fx1
    fx3 ~ e*fx2
    fx4 ~ e*fx3
    fy2 ~ f*fy1
    fy3 ~ f*fy2
    fy4 ~ f*fy3
  # cross-lagged effects
    fx2 ~ g*fy1
    fx3 ~ g*fy2
    fx4 ~ g*fy3
    fy2 ~ h*fx1
    fy3 ~ h*fx2
    fy4 ~ h*fx3
  # covariances
    fx1 ~~ fy1
    fx2 ~~ fy2
    fx3 ~~ fy3
    fx4 ~~ fy4
  # unit effects
    x =~ 1*fx2 + 1*fx3 + 1*fx4
    y =~ 1*fy2 + 1*fy3 + 1*fy4
  # covariances of unit effects
    x ~~ y
    x ~~ fx1
    x ~~ fy1
    y ~~ fx1
    y ~~ fy1
'

fit.dpm.2 <- sem(dpm.2, data = dat, missing = "FIML")
summary(fit.dpm.2, fit_measures = TRUE, standardized = TRUE)