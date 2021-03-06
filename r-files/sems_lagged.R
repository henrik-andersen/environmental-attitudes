### ==================================================== ##
### Title:  SEMS: Lagged effects
### Date:   01.02.2021
### Author: Henrik K. Andersen 
### ==================================================== ## 

Sys.setenv(LANG = "EN")

# Packages ----------------------------------------------------------------

library(reshape2)
library(tidyverse)
library(ggplot2)
library(lavaan)

# Data --------------------------------------------------------------------

rm(list = ls())

# getwd()
dfw <- readRDS("r-files/dfw.Rda")
# head(dfw)


# Regression models -------------------------------------------------------

# --- Random effects model: contemporary effects

re <- '
# --- Measurement models 
will15 =~ wtphrprcs_15 + l2*wtphrtxs_15 + l3*wtplfstl_15
will16 =~ wtphrprcs_16 + l2*wtphrtxs_16 + l3*wtplfstl_16
will17 =~ wtphrprcs_17 + l2*wtphrtxs_17 + l3*wtplfstl_17
will18 =~ wtphrprcs_18 + l2*wtphrtxs_18 + l3*wtplfstl_18
will19 =~ wtphrprcs_19 + l2*wtphrtxs_19 + l3*wtplfstl_19
# --- Individual effects
eta =~ 1*will15 + 1*will16 + 1*will17 + 1*will18 + 1*will19
# --- Regressions 
will15 ~ beta*srs_clmtchng_14
will16 ~ beta*srs_clmtchng_15 
will17 ~ beta*srs_clmtchng_16 
will18 ~ beta*srs_clmtchng_17 
will19 ~ beta*srs_clmtchng_18
# --- Correlated effects, RE = 0
eta ~~ 0*srs_clmtchng_14 + 0*srs_clmtchng_15 + 0*srs_clmtchng_16 + 0*srs_clmtchng_17 + 0*srs_clmtchng_18 
# --- Covariances of independent variables
srs_clmtchng_14 ~~ srs_clmtchng_15 + srs_clmtchng_16 + srs_clmtchng_17 + srs_clmtchng_18 
srs_clmtchng_15 ~~ srs_clmtchng_16 + srs_clmtchng_17 + srs_clmtchng_18 
srs_clmtchng_16 ~~ srs_clmtchng_17 + srs_clmtchng_18 
srs_clmtchng_17 ~~ srs_clmtchng_18 
# --- Error covariances within variables across time
wtphrprcs_15 ~~ wtphrprcs_16 + wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_16 ~~ wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_17 ~~ wtphrprcs_18 + wtphrprcs_19
wtphrprcs_18 ~~ wtphrprcs_19
wtphrtxs_15 ~~ wtphrtxs_16 + wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_16 ~~ wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_17 ~~ wtphrtxs_18 + wtphrtxs_19
wtphrtxs_18 ~~ wtphrtxs_19
wtplfstl_15 ~~ wtplfstl_16 + wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_16 ~~ wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_17 ~~ wtplfstl_18 + wtplfstl_19
wtplfstl_18 ~~ wtplfstl_19
# --- Thresholds
wtphrprcs_15 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_16 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_17 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_18 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_19 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrtxs_15  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_16  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_17  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_18  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_19  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtplfstl_15  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_16  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_17  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_18  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_19  | c1*t1 + c2*t2 + c3*t3 + c4*t4
'
re.fit <- cfa(model = re, data = dfw, #missing = "ML"#,
              ordered = c("wtphrprcs_15", "wtphrtxs_15", "wtplfstl_15",
                          "wtphrprcs_16", "wtphrtxs_16", "wtplfstl_16",
                          "wtphrprcs_17", "wtphrtxs_17", "wtplfstl_17",
                          "wtphrprcs_18", "wtphrtxs_18", "wtplfstl_18",
                          "wtphrprcs_19", "wtphrtxs_19", "wtplfstl_19")
)
summary(re.fit, fit.measures = TRUE, standardized = TRUE)

# --- Fixed effects model: contemporary effects

fe <- '
# --- Measurement models 
will15 =~ wtphrprcs_15 + l2*wtphrtxs_15 + l3*wtplfstl_15
will16 =~ wtphrprcs_16 + l2*wtphrtxs_16 + l3*wtplfstl_16
will17 =~ wtphrprcs_17 + l2*wtphrtxs_17 + l3*wtplfstl_17
will18 =~ wtphrprcs_18 + l2*wtphrtxs_18 + l3*wtplfstl_18
will19 =~ wtphrprcs_19 + l2*wtphrtxs_19 + l3*wtplfstl_19
# --- Individual effects
eta =~ 1*will15 + 1*will16 + 1*will17 + 1*will18 + 1*will19
# --- Regressions 
will15 ~ beta*srs_clmtchng_14
will16 ~ beta*srs_clmtchng_15 
will17 ~ beta*srs_clmtchng_16 
will18 ~ beta*srs_clmtchng_17 
will19 ~ beta*srs_clmtchng_18
# --- Correlated effects 
eta ~~ srs_clmtchng_14 + srs_clmtchng_15 + srs_clmtchng_16 + srs_clmtchng_17 + srs_clmtchng_18
# --- Covariances of independent variables
srs_clmtchng_14 ~~ srs_clmtchng_15 + srs_clmtchng_16 + srs_clmtchng_17 + srs_clmtchng_18
srs_clmtchng_15 ~~ srs_clmtchng_16 + srs_clmtchng_17 + srs_clmtchng_18
srs_clmtchng_16 ~~ srs_clmtchng_17 + srs_clmtchng_18
srs_clmtchng_17 ~~ srs_clmtchng_18
# --- Error covariances within variables across time
wtphrprcs_15 ~~ wtphrprcs_16 + wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_16 ~~ wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_17 ~~ wtphrprcs_18 + wtphrprcs_19
wtphrprcs_18 ~~ wtphrprcs_19
wtphrtxs_15 ~~ wtphrtxs_16 + wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_16 ~~ wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_17 ~~ wtphrtxs_18 + wtphrtxs_19
wtphrtxs_18 ~~ wtphrtxs_19
wtplfstl_15 ~~ wtplfstl_16 + wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_16 ~~ wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_17 ~~ wtplfstl_18 + wtplfstl_19
wtplfstl_18 ~~ wtplfstl_19
# --- Thresholds
wtphrprcs_15 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_16 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_17 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_18 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_19 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrtxs_15  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_16  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_17  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_18  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_19  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtplfstl_15  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_16  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_17  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_18  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_19  | c1*t1 + c2*t2 + c3*t3 + c4*t4
'
fe.fit <- cfa(model = fe, data = dfw, #missing = "ML"#,
              ordered = c("wtphrprcs_15", "wtphrtxs_15", "wtplfstl_15",
                          "wtphrprcs_16", "wtphrtxs_16", "wtplfstl_16",
                          "wtphrprcs_17", "wtphrtxs_17", "wtplfstl_17",
                          "wtphrprcs_18", "wtphrtxs_18", "wtplfstl_18",
                          "wtphrprcs_19", "wtphrtxs_19", "wtplfstl_19")
)
summary(fe.fit, fit.measures = TRUE, standardized = TRUE)

# --- Dynamic FE

fed <- '
# --- Measurement models 
will14 =~ wtphrprcs_14 + l2*wtphrtxs_14 + l3*wtplfstl_14
will15 =~ wtphrprcs_15 + l2*wtphrtxs_15 + l3*wtplfstl_15
will16 =~ wtphrprcs_16 + l2*wtphrtxs_16 + l3*wtplfstl_16
will17 =~ wtphrprcs_17 + l2*wtphrtxs_17 + l3*wtplfstl_17
will18 =~ wtphrprcs_18 + l2*wtphrtxs_18 + l3*wtplfstl_18
will19 =~ wtphrprcs_19 + l2*wtphrtxs_19 + l3*wtplfstl_19
# --- Individual effects
eta =~ 1*will15 + 1*will16 + 1*will17 + 1*will18 + 1*will19
# --- Regressions 
will15 ~ beta*srs_clmtchng_14 + rho*will14
will16 ~ beta*srs_clmtchng_15 + rho*will15
will17 ~ beta*srs_clmtchng_16 + rho*will16
will18 ~ beta*srs_clmtchng_17 + rho*will17
will19 ~ beta*srs_clmtchng_18 + rho*will18
# --- Correlated effects 
eta ~~ srs_clmtchng_14 + srs_clmtchng_15 + srs_clmtchng_16 + srs_clmtchng_17 + srs_clmtchng_18 + will14
# --- Covariances of independent variables
srs_clmtchng_14 ~~ srs_clmtchng_15 + srs_clmtchng_16 + srs_clmtchng_17 + srs_clmtchng_18
srs_clmtchng_15 ~~ srs_clmtchng_16 + srs_clmtchng_17 + srs_clmtchng_18 
srs_clmtchng_16 ~~ srs_clmtchng_17 + srs_clmtchng_18 
srs_clmtchng_17 ~~ srs_clmtchng_18 
# --- Error covariances within variables across time
wtphrprcs_14 ~~ wtphrprcs_15 + wtphrprcs_16 + wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_15 ~~ wtphrprcs_16 + wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_16 ~~ wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_17 ~~ wtphrprcs_18 + wtphrprcs_19
wtphrprcs_18 ~~ wtphrprcs_19
wtphrtxs_14 ~~ wtphrtxs_15 + wtphrtxs_16 + wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_15 ~~ wtphrtxs_16 + wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_16 ~~ wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_17 ~~ wtphrtxs_18 + wtphrtxs_19
wtphrtxs_18 ~~ wtphrtxs_19
wtplfstl_14 ~~ wtplfstl_15 + wtplfstl_16 + wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_15 ~~ wtplfstl_16 + wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_16 ~~ wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_17 ~~ wtplfstl_18 + wtplfstl_19
wtplfstl_18 ~~ wtplfstl_19
# --- Thresholds
wtphrprcs_14 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_15 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_16 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_17 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_18 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_19 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrtxs_14  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_15  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_16  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_17  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_18  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_19  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtplfstl_14  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_15  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_16  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_17  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_18  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_19  | c1*t1 + c2*t2 + c3*t3 + c4*t4
'
fed.fit <- cfa(model = fed, data = dfw, #missing = "ML"#,
               ordered = c("wtphrprcs_14", "wtphrtxs_14", "wtplfstl_14",
                           "wtphrprcs_15", "wtphrtxs_15", "wtplfstl_15",
                           "wtphrprcs_16", "wtphrtxs_16", "wtplfstl_16",
                           "wtphrprcs_17", "wtphrtxs_17", "wtplfstl_17",
                           "wtphrprcs_18", "wtphrtxs_18", "wtplfstl_18",
                           "wtphrprcs_19", "wtphrtxs_19", "wtplfstl_19")
)
summary(fed.fit, fit.measures = TRUE, standardized = TRUE)

# --- Dynamic FE, sequential exogeneity 

feds <- '
# --- Measurement models 
will14 =~ wtphrprcs_14 + l2*wtphrtxs_14 + l3*wtplfstl_14
will15 =~ wtphrprcs_15 + l2*wtphrtxs_15 + l3*wtplfstl_15
will16 =~ wtphrprcs_16 + l2*wtphrtxs_16 + l3*wtplfstl_16
will17 =~ wtphrprcs_17 + l2*wtphrtxs_17 + l3*wtplfstl_17
will18 =~ wtphrprcs_18 + l2*wtphrtxs_18 + l3*wtplfstl_18
will19 =~ wtphrprcs_19 + l2*wtphrtxs_19 + l3*wtplfstl_19
# --- Individual effects
eta =~ 1*will15 + 1*will16 + 1*will17 + 1*will18 + 1*will19
# --- Regressions 
will15 ~ beta15*srs_clmtchng_14 + rho15*will14
will16 ~ beta16*srs_clmtchng_15 + rho16*will15
will17 ~ beta17*srs_clmtchng_16 + rho17*will16
will18 ~ beta18*srs_clmtchng_17 + rho18*will17
will19 ~ beta19*srs_clmtchng_18 + rho19*will18
# --- Correlated effects 
eta ~~ srs_clmtchng_14 + srs_clmtchng_15 + srs_clmtchng_16 + srs_clmtchng_17 + srs_clmtchng_18 + will14
# --- Covariances of independent variables
srs_clmtchng_14 ~~ srs_clmtchng_15 + srs_clmtchng_16 + srs_clmtchng_17 + srs_clmtchng_18
srs_clmtchng_15 ~~ srs_clmtchng_16 + srs_clmtchng_17 + srs_clmtchng_18 
srs_clmtchng_16 ~~ srs_clmtchng_17 + srs_clmtchng_18 
srs_clmtchng_17 ~~ srs_clmtchng_18 
# --- Sequential exogeneity 
srs_clmtchng_16 ~~ will15
srs_clmtchng_17 ~~ will15 + will16
srs_clmtchng_18 ~~ will15 + will16 + will17
# --- Error covariances within variables across time
wtphrprcs_14 ~~ wtphrprcs_15 + wtphrprcs_16 + wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_15 ~~ wtphrprcs_16 + wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_16 ~~ wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_17 ~~ wtphrprcs_18 + wtphrprcs_19
wtphrprcs_18 ~~ wtphrprcs_19
wtphrtxs_14 ~~ wtphrtxs_15 + wtphrtxs_16 + wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_15 ~~ wtphrtxs_16 + wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_16 ~~ wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_17 ~~ wtphrtxs_18 + wtphrtxs_19
wtphrtxs_18 ~~ wtphrtxs_19
wtplfstl_14 ~~ wtplfstl_15 + wtplfstl_16 + wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_15 ~~ wtplfstl_16 + wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_16 ~~ wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_17 ~~ wtplfstl_18 + wtplfstl_19
wtplfstl_18 ~~ wtplfstl_19
# --- Thresholds
wtphrprcs_14 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_15 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_16 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_17 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_18 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_19 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrtxs_14  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_15  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_16  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_17  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_18  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_19  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtplfstl_14  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_15  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_16  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_17  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_18  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_19  | c1*t1 + c2*t2 + c3*t3 + c4*t4
'
feds.fit <- cfa(model = feds, data = dfw, #missing = "ML"#,
               ordered = c("wtphrprcs_14", "wtphrtxs_14", "wtplfstl_14",
                           "wtphrprcs_15", "wtphrtxs_15", "wtplfstl_15",
                           "wtphrprcs_16", "wtphrtxs_16", "wtplfstl_16",
                           "wtphrprcs_17", "wtphrtxs_17", "wtplfstl_17",
                           "wtphrprcs_18", "wtphrtxs_18", "wtplfstl_18",
                           "wtphrprcs_19", "wtphrtxs_19", "wtplfstl_19")
)
summary(feds.fit, fit.measures = TRUE, standardized = TRUE)


# --- Model comparison

anova(re.fit, fe.fit)
anova(fed.fit, feds.fit)

# Cross-Lagged ------------------------------------------------------------

cl <- '
# --- Measurement models 
will14 =~ wtphrprcs_14 + l2*wtphrtxs_14 + l3*wtplfstl_14
will15 =~ wtphrprcs_15 + l2*wtphrtxs_15 + l3*wtplfstl_15
will16 =~ wtphrprcs_16 + l2*wtphrtxs_16 + l3*wtplfstl_16
will17 =~ wtphrprcs_17 + l2*wtphrtxs_17 + l3*wtplfstl_17
will18 =~ wtphrprcs_18 + l2*wtphrtxs_18 + l3*wtplfstl_18
will19 =~ wtphrprcs_19 + l2*wtphrtxs_19 + l3*wtplfstl_19
# --- Individual effects
eta =~ 1*will15 + 1*will16 + 1*will17 + 1*will18 + 1*will19
alpha =~ 1*srs_clmtchng_15 + 1*srs_clmtchng_16 + 1*srs_clmtchng_17 + 1*srs_clmtchng_18 + 1*srs_clmtchng_19
# --- Regressions 
will15 ~ beta*srs_clmtchng_14 + rho*will14
will16 ~ beta*srs_clmtchng_15 + rho*will15
will17 ~ beta*srs_clmtchng_16 + rho*will16
will18 ~ beta*srs_clmtchng_17 + rho*will17
will19 ~ beta*srs_clmtchng_18 + rho*will18
srs_clmtchng_15 ~ gamma*will14 + phi*srs_clmtchng_14
srs_clmtchng_16 ~ gamma*will15 + phi*srs_clmtchng_15
srs_clmtchng_17 ~ gamma*will16 + phi*srs_clmtchng_16
srs_clmtchng_18 ~ gamma*will17 + phi*srs_clmtchng_17
srs_clmtchng_19 ~ gamma*will18 + phi*srs_clmtchng_18
# --- Correlated effects 
eta ~~ alpha + will14 + srs_clmtchng_14
alpha ~~ will14 + srs_clmtchng_14
# --- Error covariances within variables across time
wtphrprcs_14 ~~ wtphrprcs_15 + wtphrprcs_16 + wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_15 ~~ wtphrprcs_16 + wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_16 ~~ wtphrprcs_17 + wtphrprcs_18 + wtphrprcs_19
wtphrprcs_17 ~~ wtphrprcs_18 + wtphrprcs_19
wtphrprcs_18 ~~ wtphrprcs_19
wtphrtxs_14 ~~ wtphrtxs_15 + wtphrtxs_16 + wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_15 ~~ wtphrtxs_16 + wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_16 ~~ wtphrtxs_17 + wtphrtxs_18 + wtphrtxs_19
wtphrtxs_17 ~~ wtphrtxs_18 + wtphrtxs_19
wtphrtxs_18 ~~ wtphrtxs_19
wtplfstl_14 ~~ wtplfstl_15 + wtplfstl_16 + wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_15 ~~ wtplfstl_16 + wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_16 ~~ wtplfstl_17 + wtplfstl_18 + wtplfstl_19
wtplfstl_17 ~~ wtplfstl_18 + wtplfstl_19
wtplfstl_18 ~~ wtplfstl_19
# --- Thresholds
wtphrprcs_14 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_15 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_16 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_17 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_18 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrprcs_19 | a1*t1 + a2*t2 + a3*t3 + a4*t4
wtphrtxs_14  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_15  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_16  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_17  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_18  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtphrtxs_19  | b1*t1 + b2*t2 + b3*t3 + b4*t4
wtplfstl_14  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_15  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_16  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_17  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_18  | c1*t1 + c2*t2 + c3*t3 + c4*t4
wtplfstl_19  | c1*t1 + c2*t2 + c3*t3 + c4*t4
'
cl.fit <- sem(model = cl, data = dfw, #missing = "ML"#,
                ordered = c("wtphrprcs_14", "wtphrtxs_14", "wtplfstl_14",
                            "wtphrprcs_15", "wtphrtxs_15", "wtplfstl_15",
                            "wtphrprcs_16", "wtphrtxs_16", "wtplfstl_16",
                            "wtphrprcs_17", "wtphrtxs_17", "wtplfstl_17",
                            "wtphrprcs_18", "wtphrtxs_18", "wtplfstl_18",
                            "wtphrprcs_19", "wtphrtxs_19", "wtplfstl_19")
)
summary(cl.fit, fit.measures = TRUE, standardized = TRUE)
