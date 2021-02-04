### ==================================================== ##
### Title:  Factor analyses
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
head(dfw)

# Factor analysis  --------------------------------------------------------

# --- Configural invariance 

fac <- '
# Measurement models 
will14 =~ wtphrprcs_14 + l21*wtphrtxs_14 + l31*wtplfstl_14
will15 =~ wtphrprcs_15 + l22*wtphrtxs_15 + l32*wtplfstl_15
will16 =~ wtphrprcs_16 + l23*wtphrtxs_16 + l33*wtplfstl_16
will17 =~ wtphrprcs_17 + l24*wtphrtxs_17 + l34*wtplfstl_17
will18 =~ wtphrprcs_18 + l25*wtphrtxs_18 + l35*wtplfstl_18
will19 =~ wtphrprcs_19 + l26*wtphrtxs_19 + l36*wtplfstl_19
# Error covariances within variables across time
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
'
fac.fit <- cfa(model = fac, data = dfw, #missing = "ML",
              ordered = c("wtphrprcs_14", "wtphrtxs_14", "wtplfstl_14",
                          "wtphrprcs_15", "wtphrtxs_15", "wtplfstl_15",
                          "wtphrprcs_16", "wtphrtxs_16", "wtplfstl_16",
                          "wtphrprcs_17", "wtphrtxs_17", "wtplfstl_17",
                          "wtphrprcs_18", "wtphrtxs_18", "wtplfstl_18",
                          "wtphrprcs_19", "wtphrtxs_19", "wtplfstl_19")
              )
summary(fac.fit, fit.measures = TRUE, standardized = TRUE)

# --- Scalar invariance 

fas <- '
# Measurement models 
will14 =~ wtphrprcs_14 + l2*wtphrtxs_14 + l3*wtplfstl_14
will15 =~ wtphrprcs_15 + l2*wtphrtxs_15 + l3*wtplfstl_15
will16 =~ wtphrprcs_16 + l2*wtphrtxs_16 + l3*wtplfstl_16
will17 =~ wtphrprcs_17 + l2*wtphrtxs_17 + l3*wtplfstl_17
will18 =~ wtphrprcs_18 + l2*wtphrtxs_18 + l3*wtplfstl_18
will19 =~ wtphrprcs_19 + l2*wtphrtxs_19 + l3*wtplfstl_19
# Error covariances within variables across time
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
'
fas.fit <- cfa(model = fas, data = dfw, #missing = "ML",
              ordered = c("wtphrprcs_14", "wtphrtxs_14", "wtplfstl_14",
                          "wtphrprcs_15", "wtphrtxs_15", "wtplfstl_15",
                          "wtphrprcs_16", "wtphrtxs_16", "wtplfstl_16",
                          "wtphrprcs_17", "wtphrtxs_17", "wtplfstl_17",
                          "wtphrprcs_18", "wtphrtxs_18", "wtplfstl_18",
                          "wtphrprcs_19", "wtphrtxs_19", "wtplfstl_19")
)
summary(fas.fit, fit.measures = TRUE, standardized = TRUE)

# --- Threshold invariance

fat <- '
# Measurement models 
will14 =~ wtphrprcs_14 + l2*wtphrtxs_14 + l3*wtplfstl_14
will15 =~ wtphrprcs_15 + l2*wtphrtxs_15 + l3*wtplfstl_15
will16 =~ wtphrprcs_16 + l2*wtphrtxs_16 + l3*wtplfstl_16
will17 =~ wtphrprcs_17 + l2*wtphrtxs_17 + l3*wtplfstl_17
will18 =~ wtphrprcs_18 + l2*wtphrtxs_18 + l3*wtplfstl_18
will19 =~ wtphrprcs_19 + l2*wtphrtxs_19 + l3*wtplfstl_19
# Error covariances within variables across time
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
fat.fit <- cfa(model = fat, data = dfw, #missing = "ML",
               ordered = c("wtphrprcs_14", "wtphrtxs_14", "wtplfstl_14",
                           "wtphrprcs_15", "wtphrtxs_15", "wtplfstl_15",
                           "wtphrprcs_16", "wtphrtxs_16", "wtplfstl_16",
                           "wtphrprcs_17", "wtphrtxs_17", "wtplfstl_17",
                           "wtphrprcs_18", "wtphrtxs_18", "wtplfstl_18",
                           "wtphrprcs_19", "wtphrtxs_19", "wtplfstl_19")
)
summary(fat.fit, fit.measures = TRUE, standardized = TRUE)

