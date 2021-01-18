### ==================================================== ##
### Title:  Factor analyses
### Date:   16.01.2021
### Author: Henrik K. Andersen 
### ==================================================== ## 

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


# SEM ---------------------------------------------------------------------

# Higher prices
re <- '
# Individual effects
eta =~ 1*bczd017a + 1*cczd017a + 1*dczd017a + 1*eczd017a + 1*fczd017a + 1*gczd017a  
# Regressions 
bczd017a ~ beta*bczd032a
cczd017a ~ beta*cczd032a
dczd017a ~ beta*dczd032a
eczd017a ~ beta*eczd032a
fczd017a ~ beta*fczd032a
gczd017a ~ beta*gczd032a
# Covariances
eta ~~ 0*bczd032a + 0*cczd032a + 0*dczd032a + 0*eczd032a + 0*fczd032a + 0*gczd032a 
bczd032a ~~ cczd032a + dczd032a + eczd032a + fczd032a + gczd032a
cczd032a ~~ dczd032a + eczd032a + fczd032a + gczd032a
dczd032a ~~ eczd032a + fczd032a + gczd032a
eczd032a ~~ fczd032a + gczd032a
fczd032a ~~ gczd032a
# Errors 
bczd017a ~~ u*bczd017a
cczd017a ~~ u*cczd017a
dczd017a ~~ u*dczd017a
eczd017a ~~ u*eczd017a
fczd017a ~~ u*fczd017a
gczd017a ~~ u*gczd017a
'
re.fit <- sem(model = re, data = dfw, missing = "ML")
summary(re.fit, fit.measures = TRUE, standardized = TRUE)

# Higher prices
fe <- '
# Individual effects
eta =~ 1*bczd017a + 1*cczd017a + 1*dczd017a + 1*eczd017a + 1*fczd017a + 1*gczd017a  
# Regressions 
bczd017a ~ beta*bczd032a
cczd017a ~ beta*cczd032a
dczd017a ~ beta*dczd032a
eczd017a ~ beta*eczd032a
fczd017a ~ beta*fczd032a
gczd017a ~ beta*gczd032a
# Covariances
eta ~~ bczd032a + cczd032a + dczd032a + eczd032a + fczd032a + gczd032a 
bczd032a ~~ cczd032a + dczd032a + eczd032a + fczd032a + gczd032a
cczd032a ~~ dczd032a + eczd032a + fczd032a + gczd032a
dczd032a ~~ eczd032a + fczd032a + gczd032a
eczd032a ~~ fczd032a + gczd032a
fczd032a ~~ gczd032a
# Errors 
bczd017a ~~ u*bczd017a
cczd017a ~~ u*cczd017a
dczd017a ~~ u*dczd017a
eczd017a ~~ u*eczd017a
fczd017a ~~ u*fczd017a
gczd017a ~~ u*gczd017a
'
fe.fit <- sem(model = fe, data = dfw, missing = "ML")
summary(fe.fit, fit.measures = TRUE, standardized = TRUE)

anova(fe.fit, re.fit)

fe_seq <- '
# Individual effects
eta =~ 1*bczd017a + 1*cczd017a + 1*dczd017a + 1*eczd017a + 1*fczd017a + 1*gczd017a  
# Regressions 
bczd017a ~ beta*bczd032a
cczd017a ~ beta*cczd032a
dczd017a ~ beta*dczd032a
eczd017a ~ beta*eczd032a
fczd017a ~ beta*fczd032a
gczd017a ~ beta*gczd032a
# Covariances
eta ~~ bczd032a + cczd032a + dczd032a + eczd032a + fczd032a + gczd032a 
bczd032a ~~ cczd032a + dczd032a + eczd032a + fczd032a + gczd032a
cczd032a ~~ dczd032a + eczd032a + fczd032a + gczd032a
dczd032a ~~ eczd032a + fczd032a + gczd032a
eczd032a ~~ fczd032a + gczd032a
fczd032a ~~ gczd032a
# Sequential exogeneity
bczd017a ~~ cczd032a + dczd032a + eczd032a + fczd032a + gczd032a
cczd017a ~~ dczd032a + eczd032a + fczd032a + gczd032a
dczd017a ~~ eczd032a + fczd032a + gczd032a
eczd017a ~~ fczd032a + gczd032a
fczd017a ~~ gczd032a
# Errors 
bczd017a ~~ u*bczd017a
cczd017a ~~ u*cczd017a
dczd017a ~~ u*dczd017a
eczd017a ~~ u*eczd017a
fczd017a ~~ u*fczd017a
gczd017a ~~ u*gczd017a
'
fe_seq.fit <- sem(model = fe_seq, data = dfw, missing = "ML")
summary(fe_seq.fit, fit.measures = TRUE, standardized = TRUE)

anova(fe_seq.fit, fe.fit, re.fit)

fe_seq_ar <- '
# Individual effects
eta =~ 1*cczd017a + 1*dczd017a + 1*eczd017a + 1*fczd017a + 1*gczd017a  
# Regressions 
cczd017a ~ beta*cczd032a + rho*bczd017a
dczd017a ~ beta*dczd032a + rho*cczd017a
eczd017a ~ beta*eczd032a + rho*dczd017a
fczd017a ~ beta*fczd032a + rho*eczd017a
gczd017a ~ beta*gczd032a + rho*fczd017a
# Covariances
eta ~~ bczd017a + bczd032a + cczd032a + dczd032a + eczd032a + fczd032a + gczd032a 
bczd017a ~~ bczd032a + cczd032a + dczd032a + eczd032a + fczd032a + gczd032a 
bczd032a ~~ cczd032a + dczd032a + eczd032a + fczd032a + gczd032a
cczd032a ~~ dczd032a + eczd032a + fczd032a + gczd032a
dczd032a ~~ eczd032a + fczd032a + gczd032a
eczd032a ~~ fczd032a + gczd032a
fczd032a ~~ gczd032a
# Sequential exogeneity
cczd017a ~~ dczd032a + eczd032a + fczd032a + gczd032a
dczd017a ~~ eczd032a + fczd032a + gczd032a
eczd017a ~~ fczd032a + gczd032a
fczd017a ~~ gczd032a
# Errors 
bczd017a ~~ u*bczd017a
cczd017a ~~ u*cczd017a
dczd017a ~~ u*dczd017a
eczd017a ~~ u*eczd017a
fczd017a ~~ u*fczd017a
gczd017a ~~ u*gczd017a
'
fe_seq_ar.fit <- sem(model = fe_seq_ar, data = dfw, missing = "ML")
summary(fe_seq_ar.fit, fit.measures = TRUE, standardized = TRUE)

anova(fe_seq_ar.fit, fe_seq.fit, fe.fit, re.fit)
