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

# Long format factor analysis
dfl <- readRDS("r-files/dfl.Rda")
head(dfl)


# Long format factor analyses ---------------------------------------------
# Implies scalar + threshold invariance

#                                                                 Mayerl & Andersen  Best & Mayerl
# *- right to adapt environment to our needs: nep_adptenvrnmt_r   yes                yes
# *- human ingenuity: nep_hmingnty_r                              yes                yes 
# *- balance of nature is stable: nep_blncntr_r                   yes                yes  
# *- environmental crisis exaggerated: nep_envcrsisexgrt_r        yes                yes
# *- earth like a spaceship: nep_erthspcshp                       no                 yes 
# *- humans rule over nature: nep_hmrlntr_r                       yes                yes
# *- control nature: nep_cntrlntr_r                               yes                yes
# *- sufficient natural resources: nep_sffcntrsrs_r               yes                no


# Best & Mayerl (2013) ----------------------------------------------------

fa_bm <- '
nep =~ 1*nep_adptenvrnmt_r + nep_hmingnty_r + nep_blncntr_r + nep_envcrsisexgrt_r + nep_erthspcshp + nep_hmrlntr_r + nep_cntrlntr_r  
'
fa_bm.cfa <- cfa(fa_bm, data = dfl, 
                 ordered = c("nep_adptenvrnmt_r", "nep_hmingnty_r", "nep_blncntr_r", "nep_envcrsisexgrt_r", "nep_erthspcshp", "nep_hmrlntr_r", "nep_cntrlntr_r"))
summary(fa_bm.cfa, standardized = TRUE, fit.measures = TRUE)


# Mayerl & Andersen (2017) ------------------------------------------------

fa_ma <- '
nep =~ 1*nep_adptenvrnmt_r + nep_hmingnty_r + nep_blncntr_r + nep_envcrsisexgrt_r + nep_hmrlntr_r + nep_cntrlntr_r + nep_sffcntrsrs_r
'
fa_ma.cfa <- cfa(fa_ma, data = dfl, 
                 ordered = c("nep_adptenvrnmt_r", "nep_hmingnty_r", "nep_blncntr_r", "nep_envcrsisexgrt_r", "nep_hmrlntr_r", "nep_cntrlntr_r", "nep_sffcntrsrs_r"))
summary(fa_ma.cfa, standardized = TRUE, fit.measures = TRUE)


# Hybrid Best, Andersen & Mayerl ------------------------------------------
# Without: nep_sffcntrsrs_r + nep_erthspcshp (both low FLs)

fa_hyb <- '
nep =~ 1*nep_adptenvrnmt_r + nep_hmingnty_r + nep_blncntr_r + nep_envcrsisexgrt_r + nep_hmrlntr_r + nep_cntrlntr_r 
'
fa_hyb.cfa <- cfa(fa_hyb, data = dfl, 
                 ordered = c("nep_adptenvrnmt_r", "nep_hmingnty_r", "nep_blncntr_r", "nep_envcrsisexgrt_r", "nep_hmrlntr_r", "nep_cntrlntr_r"))
summary(fa_hyb.cfa, standardized = TRUE, fit.measures = TRUE)


# No need to test wide data: it will be the same as long format CFAs with scalar + threshold invariance 