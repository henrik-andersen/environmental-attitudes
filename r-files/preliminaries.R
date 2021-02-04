### ==================================================== ##
### Title:  Preliminary data cleaning, recodes, analyses
### Date:   14.01.2021
### Author: Henrik K. Andersen 
### ==================================================== ## 

rm(list = ls())

# Read merged data 
df <- readRDS("r-files/datc.Rda")

# head(df)[, 1:10]

# Recode sex and age ------------------------------------------------------

# a11d054a, bfzh069a, cfzh071a, d11d054a, dfzh037a, efzh031a, f119054a, ffzh031a
# Gender
# Are you male of female?
# 1: Male
# 2: Female
# Missings: < 0 

# table(df$z000006a, df$a11d054a)
# table(df$z000006a, df$d11d054a)
# table(df$z000006a, df$f11d054a)
# 
# length(df$z000001a)
# 7599 + 2124 + 1038 + 1036

# Looks like, based on those tables, that the sex variable is stored
# in the three variables above (recruitment waves, a11, d11, f11). I don't know 
# what the other variables between that (bfzh069a, cfzh071a, etc.).

# table(df$z000006a, df$bfzh069a)

df$a11d054a <- ifelse(df$a11d054a < 0, NA, df$a11d054a)
df$d11d054a <- ifelse(df$d11d054a < 0, NA, df$d11d054a)
df$f11d054a <- ifelse(df$f11d054a < 0, NA, df$f11d054a)

df$sex <- df$a11d054a; table(df$sex); sum(table(df$sex))
df$sex <- ifelse(is.na(df$sex), df$d11d054a, df$sex); table(df$sex); sum(table(df$sex))
df$sex <- ifelse(is.na(df$sex), df$f11d054a, df$sex); table(df$sex); sum(table(df$sex)); length(df$z000001a)

# a11d056b, d11d056b, f11d056b
# Year of birth, extreme values summarized
# When were you born? Please provide the year of your birth.
# 1943: <= 1943
# ...
# 1995: >= 1995
# Missings: < 0 

df$a11d056b <- ifelse(df$a11d056b < 0, NA, df$a11d056b)
df$d11d056b <- ifelse(df$d11d056b < 0, NA, df$d11d056b)
df$f11d056b <- ifelse(df$f11d056b < 0, NA, df$f11d056b)

df$yob <- df$a11d056b; table(df$yob); sum(table(df$yob))
df$yob <- ifelse(is.na(df$yob), df$d11d056b, df$yob); table(df$yob); sum(table(df$yob))
df$yob <- ifelse(is.na(df$yob), df$f11d056b, df$yob); table(df$yob); sum(table(df$yob)); length(df$z000001a)




# Rename variables --------------------------------------------------------

# --- Rename bc items to conform with rest of waves
# Klimaschutzpolitik - Tempo to Bezug Ökostrom
df$bczd031a <- df$bczd034a 
df$bczd032a <- df$bczd035a 
df$bczd033a <- df$bczd036a 
df$bczd034a <- df$bczd037a 
df$bczd035a <- df$bczd038a 
df$bczd036a <- df$bczd039a 
df$bczd037a <- df$bczd040a 
df$bczd038a <- df$bczd041a 
df$bczd039a <- df$bczd043a 
df$bczd040a <- df$bczd044a 
df$bczd041a <- df$bczd045a 
# Willingness to pay 
df$bczd017a <- df$bczd020a
df$bczd018a <- df$bczd021a
df$bczd019a <- df$bczd022a


# Select relevant columns for smaller dataframe ---------------------------

# Vector of variable names
waves <- c("bc", "cc", "dc", "ec", "fc", "gc")
varnames <- c("zd031a", "zd032a", "zd033a", "zd034a", "zd035a", "zd036a", 
              "zd037a", "zd038a", "zd039a", "zd040a", "zd041a", 
              "zd017a", "zd018a", "zd019a")
vars <- paste0(waves, rep(varnames, each = length(waves)))

# --- NEP
# bczd005a:bczd019a (just for bc)
# xxzd002a:xxd016a  (for the rest of the waves)

# Rename bc wave
df$bczd002a <- df$bczd005a
df$bczd003a <- df$bczd006a
df$bczd004a <- df$bczd007a
df$bczd005a <- df$bczd008a
df$bczd006a <- df$bczd009a
df$bczd007a <- df$bczd010a
df$bczd008a <- df$bczd011a
df$bczd009a <- df$bczd012a
df$bczd010a <- df$bczd013a
df$bczd011a <- df$bczd014a
df$bczd012a <- df$bczd015a
df$bczd013a <- df$bczd016a
df$bczd014a <- df$bczd017a
df$bczd015a <- df$bczd018a
df$bczd016a <- df$bczd019a

nep_suffix <- c("zd002a", "zd003a", "zd004a", "zd005a", "zd006a", "zd007a", 
                "zd008a", "zd009a", "zd010a", "zd011a", "zd012a", "zd013a", 
                "zd014a", "zd015a", "zd016a")

nep_vars <- paste0(waves, rep(nep_suffix, each = length(waves)))

# Make new, smaller dataframe 
dfw <- df[, 1:10]
dfw <- cbind(dfw, df[, vars])
dfw <- cbind(dfw, df[, c("sex", "yob")])
dfw <- cbind(dfw, df[, nep_vars])


# Set missings ------------------------------------------------------------

dfw[dfw < 0 | dfw == 98] <- NA
head(dfw)

# Recodes -----------------------------------------------------------------

# Test recode_func
# x1 <- c(1, 5)
# x2 <- c(5, 1)
# dftest <- data.frame(x1, x2)
# dftest
# 
# recode_func <- function(x, ncat) {
#   y <- abs(x - (ncat + 1))
#   return(y)
# }
# 
# dftest$x1 <- recode_func(dftest$x1, ncat = 5)
# dftest
# 
# dftest[, c("x1", "x2")] <- recode_func(dftest[, c("x1", "x2")], ncat = 5)
# dftest
# This works 

recode_func <- function(x, ncat) {
     y <- abs(x - (ncat + 1))
     return(y)
}

waves <- c("bc", "cc", "dc", "ec", "fc", "gc")
wtp_varnames <- c("zd017a", "zd018a", "zd019a")
wtp_vars <- paste0(waves, rep(wtp_varnames, each = length(waves)))
wtp_vars

# Recode willingness to pay so higher values = more willingness
dfw[, wtp_vars] <- recode_func(dfw[, wtp_vars], ncat = 5)
# Recode NEP so that higher values = more agreement
dfw[, nep_vars] <- recode_func(dfw[, nep_vars], ncat = 5)




# Make long dataframe for further cleaning --------------------------------

head(dfw)

library(reshape2)

# Tempo climate protection policies 
dfl_tmpo_clmtprtct <- melt(dfw, 
                           id.vars = c("z000001a", "z000002a", "z000003a", "z000005a", 
                                     "z000006a", "z000010a", "z000010b", "z000012a", 
                                     "sex", "yob"),
                           measure.vars = c("bczd031a", "cczd031a", "dczd031a",
                                         "eczd031a", "fczd031a", "gczd031a"),
                           variable.name = "year",
                           value.name = "tmpo_clmtprtct")

# Seriousness of climate change
dfl_srs_clmtchng <- melt(dfw, 
                         id.vars = c("z000001a"),
                         measure.vars = c("bczd032a", "cczd032a", "dczd032a",
                                          "eczd032a", "fczd032a", "gczd032a"),
                         variable.name = "year",
                         value.name = "srs_clmtchng")

# Own bus ticket
dfl_bstckt <- melt(dfw, 
                   id.vars = c("z000001a"),
                   measure.vars = c("bczd033a", "cczd033a", "dczd033a",
                                    "eczd033a", "fczd033a", "gczd033a"),
                   variable.name = "year",
                   value.name = "bstckt")
  
# Access car
dfl_accscar <- melt(dfw, 
                    id.vars = c("z000001a"),
                    measure.vars = c("bczd034a", "cczd034a", "dczd034a",
                                     "eczd034a", "fczd034a", "gczd034a"),
                    variable.name = "year",
                    value.name = "accscar")

# Usage car
dfl_usgscar <- melt(dfw, 
                    id.vars = c("z000001a"),
                    measure.vars = c("bczd035a", "cczd035a", "dczd035a",
                                     "eczd035a", "fczd035a", "gczd035a"),
                    variable.name = "year",
                    value.name = "usgscar")

# Usage bike 
dfl_usgsbike <- melt(dfw, 
                    id.vars = c("z000001a"),
                    measure.vars = c("bczd036a", "cczd036a", "dczd036a",
                                     "eczd036a", "fczd036a", "gczd036a"),
                    variable.name = "year",
                    value.name = "usgsbike")

# Usage bus in region 
dfl_usgsbusrgn <- melt(dfw, 
                     id.vars = c("z000001a"),
                     measure.vars = c("bczd037a", "cczd037a", "dczd037a",
                                      "eczd037a", "fczd037a", "gczd037a"),
                     variable.name = "year",
                     value.name = "usgsbusrgn")

# Usage bus long range
dfl_usgsbuslngrng <- melt(dfw, 
                     id.vars = c("z000001a"),
                     measure.vars = c("bczd038a", "cczd038a", "dczd038a",
                                      "eczd038a", "fczd038a", "gczd038a"),
                     variable.name = "year",
                     value.name = "usgsbuslngrng")

# Shop biomarket 
dfl_shpbiomrkt <- melt(dfw, 
                          id.vars = c("z000001a"),
                          measure.vars = c("bczd039a", "cczd039a", "dczd039a",
                                           "eczd039a", "fczd039a", "gczd039a"),
                          variable.name = "year",
                          value.name = "shpbiomrkt")

# Shop regional 
dfl_shprgnlprdct <- melt(dfw, 
                       id.vars = c("z000001a"),
                       measure.vars = c("bczd040a", "cczd040a", "dczd040a",
                                        "eczd040a", "fczd040a", "gczd040a"),
                       variable.name = "year",
                       value.name = "shprgnlprdct")

# Shop Oko Energy
dfl_okoenrgy <- melt(dfw, 
                         id.vars = c("z000001a"),
                         measure.vars = c("bczd041a", "cczd041a", "dczd041a",
                                          "eczd041a", "fczd041a", "gczd041a"),
                         variable.name = "year",
                         value.name = "okoenrgy")

# WTP: higher prices 
dfl_wtphrprcs <- melt(dfw, 
                         id.vars = c("z000001a"),
                         measure.vars = c("bczd017a", "cczd017a", "dczd017a",
                                          "eczd017a", "fczd017a", "gczd017a"),
                         variable.name = "year",
                         value.name = "wtphrprcs")

# WTP: higher taxes
dfl_wtphrtxs <- melt(dfw, 
                      id.vars = c("z000001a"),
                      measure.vars = c("bczd018a", "cczd018a", "dczd018a",
                                       "eczd018a", "fczd018a", "gczd018a"),
                      variable.name = "year",
                      value.name = "wtphrtxs")

# WTP: lifestyle 
dfl_wtplfstl <- melt(dfw, 
                     id.vars = c("z000001a"),
                     measure.vars = c("bczd019a", "cczd019a", "dczd019a",
                                      "eczd019a", "fczd019a", "gczd019a"),
                     variable.name = "year",
                     value.name = "wtplfstl")

# NEP: close to maximum number of humans
dfl_mxhmns <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd002a", "cczd002a", "dczd002a",
                                    "eczd002a", "fczd002a", "gczd002a"),
                   variable.name = "year",
                   value.name = "nep_mxhmns")

# NEP: right to adapt environment to our needs                                   RECODE!! 
dfl_adptenvrnmt <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd003a", "cczd003a", "dczd003a",
                                    "eczd003a", "fczd003a", "gczd003a"),
                   variable.name = "year",
                   value.name = "nep_adptenvrnmt")

# NEP: consequences human intervention
dfl_hmintrvn <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd004a", "cczd004a", "dczd004a",
                                    "eczd004a", "fczd004a", "gczd004a"),
                   variable.name = "year",
                   value.name = "nep_hmintrvn")

# NEP: human ingenuity                                                           RECODE 
dfl_hmingnty <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd005a", "cczd005a", "dczd005a",
                                    "eczd005a", "fczd005a", "gczd005a"),
                   variable.name = "year",
                   value.name = "nep_hmingnty")

# NEP: abuse of environment by humans
dfl_absenvrnmnt <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd006a", "cczd006a", "dczd006a",
                                    "eczd006a", "fczd006a", "gczd006a"),
                   variable.name = "year",
                   value.name = "nep_absenvrnmnt")

# NEP: sufficient natural resources                                              RECODE
dfl_sffcntrsrs <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd007a", "cczd007a", "dczd007a",
                                    "eczd007a", "fczd007a", "gczd007a"),
                   variable.name = "year",
                   value.name = "nep_sffcntrsrs")

# NEP: plants and animals same rights
dfl_anmlsrghts <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd008a", "cczd008a", "dczd008a",
                                    "eczd008a", "fczd008a", "gczd008a"),
                   variable.name = "year",
                   value.name = "nep_anmlsrghts")

# NEP: balance of nature is stable                                               RECODE
dfl_blncntr <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd009a", "cczd009a", "dczd009a",
                                    "eczd009a", "fczd009a", "gczd009a"),
                   variable.name = "year",
                   value.name = "nep_blncntr")

# NEP: humans subjected to laws of nature
dfl_hmnatrlaws <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd010a", "cczd010a", "dczd010a",
                                    "eczd010a", "fczd010a", "gczd010a"),
                   variable.name = "year",
                   value.name = "nep_hmnatrlaws")

# NEP: environmental crisis exaggerated                                          RECODE 
dfl_envcrsisexgrt <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd011a", "cczd011a", "dczd011a",
                                    "eczd011a", "fczd011a", "gczd011a"),
                   variable.name = "year",
                   value.name = "nep_envcrsisexgrt")

# NEP: earth like a spaceship 
dfl_erthspcshp <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd012a", "cczd012a", "dczd012a",
                                    "eczd012a", "fczd012a", "gczd012a"),
                   variable.name = "year",
                   value.name = "nep_erthspcshp")

# NEP: humans rule over nature                                                   RECODE 
dfl_hmrlntr <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd013a", "cczd013a", "dczd013a",
                                    "eczd013a", "fczd013a", "gczd013a"),
                   variable.name = "year",
                   value.name = "nep_hmrlntr")

# NEP: balance of nature is sensitive
dfl_blncntrsnstv <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd014a", "cczd014a", "dczd014a",
                                    "eczd014a", "fczd014a", "gczd014a"),
                   variable.name = "year",
                   value.name = "nep_blncntrsnstv")

# NEP: control nature                                                            RECODE 
dfl_cntrlntr <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd015a", "cczd015a", "dczd015a",
                                    "eczd015a", "fczd015a", "gczd015a"),
                   variable.name = "year",
                   value.name = "nep_cntrlntr")

# NEP: environmental disaster                                          
dfl_envrnmtdstr <- melt(dfw, 
                   id.vars = c("z000001a"), 
                   measure.vars = c("bczd016a", "cczd016a", "dczd016a",
                                    "eczd016a", "fczd016a", "gczd016a"),
                   variable.name = "year",
                   value.name = "nep_envrnmtdstr")

year_rename_func <- function(x) {
  y <- ifelse(substr(x, start = 1, stop = 1) == "b", 2014, 
              ifelse(substr(x, start = 1, stop = 1) == "c", 2015, 
                     ifelse(substr(x, start = 1, stop = 1) == "d", 2016,
                            ifelse(substr(x, start = 1, stop = 1) == "e", 2017,
                                   ifelse(substr(x, start = 1, stop = 1) == "f", 2018, 
                                          ifelse(substr(x, start = 1, stop = 1) == "g", 2019,
                                                 NA))))))
}

# Rename year to numeric 
dfl_tmpo_clmtprtct$year <- year_rename_func(dfl_tmpo_clmtprtct$year)
dfl_srs_clmtchng$year <- year_rename_func(dfl_srs_clmtchng$year)
dfl_bstckt$year <- year_rename_func(dfl_bstckt$year)
dfl_accscar$year <- year_rename_func(dfl_accscar$year)
dfl_usgscar$year <- year_rename_func(dfl_usgscar$year)
dfl_usgsbike$year <- year_rename_func(dfl_usgsbike$year)
dfl_usgsbusrgn$year <- year_rename_func(dfl_usgsbusrgn$year)
dfl_usgsbuslngrng$year <- year_rename_func(dfl_usgsbuslngrng$year)
dfl_shpbiomrkt$year <- year_rename_func(dfl_shpbiomrkt$year)
dfl_shprgnlprdct$year <- year_rename_func(dfl_shprgnlprdct$year)
dfl_okoenrgy$year <- year_rename_func(dfl_okoenrgy$year)
dfl_wtphrprcs$year <- year_rename_func(dfl_wtphrprcs$year)
dfl_wtphrtxs$year <- year_rename_func(dfl_wtphrtxs$year)
dfl_wtplfstl$year <- year_rename_func(dfl_wtplfstl$year)

dfl_mxhmns$year <- year_rename_func(dfl_mxhmns$year)
dfl_adptenvrnmt$year <- year_rename_func(dfl_adptenvrnmt$year)
dfl_hmintrvn$year <- year_rename_func(dfl_hmintrvn$year)
dfl_hmingnty$year <- year_rename_func(dfl_hmingnty$year)
dfl_absenvrnmnt$year <- year_rename_func(dfl_absenvrnmnt$year)
dfl_sffcntrsrs$year <- year_rename_func(dfl_sffcntrsrs$year)
dfl_anmlsrghts$year <- year_rename_func(dfl_anmlsrghts$year)
dfl_blncntr$year <- year_rename_func(dfl_blncntr$year)
dfl_hmnatrlaws$year <- year_rename_func(dfl_hmnatrlaws$year)
dfl_envcrsisexgrt$year <- year_rename_func(dfl_envcrsisexgrt$year)
dfl_erthspcshp$year <- year_rename_func(dfl_erthspcshp$year)
dfl_hmrlntr$year <- year_rename_func(dfl_hmrlntr$year)
dfl_blncntrsnstv$year <- year_rename_func(dfl_blncntrsnstv$year)
dfl_cntrlntr$year <- year_rename_func(dfl_cntrlntr$year)
dfl_envrnmtdstr$year <- year_rename_func(dfl_envrnmtdstr$year)


# Merge the long dataframes
dfl <- merge(dfl_tmpo_clmtprtct, dfl_srs_clmtchng, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_bstckt, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_accscar, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_usgscar, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_usgsbike, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_usgsbusrgn, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_usgsbuslngrng, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_shpbiomrkt, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_shprgnlprdct, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_okoenrgy, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_wtphrprcs, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_wtphrtxs, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_wtplfstl, by = c("z000001a", "year"))

dfl <- merge(dfl, dfl_mxhmns, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_adptenvrnmt, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_hmintrvn, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_hmingnty, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_absenvrnmnt, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_sffcntrsrs, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_anmlsrghts, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_blncntr, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_hmnatrlaws, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_envcrsisexgrt, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_erthspcshp, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_hmrlntr, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_blncntrsnstv, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_cntrlntr, by = c("z000001a", "year"))
dfl <- merge(dfl, dfl_envrnmtdstr, by = c("z000001a", "year"))


# Recode NEP variables so that they all mean: higher values more environmental concern
dfl$nep_adptenvrnmt_r <- recode_func(dfl$nep_adptenvrnmt, 5)
dfl$nep_hmingnty_r <- recode_func(dfl$nep_hmingnty, 5)
dfl$nep_sffcntrsrs_r <- recode_func(dfl$nep_sffcntrsrs, 5)
dfl$nep_blncntr_r <- recode_func(dfl$nep_blncntr, 5)
dfl$nep_envcrsisexgrt_r <- recode_func(dfl$nep_envcrsisexgrt, 5)
dfl$nep_hmrlntr_r <- recode_func(dfl$nep_hmrlntr, 5)
dfl$nep_cntrlntr_r <- recode_func(dfl$nep_cntrlntr, 5)

head(dfl)
length(dfl)
# Delete rows with NAs on all substantive variables
dfl <- dfl[rowSums(is.na(dfl)[, 12:40]) < ncol(dfl[12:40]), ]

head(dfl, n = 30)

saveRDS(dfl, "r-files/dfl.Rda")
write.table(dfl, file = "r-files/dfl.csv", sep = ",", row.names = FALSE, col.names = FALSE)


# Rename columns dfw ------------------------------------------------------

head(dfw)

tivars <- c("id", "stdynr", "datarchv", "doi", "chrt", "dsnwgt_a1d1", 
            "dsnwgt_a1d1f1", "inclsnprob_a1d1", "inclsnprob_a1d1f1", "intnutz")
years <- c("14", "15", "16", "17", "18", "19")
vars <- c("tmpo_clmtprtct", "srs_clmtchng", "bstckt", "accscar", "usgscar", 
          "usgsbike", "usgsbusrgn", "usgsbuslngrng", "shpbiomrkt", 
          "shprgnlprdct", "okoenrgy", "wtphrprcs", "wtphrtxs", "wtplfstl")
demo_vars <- c("sex", "yob")
nep_vars <- c("nep_mxhmns", "nep_adptenvrnmt_r", "nep_hmintrvn", "nep_hmingnty_r", 
              "nep_absenvrnmnt", "nep_sffcntrsrs_r", "nep_anmlsrghts", "nep_blncntr_r",
              "nep_hmnatrlaws", "nep_envcrsisexgrt_r", "nep_erthspcshp", "nep_hmrlntr_r",
              "nep_blncntrsnstv", "nep_cntrlntr_r", "nep_envrnmtdstr")


col_names <- c(tivars, 
               paste(rep(vars, each = length(years)), "_", years, sep = ""), 
               demo_vars, 
               paste(rep(nep_vars, each = length(years)), "_", years, sep = ""))
names(dfw) <- col_names
head(dfw)

cor(dfw$nep_absenvrnmnt_14, dfw$nep_adptenvrnmt_r_14, use = "complete.obs")

# Recode NEP variables so that they all mean: higher values more environmental concern
# ALSO IN WIDE 
dfw$nep_adptenvrnmt_r_14 <- recode_func(dfw$nep_adptenvrnmt_r_14, 5)
dfw$nep_adptenvrnmt_r_15 <- recode_func(dfw$nep_adptenvrnmt_r_15, 5)
dfw$nep_adptenvrnmt_r_16 <- recode_func(dfw$nep_adptenvrnmt_r_16, 5)
dfw$nep_adptenvrnmt_r_17 <- recode_func(dfw$nep_adptenvrnmt_r_17, 5)
dfw$nep_adptenvrnmt_r_18 <- recode_func(dfw$nep_adptenvrnmt_r_18, 5)
dfw$nep_adptenvrnmt_r_19 <- recode_func(dfw$nep_adptenvrnmt_r_19, 5)

dfw$nep_hmingnty_r_14 <- recode_func(dfw$nep_hmingnty_r_14, 5)
dfw$nep_hmingnty_r_15 <- recode_func(dfw$nep_hmingnty_r_15, 5)
dfw$nep_hmingnty_r_16 <- recode_func(dfw$nep_hmingnty_r_16, 5)
dfw$nep_hmingnty_r_17 <- recode_func(dfw$nep_hmingnty_r_17, 5)
dfw$nep_hmingnty_r_18 <- recode_func(dfw$nep_hmingnty_r_18, 5)
dfw$nep_hmingnty_r_19 <- recode_func(dfw$nep_hmingnty_r_19, 5)

dfw$nep_sffcntrsrs_r_14 <- recode_func(dfw$nep_sffcntrsrs_r_14, 5)
dfw$nep_sffcntrsrs_r_15 <- recode_func(dfw$nep_sffcntrsrs_r_15, 5)
dfw$nep_sffcntrsrs_r_16 <- recode_func(dfw$nep_sffcntrsrs_r_16, 5)
dfw$nep_sffcntrsrs_r_17 <- recode_func(dfw$nep_sffcntrsrs_r_17, 5)
dfw$nep_sffcntrsrs_r_18 <- recode_func(dfw$nep_sffcntrsrs_r_18, 5)
dfw$nep_sffcntrsrs_r_19 <- recode_func(dfw$nep_sffcntrsrs_r_19, 5)

dfw$nep_blncntr_r_14 <- recode_func(dfw$nep_blncntr_r_14, 5)
dfw$nep_blncntr_r_15 <- recode_func(dfw$nep_blncntr_r_15, 5)
dfw$nep_blncntr_r_16 <- recode_func(dfw$nep_blncntr_r_16, 5)
dfw$nep_blncntr_r_17 <- recode_func(dfw$nep_blncntr_r_17, 5)
dfw$nep_blncntr_r_18 <- recode_func(dfw$nep_blncntr_r_18, 5)
dfw$nep_blncntr_r_19 <- recode_func(dfw$nep_blncntr_r_19, 5)

dfw$nep_envcrsisexgrt_r_14 <- recode_func(dfw$nep_envcrsisexgrt_r_14, 5)
dfw$nep_envcrsisexgrt_r_15 <- recode_func(dfw$nep_envcrsisexgrt_r_15, 5)
dfw$nep_envcrsisexgrt_r_16 <- recode_func(dfw$nep_envcrsisexgrt_r_16, 5)
dfw$nep_envcrsisexgrt_r_17 <- recode_func(dfw$nep_envcrsisexgrt_r_17, 5)
dfw$nep_envcrsisexgrt_r_18 <- recode_func(dfw$nep_envcrsisexgrt_r_18, 5)
dfw$nep_envcrsisexgrt_r_19 <- recode_func(dfw$nep_envcrsisexgrt_r_19, 5)

dfw$nep_hmrlntr_r_14 <- recode_func(dfw$nep_hmrlntr_r_14, 5)
dfw$nep_hmrlntr_r_15 <- recode_func(dfw$nep_hmrlntr_r_15, 5)
dfw$nep_hmrlntr_r_16 <- recode_func(dfw$nep_hmrlntr_r_16, 5)
dfw$nep_hmrlntr_r_17 <- recode_func(dfw$nep_hmrlntr_r_17, 5)
dfw$nep_hmrlntr_r_18 <- recode_func(dfw$nep_hmrlntr_r_18, 5)
dfw$nep_hmrlntr_r_19 <- recode_func(dfw$nep_hmrlntr_r_19, 5)

dfw$nep_cntrlntr_r_14 <- recode_func(dfw$nep_cntrlntr_r_14, 5)
dfw$nep_cntrlntr_r_15 <- recode_func(dfw$nep_cntrlntr_r_15, 5)
dfw$nep_cntrlntr_r_16 <- recode_func(dfw$nep_cntrlntr_r_16, 5)
dfw$nep_cntrlntr_r_17 <- recode_func(dfw$nep_cntrlntr_r_17, 5)
dfw$nep_cntrlntr_r_18 <- recode_func(dfw$nep_cntrlntr_r_18, 5)
dfw$nep_cntrlntr_r_19 <- recode_func(dfw$nep_cntrlntr_r_19, 5)


# Save smaller dataframe --------------------------------------------------

head(dfw)
which(colnames(dfw) == "tmpo_clmtprtct_14")
which(colnames(dfw) == "sex")
which(colnames(dfw) == "yob")
which(colnames(dfw) == "nep_mxhmns_14")
which(colnames(dfw) == "nep_envrnmtdstr_19")
length(dfw)
# Delete rows with NAs on all substantive variables
dfw <- dfw[rowSums(is.na(dfw)[, c(11:94, 97:186)]) < ncol(dfw[c(11:94, 97:186)]), ]

# dfw for "dataframe wide"
saveRDS(dfw, "r-files/dfw.Rda")
write.table(dfw, file = "r-files/dfw.csv", sep = ",", row.names = FALSE, col.names = FALSE)

