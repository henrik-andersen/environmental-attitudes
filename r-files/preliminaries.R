### ==================================================== ##
### Title:  Preliminary data cleaning, recodes, analyses
### Date:   14.01.2021
### Author: Henrik K. Andersen 
### ==================================================== ## 

# Read merged data 
df <- readRDS("r-files/datc.Rda")

# head(df)[, 1:10]


# Overview of variables of interest ---------------------------------------

# Person ID
# z000001a

# --- Demography/controls? 

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

# German citizenship
# Do you possess the German citizenship?
# 1: Yes
# 2: No
# Missings: < 0 


# ---
# First 10 columns of dataframe are IDs, study numbers and weights
# --- 

#     Klimaschutzpolitik -- Ökostrom
# bc: 34                 -- 45
# cc: 31                 -- 41
# dc: 31                 -- 41
# ec: 31                 -- 41
# fc: 31                 -- 41
# gc: 31                 -- 41


# --- Attitudes towards climate change 

# bczd034a, XXzd031a, XX = cc, dc, ec, fc, gc
# Climate protection policy - Pace
# Do you think that Germany should lead the way in climate politics, or adapt its pace to other countries?
# 1: Rather precede
# 2: Rather adapt to other countries
# Missings: 98 (don't know), < 0
summary(df$bczd034a)

# bczd035a, XXzd032a, XX = cc, dc, ec, fc, gc
# Seriousness of climate change problem
# How serious a problem do you think climate change is at this moment?
# 1: Not at all serious
# ...
# 11: Extremely serious
# Missings: < 0 
summary(df$bczd035a)

# --- Willingness to pay

# bczd020a, XXzd017a, XX = cc, dc, ec, fc, gc 
# How willing would you be to pay much higher prices in order to protect the environment?
# 1: Very acceptable
# 2: Rather acceptable
# 3: Neither acceptable nor unacceptable
# 4: Rather unacceptable
# 5: Very unacceptable
# Missings: < 0

# bczd021a, XXzd018a, XX = cc, dc, ec, fc, gc
# Willingness to pay environment: Higher taxes
# How willing would you be to pay much higher taxes in order to protect the environment?
# 1: Very acceptable
# 2: Rather acceptable
# 3: Neither acceptable nor unacceptable
# 4: Rather unacceptable
# 5: Very unacceptable
# Missings: < 0

# bczd022a, XXzd019a, XX = cc, dc, ec, fc, gc
# Willingness to pay environment: Cut standard of living
# And how willing would you be to accept cuts in your standard of living in order to protect the environment?
# 1: Very acceptable
# 2: Rather acceptable
# 3: Neither acceptable nor unacceptable
# 4: Rather unacceptable
# 5: Very unacceptable
# Missings: < 0


# --- Behaviour 

# bczd036a, XXzd033a, XX = cc, dc, ec, fc, gc
# Property Public transport season ticket
# The following questions are about mobility and transport. Do you own one or more season tickets?
# 1: yes
# 2: No
# Missings: < 0 
summary(df$bczd036a)

# bczd037a, XXzd034a, XX = cc, dc, ec, fc, gc
# Car availability
# How ofen do you have a car at your disposal?
# 1: Anytime
# 2: Sometimes
# 3: As an exception
# 4: Not at all
# Missings: < 0 
summary(df$bczd037a)

# bczd038a, XXzd035a, XX = cc, dc, ec, fc, gc
# Frequency of use: Car
# Please specify how ofen you normally use the following means of transportation. Car
# 1: (Almost) daily
# 2: On 1 to 3 days per week
# 3: On 1 to 3 days per month
# 4: Rarer 
# 5: (Almost) never
# Missings: < 0 
summary(df$bczd038a)

# bczd039a, XXzd036a, XX = cc, dc, ec, fc, gc
# Frequency of use: Bike
# Please specify how ofen you normally use the following means of transportation. Bike
# 1: (Almost) daily
# 2: On 1 to 3 days per week
# 3: On 1 to 3 days per month
# 4: Rarer 
# 5: (Almost) never
# Missings: < 0 
summary(df$bczd039a)

# bczd040a, XXzd037a, XX = cc, dc, ec, fc, gc
# Frequency of use: Bus or train in the region
# Please specify how ofen you normally use the following means of transportation. Regional bus or train
# 1: (Almost) daily
# 2: On 1 to 3 days per week
# 3: On 1 to 3 days per month
# 4: Rarer 
# 5: (Almost) never
# Missings: < 0 
summary(df$bczd040a)

# bczd041a, XXzd038a, XX = cc, dc, ec, fc, gc
# Frequency of use: Train on longer distances
# Please specify how ofen you normally use the following means of transportation. Train on longer distances
# 1: (Almost) daily
# 2: On 1 to 3 days per week
# 3: On 1 to 3 days per month
# 4: Rarer 
# 5: (Almost) never
# Missings: < 0 
summary(df$bczd041a)

# bczd042a
# Usage Plane for leisure travel
# Did you use a plane for private travel last year? By privatetravel wemean e.g. vacations or visits. Outward andreturn journey count as one trip.
# 1: Yes, one
# 2: Yes, more than one
# 3: No
# 4: No private journey
# Missings: < 0 
summary(df$bczd042a)

# bczd043a, XXzd039a, XX = cc, dc, ec, fc, gc
# Purchase organic groceries
# Did you buy any organic food during the past week, that is food of controlled organic cultivation?
# 1: No, none
# 2: Yes, sometimes
# 3: Yes, (almost) exclusively
# Missings: 98 (don't know), < 0
summary(df$bczd043a)

# bczd044a, XXzd040a, XX = cc, dc, ec, fc, gc
# Purchase regional food
# Did you buy any fruits and vegetables from regional producers during the past week that is fruit and vegetables that was cultivated in your region?
# 1: No, none
# 2: Yes, sometimes
# 3: Yes, (almost) exclusively
# Missings: 98 (don't know), < 0
summary(df$bczd044a)

# bczd045a, XXzd041a, XX = cc, dc, ec, fc, gc
# Purchase green energy
# Many electricity suppliers offer green electricity from renewable energies such as solar, wind or water. Do you use green electricity or plan to do so in the future?
# 1: A already draw benefits
# 2: I plan to 
# 3: Maybe in the future
# 4: No 
# Missings: 98 (don't know), < 0
summary(df$bczd045a)


# Rename variables --------------------------------------------------------

# Rename bc items to conform with rest of waves
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

df$bczd017a <- df$bczd020a
df$bczd018a <- df$bczd021a
df$bczd019a <- df$bczd022a

# waves <- 1:6
# vars <- 41 - 31
# pace_vars <- paste0(rep(c("bc", "cc", "dc", "ec", "fc", "gc"), each = 1), "zd0", 31, "a")
# pace_vars
# head(df, n = 60)[, pace_vars]
# 
# df[, pace_vars] <- factor(c(df$bczd031a, df$cczd031a, df$dczd031a, df$eczd031a, df$fczd031a, df$gczd031a), 
#                           levels = c(1, 2), labels = c("Yes", "No"))
# 
# head(df, n = 60)[, pace_vars]


# Select relevant columns for smaller dataframe ---------------------------

# Vector of variable names
waves <- c("bc", "cc", "dc", "ec", "fc", "gc")
varnames <- c("zd031a", "zd032a", "zd033a", "zd034a", "zd035a", "zd036a", 
              "zd037a", "zd038a", "zd039a", "zd040a", "zd041a", 
              "zd017a", "zd018a", "zd019a")
vars <- paste0(waves, rep(varnames, each = length(waves)))

# Make new, smaller dataframe 
dfw <- df[, 1:10]
dfw <- cbind(dfw, df[, vars])
dfw <- cbind(dfw, df[, c("sex", "yob")])


# Set missings ------------------------------------------------------------

dfw[dfw < 0 | dfw == 98] <- NA


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
varnames <- c("zd017a", "zd018a", "zd019a")
vars <- paste0(waves, rep(varnames, each = length(waves)))
vars

# Recode willingness to pay so higher values = more willingness
dfw[, vars] <- recode_func(dfw[, vars], ncat = 5)

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

# WTP: higher taxes
dfl_wtplfstl <- melt(dfw, 
                     id.vars = c("z000001a"),
                     measure.vars = c("bczd019a", "cczd019a", "dczd019a",
                                      "eczd019a", "fczd019a", "gczd019a"),
                     variable.name = "year",
                     value.name = "wtplfstl")

head(dfl_wtphrprcs)

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

head(dfl)
length(dfl)
# Delete rows with NAs on all substantive variables
dfl <- dfl[rowSums(is.na(dfl)[, 12:25]) < ncol(dfl[12:25]), ]

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

col_names <- c(tivars, 
               paste(rep(vars, each = length(years)), "_", years, sep = ""), 
               demo_vars)
names(dfw) <- col_names
head(dfw)

# Save smaller dataframe --------------------------------------------------

# dfw for "dataframe wide"
saveRDS(dfw, "r-files/dfw.Rda")
write.table(dfw, file = "r-files/dfw.csv", sep = ",", row.names = FALSE, col.names = FALSE)

help(write.csv)
