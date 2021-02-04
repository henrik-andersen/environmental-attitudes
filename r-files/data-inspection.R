### ==================================================== ##
### Title:  Preliminary data inspection
### Date:   04.02.2021
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