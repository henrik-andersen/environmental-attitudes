### ==================================================== ##
### Title:  Preliminary data cleaning, recodes, analyses
### Date:   14.01.2021
### Author: Henrik K. Andersen 
### ==================================================== ## 

# Read merged data 
df <- readRDS("datc.Rda")


# Overview of variables of interest ---------------------------------------

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


# --- Demography ? 


# Select relevant columns for smaller dataframe ---------------------------

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

# Vector of variable names
waves <- c("bc", "cc", "dc", "ec", "fc", "gc")
varnames <- c("zd031a", "zd032a", "zd033a", "zd034a", "zd035a", "zd036a", 
              "zd037a", "zd038a", "zd039a", "zd040a", "zd041a")
vars <- paste0(waves, rep(varnames, each = length(waves)))

# Make new, smaller dataframe 
dfs <- df[, vars]


# Set missings ------------------------------------------------------------

dfs[dfs < 0 | dfs == 98] <- NA


# Delete rows with all missings  ------------------------------------------

dfs <- dfs[rowSums(is.na(dfs)) != ncol(dfs), ]


# Save smaller dataframe --------------------------------------------------

saveRDS(dfs, "r-files/dfs.Rda")


