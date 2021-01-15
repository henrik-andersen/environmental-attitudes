###########################################################################
###													
###		Subject:	R-script for merging/appending the GESIS Panel data
###		Date: 		March 2020
###		Author: 	Jan-Philipp Kolb	
###													
###########################################################################
  
# This R-script puts together all data files of the GESIS Panel 
# resulting in a single file containing all variables and all cohorts, 
# which is structurally identical to the data files offered prior to wave
# ec.

# In order to execute this R-script, please adjust the file path (see 
                                                                 # Preliminaries).

# If you need help or have questions regarding the data please contact:
# gesis-panel@gesis.org


# Preliminarities ---------------------------------------------------------


filepath <- "D:/Henrik/Seafile/Meine Bibliothek/data/ZA5665_GESIS_Panel_v36/data/stata"


if(!("readstata13" %in% rownames(installed.packages()))){
  install.packages("readstata13")
}


files <- grep(".dta",list.files(filepath),value=T)


# get cohorts -------------------------------------------------------------


a1_file <- grep("_a1_v",files,value=T)
dat_a1 <- readstata13::read.dta13(paste0(filepath,"/",a1_file),convert.factors = F)    

d1_file <- grep("_d1_v",files,value=T)
dat_d1 <- readstata13::read.dta13(paste0(filepath,"/",d1_file),convert.factors = F)    

f1_file <- grep("_f1_v",files,value=T)

# merge cohort data -------------------------------------------------------

# if(length(f1_file)>0){
#   dat_f1 <- readstata13::read.dta13(paste0(filepath,"/",f1_file),convert.factors = F)
#   datc <- merge(dat_a1,dat_d1,dat_f1,all=T)
# }else{
#   datc <- merge(dat_a1,dat_d1,all=T)  
# }

dat_f1 <- readstata13::read.dta13(paste0(filepath, "/", f1_file), convert.factors = F)
datc <- merge(dat_a1, dat_d1, all = TRUE)
datc <- merge(datc, dat_f1, all = TRUE)

# Implement the correct missing values ------------------------------------

datc[is.na(datc)] <- -11

# datc is the complete dataset

saveRDS(datc, file = "C:/Users/Henrik/github_projects/environmental-attitudes/r-files/datc.Rda")

