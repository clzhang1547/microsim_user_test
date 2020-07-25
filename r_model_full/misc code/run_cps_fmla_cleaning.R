
# Load and clean csv's for FMLA, ACS, and CPS surveys

cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)
library(dummies); library(stats); library(rlist); library(plyr); library(dplyr);  library(survey); library(class); library(varhandle)
source("1_cleaning_functions.R")

# read in FMLA
d_fmla <- read.csv(paste0("./csv_inputs/", fmla_csv))
#INPUT: Raw file for FMLA survey
d_fmla <- clean_fmla(d_fmla, save_csv=FALSE)
#OUTPUT: clean FMLA dataframe 
d_cps <- read.csv(paste0("./csv_inputs/",cps_csv))
#INPUT: Raw file for CPS 
d_cps <- clean_cps(d_cps)
#OUTPUT: Cleaned CPS dataframe

# save RDS files
saveRDS(d_fmla,file=paste0("./R_dataframes/","d_fmla.rds"))
saveRDS(d_cps,file=paste0("./R_dataframes/","d_cps.rds"))
