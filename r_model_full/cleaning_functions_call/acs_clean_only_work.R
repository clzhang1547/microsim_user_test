# partition full ACS file into states by place of residence and place of work
#===============================================================================================================================
# Requires 2012-2016 ACS PUMS files, found here:
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_pums_csv_2012_2016&prodType=document
#=============================================================================================================================

cat("\014")  
options(error=recover)
library(MASS); library(dummies); library(stats); library(rlist); library(plyr); library(dplyr);  library(survey); library(class); library(varhandle)
source("1_cleaning_functions.R")
source("3_impute_functions.R")

states <- read.csv("csv_inputs/ACS_state_codes.csv")

# load cleaned cps data
d_cps <- readRDS(paste0("./R_dataframes/","d_cps.rds"))
first <- TRUE
# do state of residence first; just read csv's and process
for (i in states[,'state_abbr']) {
  print(paste0('creating work data set for ',i))
  lower_abbr <- tolower(i)
  code <- toString(states[states['state_abbr']==i,'ST'])
  if (nchar(code)==1){
    code <- paste0('0',code)
  }
  # delete objects from previous loop to preserve memory
  if (first==FALSE){
    rm(d_acs,d_acs_person,d_acs_house)  
  }
  
  # load the file
  d_acs_person <- read.csv(paste0("./csv_inputs/states/p",code,'_',lower_abbr,'_pow.csv'))#,nrows=100)
  d_acs_house <- read.csv(paste0("./csv_inputs/states/h",code,'_',lower_abbr,'_pow.csv'))#,nrows=100)
  # clean the file
  d_acs <- clean_acs(d_acs_person, d_acs_house, save_csv=FALSE, POW_weight = TRUE)
  # impute CPS
  d_acs <- impute_cps_to_acs(d_acs, d_cps)
  # save R dataframe
  saveRDS(d_acs,file=paste0('./R_dataframes/work_states/',i,'_work.rds'))
  first <- FALSE
}

