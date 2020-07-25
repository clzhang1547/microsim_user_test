# partition full ACS file into states by place of residence and place of work
#===============================================================================================================================
# Requires 2012-2016 ACS PUMS files, found here:
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_pums_csv_2012_2016&prodType=document
#=============================================================================================================================

cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)
library(dummies); library(stats); library(rlist); library(plyr); library(dplyr);  library(survey); library(class); library(varhandle)
source("1_cleaning_functions.R")

states <- read.csv("csv_inputs/ACS_state_codes.csv")


# clear the state R dataframe files folder to ensure no duplication from appending occurs
for (i in dir('./R_dataframes/states')) {
  unlink(paste0('./R_dataframes/states/',i))
}
# loop through each of the four census files
for (i in c('a','b','c','d')) {
  print(paste0('reading file ',i))
  # delete objects from previous loop to preserve memory 
  rm(d_acs,d_acs_person,d_acs_house)
  # load the file
  d_acs_person <- read.csv(paste0("./csv_inputs/ss16pus",i,'.csv'))#,nrows=100)
  d_acs_house <-  read.csv(paste0("./csv_inputs/ss16hus",i,'.csv'))#,nrows=100)
  # clean the file
  d_acs <- clean_acs(d_acs_person, d_acs_house, save_csv=FALSE)  
  # get unique values that residence/work state variables take in the data, <=56 values only so only states/DC are included
  res_states <- na.omit(unique(d_acs$ST))
  res_states <- res_states[res_states<=56]
  work_states <- na.omit(unique(d_acs$POWSP))
  work_states <- work_states[work_states<=56]
  # loop through each of the resident states in the census file
  for (j in res_states) {
    # get state name
    name <- states %>% filter(ST==j) %>% select(state_name) %>% unfactor() %>% pull()
    print(paste0('creating resident state ',name))
    # check if file already exists in states folder
    if (paste0(name,'_resid.rds') %in% dir('./R_dataframes/states')){
      # if yes, load the existing .rds file
      temp_state <- readRDS(paste0('./R_dataframes/states/',name,'_resid.rds'))
      # then append all applicable values from acs dataframe
      temp_state <- rbind(temp_state, d_acs %>% filter(ST==j))
      # save the updated state dataframe
      saveRDS(temp_state,file=paste0('./R_dataframes/states/',name,'_resid.rds'))
    }
    # if the state doesn't exist, save the applicable values from the dataframe as a new file
    else {
      saveRDS(d_acs %>% filter(ST==j),file=paste0('./R_dataframes/states/',name,'_resid.rds'))
    }
  }
  # repeat for state of work
  for (j in work_states) {
    print(paste0('creating working state ',name))
    # get state name
    name <- states %>% filter(ST==j) %>% select(state_name) %>% unfactor() %>% pull()
    # check if file already exists in states folder
    if (paste0(name,'_work.rds') %in% dir('./R_dataframes/states')){
      # if yes, load the existing .rds file
      temp_state <- readRDS(paste0('./R_dataframes/states/',name,'_work.rds'))
      # then append all applicable values from acs dataframe
      temp_state <- rbind(temp_state, d_acs %>% filter(POWSP==j))
      # save the updated state dataframe
      saveRDS(temp_state,file=paste0('./R_dataframes/states/',name,'_work.rds'))
    }
    # if the state doesn't exist, save the applicable values from the dataframe as a new file
    else {
      saveRDS(d_acs %>% filter(POWSP==j),file=paste0('./R_dataframes/states/',name,'_work.rds'))
    }
  }
}

