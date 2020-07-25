# partition full ACS file into states by place of residence and place of work
#===============================================================================================================================
# Requires 2012-2016 ACS PUMS files, found here:
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_pums_csv_2012_2016&prodType=document
#=============================================================================================================================

cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)
library(MASS); library(dummies); library(stats); library(rlist); library(plyr); library(dplyr);  library(survey); library(class); library(varhandle)
source("1_cleaning_functions.R")
source("3_impute_functions.R")

states <- read.csv("csv_inputs/ACS_state_codes.csv")

# load cleaned cps data
d_cps <- readRDS(paste0("./R_dataframes/","d_cps.rds"))

# do state of residence first; just read csv's and process
for (i in states[,'state_abbr']) {
  print(paste0('creating residence data set for ',i))
  lower_abbr <- tolower(i)
  # delete objects from previous loop to preserve memory
  rm(d_acs,d_acs_person,d_acs_house)
  # load the file
  d_acs_person <- read.csv(paste0("./csv_inputs/states/ss16p",lower_abbr,'.csv'))#,nrows=100)
  d_acs_house <- read.csv(paste0("./csv_inputs/states/ss16h",lower_abbr,'.csv'))#,nrows=100)
  # clean the file
  d_acs <- clean_acs(d_acs_person, d_acs_house, save_csv=FALSE)
  # impute CPS
  d_acs <- impute_cps_to_acs(d_acs, d_cps)
  # save R dataframe
  saveRDS(d_acs,file=paste0('./R_dataframes/resid_states/',i,'_resid.rds'))
}

# remove state of work files to ensure append isn't duplicating records
for (i in dir('./R_dataframes/work_states')) {
  unlink(paste0('./R_dataframes/work_states/',i))
}

# Create state of work files: take each state of residence, and either create new file or append 
# to existing one for each state of work in the data set
for (i in states[,'state_abbr']) {
  print(paste0('loading state of residence data set for ',i))
  lower_abbr <- tolower(i)
  work_dfs <- dir('./R_dataframes/work_states')
  # load state of residence data set
  d_acs <- readRDS(paste0('./R_dataframes/resid_states/',i,'_resid.rds'))
  # update list of existing state of work dataframes created
  # drop those without state of work
  d_acs <- d_acs[is.na(d_acs$POWSP)==FALSE,]
  # for every different state of work...
  for (j in unique(d_acs$POWSP)) {
    # if value is a state/DC...
    if (j %in% states$ST) {
      # filter to those in the state that 
      d_filt <- d_acs[d_acs$POWSP==j,]
      abbr <- states %>% filter(ST==j) %>% select(state_abbr) %>% unfactor() %>% pull()
      # if dataframe exists, load the data frame, append state of work records, and save the data frame
      if (paste0(abbr,'_work.rds') %in% work_dfs) {
        print(paste0('appending to state of work data set for ', abbr))
        existing_df <- readRDS(paste0('./R_dataframes/work_states/',abbr,'_work.rds'))
        new_df <- rbind(existing_df,d_filt)
        print(paste0('Old Record num: ',nrow(existing_df)))
        print(paste0('New Record num: ',nrow(new_df)))
        stopifnot(nrow(new_df)>=nrow(existing_df))
        saveRDS(new_df,file=paste0('./R_dataframes/work_states/',abbr,'_work.rds'))
      }
      # if it doesn't save the filtered data set as a new dataframe
      else {
        print(paste0('creating state of work data set for ', abbr))
        # check to make sure index is not missing
        saveRDS(d_filt,file=paste0('./R_dataframes/work_states/',abbr,'_work.rds'))
      }  
    }
  }
}

# open final work data sets and tabulate number of records for validation
states['num_obs'] <- NA
for (i in states[,'state_abbr']) {
  print(paste0('loading state of work data set for ',i))
  # load state of work data set
  d_acs <- readRDS(paste0('./R_dataframes/work_states/',i,'_work.rds'))
  states[states['state_abbr']==i,'num_obs'] <- nrow(d_acs)
}

write.csv(states, file = "R_dataframes/State of work dataset obs counts.csv", row.names = FALSE)

