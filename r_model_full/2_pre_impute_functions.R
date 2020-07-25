
# """
# 2_pre_impute_functions
#
# These functions prepare the FMLA data set for ACS to impute leave taking behavior from it.
# and then execute the imputation from CPS, then FMLA into ACS.
#
# 9 Sept 2018
# Luke
# 
# TESTING TODO: what happens when filtered test data sets of 0 obs are fed into imputation functions
#               currently is handled properly by runOrdinalImpute and runRandDraw. 
#                KNN1_scratch and Logitestimate do not currently, should consider including handling methods.
#
# """



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table of Contents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. acs_filtering


# ============================ #
# 1. ACS Filtering
# ============================ #
# Apply filters and modifications to ACS data based on user inputs
acs_filtering <- function(d, weightfactor, place_of_work, state) {
  
  if (weightfactor!=1) {
    d$PWGTP=d$PWGTP*weightfactor
  }  
  
  # apply state filters
  if (state!='') {
    if (place_of_work==FALSE) {
      # merge in state abbreviations
      state_codes <- read.csv(paste0("../data/ACS_state_codes.csv"))
      d <- merge(d,state_codes, by="ST",all.x=TRUE)  
      d <- d %>% filter(state_abbr==state)
    }
    if (place_of_work==TRUE) {
      # merge in state abbreviations
      state_codes <- read.csv(paste0("../data/ACS_state_codes.csv"))
      state_codes["POWSP"] <- state_codes["ST"]  
      d <- merge(d,state_codes, by="POWSP",all.x=TRUE)
      d <- d %>% filter(state_abbr==state)
    }
  }
  
  # make sure there's not zero observations
  if (nrow(d)==0) {
    stop('Error: no rows in ACS dataframe')
  }
  
  return(d)
}
