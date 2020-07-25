
# """
# 3_impute_functions
#
# These functions impute the FMLA data set into the ACS.
#
# 9 Sept 2018
# Luke
#
# """



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table of Contents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. impute_fmla_to_acs
# Modular imputation methods - can be swaped out for one another for FMLA to ACS imputation of:
# take_* vars, resp_len, prop_pay_employer variables
  # 1A. KNN1_scratch
  # 1B. logit_leave_method
      # 1Ba. runLogitImpute - used in hardcoded methods found elsewhere as well
      # 1Bb. runOrdinalImpute - used in hardcoded methods found elsewhere as well
      # 1Bc. runRandDraw - used in hardcoded methods found elsewhere as well
  # 1C. KNN_multi
  # 1D. Naive_Bayes
  # 1E. ridge_class
  # 1F. random_forest
  # 1G. svm_impute - Gets bad results right now
  # 1H. xG Boost

# ============================ #
# 1. impute_fmla_to_acs
# ============================ #
# master fmla to acs imputation function
# Based on user-specified method, impute leave taking behavior in fmla to ACS
# default is KNN1

impute_fmla_to_acs <- function(d_fmla, d_acs, impute_method,xvars,kval,xvar_wgts) {
  # d_fmla - modified fmla data set
  # d_acs - ACS data set
  # impute_method - method to use for imputation
  # xvars - dependent variables used by imputation method. Must be present and have same name in 
  #         both fmla and acs data sets.
  
  # dplyr::filter_() is softly depreciated: https://dplyr.tidyverse.org/reference/se-deprecated.html
  # The standard evalution of string filters is required throughout 3_impute_functions.R.
  # However, depsite the site's claims filter() now functions with both standard and non-standard evaluation,
  # filter() returns an error when fed a string condition. As a result, we need to use the filter_() function 
  # for filters to work properly. Despite their soft depreciation, We are going to suppress the filter_() depriciation warnings
  # to not confuse users. This is also noted in the model's accompanying Technical Documentation.
  suppressWarnings(dplyr::filter_)
  
    # ---------------------------------------------------------------------------------------------------------
  # A. Leave characteristics needed: leave taking behavior, proportion of income paid by employer,
  #     whether leave was affordable or not
  # ---------------------------------------------------------------------------------------------------------
  
  # -----------------Hard coded objects all methods must use-----------------------------------------------
  # yvars: the dependent vars that must be imputed by the selected method
  yvars <- c(own = "take_own", 
             illspouse = "take_illspouse",
             illchild = "take_illchild",
             illparent = "take_illparent",
             matdis = "take_matdis",
             bond = "take_bond",
             need_own = "need_own", 
             need_illspouse = "need_illspouse",
             need_illchild = "need_illchild",
             need_illparent = "need_illparent",
             need_matdis = "need_matdis",
             need_bond = "need_bond",
             anypay = "anypay",
             prop_pay_employer = "prop_pay_employer",
             resp_len= "resp_len")
  
  # filters: logical conditionals always applied to filter vraiable imputation 
  filts <- c(own = "TRUE",
                   illspouse = "nevermarried == 0 & divorced == 0",
                   illchild = "TRUE",
                   illparent = "TRUE",
                   matdis = "female == 1 & nochildren == 0 & age <= 50",
                   bond = "nochildren == 0 & age <= 50",
                   need_own = "TRUE",
                   need_illspouse = "nevermarried == 0 & divorced == 0",
                   need_illchild = "TRUE",
                   need_illparent = "TRUE",
                   need_matdis = "female == 1 & nochildren == 0 & age <= 50",
                   need_bond = "nochildren == 0 & age <= 50",
                   anypay = "TRUE",
                   prop_pay_employer="TRUE",
                   resp_len="TRUE")
  
  # weight: if method uses FMLA weights, the weight variable to use
  weights <- c(own = "~ weight",
              illspouse = "~ weight",
              illchild = "~ weight",
              illparent = "~ weight",
              matdis = "~ weight",
              bond = "~ weight",
              need_own = "~ weight",
              need_illspouse = "~ weight",
              need_illchild = "~ weight",
              need_illparent = "~ weight",
              need_matdis = "~ weight",
              need_bond = "~ weight",
              anypay = "~ weight",
              prop_pay_employer = '~ weight',
              resp_len = "~ weight")
  
  # Save ACS and FMLA Dataframes at this point to document format that 
  # alternative imputation methods will need to expect
  # saveRDS(d_fmla, file="./R_dataframes/d_fmla_impute_input.rds") # TODO: Remove from final version
  # saveRDS(d_acs, file="./R_dataframes/d_acs_impute_input.rds") # TODO: Remove from final version
  
  # KNN1 imputation method
  if (impute_method=="KNN1") {
    # separate KNN1 calls for each unique conditional doesn't work because of differing missing values
    # INPUTS: variable to be imputed, conditionals to filter training and test data on, FMLA data (training), and
    #         ACS data (test), id variable, and dependent variables to use in imputation
    impute <- mapply(KNN1_scratch, imp_var=yvars,train_filt=filts, test_filt=filts,
                        MoreArgs=list(d_train=d_fmla,d_test=d_acs,xvars=xvars, xvar_wgts = xvar_wgts), SIMPLIFY = FALSE)
    # OUTPUTS: list of data sets for each leave taking/other variables requiring imputation. 
   # merge imputed values with acs data
    for (i in impute) {
      # old merge code, caused memory issues. using match instead
      #d_test <- merge(d_filt, d_test, by='id', all.y=TRUE)
      for (j in names(i)) {
        if (j %in% names(d_acs)==FALSE){
          d_acs[j] <- i[match(d_acs$id, i$id), j]    
        }
      }
    }  
    
    # save output for reference when making other methods
    saveRDS(d_acs, file="./R_dataframes/d_acs_impute_output.rds") # TODO: Remove from final version
  }
  
  # Logit estimation of leave taking to compare with Chris' results in Python
  if (impute_method=="Logistic Regression GLM") {
    d_acs <- logit_leave_method(d_test=d_acs, d_train=d_fmla, xvars=xvars, 
                                yvars=yvars, test_filts=filts, train_filts=filts, 
                                weights=weights, create_dummies=TRUE)
  }
  
  if (impute_method=="K Nearest Neighbor") {
    # INPUTS: variable to be imputed, conditionals to filter training and test data on, FMLA data (training), and
    #         ACS data (test), id variable, and dependent variables to use in imputation, number of nbors
    impute <- mapply(KNN_multi, imp_var=yvars,train_filt=filts, test_filt=filts,
                     MoreArgs=list(d_train=d_fmla,d_test=d_acs,xvars=xvars, kval=kval), SIMPLIFY = FALSE)
    
    # OUTPUTS: list of data sets for each leave taking/other variables requiring imputation. 
    # merge imputed values with acs data
    for (i in impute) {
      # old merge code, caused memory issues. using match instead
      #d_acs <- merge(i, d_acs, by="id",all.y=TRUE)
      for (j in names(i)) {
        if (j %in% names(d_acs)==FALSE){
          d_acs[j] <- i[match(d_acs$id, i$id), j]    
        }
      }
    }
    
  }
  if (impute_method=="Naive Bayes") {
    # xvars must be all categorical vars for naive bayes
    xvars <-c("widowed", "divorced", "separated", "nevermarried", "female", 
              "ltHS", "someCol", "BA", "GradSch", "black", 
              "other", "asian",'native', "hisp","nochildren",'fmla_eligible',
              'union','hourly')
    
    options(warn=-1)
    d_acs <- Naive_Bayes(d_test=d_acs, d_train=d_fmla, xvars=xvars, 
                         yvars=yvars, test_filts=filts, train_filts=filts, 
                         weights=weights)
    options(warn=0)
  }
  
  if (impute_method=="Ridge Classifier") {
    d_acs <- ridge_class(d_test=d_acs, d_train=d_fmla, xvars=xvars, 
                         yvars=yvars, test_filts=filts, train_filts=filts, 
                         weights=weights)
  }
  
  if (impute_method=="Random Forest") {
    d_acs <- random_forest(d_test=d_acs, d_train=d_fmla, xvars=xvars, 
                         yvars=yvars, test_filts=filts, train_filts=filts, 
                         weights=weights)
  } 
  
  if (impute_method=="Support Vector Machine") {
    d_acs <- svm_impute(d_test=d_acs, d_train=d_fmla, xvars=xvars, 
                           yvars=yvars, test_filts=filts, train_filts=filts, 
                           weights=weights)
  }
  if (impute_method=="XGBoost") {
    d_acs <- xg_boost_impute(d_test=d_acs, d_train=d_fmla, xvars=xvars, 
                        yvars=yvars, test_filts=filts, train_filts=filts, 
                        weights=weights)
  }
  
  # This is a random leave assignment for comparison purposes.
  # This is assigns leaves randomly based on the population mean for each var
  # Done by running logit with no xvars
  if (impute_method=="random") {
    d_acs <- logit_leave_method(d_test=d_acs, d_train=d_fmla, xvars = "",
                                yvars=yvars, test_filts=filts, train_filts=filts, 
                                weights=weights, create_dummies=TRUE)
  }
  
  # finally, account for two-stage estimation of anypay and prop_pay_employer. prop_pay_employer has no 0 values, just .125-1.
  # Two-stage estimation is implemented by giving anypay prescendence - prop_pay_employer set to 0 if anypay==0. 
  # prop_pay_employer unchanged if anypay==1.
  d_acs <- d_acs %>% mutate(prop_pay_employer=ifelse(anypay==0,0,prop_pay_employer))
  
  return(d_acs)
}

# ============================ #
# 1A. KNN1_scratch
# ============================ #
# Define KNN1 matching method

KNN1_scratch <- function(d_train, d_test, imp_var, train_filt, test_filt, xvars, xvar_wgts) { 
  
  # This returns a dataframe of length equal to acs with the employee id and a column for each leave type
  # that indicates whether or not they took the leave.

  # create training data
  
  # filter dataset and keep just the variables of interest
  options(warn=-1)
  train <-  d_train %>% filter(complete.cases(dplyr::select(d_train, 'id', all_of(imp_var),all_of(xvars)))) %>% 
    filter_(train_filt) %>%
    dplyr::select(imp_var, all_of(xvars)) %>%
    mutate(id = NULL)
  options(warn=0)
  train ['nbor_id'] <- as.numeric(rownames(train))
  
  # create test data set 
  # This is a dataframe just with the variables in the acs that will be used to compute distance
  options(warn=-1)
  test <- d_test %>% filter_(test_filt) %>%
    dplyr::select(id, all_of(xvars)) %>%
    filter(complete.cases(.))
  options(warn=0)
  
  # Initial checks
  
  # check for data frames
  if ((!is.data.frame(train)) | (!is.data.frame(test))) {
    stop("train_set and test_set must be data frames")
  }  
  
  # check for missing data
  if (anyNA(train) | anyNA(test)) {
    stop("missing values not allowed in train_test or test_set")
  }
  
  
  # normalize training data to weight differences between variables
  names(xvar_wgts) <- xvars
  for (i in colnames(train)) {
    if (i != 'nbor_id' & i != imp_var & sum(train[i])!=0 ){
      train[i] <- scale(train[i],center=0,scale=max(train[,i])/xvar_wgts[[i]])
    }
  } 
  
  for (i in colnames(test)) {
    if (i != 'id' & sum(test[i])!=0 ){
      test[i] <- scale(test[i],center=0,scale=max(test[,i])/xvar_wgts[[i]])
    }
  } 
  
  # id var must be first variable of data
    # find distance
  
  m_test <- as.matrix(test)
  m_train <-as.matrix(train)
  
  nest_test <- list()
  nest_train <- list()
  # nested lists of vectors for apply functions
  
  nest_test <- lapply(seq(1,nrow(m_test)) , function(y){ 
    m_test[y,colnames(test)!='id']
  })
  nest_train <- lapply(seq(1,nrow(m_train)) , function(y){ 
    m_train[y,colnames(train)!='nbor_id' & colnames(train)!=imp_var]
  })
  
  # mark minimium distance
  min_start <- ncol(train)-2
  
  where_min <- function(j) {
    min_dist <- min_start
    d <- mapply(find_dist, x=nest_train, MoreArgs=list(y=j))
    return(which.min(d))
  }
  
  find_dist <- function(x,y) {
    return((sum((x - y) ^ 2))^(0.5))
  } 
  
  temp <- lapply(nest_test, where_min)
  temp <- unlist(temp)
  temp <- cbind(test["id"],as.data.frame(unlist(temp)))
  colnames(temp)[colnames(temp)=="unlist(temp)"] <- "nbor_id"
  temp <-plyr::join(temp[c("id","nbor_id")], train[c("nbor_id",imp_var)], by=c("nbor_id"), type="left")
  temp <- temp[c("id",imp_var)]
  return(temp)
}
# ============================ #
# 1B. logit_leave_method
# ============================ #
# logit imputation of leave characteristics

logit_leave_method <- function(d_test, d_train, xvars=NULL, yvars, test_filts, train_filts, 
                               weights, create_dummies) {
  
  # placeholder modification of xvars to follow Chris' specification in python
  # should be removed in final version
  # xvars <- c('age', 'agesq', 'male', 'wkhours', 'ltHS', 'BA', 'GradSch', 
  #            'empgov_fed', 'empgov_st', 'empgov_loc',
  #            'lnfaminc', 'black', 'asian', 'hisp', 'other',
  #            'ndep_kid', 'ndep_old', 'nevermarried', 'partner',
  #            'widowed', 'divorced', 'separated')
  
  # population mean imputation for missing xvars in logit regression
  d_test_no_imp <- d_test
  if (xvars[1]!="") {
    options(warn=-1)
    for (i in xvars) {
      # In test and training data if xvar is numeric, fill missing values with mean value
      if (is.numeric(d_test[,i]) & any(unique(d_test[!is.na(d_test[,i]), i])!=c(0,1))) {
        d_test[is.na(d_test[,i]), i] <- mean(d_test[,i], na.rm = TRUE)  
      }
      if (is.numeric(d_train[,i]) & any(unique(d_train[!is.na(d_train[,i]), i])!=c(0,1))) {
        d_train[is.na(d_train[,i]), i] <- mean(d_train[,i], na.rm = TRUE)  
      }
      # if it is a dummy var, then take a random draw with probability = to the non-missing mean
      if (is.numeric(d_test[,i]) & all(unique(d_test[!is.na(d_test[,i]), i])==c(0,1))) {
        if (any(is.na(d_test[,i]))){
          d_test$prob <- mean(d_test[,i], na.rm = TRUE)
          d_test['rand']=runif(nrow(d_test))
          d_test[is.na(d_test[,i]), i] <- with(d_test[is.na(d_test[,i]), c(i,'rand','prob')], ifelse(rand>prob,0,1))    
          d_test['rand'] <- NULL
          d_test$prob <- NULL
        }
      }
      if (is.numeric(d_train[,i]) & all(unique(d_train[!is.na(d_train[,i]), i])==c(0,1))) {
        if (any(is.na(d_train[,i]))){
          d_train$prob <- mean(d_train[,i], na.rm = TRUE)
          d_train['rand']=runif(nrow(d_train))
          d_train[is.na(d_train[,i]), i] <- with(d_train[is.na(d_train[,i]), c(i,'rand','prob')], ifelse(rand>prob,0,1))    
          d_train['rand'] <- NULL
          d_train$prob <- NULL
        }
      }
    }
    options(warn=0)
  }
 
  
  # remove prop_pay_employer from lists as we need to use ordinal regression for it
  train_filts <- list.remove(train_filts, 'prop_pay_employer')
  test_filts <- list.remove(test_filts, 'prop_pay_employer')
  yvars <- list.remove(yvars, 'prop_pay_employer')
  weights <- list.remove(weights, 'prop_pay_employer')
  
  # generate formulas for logistic regression
  # need formula strings to look something like "take_own ~ age + agesq + male + ..." 
  
  if (xvars[1]!="") {
    formulas=c()
    for (i in yvars) { 
      formulas= c(formulas, 
                  paste(i, "~",  paste(xvars[1],'+', paste(xvars[2:length(xvars)] , collapse=" + "))))
    }
  }
  else {
    formulas=c()
    for (i in yvars) { 
      formulas= c(formulas, paste(i, "~ 1"))
    }
  }
  
  # create columns based on logit estimates  
  sets <-  mapply(runLogitEstimate, formula = formulas, train_filt = train_filts,
                       test_filt=test_filts, weight = weights, varname=yvars,
                       MoreArgs=list(d_train=d_train, d_test=d_test, create_dummies=TRUE), 
                       SIMPLIFY = FALSE)

  # merge imputed values into single data set
  
  for (i in sets) {
    # old merge code, caused memory issues. using match instead
    # d_test <- merge(i, d_test, by="id",all.y=TRUE)
    for (j in names(i)) {
      if (j %in% names(d_test)==FALSE){
        d_test_no_imp[j] <- i[match(d_test_no_imp$id, i$id), j]    
      }
    }
    # set missing probability = 0
    d_test[is.na(d_test[colnames(i[2])]), colnames(i[2])] <- 0
  } 
  
  # set formula
  if (xvars[1]!="") {
    formula <- paste("factor(prop_pay_employer) ~", paste(xvars[1],'+', paste(xvars[2:length(xvars)], collapse=" + ")))
  }
  else {
    formula <- paste("factor(prop_pay_employer) ~ 1")
  }
    
  # Do an ordinal logit imputation for prop_pay_employer
  d_filt <- runOrdinalEstimate(d_train=d_train,d_test=d_test, formula=formula,
                               test_filt="TRUE", train_filt="TRUE", varname='prop_pay_employer')
  
  # old merge code caused memory issues. Using match instead.
  #d_test <- merge(d_filt, d_test, by='id', all.y=TRUE)
  for (i in names(d_filt)) {
    if ((i %in% names(d_test))==FALSE){
      d_test_no_imp[i] <- d_filt[match(d_test_no_imp$id, d_filt$id), i]    
    }
  }
  d_test <- d_test_no_imp
  # replace factor levels with prop_pay_employer proportions
  d_test <- d_test %>% mutate(prop_pay_employer = ifelse(prop_pay_employer == 1, .125, prop_pay_employer))
  d_test <- d_test %>% mutate(prop_pay_employer = ifelse(prop_pay_employer == 2, .375, prop_pay_employer))
  d_test <- d_test %>% mutate(prop_pay_employer = ifelse(prop_pay_employer == 3, .5, prop_pay_employer))
  d_test <- d_test %>% mutate(prop_pay_employer = ifelse(prop_pay_employer == 4, .625, prop_pay_employer))
  d_test <- d_test %>% mutate(prop_pay_employer = ifelse(prop_pay_employer == 5, .875, prop_pay_employer))
  d_test <- d_test %>% mutate(prop_pay_employer = ifelse(prop_pay_employer == 6, 1, prop_pay_employer))
  
  return(d_test)
}

# ============================ #
# 1Ba. runLogitEstimate
# ============================ #
# function to construct logit estimation model from training data set, 
# then create imputed columns for valid observations in test data set
# returns a separate copy of only those observations with valid imputed values

runLogitEstimate <- function(d_train,d_test, formula, test_filt,train_filt, weight, 
                             varname, create_dummies){
  options(warn=-1)
  des <- svydesign(id = ~1,  weights = as.formula(weight), data = d_train %>% filter_(train_filt))
  complete <- svyglm(as.formula(formula), data = d_train %>% filter_(train_filt), family = "quasibinomial",design = des)
  options(warn=0)
  estimate <- complete$coefficients 
  
  # if making a log, record sample size of filtered data set
  if (exists('makelog')) {
    if ( makelog == TRUE) {
      options(warn=-1)
      temp_filt = d_train %>% filter_(train_filt)
      options(warn=0)
      cat("", file = log_name, sep="\n", append = TRUE)
      cat("------------------------------", file = log_name, sep="\n", append = TRUE)
      cat(paste("Filtered FMLA Sample Size:", nrow(temp_filt)), file = log_name, sep="\n", append = TRUE)
      cat(paste("Formula:", formula), file = log_name, sep="\n", append = TRUE)
      cat(paste("Filter condition:", train_filt), file = log_name, sep="\n", append = TRUE)
      cat("------------------------------", file = log_name, sep="\n", append = TRUE)
      cat("", file = log_name, sep="\n", append = TRUE)
    }
  }
  var_prob= paste0(varname,"_prob")
  options(warn=-1)
  d_filt <- d_test %>% filter_(test_filt)
  options(warn=0)
  d_filt[var_prob]=estimate['(Intercept)']
  for (dem in names(estimate)) {
    if (dem !='(Intercept)' & !is.na(estimate[dem])) { 
      d_filt[is.na(d_filt[,dem]),dem]=0
      d_filt[var_prob]= d_filt[,var_prob] + d_filt[,dem]*estimate[dem]
    }
  }
  
  d_filt[var_prob] <- with(d_filt, exp(get(var_prob))/(1+exp(get(var_prob))))
  d_filt <- d_filt[,c(var_prob, 'id')]
  
  # option to create dummy variables in addition to probabilities
  if (create_dummies==TRUE) {
    d_filt [is.na(d_filt[var_prob]), var_prob] <- 0
    d_filt['rand']=runif(nrow(d_filt))
    d_filt[varname] <- with(d_filt, ifelse(rand>get(var_prob),0,1))    
    d_filt <- d_filt[,c(varname, 'id')]
  }
  
  return(d_filt)
}

# ============================ #
# 1Bb. runOrdinalEstimate
# ============================ #
# MASS implementation, polr function
# biggest problem with ordered logit currently is it is unweighted; can't use CPS weight without getting a non-convergence error
runOrdinalEstimate <- function(d_train,d_test, formula, test_filt,train_filt, varname){
  
  # 
  #   # OGLMX ordinal implementation - gives pretty non sensical results from my efforts
  #   runOrdinal <- function(x,y,z){
  #      results.ologit <- oglmx(as.formula(x), data = d_cps %>% filter_(y), weights=marsupwt)
  #      pause()
  #      return(estimate)
  #   }
  
  # get estimates from training data
  options(warn=-1)
  estimate <- polr(as.formula(formula), data = d_train %>% filter_(train_filt))
  
  #filter test data
  d_filt <- d_test %>% filter_(test_filt)
  options(warn=0)
  
  # ensure there is at least one row in test data set that needs imputing
  if (!is.null(rownames(d_filt))) {
    
    # calculate score from ordinal model
    model=estimate$coefficients
    d_filt['var_score']=0
    for (dem in names(model)) {
      if (dem !='(Intercept)') { 
        d_filt[is.na(d_filt[,dem]),dem]=0
        d_filt[,'var_score']= d_filt[,'var_score'] + d_filt[,dem]*model[dem]
      }
    }
    
    # assign categorical variable based on ordinal cuts
    cuts= estimate$zeta
    cat_num= length(cuts)+1
    d_filt[varname] <- 0
    d_filt['rand']=runif(nrow(d_filt))
    for (i in seq(cat_num)) {
      if (i!=cat_num) {
        d_filt <- d_filt %>% mutate(cumprob= var_score-cuts[i])
        d_filt <- d_filt %>% mutate(cumprob2= exp(cumprob)/(1+exp(cumprob)))
        d_filt[varname] <- with (d_filt, ifelse(get(varname)==0 & rand>=cumprob2,i,get(varname)))
      }
      else {
        d_filt[varname] <- with (d_filt, ifelse(get(varname)==0,i,get(varname)))
      }
    }
    d_filt <- d_filt[,c(varname, 'id')]
    return(d_filt)
  }
}

# ============================ #
# 1Bc. runRandDraw
# ============================ #
# run a random draw for leave length 
runRandDraw <- function(d, yvar, filt, leave_dist, ext_resp_len, rr_sensitive_leave_len,wage_rr,maxlen) {

  # filter test cases
  d_filt <- d %>% filter_(filt)
  
  # filter to distribution of lengths for the leave type
  leave_dist['Cumulative'] <- ave(leave_dist$Percent, leave_dist[,c('Leave.Type','State.Pay')], FUN=cumsum)
  filt_dist <- leave_dist %>% filter(Leave.Type==yvar & State.Pay==0)
  
  
  # function to draw a leave length from the distribution
  draw_length <- function(dist) {
    return(sample())
  }
   
  # if filter means no rows present, stop and return nothing  
  if (nrow(d_filt)==0) {
    return()
  }
  
  if (nrow(d_filt)!=0) {
    
    # random draw of leave length
    # first, set up objects needed to store results
    if (!yvar %in% colnames(d_filt)) {
      d_filt[yvar]=NA
    }
    squo_var = paste0('squo_',yvar)
    est_df <- data.frame(matrix(ncol = 3, nrow = 0))  
    colnames(est_df) <- c('id', squo_var, yvar)
    
    # status quo length
    d_filt[squo_var] <-sample(filt_dist$Leave.Length..Days, size=nrow(d_filt), replace=TRUE, prob = filt_dist$Percent)
    est_df <- rbind(est_df, d_filt[c('id', squo_var)])

    # changing counterfactual length option:
    # for constrained individuals, draw length from unconstrained draws
    if (ext_resp_len==TRUE) {
      temp_filt <- d_filt %>% filter(resp_len == 1)
      
      # make sure columns from est_df are in temp_filt
      # old merge code caused memory issues. using match instead
      #temp_test <- merge(temp_test, est_df, by='id', all.x=TRUE)
      for (i in names(est_df)) {
        if (i %in% names(temp_filt)==FALSE){
          temp_filt[i] <- est_df[match(temp_filt$id, est_df$id), i]    
        }
      }
  
      # if there are resp_len==1 individuals, we find the counterfactual lengths
      if (nrow(temp_filt)!= 0) {
        
        draw_len_cfact <- function(x,ml=maxlen) {
          # for some reason squo_var turns to string when this function is applied, making sure 
          # to revert that back to numeric before doing comparison operations.
          squo_len <- as.numeric(x[squo_var])
          
          # take random draw conditional on being greater than squo_len
          d_result <- filt_dist %>% filter(Leave.Length..Days > squo_len) 
          if (nrow(d_result) > 1) { 
            
            result <- sample(d_result$Leave.Length..Days, size=1, prob = d_result$Percent)
            
          } else if (nrow(d_result) == 1) { 
            
            result <- d_result[1, 'Leave.Length..Days']
            
          } else if (nrow(d_result) == 0) { 
            
            result <- squo_len
            
          }
          
          # top code result at 261 days - how many working days there are in a year, and at program max length, but no less than squo
          return(max(squo_len,min(result,261,ml)))
        }
        
        # adjust squo lengths by factor to get counterfact lengths for resp_len == 1z
        temp_filt[yvar] <- data.frame(unlist(apply(temp_filt, 1, draw_len_cfact)))

        
        # old merge code caused memory issues. using match instead
        #est_df <- merge(temp_test[c('id', yvar)], est_df, by='id', all.y=TRUE)  
        est_df[yvar] <- temp_filt[match(est_df$id,temp_filt$id), yvar]    
        
        # for the rest, resp_len = 0 and so leave length does not respond to presence or absence of program, 
        # so that variable remains the same
        if (is.factor(est_df[[yvar]])){
          est_df[yvar] <- unfactor(est_df[[yvar]])
        }
        est_df[is.na(est_df[yvar]),yvar] <- est_df[is.na(est_df[yvar]),squo_var]
        
        # if leave extension response ratio sensitivity is enabled, interpolate leave length to be somewhere 
        # between the max needed and status quo leave length
        # prop_pay_employer -> status quo receipt
        # first, create maximum need length var or "mnl_" var - will be same as yvar if sensitivity not enabled.
        mnl_var <- sub('length','mnl',yvar)
        est_df[mnl_var] <- est_df[yvar]
        est_df[is.na(est_df[mnl_var]),mnl_var] <-0
        
        if (rr_sensitive_leave_len==TRUE) {
          merge_ids <-match(est_df$id,d_filt$id)
          # load variables from test data needed for interpolation
          est_df['prop_pay_employer'] <- d_filt[merge_ids, 'prop_pay_employer'] 
          est_df['resp_len'] <- d_filt[merge_ids, 'resp_len'] 
          est_df['dual_receiver'] <- d_filt[merge_ids, 'dual_receiver'] 
          # set couterfactual leave taking var equal to Z+ (X-Z)*(rrp-rre)/(1-rre), where:
          # Z is status quo leave length
          # X is maximum length needed
          # rre is the status quo replacement rate (i.e. proportion of pay received)
          # wage_rr <- wage of program
          est_df['wage_rr'] <- wage_rr
          # rrp is max(wage_rr, rre)
          est_df['rrp'] <- apply(est_df[c('wage_rr','prop_pay_employer')], 1, max)
          # apply formula to estimate couterfactual leave for single receivers
          est_df[yvar] <- with(est_df, ifelse(dual_receiver==0,
              get(squo_var)+ (get(mnl_var)-get(squo_var))*(rrp-prop_pay_employer)/(1-prop_pay_employer),get(yvar)))
          # formula for dual receivers is the same, except rrp = min(prop_pay_employer+wage_rr, 1)  
          est_df['rrp_dual'] <- est_df['prop_pay_employer']+est_df['wage_rr']
          est_df <- est_df %>% mutate(rrp_dual=ifelse(rrp_dual>1,1,rrp_dual))
          est_df[yvar] <- with(est_df, ifelse(dual_receiver==1,
                                              get(squo_var)+ (get(mnl_var)-get(squo_var))*(rrp_dual-prop_pay_employer)/(1-prop_pay_employer),get(yvar)))

          # round to nearest whole day
          est_df[yvar] <- round(est_df[yvar])
          
          # get nan/infinity for those with prop_pay_employer=1 b/c of dividing by 0; these are already at maximum needed length,
          # so we just keep the value the same
          est_df[is.na(est_df[yvar]),yvar] <- est_df[is.na(est_df[yvar]),mnl_var] 
          est_df[!is.finite(est_df[[yvar]]),yvar] <- est_df[!is.finite(est_df[[yvar]]),mnl_var] 
          est_df <- est_df[, !(names(est_df) %in% c('prop_pay_employer','resp_len','wage_rr','rrp','rrp_dual','dual_receiver'))]
        }
      }
      # if these dataframes are empty, then we only have resp_len = 0 in test and we make the cfact var the same
      # as status quo: their leave length does not respond to presence or absence of program, 
      
      else {
        est_df[yvar] <- est_df[squo_var]
        mnl_var <- sub('length','mnl',yvar)
        est_df[mnl_var] <- est_df[yvar]
      }
    }
    
    # if option not used, counterfactual will start out the same as status quo
    if (ext_resp_len==FALSE) {
      est_df[yvar] <- est_df[squo_var]
      mnl_var <- sub('length','mnl',yvar)
      est_df[mnl_var] <- est_df[yvar]
    }
     return(est_df) 
  }
}

# ============================ #
# 1C. KNN_multi
# ============================ #
# Define KNN matching method, but allowing multiple (k) neighbors 
KNN_multi <- function(d_train, d_test, imp_var, train_filt, test_filt, xvars, kval=5) { 
  
  # starts the same way as KNN1_scratch
  
  # This returns a dataframe of length equal to acs with the employee id and a column for each leave type
  # that indicates whether or not they took the leave.
  
  # create training data
  
  # filter dataset and keep just the variables of interest
  options(warn=-1)
  train <-  d_train %>% filter(complete.cases(dplyr::select(d_train, 'id', all_of(imp_var),all_of(xvars)))) %>% 
    filter_(train_filt) %>%
    dplyr::select(imp_var, all_of(xvars)) %>%
    mutate(id = NULL)
  options(warn=0)
  train ['nbor_id'] <- as.numeric(rownames(train))
  
  # create test data set 
  # This is a dataframe just with the variables in the acs that will be used to compute distance
  options(warn=-1)
  test <- d_test %>% filter_(test_filt) %>%
    dplyr::select(id, all_of(xvars)) %>%
    filter(complete.cases(.))
  options(warn=0)
  # Initial checks
  
  # check for data frames
  if ((!is.data.frame(train)) | (!is.data.frame(test))) {
    stop("train_set and test_set must be data frames")
  }  
  
  # check for missing data
  if (anyNA(train) | anyNA(test)) {
    stop("missing values not allowed in train_test or test_set")
  }
  
  
  # normalize training data to equally weight differences between variables
  for (i in colnames(train)) {
    if (i != 'nbor_id' & i != imp_var & sum(train[i])!=0 ){
      train[i] <- scale(train[i],center=0,scale=max(train[,i]))
    }
  } 
  
  for (i in colnames(test)) {
    if (i != 'id' & sum(test[i])!=0 ){
      test[i] <- scale(test[i],center=0,scale=max(test[,i]))
    }
  } 
  
  # id var must be first variable of data
  
  # find distance
  
  m_test <- as.matrix(test)
  m_train <-as.matrix(train)
  
  nest_test <- list()
  nest_train <- list()
  
  # nested lists of vectors for apply functions
  nest_test <- lapply(seq(1,nrow(m_test)) , function(y){ 
    m_test[y,colnames(test)!='id']
  })
  nest_train <- lapply(seq(1,nrow(m_train)) , function(y){ 
    m_train[y,colnames(train)!='nbor_id' & colnames(train)!=imp_var]
  })
  
  # mark minimium distance
  min_start <- ncol(train)-2
  
  find_dist <- function(x,y) {
    return((sum((x - y) ^ 2))^(0.5))
  } 
  
  # same as KNN1 up until here
  where_min <- function(j) {
    min_dist <- min_start
    d <- mapply(find_dist, x=nest_train, MoreArgs=list(y=j))
    nbors <- order(d)[1:kval]
    return(nbors)
  }
  
  # create nbors: data set for each test obs' nearest neighbors in train dataset with their index
  nbors <- lapply(nest_test, where_min)
  nbors <- as.data.frame(t(as.data.frame(nbors)))
  # store test id in nbors set before continuing
  rownames(nbors) <- NULL
  colnames(nbors) <- paste0(rep('nbor_id',kval), seq(1,kval))
  nbors['id'] <- test$id
  # add impute variable's values for each index from training data set
  for (i in seq(kval)) {
    nbors <- merge(nbors, train[c('nbor_id',imp_var)], by.x=paste0('nbor_id',i), by.y='nbor_id', how='left')
    colnames(nbors)[kval+i+1] <- c(paste0('nbor_val', i))
  }

  # To decide on a single value to impute:
  # we will pick the mode value among nbors, weighted by distance
  # There are other ways of picking a single value, I'm sure
  
  # throwaway function to get mode of rows
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  nbors[imp_var] <- apply(nbors[(kval+2):(kval*2+1)], 1, Mode)
  temp <- merge(nbors[c('id',imp_var)], test['id'], by='id')
  return(temp)
}

# ============================ #
# 1D. Naive_Bayes
# ============================ #
# Naive Bayes imputation function

Naive_Bayes <- function(d_train, d_test, yvars, train_filts, test_filts, weights, xvars) {
  
  # generate formulas for Naive Bayes model
  # need formula strings to look something like "take_own ~ age + agesq + male + ..." 
  formulas <- c()
  for (i in yvars) { 
    formulas <- c(formulas, 
                paste(i, "~",  paste(xvars[1],'+', paste(xvars[2:length(xvars)] , collapse=" + "))))
  }
  names(formulas) <- names(yvars)
  
  # ---- Using WANBIA weights
  # from -> https://rdrr.io/cran/bnclassify/src/R/learn-params-wanbia.R 
  
  # predict each yvar 
  for (i in names(yvars)) {
    # # generate NB model from training data
    w_train <- as.data.frame(sapply(d_train %>% filter(complete.cases(dplyr::select(d_train, yvars[[i]], all_of(xvars)))) 
                                    %>% filter_(train_filts[i]) %>% dplyr::select(all_of(xvars), yvars[[i]]), as.factor))
    w_test <- as.data.frame(sapply(d_test %>% filter(complete.cases(dplyr::select(d_test, all_of(xvars)))) %>% filter_(test_filts[i])%>% dplyr::select(all_of(xvars)),as.factor))
    
    # make sure that testing and training sets have same factor levels and are factors
    for (j in xvars) {
      # make sure categorical vars are factors with equivalent levels
      d_test[,c(j)] <- factor(d_test[,c(j)])
      w_test[,c(j)] <- factor(w_test[,c(j)])
      d_train[,c(j)] <- factor(d_train[,c(j)], levels=levels(d_test[,c(j)]))
      w_train[,c(j)] <- factor(w_train[,c(j)], levels=levels(w_test[,c(j)]))
    }
    d_train[,yvars[[i]]] <- factor(d_train[,yvars[[i]]])
    w_train[,yvars[[i]]] <- factor(w_train[,yvars[[i]]])

    #wanbia <- compute_wanbia_weights('prop_pay_employer', as.data.frame(sapply(w_train, as.factor))) 
    wanbia <- bnc(dag_learner = 'nb',class=yvars[[i]], dataset=w_train,smooth=0,wanbia=TRUE) 
    
    
    # apply model to test data 
    w_test_ids <- d_test %>% filter(complete.cases(dplyr::select(d_test, all_of(xvars)))) %>% filter_(test_filts[i])%>% dplyr::select(id)
    wanbia_imp <- as.data.frame(predict(object=wanbia, newdata = w_test, prob=TRUE)) 
    
    # Play wheel of fortune with which prop_val to assign to each test obs
    wanbia_imp['rand'] <- runif(nrow(wanbia_imp))
    if (i == 'prop_pay_employer') {
      wanbia_imp['prop_pay_employer'] <- NA
      wanbia_imp['cum']=0
      var_vals <- sapply(unname(sort(unlist(c(unique(w_train['prop_pay_employer']))))),toString)
      for (j in var_vals) {
        wanbia_imp['prop_pay_employer'] <- with(wanbia_imp, ifelse(rand > cum & rand<(cum + get(j)), j, prop_pay_employer))
        wanbia_imp['cum']= wanbia_imp[,'cum'] + wanbia_imp[,j]
      }
      wanbia_imp['prop_pay_employer'] <- as.numeric(wanbia_imp[,'prop_pay_employer'])
    }
    else {
      wanbia_imp[yvars[[i]]] <- as.data.frame(ifelse(wanbia_imp[2]>wanbia_imp['rand'],1,0)) 

    }
    
    # add imputed value to test data set
    wanbia_imp <- cbind(w_test_ids['id'], wanbia_imp)
    # old merge code, was causing issues. Using match instead.
    #d_test <- merge(d_test, wanbia_imp[c('id', yvars[i])], by='id', all.x = TRUE)
    d_test[yvars[[i]]] <- wanbia_imp[match(d_test$id, wanbia_imp$id), yvars[[i]]]
    
  }
  return(d_test)
}

# ============================ #
# 1E. ridge_class
# ============================ #
# Ridge Classifier imputation function
ridge_class <- function(d_train, d_test, yvars, train_filts, test_filts, weights, xvars) {
  # generate formulas for Ridge Regression model
  # need formula strings to look something like "take_own ~ age + agesq + male + ..." 
  formulas <- c()
  for (i in yvars) { 
    formulas <- c(formulas, 
                  paste(i, "~",  paste(xvars[1],'+', paste(xvars[2:length(xvars)] , collapse=" + "))))
  }
  names(formulas) <- names(yvars)
  
  # predict each yvar
  
  for (i in names(yvars)) {
    # generate Ridge Regression model from training data
    options(warn=-1)
    ftrain <- d_train %>% filter(complete.cases(dplyr::select(d_train, yvars[i], all_of(xvars)))) %>% filter_(train_filts[i])  
    options(warn=0)
    # normalize training data to equally weight differences between variables
    for (j in xvars) {
      if (sum(ftrain[j])!=0 ){
        ftrain[j] <- scale(ftrain[j],center=0,scale=max(ftrain[,j]))
      }
    } 
    
    # check for xvars with all identical values. need to not use xvar if all values are the same in training 
    # will return a column of NaN for svd() in the lm.ridge function
    temp_xvars <- xvars
    for (j in xvars) {
      
      if (dim(table(ftrain[,j])) == 1) {
        temp_xvars <- temp_xvars[!temp_xvars %in% j]
        formulas[i] <- paste(yvars[i], "~",  paste(temp_xvars[1],'+', paste(temp_xvars[2:length(temp_xvars)] 
                                                                            , collapse=" + ")))
      }
    }
    
    # as prop_pay_employer is categorical, we need to do ordinal version of ridge regression
    # rest of the vars are handled in this loop
    if (i!= 'prop_pay_employer') {
      
      model <- lm.ridge(formula = formulas[i], data = ftrain[c(yvars[i], temp_xvars)])
      
      # TODO: getting a weighted version of ridge regression to work
      
      # apply model to test data
      options(warn=-1)
      ftest <- d_test %>% filter(complete.cases(dplyr::select(d_test, all_of(temp_xvars)))) %>% filter_(test_filts[i]) 
      options(warn=0)
      
      # normalize test data to equally weight differences between variables
      for (j in temp_xvars) {
        if (sum(ftest[j])!=0 ){
          ftest[j] <- scale(ftest[j],center=0,scale=max(ftest[,j]))
        }
      }
      # calculate probabilities of test data set
      impute <- as.data.frame(as.matrix(cbind(constant=1,ftest[temp_xvars])) %*% coef(model))
      colnames(impute) <- 'prob'
      impute['rand'] <- runif(nrow(impute))
      impute[yvars[i]] <- with(impute, ifelse(prob >= rand, 1, 0))
      impute <- cbind(ftest['id'], impute)
      # old merge code, was causing issues. Using match instead.
      #d_test <- merge(d_test, impute[c('id', yvars[i])], by='id', all.x = TRUE)
      d_test[yvars[[i]]] <- impute[match(d_test$id, impute$id), yvars[[i]]]
    }
    # ordinal ridge regression for prop_pay_employer
    # can't find a package that does an ordinal implementation, so writing one from scratch here
    # TODO: Not exactly ordinal in implementation right now, if there's time to revisit this could be done 
    else {
      # create dummies for each value of prop_pay_employer
      dums <- dummy(ftrain$prop_pay_employer)
      var_vals <- paste0('prop_pay_employer_',sort(unlist(c(unique(ftrain['prop_pay_employer'])))))
      colnames(dums) <- var_vals
      ftrain <- cbind(ftrain, dums)
      
      # prep test data
      options(warn=-1)
      ftest <- d_test %>% filter(complete.cases(dplyr::select(d_test, all_of(temp_xvars)))) %>% filter_(test_filts[i]) 
      options(warn=0)
      # normalize test data to equally weight differences between variables
      for (j in temp_xvars) {
        if (sum(ftest[j])!=0 ){
          ftest[j] <- scale(ftest[j],center=0,scale=max(ftest[,j]))
        }
      }
      
      for (j in var_vals) {
        # run ridge regression on each dummy
        formula <- paste(j, "~",  paste(temp_xvars[1],'+', paste(temp_xvars[2:length(temp_xvars)] 
                                                                        , collapse=" + ")))
        model <- lm.ridge(formula = formula, data = ftrain[c(j, temp_xvars)])  
        
        # apply model to test data to get probabilities of each level
        impute <- as.data.frame(as.matrix(cbind(constant=1,ftest[temp_xvars])) %*% coef(model))
        colnames(impute) <- j
        impute <- cbind(ftest['id'], impute)
        
        # old merge code, was causing issues. Using match instead.
        #d_test <- merge(d_test, impute[c('id', j)], by='id', all.x = TRUE)
        d_test[j] <- impute[match(d_test$id, impute$id), j]
      }
      # normalize val probabilities to sum to 1
      d_test['total'] = rowSums(d_test[var_vals])
      for (j in var_vals) {
        d_test[j] =  d_test[j]/d_test['total']
      }
      
      # Play wheel of fortune with which prop_val to assign to each test obs
      d_test['rand'] <- runif(nrow(d_test))
      d_test['prop_pay_employer'] <- NA
      d_test['cum']=0
      for (j in var_vals) {
        pay_val <- as.numeric(gsub("prop_pay_employer_", "", j))
        d_test['prop_pay_employer'] <- with(d_test, ifelse(rand > cum & rand<(cum + get(j)), pay_val, prop_pay_employer))
        d_test['cum']= d_test[,'cum'] + d_test[,j]
      }
      
      # clean up columns 
      d_test <- d_test[!colnames(d_test) %in% c(var_vals, 'rand', 'cum')]
    }
  }
  return(d_test)
}

# ============================ #
# 1F. random_forest
# ============================ #


# Random Forest imputation function
random_forest <- function(d_train, d_test, yvars, train_filts, test_filts, weights, xvars) {
  # generate formulas for Random Forest model
  # need formula strings to look something like "take_own ~ age + agesq + male + ..." 
  formulas <- c()
  for (i in yvars) { 
    formulas <- c(formulas, 
                  paste(i, "~",  paste(xvars[1],'+', paste(xvars[2:length(xvars)] , collapse=" + "))))
  }
  names(formulas) <- names(yvars)
  
  # predict each yvar 
  for (i in names(yvars)) {
    # generate model from training data 
    options(warn=-1)
    ftrain <- d_train %>% filter(complete.cases(dplyr::select(d_train, yvars[i], all_of(xvars)))) %>% filter_(train_filts[i])  
    options(warn=0)
    model <- randomForest(x = ftrain[xvars], y = factor(ftrain[,yvars[i]]))
    # apply model to test data 
    options(warn=-1)
    ftest <- d_test %>% filter(complete.cases(dplyr::select(d_test, all_of(xvars)))) %>% filter_(test_filts[i]) 
    options(warn=0)
    impute <- as.data.frame(predict(object=model, newdata = ftest[xvars], type='prob'))
  
    # play wheel of fortune with predicted probabilities to get imputed value
    impute['rand'] <- runif(nrow(impute))
    # do this differently for prop_pay_employer as a non-binary categorical var
    if (i=='prop_pay_employer'){
      impute['prop_pay_employer'] <- NA
      impute['cum']=0
      var_vals <- sapply(unname(sort(unlist(c(unique(ftrain['prop_pay_employer']))))),toString)
      for (j in var_vals) {
        impute['prop_pay_employer'] <- with(impute, ifelse(rand > cum & rand<(cum + get(j)), j, prop_pay_employer))
        impute['cum']= impute[,'cum'] + impute[,j]
      }
      impute['prop_pay_employer'] <- as.numeric(impute[,'prop_pay_employer'])
    }
    # rest of vars are binary
    else {
      impute[yvars[i]] <- as.data.frame(ifelse(impute[2]>impute['rand'],1,0))  
    }
    
    # add imputed value to test data set
    impute <- cbind(ftest['id'], impute)
    # old merge code, was causing issues. Using match instead.
    #d_test <- merge(d_test, impute[c('id', yvars[i])], by='id', all.x = TRUE)
    d_test[yvars[[i]]] <- impute[match(d_test$id, impute$id), yvars[[i]]]
    
  }
  return(d_test) 
}


# ================================ #
# 1G. Support Vector Machine
# ================================ #
# SVM Imputation Function
# Functional, but produces pretty bizarre results. Never predicts leave 
svm_impute <- function(d_train, d_test, yvars, train_filts, test_filts, weights, xvars) {
  # generate formulas for model
  # need formula strings to look something like "take_own ~ age + agesq + male + ..." 
  formulas <- c()
  for (i in yvars) { 
    formulas <- c(formulas, 
                  paste(i, "~",  paste(xvars[1],'+', paste(xvars[2:length(xvars)] , collapse=" + "))))
  }
  names(formulas) <- names(yvars)
  
  # predict each yvar 
  for (i in names(yvars)) {
    
    # generate model from training data 
    options(warn=-1)
    ftrain <- d_train %>% filter(complete.cases(dplyr::select(d_train, yvars[i], all_of(xvars)))) %>% filter_(train_filts[i])  
    options(warn=0)
    # normalize training data to equally weight differences between variables
    for (j in xvars) {
      if (sum(ftrain[j])!=0 ){
        ftrain[j] <- scale(ftrain[j],center=0,scale=max(ftrain[,j]))
      }
    } 
    # check for xvars with all identical values. need to not use xvar if all values are the same in training 
    # will return a column of NaN for svd() in the lm.ridge function
    temp_xvars <- xvars
    for (j in xvars) {
      if (dim(table(ftrain[,j])) == 1) {
        temp_xvars <- temp_xvars[!temp_xvars %in% j]
        formulas[i] <- paste(yvars[i], "~",  paste(temp_xvars[1],'+', paste(temp_xvars[2:length(temp_xvars)] 
                                                                            , collapse=" + ")))
      }
    }  
    model <- svm(x = ftrain[temp_xvars], y = factor(ftrain[,yvars[i]]), scale = FALSE , type='one-classification')
    # apply model to test data 
    options(warn=-1)
    ftest <- d_test %>% filter(complete.cases(dplyr::select(d_test, all_of(temp_xvars)))) %>% filter_(test_filts[i]) 
    options(warn=0)
    # normalize test data to equally weight differences between variables
    for (j in temp_xvars) {
      if (sum(ftest[j])!=0 ){
        ftest[j] <- scale(ftest[j],center=0,scale=max(ftest[,j]))
      }
    }
    impute <- as.data.frame(as.integer(predict(object=model, newdata = ftest[temp_xvars])))
    colnames(impute)[1] <- yvars[i]
    
    # add imputed value to test data set
    impute <- cbind(ftest['id'], impute)
    # old merge code, caused memory issues. using match instead
    #d_test <- merge(d_test, impute[c('id', yvars[i])], by='id', all.x = TRUE)
    d_test[yvars[[i]]] <- impute[match(d_test$id, impute$id), yvars[[i]]]
  }
  return(d_test) 
}

# ================================ #
# 1H. xG Boost
# ================================ #
# xG boost imputation function
xg_boost_impute <- function(d_train, d_test, yvars, train_filts, test_filts, weights, xvars) {
  # generate formulas for model
  # need formula strings to look something like "take_own ~ age + agesq + male + ..." 
  formulas <- c()
  for (i in yvars) { 
    formulas <- c(formulas, 
                  paste(i, "~",  paste(xvars[1],'+', paste(xvars[2:length(xvars)] , collapse=" + "))))
  }
  names(formulas) <- names(yvars)
  
  # predict each yvar 
  for (i in names(yvars)) {
    
    # generate model from training data 
    options(warn=-1)
    ftrain <- d_train %>% filter(complete.cases(dplyr::select(d_train, yvars[i], all_of(xvars)))) %>% filter_(train_filts[i])  
    options(warn=0)
    # normalize training data to equally weight differences between variables
    for (j in xvars) {
      if (sum(ftrain[j])!=0 ){
        ftrain[j] <- scale(ftrain[j],center=0,scale=max(ftrain[,j]))
      }
    } 
    
    # check for xvars with all identical values. need to not use xvar if all values are the same in training 
    temp_xvars <- xvars
    for (j in xvars) {
      if (dim(table(ftrain[,j])) == 1) {
        temp_xvars <- temp_xvars[!temp_xvars %in% j]
        formulas[i] <- paste(yvars[i], "~",  paste(temp_xvars[1],'+', paste(temp_xvars[2:length(temp_xvars)] 
                                                                            , collapse=" + ")))
      }
    } 
    # normalize for test xvars
    options(warn=-1)
    ftest <- d_test %>% filter(complete.cases(dplyr::select(d_test, all_of(temp_xvars)))) %>% filter_(test_filts[i]) 
    options(warn=0)
    for (j in temp_xvars) {
      if (sum(ftest[j])!=0 ){
        ftest[j] <- scale(ftest[j],center=0,scale=max(ftest[,j]))
      }
    }
    
    # set up training and testing data as xgb objects
    mtrain <-as.matrix(ftrain[,c(temp_xvars,yvars[[i]])])
    xgb_train <- xgb.DMatrix(data=mtrain[,temp_xvars],label=mtrain[,yvars[i]])
    mtest <-as.matrix(ftest[,c(temp_xvars)])
    xgb_test <- xgb.DMatrix(data=mtest[,temp_xvars])
    
    # create training model
    model <- xgb.train(data = xgb_train,nrounds=2)

    # apply model to test data 
    impute <- as.data.frame(predict(object=model, newdata = xgb_test))
    colnames(impute)[1] <- yvars[i]
    
    # if a binary variable, play wheel of fortune to assign value based on estimated probability
    if (i != 'prop_pay_employer') {
      impute['rand'] <- runif(nrow(impute))
      impute[yvars[[i]]] <- as.data.frame(ifelse(impute[1]>impute['rand'],1,0))
    }
    
    # add imputed value to test data set
    impute <- cbind(ftest['id'], impute)
    
    # old merge code, caused memory issues. using match instead
    #d_test <- merge(d_test, impute[c('id', yvars[i])], by='id', all.x = TRUE)
    d_test[yvars[[i]]] <- impute[match(d_test$id, impute$id), yvars[[i]]]
  }
  return(d_test) 
}