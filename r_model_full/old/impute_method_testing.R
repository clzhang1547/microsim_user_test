
# """
# impute_method_testing
#
# This file sets up the framework for testing the performance of various imputation methods 
# This runs independently of the simulation model
# Luke
# 
#
# """
rm(list=ls())
cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table of Contents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0. Data Preparation
# 1. FMLA train/test split performance testing


# ============================ #
# 0. Data Preparation
# ============================ #
# check for required libraries
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
  return("OK")
}

global.libraries <- c('randomForest','magick','stats', 'rlist', 'MASS', 'plyr', 'dplyr', 
                      'survey', 'class', 'dummies', 'varhandle', 'oglmx', 
                      'foreign', 'ggplot2', 'reshape2','e1071','pander','ridge')
results <- sapply(as.list(global.libraries), pkgTest)

# load imputation method functions
source("3_impute_functions.R")

# load fmla data
d_fmla <- readRDS(paste0("./R_dataframes/","d_fmla.rds"))

# remove non-takers/needers
d_fmla <- d_fmla %>% filter(LEAVE_CAT!=3)

# replace prop_pay with proper values

d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 1, 0, prop_pay))
d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 2, .125, prop_pay))
d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 3, .375, prop_pay))
d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 4, .5, prop_pay))
d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 5, .625, prop_pay))
d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 6, .875, prop_pay))
d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 7, 1, prop_pay))

# ============================ #
# 1. FMLA train/test split performance testing
# ============================ #


# function for predicting variables from test sample
# impute_method - method of imputation
# prop - proportion of data set to use as test data
# kval - if method is "KNN_multi", number of  neighbors to use
# n - number of iterations to run
# seeded - whether to set random see or not

# dependent Variables to be used as predictors
xvars=c("widowed", "divorced", "separated", "nevermarried", "female", 
        "agesq", "ltHS", "someCol", "BA", "GradSch", "black", 
        "white", "asian", "hisp","nochildren",'faminc')

# independent variables  to be predicted
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
           prop_pay = "prop_pay",
           resp_len= "resp_len")


predict_test <- function(impute_method, prop=.25, kval = 5,  n=3, seeded = TRUE, weighted = FALSE) {

  seed = 123
  count = 1
  
  # number of columns needed for output dataframe
  ncols = 7
  
  for (h in seq(n)) {
    if (seeded == TRUE) {
      set.seed(seed)  
    }
    all <- 1:nrow(d_fmla)
    
    # partition FMLA into test and training data sets
    test_i <- sample(all, round(nrow(d_fmla)*prop,digits = 0))
    train_i <- all[-test_i]
    fmla_train <- d_fmla[train_i,]
    fmla_test <- d_fmla[test_i,]
    #rename yvars in test to designate them as actual values
    for (i in yvars) {
      new_name <- paste0(i, '_act')
      fmla_test[new_name] <- fmla_test[i]
      fmla_test[i] <- NULL
    }
    
    # use master imputation function to return predicted values for all variables
    df <- impute_fmla_to_acs(d_fmla=fmla_train, d_acs = fmla_test, 
                       impute_method=impute_method, xvars=xvars,kval=kval)
  
    # now for each yvar, assess accuracy of predictions
    # start a matrix to store results
    d_perf <- matrix(0, ncol = ncols, nrow = 0)
    
    for (i in yvars) {
      act_var <- paste0(i, '_act')
      
      # mark results of each test row
      df['result'] <- NA
      df <- df %>% mutate(result = ifelse(get(act_var)==1 & get(i)==1, 'TP', result))
      df <- df %>% mutate(result = ifelse(get(act_var)==0 & get(i)==0, 'TN', result))
      df <- df %>% mutate(result = ifelse(get(act_var)==0 & get(i)==1, 'FP', result))
      df <- df %>% mutate(result = ifelse(get(act_var)==1 & get(i)==0, 'FN', result))
      
      # total predictions made
      tot_pred <- nrow(df %>% filter(!is.na(result)))

      # individual confusion cells (TP, TN, FP, FN)
      TP <- nrow(df %>% filter(result == 'TP'))
      TN <- nrow(df %>% filter(result == 'TN'))
      FP <- nrow(df %>% filter(result == 'FP'))
      FN <- nrow(df %>% filter(result == 'FN'))
      
      # option to weight results 
      if (weighted == TRUE) {
        tot_pred <- sum(df %>% filter(!is.na(result)) %>% select(weight))
        TP <- ifelse(nrow(df %>% filter(result == 'TP')) > 0 ,
                     sum(df %>% filter(result == 'TP') %>% select(weight)), 0)
        TN <- ifelse(nrow(df %>% filter(result == 'TN')) > 0 ,
                         sum(df %>% filter(result == 'TN') %>% select(weight)), 0)
        FP <- ifelse(nrow(df %>% filter(result == 'FP')) > 0 ,
                         sum(df %>% filter(result == 'FP') %>% select(weight)), 0)
        FN <- ifelse(nrow(df %>% filter(result == 'FN')) > 0 ,
                     sum(df %>% filter(result == 'FN') %>% select(weight)), 0)
      }
  
      
      TP_prop <- nrow(df %>% filter(result == 'TP'))/tot_pred
      TN_prop <- nrow(df %>% filter(result == 'TN'))/tot_pred
      FP_prop <- nrow(df %>% filter(result == 'FP'))/tot_pred
      FN_prop <- nrow(df %>% filter(result == 'FN'))/tot_pred
      
      # put all values into a row to append
      row <- matrix(c(impute_method, i, tot_pred, TP, TN, FP, FN), ncol = ncols, nrow = 1)
  
      # add performance measures as row to results matrix
      # prop_pay as a non-binary needs different measurements
      if (i != 'prop_pay') {
        d_perf <- rbind(d_perf, row)  
      }
    }
    # do some formatting, make results into dataframe and assign names
    d_perf <- as.data.frame(d_perf)
    d_perf <- unfactor(d_perf)
    colnames(d_perf) <- c('Method','Variable', 'Total_Predictions', 'True_Positives', 'True_Negatives', 
                          'False_Positives', 'False_Negatives')
    
    # make a list of variable names that are going to be numeric
    num_vars <- c('Total_Predictions', 'True_Positives', 'True_Negatives', 'False_Positives', 'False_Negatives')
    
    # add to the aggregate dataframe
    if (count == 1) {
      # create an aggregate dataframe that will report means of all iterations
      agg_df <- d_perf
    }
    else {
      # add numeric values to dataframe
      agg_df[,num_vars] <- agg_df[,num_vars] + d_perf[,num_vars]
      
    }
    # increment for next iteration
    seed = seed + 1
    count = count + 1
  }
  
  # generate precision, recall, accuracy scores 
  agg_df <- agg_df %>% mutate(Precision = True_Positives/(True_Positives+False_Positives))
  agg_df <- agg_df %>% mutate(Recall = True_Positives/(True_Positives+False_Negatives))
  agg_df <- agg_df %>% mutate(Accuracy = (True_Positives+True_Negatives)/
                                        (True_Positives+True_Negatives+False_Positives+False_Negatives))
  agg_df <- format(agg_df, digits = 2)
  
  return(list(agg_df, df))
}


# make some summaries of detailed stats
summ_stats <- function(df, out_suff) {
  # make data set numeric where possible
  num_vars <- c('Total_Predictions', 'True_Positives', 'True_Negatives', 'False_Positives', 'False_Negatives',
                'Precision', 'Recall','Accuracy')
  temp <- as.matrix(df[,num_vars])
  mode(temp) <- 'numeric'
  df[3:ncol(df)] <- as.data.frame(temp)
  
  # generate what randomly assigning the same number of predictive positives would score on these metrics
  df <- df %>% mutate(
    Actual_Positives = True_Positives + False_Negatives,
    Actual_Negatives = True_Negatives + False_Positives,
    Predicted_Positives = True_Positives + False_Positives,
    Predicted_Negatives = True_Negatives + False_Negatives,
    Positives_Diff = Actual_Positives - Predicted_Positives,
    Negatives_Diff = Actual_Negatives - Predicted_Negatives,
    Rand_TP = True_Positives*True_Positives/Total_Predictions,
    Rand_TN = True_Negatives*True_Negatives/Total_Predictions,
    Rand_FP = Actual_Negatives - Rand_TN,
    Rand_FN = Actual_Positives - Rand_TP, 
    Rand_Precision = Rand_TP/(Rand_TP+Rand_FP),
    Rand_Recall = Rand_TP/(Rand_TP+Rand_FN),
    Rand_Accuracy = (Rand_TP+Rand_TN)/
      (Rand_TP+Rand_TN+Rand_FP+Rand_FN),
    Precision_Index = Precision - Rand_Precision,
    Recall_Index = Recall - Rand_Recall,
    Accuracy_Index = Accuracy - Rand_Accuracy
  )
  
  # export detailed statistics
  write.csv(df, file=paste0('./output/detail_meth_compar_',out_suff,'.csv'), row.names=FALSE)
  
  # now find mean scores by method, and by variable
  meth_df <- aggregate(df[,c('Precision','Recall','Accuracy','Rand_Precision','Rand_Recall','Rand_Accuracy',
                             'Precision_Index','Recall_Index','Accuracy_Index')], 
                       list(df[,'Method']), mean, na.action = na.omit)
  var_df <- aggregate(df[,c('Precision','Recall','Accuracy','Rand_Precision','Rand_Recall','Rand_Accuracy',
                            'Precision_Index','Recall_Index','Accuracy_Index')], 
                      list(df[,'Variable']), mean, na.action = na.omit)
  
  write.csv(meth_df, file=paste0('./output/summary_meth_compar_',out_suff,'.csv'), row.names=FALSE)
  write.csv(var_df, file=paste0('./output/summary_var_compar_',out_suff,'.csv'), row.names=FALSE)
}

# summarize how well overall leave length is predicted by the method
leave_stats <- function(result_dfs, meths) {
  
  count <- 1
  # calculate actual vs predicted leave taking for each of the different methods
  for (i in result_dfs) {
    # keep track of method
    meth <- meths[count]
    # define lists  of leave type variables
    leave_types <- c("own","illspouse","illchild","illparent","matdis","bond")
    pred_take_vars <- paste('take', leave_types,sep="_")
    act_take_vars <- paste('take', leave_types,'act',sep="_")
    pred_need_vars <- paste('need', leave_types,sep="_")
    act_need_vars <- paste('need', leave_types,'act',sep="_")
    
    # create two columns of total leaves predicted and actual, and two columns for total leave lengths
    # i['num_leaves_take'] is actual number of leaves taken
    i['tot_pred_take_num'] <- rowSums(i[pred_take_vars],na.rm=TRUE)
    # i['num_leaves_need'] is actual number of leaves needed
    i['tot_pred_need_num'] <- rowSums(i[pred_need_vars],na.rm=TRUE)
    
    # keep track of how many leaves individual was eligible for and predicted
    i['tot_elig_types'] <- rowSums(ifelse(is.na(i[pred_take_vars]),0,1))
    
    
    # aggregate results into a single data frame 
    result_agg <- row
    count <- count + 1
  }
  return(result_agg_df)
}

# # run for all of the methods
#meths <- c('random_forest','Naive_Bayes', 'ridge_class', 'KNN1', 'KNN_multi', 'logit')
meths <- c('random_forest', 'logit')
# unweighted results
# timestart <<- Sys.time()
# df <- predict_test('logit', n = 10)
# print('logit')
# print(Sys.time() - timestart)
# timestart <<- Sys.time()
# 
# for (meth in c('Naive_Bayes', 'ridge_class', 'KNN1', 'KNN_multi')) {
#   result <- predict_test(meth, n = 10)
#   df <- rbind(df, result)
#   print(meth)
#   print(Sys.time() - timestart)
#   timestart <<- Sys.time()
# }
# 
# summ_stats(df, 'unwgt')
# 
# rerun for weighted results∟

for (meth in meths) {
  # different loop for first method
  if (meth==meths[[1]]) {
    timestart <<- Sys.time()
    result <- predict_test(meth, n = 10, weighted=TRUE)
    result_dfs <- list(random_forest=result[[2]])
    agg_df <- result[[1]]
    print(meth)
    print(Sys.time() - timestart)
    timestart <<- Sys.time()
  }
  else {
    result <- predict_test(meth, n = 10, weighted=TRUE)
    result_dfs[[meth]] <- result[[2]]
    agg_df <- rbind(agg_df, result[[1]])
    print(meth)
    print(Sys.time() - timestart)
    timestart <<- Sys.time()  
  }
}

# summary stats regarding the performance of each measure
summ_stats(agg_df, 'wgt')

# summary stats regarding overall leave taking  
leave_stats(result_dfs, meths)