
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
options(error=recover)
# turn off scientific notation conversion
options(scipen=999)

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

global.libraries <- c('bnclassify', 'randomForest','magick','stats', 'rlist', 'MASS', 'plyr', 'dplyr', 
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
# 
# d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 1, 0, prop_pay))
# d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 2, .125, prop_pay))
# d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 3, .375, prop_pay))
# d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 4, .5, prop_pay))
# d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 5, .625, prop_pay))
# d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 6, .875, prop_pay))
# d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(prop_pay == 7, 1, prop_pay))

# ============================ #
# 1. FMLA train/test split performance testing
# ============================ #


# function for predicting variables from test sample
# impute_method - method of imputation
# prop - proportion of data set to use as test data
# kval - if method is "KNN_multi", number of  neighbors to use
# n - number of iterations to run
# seeded - whether to set random seed or not

# dependent Variables to be used as predictors
xvars=c("widowed", "divorced", "separated", "nevermarried", "female", 
        "agesq", "ltHS", "someCol", "BA", "GradSch", "black", 
        "white", "asian", "hisp","nochildren",'faminc')

# independent variables  to be predicted
yvars <- c(pred_leave = 'pred_leave',
           need_own = "need_own", 
           need_illspouse = "need_illspouse",
           need_illchild = "need_illchild",
           need_illparent = "need_illparent",
           need_matdis = "need_matdis",
           need_bond = "need_bond",
           pred_need = 'pred_need',
           resp_len= "resp_len")

#function to generate SE for an FMLA variable using replicate weights
replicate_weights_SE <- function(d, var, prop) {
  # base estimate of population mean, total
  x= weighted.mean(d[,var], d[,'weight'], na.rm=TRUE)
  tot=sum(d[,var]* d[,'weight'], na.rm=TRUE)

  # Estimates from replicate weights
  replicate_weights <- c(paste0('rpl0',seq(1,9)),paste0('rpl',seq(10,80)))
  count=0
  for (i in replicate_weights) {
    count=count+1
    assign(paste("x", count, sep = ""), weighted.mean(d[,var], d[,i], na.rm=TRUE))   
    assign(paste("tot", count, sep = ""), sum(d[,var]* d[,i], na.rm=TRUE))
  }
  replicate_means <- paste0('x',seq(1,80))
  
  # calculate standard error, confidence interval
  SE= sqrt(4/80*sum(sapply(mget(paste0('x',seq(1,80))), function(y) {(y-x)^2})))
  CI_low=x-1.96*SE
  CI_high=x+1.96*SE
  CI= paste("[",format(x-1.96*SE, digits=3,nsmall=3, scientific=FALSE, big.mark=","),",", format(x+1.96*SE, digits=3, nsmall=3, scientific=FALSE, big.mark=","),"]")
  total=sum(d[,var]*d[,'weight'], na.rm=TRUE)
  total_SE= sqrt(4/80*sum(sapply(mget(paste0('tot',seq(1,80))), function(y) {(y-tot)^2})))
  total_CI_low= total-total_SE*1.96
  total_CI_high= total+total_SE*1.96
  total_CI=paste("[",format(total_CI_low, digits=2, scientific=FALSE, big.mark=","),",", format(total_CI_high, digits=2, scientific=FALSE, big.mark=","),"]")
  
  # return statistics
  stats= list(var, estimate=x, std_error=SE,confidence_int=CI,CI_low=CI_low,CI_high=CI_high, 
              total=total, total_SE=total_SE,total_CI_low=total_CI_low,total_CI_high=total_CI_high, total_CI=total_CI)
  for (i in stats[c(2:3,7:8,11)]) {
    i <- format(i, nsmall=3)
  }
  return(stats)
}

# type/number of leaves taken and prop_pay have more than one category, so we need to handle them differently

predict_test <- function(impute_method, prop=.25, kval = 5,  n=3, seeded = TRUE, weighted = FALSE, append = FALSE) {

  seed = 123
  count = 1
  
    # n is number of iterations to run the calculation by 
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
    leave_types <- c("own","illspouse","illchild","illparent","matdis","bond")
    take_vars <- paste('take', leave_types,sep="_")
    act_take_vars <- paste('take', leave_types,'act',sep="_")
    need_vars <- paste('need', leave_types,sep="_")
    act_need_vars <- paste('need', leave_types,'act',sep="_")
    for (i in c(yvars, take_vars, 'prop_pay')) {
      if (i != 'pred_leave' & i != 'pred_need') {
        new_name <- paste0(i, '_act')
        fmla_test[new_name] <- fmla_test[i]
        fmla_test[i] <- NULL  
      }
    }
    # reweight test data set so weights stay nationally representative
    fmla_test['weight'] <- fmla_test['weight'] * 1/prop
    for (i in seq(1,80)) {
      if (i >= 10) {
        fmla_test[paste0('rpl',i)] <- fmla_test[paste0('rpl',i)]*1/prop 
      }
      else {
        fmla_test[paste0('rpl0',i)] <- fmla_test[paste0('rpl0',i)]*1/prop
      }
    }
    # use master imputation function to return predicted values for all variables
    df <- impute_fmla_to_acs(d_fmla=fmla_train, d_acs = fmla_test, 
                       impute_method=impute_method, xvars=xvars,kval=kval, xvar_wgts = rep(1, length(xvars)))
    

    # now for each yvar, assess accuracy of predictions
    
    # start a matrix to store results
    # number of columns needed for output dataframe
    ncols = 15
    
    d_perf <- matrix(0, ncol = ncols, nrow = 0)

    
    for (i in yvars) {
      
      act_var <- paste0(i, '_act')
      
      # going to handle results of take vars differently since we dont observe all leave types taken
      if (i == 'pred_leave') {
        # note whether leave taken
        df['act_leave'] <- rowSums(df[act_take_vars], na.rm=TRUE)
        # note what type of leave was taken
        df['act_type'] <- 'None'
        for (j in take_vars) {
          df['act_type'] <- with(df, ifelse(get(paste0(j,"_act"))==1, j, act_type))
        }
        # measure number of predictions
        df['num_pred'] <- rowSums(df[take_vars], na.rm=TRUE)
      
        # mark if predicting any leave
        df['pred_leave'] <- with(df, ifelse(num_pred>0,1,0))
        
        # mark results of each test row
        df['result'] <- NA
        df <- df %>% mutate(result = ifelse(act_leave==1 & pred_leave==1, 'TP', result))
        df <- df %>% mutate(result = ifelse(act_leave==0 & pred_leave==0, 'TN', result))
        df <- df %>% mutate(result = ifelse(act_leave==0 & pred_leave==1, 'FP', result))
        df <- df %>% mutate(result = ifelse(act_leave==1 & pred_leave==0, 'FN', result))
        act_var <- 'act_leave'
      }
      # leave needers
      else if (i=='pred_need') {
        # note whether leave needed
        df['pred_need'] <- rowSums(df[need_vars], na.rm=TRUE)
        df['act_need'] <- rowSums(df[act_need_vars], na.rm=TRUE)

        # mark results of each test row
        df['result'] <- NA
        df <- df %>% mutate(result = ifelse(act_need>=1 & pred_need>=1, 'TP', result))
        df <- df %>% mutate(result = ifelse(act_need==0 & pred_need==0, 'TN', result))
        df <- df %>% mutate(result = ifelse(act_need==0 & pred_need>=1, 'FP', result))
        df <- df %>% mutate(result = ifelse(act_need>=1 & pred_need==0, 'FN', result))
        act_var <- 'act_need'
      }
      else {
        # mark results of each test row
        df['result'] <- NA
        df <- df %>% mutate(result = ifelse(get(act_var)==1 & get(i)==1, 'TP', result))
        df <- df %>% mutate(result = ifelse(get(act_var)==0 & get(i)==0, 'TN', result))
        df <- df %>% mutate(result = ifelse(get(act_var)==0 & get(i)==1, 'FP', result))
        df <- df %>% mutate(result = ifelse(get(act_var)==1 & get(i)==0, 'FN', result))
      }
      
      pred_SE_results <- replicate_weights_SE(df, i, prop)
      act_SE_results <- replicate_weights_SE(df, act_var, prop)
      pred_mean <- pred_SE_results[['estimate']]
      pred_SE <- pred_SE_results[['std_error']]
      pred_CI_low <- pred_SE_results[['CI_low']]
      pred_CI_high <- pred_SE_results[['CI_high']]
      act_mean <- pred_SE_results[['estimate']]
      act_SE <- act_SE_results[['std_error']]
      act_CI_low <- act_SE_results[['CI_low']]
      act_CI_high <- act_SE_results[['CI_high']]
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
      row <- matrix(c(impute_method, i, tot_pred, TP, TN, FP, FN,pred_mean, pred_SE,pred_CI_low,pred_CI_high,act_mean, act_SE,act_CI_low,act_CI_high), ncol = ncols, nrow = 1)

      # add performance measures as row to results matrix
      d_perf <- rbind(d_perf, row)  
    }
    # do some formatting, make results into dataframe and assign names
    d_perf <- as.data.frame(d_perf)
    d_perf <- unfactor(d_perf)
    colnames(d_perf) <- c('Method','Variable', 'Total_Predictions', 'True_Positives', 'True_Negatives', 
                          'False_Positives', 'False_Negatives','Predicted_Mean','Predicted_SE','Predicted_CI_low','Predicted_CI_high',
                          'Actual_Mean','Actual_SE','Actual_CI_low','Actual_CI_high')
    
    # make a list of variable names that are going to be numeric
    num_vars <- colnames(d_perf)[!colnames(d_perf) %in% c('Method','Variable')]
    
    # add to the aggregate dataframe
    if (count == 1) {
      # create an aggregate dataframe that will report means of all iterations
      agg_df <- d_perf
    }
    else {
      # add numeric values to dataframe
      agg_df[,num_vars] <- agg_df[,num_vars] + d_perf[,num_vars]
      
    }
    # if it's the last iteration, average the numeric variables by number of iterations
    if (count == n) {
      # make a list of variable names that are going to be numeric
      num_vars <- colnames(d_perf)[!colnames(d_perf) %in% c('Method','Variable')]
      agg_df[,num_vars] <- agg_df[,num_vars] / count
    }
    
    # repeat process for non-binary variables
    # ---- Accuracy in predicting the type of leave correctly ------
    # for each method, calculate weighted values for predicting leave type properly
    ncols = 10
    d_perf2 <- matrix(0, ncol = ncols, nrow = 0)
    colnames(d_perf2) <- c('Method','Variable','Pop_Predicted','Total_Predictions','Types_Pred_Per_Pop', 
                          'Correct', 'Incorrect', 'Accuracy','Random Accuracy', 'Adjusted Accuracy')
    
    # check if any predictions match actual leave type
    df['pred_type'] <- 0
    for (j in take_vars) {
      df['pred_type'] <- with(df, ifelse(get(j) == 1 & act_type == j, 1, pred_type))
    }
    # mark those with no leave predicted as NaN
    df['pred_type'] <- with(df, ifelse(num_pred>0, pred_type, NaN))
    
    # number of individuals at least one leave predicted
    pop_pred <- sum(df[!is.na(df['pred_type']),'weight'])
    # number of leaves predicted 
    tot_pred <- sum(df$num_pred*df$weight, na.rm = TRUE)
    
    # average number of leave types per individual predicted
    types_per_pop <- tot_pred/pop_pred
    correct <- sum(df$pred_type*df$weight, na.rm = TRUE)
    incorrect <- sum(df[!is.na(df['pred_type']) & df['pred_type']==0,'weight'])
    accuracy <- correct/(correct+incorrect)
    # rand accuracy is based on number of guesses 
    rand_accuracy <- 1-(5/6) ** types_per_pop
    adj_accuracy <- accuracy - rand_accuracy
    # put results in a row
    row <- matrix(c(impute_method, 'pred_type', pop_pred, tot_pred, types_per_pop, correct, incorrect, accuracy,rand_accuracy, adj_accuracy), ncol = ncols, nrow = 1)
    # append to data frame
    d_perf2 <- rbind(d_perf2, row)  

    # do some formatting, make results into dataframe
    d_perf2 <- as.data.frame(d_perf2)
    d_perf2 <- unfactor(d_perf2)
    
    # add to the aggregate dataframe
    if (count == 1) {
      # create an aggregate dataframe that will report means of all iterations
      agg_df2 <- d_perf2
    }
    
    else {
      # make a list of variable names that are going to be numeric
      num_vars <- colnames(d_perf2)[!colnames(d_perf2) %in% c('Method','Variable')]
      
      # add numeric values to dataframe
      agg_df2[,num_vars] <- agg_df2[,num_vars] + d_perf2[,num_vars]
    }
    # if it's the last iteration, average the numeric variables by number of iterations
    if (count == n) {
      # make a list of variable names that are going to be numeric
      num_vars <- colnames(d_perf2)[!colnames(d_perf2) %in% c('Method','Variable')]
      
      agg_df2[,num_vars] <- agg_df2[,num_vars] / count
    }
    
    # ----- Accuracy in predicting prop_pay correctly -----------
    ncols = 16
    d_perf3 <- matrix(0, ncol = ncols, nrow = 0)
    colnames(d_perf3) <- c('Method','Variable','Pop_Predicted','Predicted_Prop_Pay_Avg','Predicted_SE','Predicted_CI_low','Predicted_CI_high', 'Actual_Prop_Pay_Avg', 
                          'Actual_SE','Actual_CI_low','Actual_CI_high',
                          'Prop_Pay_Avg_Difference','Correct_Prop_Pay', 'Incorrect_Prop_Pay', 'Accuracy','Dist_Accuracy')
    # # Population Aggregate states on prop pay - no restriction on prop_pay
    # # number of individuals with prop pay predicted
    # pop_pred <- sum(df[!is.na(df['prop_pay']),'weight'])
    # # predicted prop_pay average
    # avg_pred <- weighted.mean(x = df$prop_pay, w = df$weight, na.rm = TRUE)
    # # number of leaves actually taken by population
    # avg_act <- weighted.mean(x = df$prop_pay_act, w = df$weight, na.rm = TRUE)
    # # difference between the two
    # avg_diff <- avg_pred - avg_act

    # Population Aggregate states on prop pay - restricted to prop_pay>0
    pp_df <- df %>% filter(prop_pay>0)
    # number of individuals with prop pay predicted
    pop_pred <- sum(pp_df[!is.na(pp_df['prop_pay']),'weight'])
    # predicted prop_pay average
    avg_pred <- weighted.mean(x = pp_df$prop_pay, w = pp_df$weight, na.rm = TRUE)
    # number of leaves actually taken by population
    avg_act <- weighted.mean(x = pp_df$prop_pay_act, w = pp_df$weight, na.rm = TRUE)
    # difference between the two
    avg_diff <- avg_pred - avg_act
    
        
    # calculate standard errors
    pred_SE_results <- replicate_weights_SE(pp_df, 'prop_pay', prop)
    act_SE_results <- replicate_weights_SE(pp_df, 'prop_pay_act', prop)
    pred_SE <- pred_SE_results[['std_error']]
    pred_CI_low <- pred_SE_results[['CI_low']]
    pred_CI_high <- pred_SE_results[['CI_high']]
    act_SE <- act_SE_results[['std_error']]
    act_CI_low <- act_SE_results[['CI_low']]
    act_CI_high <- act_SE_results[['CI_high']]
    
    # Accuracy for individual num of leaves
    pp_df['result'] <- with(pp_df, ifelse(prop_pay == prop_pay_act, 1, 0))
    correct <- sum(pp_df$result*pp_df$weight, na.rm = TRUE)
    incorrect <- sum(pp_df[!is.na(pp_df['result']) & pp_df['result']==0,'weight'])
    accuracy <- correct/(correct+incorrect)
    
    # Look at accuracy, weighting by distance of the incorrect guess
    pp_df['result_dist'] <- with(pp_df, abs(prop_pay - prop_pay_act))
    dist_accuracy <- sum(pp_df$result*pp_df$weight, na.rm = TRUE)

    # random guessing is close to zero accuracy, so no adjusted accuracy calculated
    # put results in a row
    row <- matrix(c(impute_method, 'pred_type', pop_pred, avg_pred, pred_SE,pred_CI_low,pred_CI_high, avg_act, act_SE,act_CI_low,act_CI_high,
                    avg_diff, correct, incorrect, accuracy, dist_accuracy), ncol = ncols, nrow = 1)
    # append to data frame
    d_perf3 <- rbind(d_perf3, row)  
    
    # do some formatting, make results into dataframe
    d_perf3 <- as.data.frame(d_perf3)
    d_perf3 <- unfactor(d_perf3)  
    
    # add to the aggregate dataframe
    if (count == 1) {
      # create an aggregate dataframe that will report means of all iterations
      agg_df3 <- d_perf3
    }
    else {
      # make a list of variable names that are going to be numeric
      num_vars <- colnames(d_perf3)[!colnames(d_perf3) %in% c('Method','Variable')]
      
      # add numeric values to dataframe
      agg_df3[,num_vars] <- agg_df3[,num_vars] + d_perf3[,num_vars]
    }
    # if it's the last iteration, average the numeric variables by number of iterations
    if (count == n) {
      # make a list of variable names that are going to be numeric
      num_vars <- colnames(d_perf3)[!colnames(d_perf3) %in% c('Method','Variable')]
      agg_df3[,num_vars] <- agg_df3[,num_vars] / count
    }

    # ----- Accuracy in predicting the number of leaves correctly -------
    ncols = 17
    d_perf4 <- matrix(0, ncol = ncols, nrow = 0)
    colnames(d_perf4) <- c('Method','Variable','Pop_Predicted','Predicted_Leave_Num','Predicted_SE','Predicted_CI_low','Predicted_CI_high',
                           'Actual_Leave_Num', 'Actual_SE','Actual_CI_low','Actual_CI_high',
                          'Leave_Num_Difference','Correct_Num_Leaves', 'Incorrect_Num_Leaves', 'Accuracy', 'Rand_Accuracy','Adj_Accuracy')

    # Population Aggregate states on leave taken
    # number of individuals at least one leave predicted
    pop_pred <- sum(df[!is.na(df['pred_type']),'weight'])
    # number of leaves predicted 
    tot_pred <- sum(df$num_pred*df$weight, na.rm = TRUE)
    # number of leaves actually taken by population
    tot_act <- sum(df$num_leaves_take*df$weight, na.rm = TRUE)
    # difference between the two
    tot_diff <- tot_pred - tot_act
    
    # calculate standard errors
    pred_SE_results <- replicate_weights_SE(df, 'num_pred', prop)
    act_SE_results <- replicate_weights_SE(df, 'num_leaves_take', prop)
    pred_SE <- pred_SE_results[['total_SE']]
    pred_CI_low <- pred_SE_results[['total_CI_low']]
    pred_CI_high <- pred_SE_results[['total_CI_high']]
    act_SE <- act_SE_results[['total_SE']]
    act_CI_low <- act_SE_results[['total_CI_low']]
    act_CI_high <- act_SE_results[['total_CI_high']]
    
    # Accuracy for individual num of leaves
    df['result'] <- with(df, ifelse(num_pred == num_leaves_take, 1, 0))
    correct <- sum(df$result*df$weight, na.rm = TRUE)
    incorrect <- sum(df[!is.na(df['result']) & df['result']==0,'weight'])
    accuracy <- correct/(correct+incorrect)
    # 7 possible choices, so random accuracy is 1/7
    rand_accuracy <- 1/7
    adj_accuracy <- accuracy - rand_accuracy
    # put results in a row
    row <- matrix(c(impute_method, 'pred_type', pop_pred, tot_pred,pred_SE,pred_CI_low,pred_CI_high, tot_act,act_SE, act_CI_low,act_CI_high, 
                    tot_diff, correct, incorrect, accuracy, rand_accuracy, adj_accuracy), ncol = ncols, nrow = 1)
    
    # append to data frame
    d_perf4 <- rbind(d_perf4, row)  
    
    # do some formatting, make results into dataframe
    d_perf4 <- as.data.frame(d_perf4)
    d_perf4 <- unfactor(d_perf4)
    
    # add to the aggregate dataframe
    if (count == 1) {
      # create an aggregate dataframe that will report means of all iterations
      agg_df4 <- d_perf4
    }
    else {
      # make a list of variable names that are going to be numeric
      num_vars <- colnames(d_perf4)[!colnames(d_perf4) %in% c('Method','Variable')]
      
      # add numeric values to dataframe
      agg_df4[,num_vars] <- agg_df4[,num_vars] + d_perf4[,num_vars]
    }
    # if it's the last iteration, average the numeric variables by number of iterations
    if (count == n) {
      # make a list of variable names that are going to be numeric
      num_vars <- colnames(d_perf4)[!colnames(d_perf4) %in% c('Method','Variable')]
      agg_df4[,num_vars] <- agg_df4[,num_vars] / count
    }
    
    # increment for next iteration
    seed = seed + 1
    count = count + 1
  }
  
  # generate precision, recall, accuracy scores for binary variables 
  agg_df <- agg_df %>% mutate(Precision = True_Positives/(True_Positives+False_Positives))
  agg_df <- agg_df %>% mutate(Recall = True_Positives/(True_Positives+False_Negatives))
  agg_df <- agg_df %>% mutate(Accuracy = (True_Positives+True_Negatives)/
                                        (True_Positives+True_Negatives+False_Positives+False_Negatives))
  agg_df <- format(agg_df, digits = 2)
  
  # write csv's for the non-binary variables
  write.table(agg_df2, file='./output/pred_type_stats.csv', row.names=FALSE, sep ="," , append = append, col.names = !append)
  write.table(agg_df3, file='./output/prop_pay_stats.csv', row.names=FALSE, sep ="," , append = append, col.names = !append)
  write.table(agg_df4, file='./output/leave_num_stats.csv', row.names=FALSE, sep ="," , append = append, col.names = !append)
  
  return(list(agg_df, df))
}


# make some summaries of detailed stats
summ_stats <- function(df, out_suff) {
  # make data set numeric where possible
  num_vars <- colnames(df)[!colnames(df) %in% c('Method','Variable')]
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
    Rand_TP = Predicted_Positives*Actual_Positives/Total_Predictions,
    Rand_TN = Predicted_Negatives*Actual_Negatives/Total_Predictions,
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
  write.table(df, file=paste0('./output/detail_meth_compar_',out_suff,'.csv'), row.names=FALSE, sep ="," )
  
  # now find mean scores by method, and by variable
  meth_df <- aggregate(df[,c('Precision','Recall','Accuracy','Rand_Precision','Rand_Recall','Rand_Accuracy',
                             'Precision_Index','Recall_Index','Accuracy_Index')], 
                       list(df[,'Method']), mean, na.action = na.omit)
  var_df <- aggregate(df[,c('Precision','Recall','Accuracy','Rand_Precision','Rand_Recall','Rand_Accuracy',
                            'Precision_Index','Recall_Index','Accuracy_Index')], 
                      list(df[,'Variable']), mean, na.action = na.omit)
  
  write.table(meth_df, file=paste0('./output/summary_meth_compar_',out_suff,'.csv'), row.names=FALSE, sep ="," )
  write.table(var_df, file=paste0('./output/summary_var_compar_',out_suff,'.csv'), row.names=FALSE, sep ="," )
}


methods <- c('logit','random', 'KNN1', 'KNN_multi','random_forest','Naive_Bayes', 'ridge_class')
#methods <- c('logit')

# run for weighted results

for (meth in methods) {
  # different loop for first method
  if (meth==methods[[1]]) {
    timestart <<- Sys.time()
    result <- predict_test(meth, n = 1, weighted=TRUE, append = FALSE)
    result_dfs <- list(logit=result[[2]])
    agg_df <- result[[1]]
    print(meth)
    print(Sys.time() - timestart)
    timestart <<- Sys.time()
  }
  else {
    result <- predict_test(meth, n = 1, weighted=TRUE, append = TRUE)
    result_dfs[[meth]] <- result[[2]]
    agg_df <- rbind(agg_df, result[[1]])
    print(meth)
    print(Sys.time() - timestart)
    timestart <<- Sys.time()  
  }
}

# summary stats regarding the performance of each measure
summ_stats(agg_df, out_suff = 'wgt')


# multi-class ROC function
multi_class_ROC <- function(meth, n = 1) {
}
  