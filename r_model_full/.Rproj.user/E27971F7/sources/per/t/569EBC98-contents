
# """
# 3_post_imputation_functions
#
# These functions are Post-imputation Leave Parameter Functions [on post-imputation ACS data set]
# that execute the policy simulation after leave taking behavior has been established.
#
# 
# """

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table of Contents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0. LEAVEPROGRAM
# 1. impute_leave_length
  # 1A. RunRandDraw - see 3_impute_functions.R, function 1Bc
# 2. CLONEFACTOR
# 3. PAY_SCHEDULE
# 4. ELIGIBILITYRULES
    # 4A. FORMULA
# 5. EXTENDLEAVES
    # 5A. runLogitEstimate - see 3_impute_functions.R, function 1Ba
# 6. UPTAKE
    # 6A. check_caps
# 7. BENEFITS
# 8. BENEFITEFFECT
# 9. TOPOFF
# 10. DEPENDENTALLOWANCE
# 11. DIFF_ELIG
# 12. CLEANUP
    # 12a. check_caps

# ============================ #
# 0. LEAVEPROGRAM
# ============================ #
# Baseline changes for addition of a leave program
# follows baseline changes of ACM model (see p.11 of ACM model description paper). Main change needed to base cleaning:
#   Leave needers who did not take a leave in the absence of a program, and
#   who said the reason that they did not take a leave was because they could not afford to
#   take one, take a leave in the presence of a program.
LEAVEPROGRAM <- function(d, sens_var,dual_receiver) {
  for (i in leave_types) {
    squo_take_var=paste0("squo_take_",i)
    take_var=paste0("take_",i)
    need_var=paste0("need_",i)
    # denote status quo leave taking
    d[,squo_take_var] <- d[,take_var]
    # change leave taking in presence of program
    d[,take_var] <- ifelse(d[,sens_var]==1 & d[,need_var]==1 & !is.na(d[,sens_var]) & !is.na(d[,need_var]),1,d[,take_var])
  }
  
  # create status quo taker of any leave type
  d[,'squo_taker'] <- 0
  for (i in leave_types) {
    squo_take_var=paste0("squo_take_",i)
    d <- d %>% mutate(squo_taker= ifelse(get(squo_take_var)==1,1,squo_taker))
  }
  
  # generate dual receiver (can receive both state and employer benefits simultaenously) column based on proportion specified in dual_receiver parameter
  pop_target <- sum(d$PWGTP)*dual_receiver
  # guess samp size needed based on mean weighted
  samp_size <- nrow(d) * dual_receiver
  samp_idx <- sample(seq_len(nrow(d)), samp_size)
  # make sure there aren't any
  # add/remove individuals to get to pop target
  samp_sum <- sum(d[samp_idx,'PWGTP'])
  if (samp_sum> pop_target) {
    while (samp_sum> pop_target) {
      samp_idx <- samp_idx[2:length(samp_idx)]
      samp_sum <- sum(d[samp_idx,'PWGTP'])
    }
  } else if (samp_sum< pop_target) { 
    while (samp_sum < pop_target){
      remain_idx <- setdiff(rownames(d),samp_idx)
      samp_idx  <- append(samp_idx,remain_idx[2])
      samp_sum <- sum(d[samp_idx,'PWGTP'])
    }
  }
  # set dual receiver status for
  d$dual_receiver <- 0
  d[samp_idx, 'dual_receiver'] <- 1
  
  return(d)
}



# ============================ #
# 1. impute_leave_length
# ============================ #
# function to impute leave length once leave taking behavior has been imputed 
# currently impute method is hardcoded as a random draw from a specified distribution of FMLA observations
# but this is a candidate for modual imputation

impute_leave_length <- function(d_train, d_test, ext_resp_len,rr_sensitive_leave_len,wage_rr,
                                maxlen_DI,maxlen_PFL) { 
  
  #Days of leave taken - currently takes length from most recent leave only
  yvars <- c(own = "length_own",
              illspouse = "length_illspouse",
              illchild = "length_illchild",
              illparent = "length_illparent",
              matdis = "length_matdis",
              bond = "length_bond")
  
  #   Leave lengths are the same, except for own leaves, which are instead taken from the distribution of leave takers in FMLA survey reporting 
  #   receiving some pay from state programs. 
  
  filts <- c(own = "(take_own==1|need_own==1)",
                  illspouse = "(take_illspouse==1|need_illspouse==1) & nevermarried == 0 & divorced == 0",
                  illchild = "(take_illchild==1|need_illchild==1)",
                  illparent = "(take_illparent==1|need_illparent==1)",
                  matdis = "(take_matdis==1|need_matdis==1) & female == 1 & nochildren == 0",
                  bond = "(take_bond==1|need_bond==1) & nochildren == 0")

  maxlen <- c(own=maxlen_DI,
              illspouse=maxlen_PFL,
              illchild=maxlen_PFL,
              illparent=maxlen_PFL,
              matdis=maxlen_DI,
              bond=maxlen_PFL)
  # using random draw from leave distribution rather than KNN prediction for computational issues
  #INPUTS: variable requiring imputation, conditionals to filter test and training data on,
  #        ACS or FMLA observations requiring imputed leave length (test data), FMLA observations constituting the
  #        sample from which to impute length from (training data), and presence/absence of program
  
  # using actual leave length distribution data since FMLA only gives ranges of leave lengths
  d_lens <- read.csv('../data/leave_length_prob_dist.csv')
  #d_lens2 <- fromJSON(file = "./data/length_distributions_exact_days.json")
  
  predict <- mapply(runRandDraw, yvar=yvars, filt=filts, maxlen=maxlen,
                    MoreArgs = list(leave_dist=d_lens, d=d_test, ext_resp_len=ext_resp_len, 
                                    rr_sensitive_leave_len=rr_sensitive_leave_len,
                                    wage_rr=wage_rr)
                                    , SIMPLIFY = FALSE)
  # Outputs: data sets of imputed leave length values for ACS or FMLA observations requiring them
  # merge imputed values with fmla data

  for (i in names(predict)) {
    d_pred <- predict[[i]]
    if (!is.null(d_pred)) {
      
      # old merge code, caused memory issues. using match instead
      #d_test <- merge(i, d_test, by="id",all.y=TRUE)  
      for (j in names(d_pred)) {
        if (j %in% names(d_test)==FALSE){
          d_test[j] <- d_pred[match(d_test$id, d_pred$id), j]    
        }
      }
      
      
    }
    else {
      d_test[paste0('length_',i)] <- 0
      d_test[paste0('mnl_',i)] <- 0
      d_test[paste0('squo_length_',i)] <- 0
      d_test[paste0('take_',i)] <- 0
    }
  }  
  vars_name=c()
  for (i in leave_types) {
    vars_name= c(vars_name, paste("length",i, sep="_"))
  }

  # replace leave taking and length NA's with zeros now
  # wanted to distinguish between NAs and zeros in FMLA survey, 
  # but no need for that in ACS now that we're "certain" of ACS leave taking behavior 
  # We are "certain" because we only imputed leave takers/non-takers, discarding those with 
  # uncertain/ineligible status (take_[type]=NA).
  
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    mnl_var=paste("mnl_",i,sep="")
    squo_var=paste0('squo_', len_var)
    take_var=paste("take_",i,sep="")
    d_test[len_var] <- with(d_test, ifelse(is.na(get(len_var)),0,get(len_var)))
    d_test[take_var] <- with(d_test, ifelse(is.na(get(take_var)),0,get(take_var)))
    d_test[squo_var] <- with(d_test, ifelse(is.na(get(squo_var)),0,get(squo_var)))
    d_test[mnl_var] <- with(d_test, ifelse(is.na(get(mnl_var)),0,get(mnl_var)))
  }
  
  # calculate total status quo length 
  d_test['squo_total_length'] <- 0 
  for (i in leave_types) {
    d_test['squo_total_length'] <- d_test['squo_total_length'] + d_test[paste("squo_length_",i,sep="")]
  }
  
  return(d_test)
}
# ============================ #
# 1A. runRandDraw
# ============================ #
# see 3_impute_functions.R, function 1Bc

# ============================ #
# 2. CLONEFACTOR
# ============================ #
# allow users to clone ACS individuals

CLONEFACTOR <- function(d, clone_factor) {
  if (clone_factor > 1) {
    d$clone_flag=0
    num_clone <- round((clone_factor-1)*nrow(d), digits=0)
    d_clones <- data.frame(sample(d$id,num_clone,replace=TRUE))
    colnames(d_clones)[1] <- "id"
    d_clones <- plyr :: join(d_clones,d,by='id', type='left')
    d_clones$clone_flag=1
    d <- rbind(d,d_clones)
    # reset id var
    d['id'] <- as.numeric(rownames(d))
  }
  return(d)
}

 

# ============================ #
# 3. PAY_SCHEDULE
# ============================ #
# Calculate pay schedule for employer paid leave

PAY_SCHEDULE <- function(d) {
  
  # two possible pay schedules: paid the same amount each week, or paid in full until exhausted
  # Here we randomly assign one of these three pay schedules 
  # based on conditional probabilities of total pay received and pay schedules 
  # probabilities are obtained from 2001 Westat survey which ACM used for this purpose
  # dist <- read.csv("pay_dist_prob.csv")
  
  # columns from this csv written manually to avoid dependency on csv file
  # proportion of pay received (prop_pay_employer in FMLA data)
  # Westat 2001 survey: About how much of your usual pay did you receive in total?
  Total_paid=c("Less than half","Half","More than half")
  
  # Prob of 1st pay schedule - some pay, all weeks
  # Westat 2001 survey: Receive receive some  pay for each pay period  that you were on leave?
  Always_paid=c(0.6329781, 0.8209731, 0.9358463)
  
  # Prob of 2nd pay schedule - full pay, some weeks
  # Westat 2001 survey: If not, when you did receive pay, was it for your full salary?
  Fully_paid=c(0.3273122,0.3963387,0.3633615)
  
  # Prob of 3rd pay schedule - some pay, some weeks
  # Neither paid each pay period, nor receive full pay when they did receive pay.
  Neither_paid=1-Fully_paid
  d_prob=data.frame(Total_paid,Always_paid,Fully_paid,Neither_paid)
  if (is.factor(d_prob$Total_paid)){
    d_prob$Total_paid=unfactor(d_prob$Total_paid)  
  }
  
  
  
  # denote bucket of proportion of pay
  d <- d %>% mutate(Total_paid= ifelse(prop_pay_employer>0 & prop_pay_employer<.5,"Less than half",NA))
  d <- d %>% mutate(Total_paid= ifelse(prop_pay_employer==.5, "Half" ,Total_paid))
  d <- d %>% mutate(Total_paid= ifelse(prop_pay_employer>.5 & prop_pay_employer<1, "More than half",Total_paid))
  
  # merge probabilities in
  d <- plyr :: join(d,d_prob, type="left",match="all",by="Total_paid")

  # assign pay schedules
  d['rand']=runif(nrow(d))
  d['rand2']=runif(nrow(d))
  
  d <- d %>% mutate(pay_schedule= ifelse(rand<Always_paid,"some pay, all weeks",NA))
  d <- d %>% mutate(pay_schedule= ifelse(rand>=Always_paid & rand2<Fully_paid,"all pay, some weeks",pay_schedule))
  d <- d %>% mutate(pay_schedule= ifelse(rand>=Always_paid & rand2>=Fully_paid,"some pay, some weeks",pay_schedule))
  d <- d %>% mutate(pay_schedule= ifelse(prop_pay_employer==1,"all pay, all weeks",pay_schedule))
  d <- d %>% mutate(pay_schedule= ifelse(prop_pay_employer==0,"no pay",pay_schedule))
  
  # total_length - number of days leave taken of all types
  d['total_length']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    d <- d %>% mutate(total_length=ifelse(get(paste(take_var)) == 1, total_length+get(paste('length_',i,sep="")), total_length))
  }
  
  # count up number of types of leaves
  d['total_leaves']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    d <- d %>% mutate(total_leaves = ifelse(get(paste(take_var))==1, total_leaves+1,total_leaves))
  }
  
  # Keep track of what day employer benefits will be exhausted for those receiving pay in some but not all of their leave
  # all pay, some weeks
  d <-  d %>% mutate(exhausted_by=ifelse(pay_schedule=="all pay, some weeks",round(total_length*prop_pay_employer, digits=0), NA))
  
  # some pay, some weeks - like ACM, assumes equal distribution of partiality among pay proportion and weeks taken
  d <-  d %>% mutate(exhausted_by=ifelse(pay_schedule=="some pay, some weeks",round(total_length*sqrt(prop_pay_employer), digits=0), exhausted_by))
  
  # clean up vars
  d <- d[, !(names(d) %in% c('rand','rand2','Always_paid','Total_paid','Fully_paid', 'Neither_paid'))]
  
  return(d)
}

# ============================ #
# 4. ELIGIBILITYRULES
# ============================ #
# apply user-specified eligibility criteria and set initial 

ELIGIBILITYRULES <- function(d, earnings=NULL, weeks=NULL, ann_hours=NULL, minsize=NULL, 
                             base_bene_level, week_bene_min, formula_prop_cuts=NULL, formula_value_cuts=NULL,
                             formula_bene_levels=NULL, elig_rule_logic, FEDGOV, STATEGOV, LOCALGOV, SELFEMP,PRIVATE, dual_receiver) {
  
  # ----- apply eligibility rules logic to calculate initial participation ---------------
  # strip terms from those criteria in elig_rule_logic that have corresponding NULL values
  for (i in c('earnings', 'weeks', 'ann_hours', 'minsize')) {
    if (is.null(get(i))) {
      elig_rule_logic <- gsub(i,'TRUE',elig_rule_logic)
    }
  }
  
  # replace terms in logic string with appropriate conditionals
  elig_rule_logic <- gsub('earnings','wage12>=earnings',elig_rule_logic)
  elig_rule_logic <- gsub('weeks','weeks_worked>=weeks',elig_rule_logic)
  elig_rule_logic <- gsub('ann_hours','weeks_worked*WKHP>=ann_hours',elig_rule_logic)
  elig_rule_logic <- gsub('minsize','empsize>=minsize',elig_rule_logic)
  
  # create elig_worker flag based on elig_rule_logic
  d <- d %>% mutate(eligworker= ifelse(eval(parse(text=elig_rule_logic)), 1,0))
  
  # apply government worker filters
  if (FEDGOV==FALSE) {
    d <- d %>% mutate(eligworker = ifelse(COW==5,0,eligworker)) 
  }
  
  if (STATEGOV==FALSE) {
    d <- d %>% mutate(eligworker = ifelse(COW==4,0,eligworker))
  }
  
  if (LOCALGOV==FALSE) {
    d <- d %>% mutate(eligworker = ifelse(COW==3,0,eligworker))
  }
  
  # apply self employment filter
  if (SELFEMP==FALSE) {
    d <- d %>% mutate(eligworker = ifelse(COW==6 | COW==7,0,eligworker))
  }
  
  # apply private sector employment filter
  if (PRIVATE==FALSE) {
    d <- d %>% mutate(eligworker = ifelse(COW==1 | COW==2,0,eligworker))
  }
  
  # filter to only eligible participants
  d <- d %>% filter(eligworker==1)
  
  # ------ benefit calc --------------
  # if formulary benefits are not specificed, everyone will simply receive base_bene_level
  d["benefit_prop"] <- base_bene_level
  
  # adjust proportion of pay received if formulary benefits are specified; 
  # different benefit levels for different incomes with cuts defined by either 
  # proportion of mean state wage, or absolute wage values
  if (!is.null(formula_prop_cuts) | !is.null(formula_value_cuts)) {
    if (is.null(formula_bene_levels)) {
      stop('if formula_prop_cuts or formula_value_cuts are specified, 
           formula_bene_levels must also be specified')
    }
    d <- FORMULA(d, formula_prop_cuts, formula_value_cuts, formula_bene_levels)
  }
  
  # A non-zero minimum weekly benefit payment will increase effective benefit prop for those that
  # would otherwise receive lower than that. Adjust bene_prop to account for that when 
  # simulating participation decision. We're creating a throwaway bene_prop variable, 
  # as we still want to use actual bene_prop for determining benefits received, then will 
  # increase weekly payments at the end after all participation is determined.
  d <- d %>% mutate(benefit_prop_temp = pmax(week_bene_min/(wage12/weeks_worked), benefit_prop))
  
  # calculate general participation decision based on employer pay vs state program pay    
  # those who will receive more under the program and are not dual receivers will participate
  d["particip"] <- 0
  d["particip"] <- ifelse(d[,"eligworker"]==1 & d[,"prop_pay_employer"]<d[,"benefit_prop_temp"]& d[,'dual_receiver']==0,1,0)    
  
  # those who exhaust employer benefits before leave ends and are not dual receivers will participate
  d["particip"] <- ifelse(d[,"eligworker"]==1 & !is.na(d[,'exhausted_by']) & d[,'dual_receiver']==0,1,d[,"particip"])  
  
  # dual receiver will participate regardless of employer benefits
  d["particip"] <- ifelse(d[,"eligworker"]==1 & d[,'dual_receiver']==1,1,d[,'particip'])    
  return(d)  
}

# ============================ #
# 4A. FORMULA
# ============================ #
# subfunction to implement formulaic benefit payouts by wage, 
# rather than a flat proportion for all participants

FORMULA <- function(d, formula_prop_cuts=NULL, formula_value_cuts=NULL, formula_bene_levels) {
  
  #-----------Validation Checks---------------
  
  # make sure exactly one of prop cuts and value cuts are specified
  if (!is.null(formula_prop_cuts) & !is.null(formula_value_cuts)) {
    stop("formula_prop_cuts and formula_value_cuts are both specified. Only one should be specified")
  }
  if (is.null(formula_prop_cuts) & is.null(formula_value_cuts)) {
    stop("Neither formula_prop_cuts and formula_value_cuts are specified. One must be specified")
  }
    
  # checks to make sure formula_cuts and values are positive and ascending
  if (!is.null(formula_prop_cuts)) {
    # make sure formula cuts and bene levels are proper length
    if (length(formula_prop_cuts)+1 != length(formula_bene_levels)) {
      stop("formula_bene_levels length must be one greater than formula_prop_cuts length")
    }
    
    prev_val=0
    for (i in formula_prop_cuts) {
      if (!is.numeric(i)) {
        stop("formula_prop_cuts must be numeric")
      }
      if (0>i) {
        stop("formula_prop_cuts must be positive") 
      }
      if (prev_val>i) {
        stop("formula_prop_cuts must be in ascending order")
      }
      prev_val=i
    }
  }
  
  if (!is.null(formula_value_cuts)) {
    # make sure formula cuts and bene levels are proper length
    if (length(formula_value_cuts)+1 != length(formula_bene_levels)) {
      stop("formula_bene_levels length must be one greater than formula_value_cuts length")
    }
    
    prev_val=0
    for (i in formula_value_cuts) {
      if (!is.numeric(i)) {
        stop("formula_value_cuts must be numeric")
      }
      if (0>i) {
        stop("formula_value_cuts must be positive") 
      }
      if (prev_val>i) {
        stop("formula_value_cuts must be nonduplicated, and in ascending order")
      }
      prev_val=i
    }
  }
  
  #------------------Adjust benefit levels: proportionate cuts----------------------
  if (!is.null(formula_prop_cuts)) {
    
    # establish mean wage of population, and everyone's proportion of that value
    mean_wage=mean(d$wage12/d$weeks_worked)
    d['mean_wage_prop']=(d$wage12/d$weeks_worked)/mean_wage
    
    # adjust benefit_prop accordingly
    # first interval of formula_bene_levels
    len_cuts=length(formula_prop_cuts)
    len_lvls=length(formula_bene_levels)
    d <- d %>% mutate(benefit_prop = ifelse(formula_prop_cuts[1]>mean_wage_prop, 
                                           formula_bene_levels[1], benefit_prop))
    # last interval 
    d <- d %>% mutate(benefit_prop = ifelse(formula_prop_cuts[len_cuts]<=mean_wage_prop, 
                                           formula_bene_levels[len_lvls], benefit_prop))
    
    # rest of the intervals in between
    prev_val=formula_prop_cuts[1]
    lvl=1
    for (i in formula_prop_cuts[2:len_cuts]) {
      print(i)
      lvl=lvl+1
      d <- d %>% mutate(benefit_prop = ifelse(i>mean_wage_prop & prev_val<=mean_wage_prop,
                                             formula_bene_levels[lvl], benefit_prop))
      prev_val=i
    }
  }
  
  #------------------Adjust benefit levels: absolute value cuts----------------------
  if (!is.null(formula_value_cuts)) {
    # adjust benefit_prop accordingly
    # first interval of formula_bene_levels
    len_cuts=length(formula_value_cuts)
    len_lvls=length(formula_bene_levels)
    d <- d %>% mutate(benefit_prop = ifelse(formula_value_cuts[1]>wage12, 
                                            formula_bene_levels[1], benefit_prop))
    # last interval 
    d <- d %>% mutate(benefit_prop = ifelse(formula_value_cuts[len_cuts]<=wage12, 
                                            formula_bene_levels[len_lvls], benefit_prop))
    
    # rest of the intervals in between
    prev_val=formula_value_cuts[1]
    lvl=1
    for (i in formula_value_cuts[2:len_cuts]) {
      lvl=lvl+1
      d <- d %>% mutate(benefit_prop = ifelse(i>wage12 & prev_val<=wage12,
                                              formula_bene_levels[lvl], benefit_prop))
      prev_val=i
    }
  }
  return(d)
}

# ============================ #
# 5. EXTENDLEAVES
# ============================ #

# Replication of ACM Model Options to simulate extension of leaves in the presence of an FMLA program

EXTENDLEAVES <-function(d_train, d_test,wait_period, ext_base_effect, 
                        extend_prob, extend_days, extend_prop, fmla_protect) {
  
  # copy original leave lengths
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    orig_var=paste("orig_len_",i,sep="")
    d_test[orig_var] <- with(d_test, get(len_var))
  }
  
  # Base extension effect from ACM model (referred to as the "old" extension simulation there)
  # this is a candidate for modular imputation methods
  d_test["extend_flag"]=0
  
  if (ext_base_effect==TRUE) { 
    
    # specifications
    # using ACM specifications
    formula <- "longerLeave ~ age + agesq + female"
    
    # subsetting data
    filt <- "TRUE"

    # weights
    weight <- "~ weight"
    
    # Run Estimation
    # INPUT: FMLA (training) data set, ACS (test) data set, logit regression model specification, 
    # filter conditions, weight to use
    d_filt <- runLogitEstimate(d_train=d_train, d_test=d_test, formula=formula, test_filt=filt, 
                               train_filt=filt, weight=weight, varname='longer_leave', create_dummies=TRUE)
    
    # old merge code, caused memory issues. using match instead
    #d_test <- merge(d_filt, d_test, by='id', all.y=TRUE)
    for (i in names(d_filt)) {
      if (i %in% names(d_test)==FALSE){
        d_test[i] <- d_filt[match(d_test$id, d_filt$id), i]    
      }
    }
    
    # OUTPUT: ACS data with imputed column indicating those taking a longer leave.
    
    # Following ACM implementation:
    # i. For workers who have leave lengths in the absence of a program that are
    # less than the waiting period for the program: the leave is extended for 1 week into the program.
    

    for (i in leave_types) {
      len_var=paste("length_",i,sep="")
      take_var=paste("take_",i,sep="")
      d_test["extend_flag"] <- with(d_test, ifelse(get(len_var)<wait_period & particip==1 &
                                                     longer_leave == 1 & get(take_var)==1
                                                   ,1,extend_flag))
      d_test[len_var] <- with(d_test, ifelse(get(len_var)<wait_period & particip== 1 &
                                               longer_leave == 1 & get(take_var)==1
                                             ,get(len_var)+wait_period+5,get(len_var)))
      d_test["total_length"] <-  with(d_test, ifelse(get(len_var)<wait_period & particip== 1 &
                                                       longer_leave == 1 & get(take_var)==1
                                                     ,total_length+wait_period+5, total_length))
    }
    
    # ii. For workers who do not receive any employer pay or who exhaust their
    # employer pay and then go on the program: The probability of extending a leave using
    # program benefits is set to 25 percent; and for those who do extend their leave, the
    # extension is equal to 25 percent of their length in the absences of a program.
    d_test['rand']=runif(nrow(d_test))
    d_test <- d_test %>% mutate(longer_leave=ifelse(.25>rand,1,0))
    for (i in leave_types) {
      len_var=paste("length_",i,sep="")
      take_var=paste("take_",i,sep="")
      d_test["extend_flag"] <- with(d_test, ifelse((prop_pay_employer==0 | !is.na(exhausted_by)) & particip==1 &
                                                     longer_leave == 1 & get(take_var)==1 & extend_flag==0 & get(len_var)*1.25>wait_period
                                                   ,1,extend_flag))
      d_test[len_var] <- with(d_test, ifelse((prop_pay_employer==0 | !is.na(exhausted_by)) & particip==1 &
                                               longer_leave == 1 & get(take_var)==1 & extend_flag==0 & get(len_var)*1.25>wait_period
                                             ,get(len_var)*1.25,get(len_var)))
      d_test["total_length"] <-  with(d_test, ifelse((prop_pay_employer==0 | !is.na(exhausted_by)) & particip==1 &
                                                       longer_leave == 1 & get(take_var)==1 & extend_flag==0 & get(len_var)*1.25>wait_period
                                                     ,total_length+get(len_var)*.25, total_length))
    }
    
    # iii. For workers who exhaust program benefits and then receive employer pay:
    #   In this case the simulator assigns a 50 percent probability of taking an extended leave
    # until their employer pay is exhausted.
    
    # Not implemented, don't really get why this would be allowed or with what probability if it was
    
    # clean up vars
    d_test <- d_test[, !(names(d_test) %in% c("longerLeave_prob"))]
  } 
  
  # Additional option to extend leave a+bx additional days with c probability if the user wishes. 
  # a = extend_days
  # b = extend_prop
  # c = extend_prob
  # simplified from the ACM model; there they allowed it to be customized by leave type, just allowing for overall adjustments for now.
  
  
  if (extend_prob > 0) {
    d_test['rand']=runif(nrow(d_test))
    d_test["extend_flag"] <- with(d_test, ifelse(rand<extend_prob & particip==1 & resp_len==1 & total_length!=0,1,extend_flag))
    
    for (i in leave_types) {
      len_var=paste("length_",i,sep="")
      d_test[len_var] <- with(d_test, ifelse(rand<extend_prob & particip==1 & resp_len==1 & get(paste(len_var))!=0,
                                             round(get(paste(len_var))*extend_prop),get(paste(len_var))))
      d_test[len_var] <- with(d_test, ifelse(rand<extend_prob & particip==1& resp_len==1 & get(paste(len_var))!=0,
                                             round(get(paste(len_var))+(extend_days/total_leaves)),get(paste(len_var))))
    }
    
    # clean up vars
    d_test <- d_test[, !(names(d_test) %in% c("rand","extend_amt"))]
  }
  
  # FMLA Protection Constraint option
  # If enabled, leaves that are extended in the presence of a program that
  # originally were less than 12 weeks in length are constrained to be no longer than
  # 12 weeks in the presence of the program.
  
  d_test["fmla_constrain_flag"] <- 0
  if (fmla_protect==TRUE) {
    for (i in leave_types) {
      len_var=paste("length_",i,sep="")
      take_var=paste("take_",i,sep="")
      orig_var=paste("orig_len_",i,sep="")
      d_test["fmla_constrain_flag"] <- with(d_test, ifelse(extend_flag==1 & get(len_var)>60 & get(orig_var)<=60
                                                           ,1,fmla_constrain_flag))
      d_test[len_var] <- with(d_test, ifelse(extend_flag==1 & get(len_var)>60 & get(orig_var)<=60
                                             ,60,get(len_var)))
    }
  }
  
  # adjust total_length to match extensions of individual leaves
  d_test['total_length']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    d_test <- d_test %>% mutate(total_length=ifelse(get(paste(take_var)) == 1, total_length+get(paste('length_',i,sep="")), total_length))
  }
  
  return(d_test)
}

# ============================ #
# 5A. runLogitEstimate
# ============================ #
# see 3_impute_functions.R, function 1Ba


# ============================ #
# 6. UPTAKE
# ============================ #
# specifies uptake rate of those that are eligible for the paid leave program
# default is "full" - all who are eligible and would receive more money than employer would pay
# would pay choose to participate 


UPTAKE <- function(d, own_uptake, matdis_uptake, bond_uptake, illparent_uptake, 
                   illspouse_uptake, illchild_uptake, full_particip, wait_period, wait_period_recollect,
                   maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild,
                   maxlen_total, maxlen_DI, maxlen_PFL,dual_receiver,min_takeup_cpl,alpha) {
  
  # if wait_period_recollect is true, essentially wait_period is zero for this purpose
  if (wait_period_recollect) {
    wait_period=0
  }
  # calculate uptake -> days of leave that program benefits are collected
  d['particip_length']=0
  for (i in leave_types) {
    take_var=paste("take_",i,sep="")
    need_var=paste("need_",i,sep="")
    length_var = paste('length_',i,sep="")
    uptake_val=paste(i,"_uptake",sep="")
    uptake_var=paste0('takes_up_',i)
    plen_var= paste("plen_",i, sep="")
    
    # generate uptake column based on uptake val
    elig_d <- d %>% filter(eligworker==1)
    pop_target <- sum(elig_d %>% dplyr::select(PWGTP))*get(uptake_val)
    # filter to only those eligible for the program and taking or needing leave
    samp_frame <- d %>% filter(eligworker==1 & (get(take_var)==1|get(need_var)==1) & get(length_var)>wait_period+min_takeup_cpl)
    
    # if no one is taking leave, then return columns of zeros for created variables, otherwise continue with this process
    if (nrow(samp_frame)==0){
      ptake_var=paste("ptake_",i,sep="")
      d[ptake_var] <- 0
      plen_var=paste("plen_",i,sep="")
      d[plen_var] <- 0
      
    }
    else {
      # randomize order of sample rows - we'll be drawing in order from the top of the dataframe so we want order to be random.
      if (alpha==0) {
        
        rows <- sample(nrow(samp_frame))
        samp_frame <- samp_frame[rows,]
        
      } else if (alpha>0) {
        
        # if alpha is not 0, shuffle the rows randomly, but weighted by leave length ^ alpha
        samp_frame[plen_var] <- samp_frame[length_var] - wait_period
        samp_frame[plen_var] <- with(samp_frame, ifelse(get(plen_var)<0,0,get(plen_var)))
        samp_frame['org_wgt'] <- samp_frame[plen_var] ** alpha
        rows <- sample(nrow(samp_frame), prob = samp_frame$org_wgt)
        samp_frame <- samp_frame[rows,]
    
      }
      
      # create cumulative sum of weights
      samp_frame <- samp_frame %>% mutate(cumsum = cumsum(PWGTP))
      
      # select rows where cumsum is less than pop_target to take up
      samp_selected <- samp_frame[samp_frame$cumsum < pop_target,]
      samp_selected[uptake_var] <- 1
      
      # set uptake status for leave type by merging in uptake var from samp_selected
      d[uptake_var] <- samp_selected[match(d$id, samp_selected$id), uptake_var] 
      d[is.na(d[uptake_var]),uptake_var] <- 0
      
      # ensure any leave needers are now indicated as taking leave
      d[take_var] <- with(d, ifelse(get(uptake_var)==1, 1, get(take_var)))
    
        # update/create participation vars
      d <- d %>% mutate(particip_length=ifelse(wait_period<get(paste('length_',i,sep="")) &
                                                 get(uptake_var)==1 & particip==1 & get(paste(take_var)) == 1, 
                                               particip_length+get(paste('length_',i,sep=""))-wait_period, particip_length))
      d[plen_var] <- with(d, ifelse(wait_period<get(paste('length_',i,sep="")) &
                                      get(uptake_var)==1 & particip==1 & get(paste(take_var)) == 1, 
                                    get(paste('length_',i,sep=""))-wait_period, 0))
      d <- d %>% mutate(change_flag=ifelse(wait_period<get(paste('length_',i,sep="")) &
                                             get(uptake_var)==1 & particip==1 & get(paste(take_var)) == 1,1,0))
  
      
      # subtract days spent on employer benefits from those that exhausting employer benefits (received pay for some days of leave)
      # Also accounting for wait period here, as that can tick down as a person is still collecting employer benefits
      # only if not a dual receiver (can't receive both employer and state benefits)
      d <- d %>% mutate(particip_length= ifelse(change_flag==1 & !is.na(exhausted_by) & dual_receiver==0,
                                              ifelse(get(paste('length_',i,sep="")) > exhausted_by & exhausted_by>wait_period, 
                                                       particip_length - exhausted_by + wait_period, particip_length), particip_length))
      d[plen_var] <- with(d, ifelse(change_flag==1 & !is.na(exhausted_by)& dual_receiver==0,
                                    ifelse(get(paste('length_',i,sep="")) > exhausted_by & exhausted_by>wait_period, 
                                           get(plen_var) - exhausted_by + wait_period, get(plen_var)), get(plen_var)))
      
      ptake_var=paste("ptake_",i,sep="")
      d[ptake_var] <- with(d, ifelse(get(plen_var)>0 & get(take_var)>0,1,0))
    }
  }

  
  # make sure those with particip_length 0 are also particip 0
  d <- d %>% mutate(particip= ifelse(particip_length==0,0, particip))

  
  # cap particip_length at max program days
  # INPUT: ACS data set
  d <- check_caps(d,maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild,
                  maxlen_total, maxlen_DI, maxlen_PFL)
  # OUTPUT: ACS data set with participating leave length capped at user-specified program maximums
  
  # clean up vars
  d <- d[, !(names(d) %in% c('change_flag','reduce'))]
  return(d)
}
# ============================ #
# 6A. check_caps
# ============================ #
# cap particip_length at max program days
check_caps <- function(d,maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild,
                       maxlen_total, maxlen_DI, maxlen_PFL) {
  
  # for each individual leave type
  for (i in leave_types) {
    plen_var= paste("plen_",i, sep="")
    max_val=paste("maxlen_",i,sep="")
    d[plen_var] <- with(d, ifelse(get(plen_var)>get(max_val),get(max_val), get(plen_var)))
  }
  
  # apply cap for DI and PFL classes of leaves
  if (maxlen_DI!=maxlen_bond+maxlen_matdis) {
    d <- d %>% mutate(DI_plen=plen_matdis+plen_own)
    d['DI_plen'] <- with(d, ifelse(DI_plen>maxlen_DI,maxlen_DI,DI_plen))
    # evenly distributed cap among leave types
    d['reduce'] <- with(d, ifelse(plen_matdis+plen_own!=0, DI_plen/(plen_matdis+plen_own),0))
    d['plen_matdis']=round(d[,'plen_matdis']*d[,'reduce'])
    d['plen_own']=round(d[,'plen_own']*d[,'reduce'])
  }
  
  if (maxlen_PFL!=maxlen_illparent+maxlen_illspouse+maxlen_illchild+maxlen_bond) {  
    d <- d %>% mutate(PFL_plen=plen_bond+plen_illparent+plen_illchild+plen_illspouse)
    d['PFL_plen'] <- with(d, ifelse(PFL_plen>maxlen_PFL,maxlen_PFL,PFL_plen))
    # evenly distributed cap among leave types
    d['reduce'] <- with(d, ifelse(plen_bond+plen_illparent+plen_illchild+plen_illspouse!=0, 
                                  PFL_plen/(plen_bond+plen_illparent+plen_illchild+plen_illspouse),0))
    d['plen_bond']=round(d[,'plen_bond']*d[,'reduce'])
    d['plen_illchild']=round(d[,'plen_illchild']*d[,'reduce'])
    d['plen_illspouse']=round(d[,'plen_illspouse']*d[,'reduce'])
    d['plen_illparent']=round(d[,'plen_illparent']*d[,'reduce'])
  }
  
  # apply cap for all leaves
  if (maxlen_total!=maxlen_DI+maxlen_PFL | maxlen_total!=maxlen_illparent+maxlen_illspouse+maxlen_illchild+maxlen_bond+maxlen_bond+maxlen_matdis) {
    d['particip_length']=0
    for (i in leave_types) {
      plen_var=paste("plen_",i,sep="")
      d <- d %>% mutate(particip_length=particip_length+get(plen_var))
    }
    d['particip_length'] <- with(d, ifelse(particip_length>maxlen_total,maxlen_total,particip_length))
    d['reduce'] <- with(d, ifelse(plen_matdis+plen_own+plen_bond+plen_illparent+plen_illchild+plen_illspouse!=0, 
                                  particip_length/(plen_matdis+plen_own+plen_bond+plen_illparent+plen_illchild+plen_illspouse),0))
    
    # evenly distributed cap among leave types
    d['plen_matdis']=round(d[,'plen_matdis']*d[,'reduce'])
    d['plen_own']=round(d[,'plen_own']*d[,'reduce'])
    d['plen_bond']=round(d[,'plen_bond']*d[,'reduce'])
    d['plen_illchild']=round(d[,'plen_illchild']*d[,'reduce'])
    d['plen_illspouse']=round(d[,'plen_illspouse']*d[,'reduce'])
    d['plen_illparent']=round(d[,'plen_illparent']*d[,'reduce'])
    
    # recalculate DI/PFL/total lengths
    d <- d %>% mutate(DI_plen=plen_matdis+plen_own)
    d <- d %>% mutate(PFL_plen=plen_bond+plen_illparent+plen_illchild+plen_illspouse)
    d <- d %>% mutate(particip_length=DI_plen+ PFL_plen)
  }
  
  # also implement caps for max needed length 
  d['temp_length']=0
  for (i in leave_types) {
    len_var=paste("mnl_",i,sep="")
    d <- d %>% mutate(temp_length=temp_length+get(len_var))
  }
  d['temp_length'] <- with(d, ifelse(temp_length>260,260,temp_length))
  d['reduce'] <- with(d, ifelse(mnl_matdis+mnl_own+mnl_bond+mnl_illparent+mnl_illchild+mnl_illspouse!=0, 
                                temp_length/(mnl_matdis+mnl_own+mnl_bond+mnl_illparent+mnl_illchild+mnl_illspouse),0))
    
  # evenly distributed cap among leave types
  d['mnl_matdis']=round(d[,'mnl_matdis']*d[,'reduce'])
  d['mnl_own']=round(d[,'mnl_own']*d[,'reduce'])
  d['mnl_bond']=round(d[,'mnl_bond']*d[,'reduce'])
  d['mnl_illchild']=round(d[,'mnl_illchild']*d[,'reduce'])
  d['mnl_illspouse']=round(d[,'mnl_illspouse']*d[,'reduce'])
  d['mnl_illparent']=round(d[,'mnl_illparent']*d[,'reduce'])
  
  d <- d %>% mutate(mnl_all=mnl_bond+mnl_illchild+mnl_illparent+mnl_illspouse+mnl_matdis+mnl_own)
  # clean up vars
  d <- d[, !(names(d) %in% c('benefit_prop_temp','reduce','temp_length'))] 
  return(d)
}
# ============================ #
# 7. BENEFITS
# ============================ #
# Adding base values for new ACS variables involving imputed FMLA values

BENEFITS <- function(d) {
  
  # base benefits received from program
  d <- d %>% mutate(base_benefits=wage12/(round(weeks_worked*5))*particip_length*benefit_prop)
  d <- d %>% mutate(base_benefits=ifelse(is.na(base_benefits),0,base_benefits))

  # Note status quo leave pay
  d <- d %>% mutate(squo_leave_pay=wage12/(round(weeks_worked*5))*squo_total_length* prop_pay_employer)
  
  # base pay received from employer based on schedule
  # pay received is same across all pay schedules
  d <- d %>% mutate(base_leave_pay=wage12/(round(weeks_worked*5))*total_length* prop_pay_employer)
  d <- d %>% mutate(base_leave_pay=ifelse(is.na(base_leave_pay),0,base_leave_pay))
  
  # actual pay and benefits - to be modified by remaining parameter functions
  d <- d %>% mutate(actual_leave_pay=base_leave_pay)
  d <- d %>% mutate(actual_benefits=base_benefits)
  
  return(d)
}
# ============================ #
# 8. BENEFITEFFECT
# ============================ #
# Accounting for some "cost" of applying for the program when deciding between employer paid leave and program


BENEFITEFFECT <- function(d, bene_effect) {
  # if bene_effect not selected, generate a column of zero's for the flag and stop here
  
  if (bene_effect==FALSE){
    d$bene_effect_flg <- 0
    return(d)
  }
  
  # otherwise, continue with bene_effect simulation
  
  # Create uptake probabilities dataframe 
  # obtained from 2001 Westat survey which ACM used for this purpose
  # d_prob <- read.csv("bene_effect_prob.csv")
  
  # Hardcoding above CSV to remove dependency
  
  # Three columns of data set
  #Family income category
  finc_cat=rep(seq.int(10000,100000, by = 10000),4)
  
  #Benefit difference  
  bene_diff=c(rep(0,10),rep(25,10),rep(50,10),rep(125,10))
  
  #Probability of taking up benefits
  uptake_prob=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.12, 0.08, 0.05, 
                0.04, 0.02, 0.02, 0.01, 0.01, 0, 0, 0.59, 0.48, 
                0.38, 0.28, 0.21, 0.15, 0.1, 0.07, 0.05, 0.03, 
                1, 1, 1, 1, 1, 1, 0.99, 0.99, 0.98, 0.98)

  # create data frame
  d_prob=data.frame(finc_cat,bene_diff,uptake_prob)
  
  
  # define benefit difference to match 2001 Westat survey categories
  d <- d %>% mutate(bene_diff=(actual_benefits-actual_leave_pay)/particip_length*5)
  d <- d %>% mutate(bene_diff=ifelse(bene_diff<=25, 0, bene_diff))
  d <- d %>% mutate(bene_diff=ifelse(bene_diff<=50 & bene_diff>25, 25, bene_diff))
  d <- d %>% mutate(bene_diff=ifelse(bene_diff<=125 & bene_diff>50, 50, bene_diff))
  d <- d %>% mutate(bene_diff=ifelse(bene_diff>125, 125, bene_diff))
  d <- d %>% mutate(bene_diff=ifelse(is.na(bene_diff), 0, bene_diff))
  d['bene_diff']=as.integer(d[,'bene_diff'])
  
  d <- d %>% mutate(faminc=ifelse(is.na(faminc),wage12,faminc))
  
  # define family income to match 2001 Westat survey categories
  d <- d %>% mutate(finc_cat=ifelse(faminc<=10000,10000,NA))
  inc_cut <- seq(10000, 90000, by=10000)
  for (i in inc_cut) {
    d <- d %>% mutate(finc_cat=ifelse(faminc>i & faminc<=i+10000,i,finc_cat))
  }
  d <- d %>% mutate(finc_cat=ifelse(faminc>100000,100000,finc_cat))
  
  d['finc_cat']=as.numeric(d[,'finc_cat'])
  
  # recalculate uptake based on bene_diff
  d <- plyr :: join(d,d_prob, type="left",match="all",by=c("bene_diff", "finc_cat"))
  d['rand']=runif(nrow(d))
  
  # exclude those participants that will not be affected by benefit effect
  # those who exhaust employer benefits before leave ends will always participate
  d["universe"] <- ifelse(d[,"eligworker"]==1 & !is.na(d[,'exhausted_by']),0,1)  
  
  # those who choose to extend leaves in the presence of the program will always participate
  d["universe"] <- ifelse(d[,"eligworker"]==1 & d[,'extend_flag']==1,0,d[,'universe']) 
  
  # flag those who are not claiming benefits due to benefit effect
  d <- d %>% mutate(bene_effect_flg=ifelse(rand>uptake_prob & particip==1 & universe==1,1,0))
  
  # update leave vars
  d <- d %>% mutate(actual_benefits=ifelse(rand>uptake_prob & particip==1 & universe==1,0,actual_benefits))
  d <- d %>% mutate(particip_length=ifelse(rand>uptake_prob & particip==1 & universe==1,0,particip_length))
  for (i in leave_types) {
    plen_var= paste("plen_",i, sep="")
    d[plen_var] <- with(d, ifelse(rand>uptake_prob & particip==1 & universe==1,0,get(plen_var)))
  }
  d['DI_plen'] <- with(d, ifelse(rand>uptake_prob & particip==1 & universe==1,0,DI_plen))
  d['PFL_plen'] <- with(d, ifelse(rand>uptake_prob & particip==1 & universe==1,0,PFL_plen))
  d <- d %>% mutate(particip=ifelse(rand>uptake_prob & particip==1 & universe==1,0,particip))
  
  d <- d[, !(names(d) %in% c('rand','bene_diff','finc_cat','uptake_prob','universe'))]
  
  return(d)
}

# ============================ #
# 9. TOPOFF
# ============================ #
# employers who would pay their employees
# 100 percent of wages while on leave would instead require their employees to participate
# in the program and would "top-off" the program benefits by paying the difference
# between program benefits and full pay. 
# User can specify percent of employers that engage in this, and minimum length of leave this is required for

TOPOFF <- function(d, topoff_rate, topoff_minlength) {
  # if topoff rate is 0, just generate a topoff flag =0 and stop there
  
  if (topoff_rate==0){
    d['topoff_flg'] <- 0
    return(d)
  }
  
  # else continue with simulation
  
  len_vars <- c("length_own", "length_illspouse", "length_illchild","length_illparent","length_matdis","length_bond")
  d['topoff_rate'] <- topoff_rate
  d['topoff_min'] <- topoff_minlength
  d['rand'] <- runif(nrow(d))
  d <- d %>% mutate(topoff= ifelse(rand<topoff_rate & prop_pay_employer==1,1,0))
  d <- d %>% mutate(topoff_count=0)
  for (i in leave_types) {
    len_var=paste("length_",i,sep="")
    plen_var=paste("plen_",i,sep="")
    take_var=paste("take_",i,sep="")
    d['topoff_temp'] <- with(d,ifelse(topoff==1 & topoff_min<=get(paste(len_var)) & get(paste(take_var))==1,1,0))
    d[plen_var] <- with(d,ifelse(topoff_temp==1,get(len_var),get(plen_var)))
    d <- d %>% mutate(topoff_count= ifelse(topoff_temp==1 ,topoff_count+1,topoff_count))
  }
  d['particip_length']=0
  for (i in leave_types) {
    plen_var=paste("plen_",i,sep="")
    d <- d %>% mutate(particip_length=particip_length+get(plen_var))
  }
  
  # recalculate benefits based on updated participation length
  # actual benefits received from program
  # note: topoff will override benefiteffect changes
  d <- d %>% mutate(actual_benefits=wage12/(round(weeks_worked*5))*particip_length*benefit_prop)
  d <- d %>% mutate(actual_benefits=ifelse(is.na(actual_benefits),0,actual_benefits))

  #subtract benefits from pay
  d <- d %>% mutate(actual_leave_pay=ifelse(topoff_count>0,base_leave_pay-actual_benefits,actual_leave_pay))
  
  d <- d %>% mutate(topoff_flg= ifelse(topoff_count>0,1,0))
  
  # adjust participation flag. leave taken assumed to not be affected by top off behavior
  d <- d %>% mutate(particip=ifelse(topoff_count>0,1,particip))
  
  # clean up vars
  d <- d[, !(names(d) %in% c('rand','topoff_rate','topoff_temp','topoff_min','topoff', 'topoff_count'))]
  
  return(d)
}

# ============================ #
# 10. DEPENDENTALLOWANCE
# ============================ #
# include a weekly dependent allowance for families with children
# dependent_allow expected to be a number (all families with children get benefit), or vector of numbers (each element is the marginal increase of an
# additional child up n children, where n = length of vector)

DEPENDENTALLOWANCE <- function(d,dependent_allow) {
  d$dep_bene_allow <- 0
  kid_count <- 1
  for (x in dependent_allow) {
    d <- d %>% mutate(dep_bene_allow=ifelse(ndep_kid>=kid_count,dep_bene_allow+x,dep_bene_allow))
    kid_count <- kid_count + 1
  }
  d <- d %>% mutate(benefit_prop=benefit_prop + dep_bene_allow)
  # cap at full salary
  d <- d %>% mutate(benefit_prop=pmin(1,benefit_prop))

  # recalculate benefits based on updated benefit_prop
  # actual benefits received from program
  d <- d %>% mutate(actual_benefits=wage12/(round(weeks_worked*5))*particip_length*benefit_prop)
  d <- d %>% mutate(actual_benefits=ifelse(is.na(actual_benefits),0,actual_benefits))
  
  # create overall effective rrp
  d <- d %>% mutate(effective_rrp= benefit_prop)
  return(d)
}

# ============================ #
# 11. DIFF_ELIG
# ============================ #
# Some state programs have differential eligibility by leave type.
# For example, NJ's private plan option means about 30% of the PFL eligble population is not
# eligible for DI.
# However, eligibility is currently programmed as universally binary.
# As a workaround for now, this function allows users to simulate this differential eligibility
# by removing some specified proportion of participation for specific leave types
# at random from the population 

DIFF_ELIG <- function(d, own_elig_adj, illspouse_elig_adj, illchild_elig_adj,
                      illparent_elig_adj, matdis_elig_adj, bond_elig_adj) {
  adjs_vals <- paste0(leave_types, '_elig_adj')
  plen_vars <- paste0('plen_',leave_types)
  zip <- mapply(list, adjs_vals, plen_vars, SIMPLIFY=F)
  # for each pair of leave type/adj val...
  for (i in zip) {
    adjs_val=i[[1]]
    plen_var=i[[2]]

    # select proportion of participants equal of adj value. rest will no longer collect benefits 
    # for that type (simulating they are ineligible)
    nsamp <- ceiling(get(adjs_val)*nrow(filter(d, get(plen_var)>0)))
    psamp <- sample_n(filter(d, get(plen_var)>0), nsamp)
    d[d[,'id'] %in% psamp[,'id'],'pflag'] <-1
    d['pflag'] <- d['pflag'] %>% replace(., is.na(.), 0)
    d[plen_var] <- with(d, ifelse(get(plen_var)>0 & pflag==0, 0, get(plen_var)))
    d <- d[, !(names(d) %in% c('pflag'))]
    # rest of the appropriate vars to adjust are handled in the CLEANUP function next
  }
  return(d)
}


# ============================ #
# 12. CLEANUP
# ============================ #
# Final variable alterations and consistency checks

CLEANUP <- function(d, week_bene_cap,week_bene_cap_prop,week_bene_min, maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild,
                    maxlen_total,maxlen_DI,maxlen_PFL) {
  # Check leave length participation caps again 
  # INPUT: ACS data set 
  d <- check_caps(d,maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild,
                  maxlen_total, maxlen_DI, maxlen_PFL)
  
  # OUTPUT: ACS data set with participating leave length capped at user-specified program maximums
  
  # cap benefit payments at program's weekly benefit cap
  d <- d %>% mutate(actual_benefits= ifelse(actual_benefits>ceiling(week_bene_cap*particip_length)/5,
                                            ceiling(week_bene_cap*particip_length)/5, actual_benefits))
  
  # cap benefits payments as a function of mean weekly wage in the population
  if (!is.null(week_bene_cap_prop)) {
    cap <- mean(d$wage12/d$weeks_worked)*week_bene_cap_prop
    d <- d %>% mutate(actual_benefits= ifelse(actual_benefits>ceiling(cap*particip_length)/5,
                                              ceiling(cap*particip_length)/5, actual_benefits))
  }
  
  # establish minimum weekly benefits for program participants
  d <- d %>% mutate(actual_benefits= ifelse(actual_benefits<ceiling(week_bene_min*particip_length)/5,
                                            ceiling(week_bene_min*particip_length)/5, actual_benefits))
  
  # make sure those with particip_length 0 are also particip 0
  d <- d %>% mutate(particip= ifelse(particip_length==0,0, particip))
  
  # calculate leave specific benefits
  d['ptake_PFL'] <-0
  d['ptake_DI'] <-0
  d['bene_DI'] <- 0
  d['bene_PFL'] <- 0
  
  for (i in leave_types) {
    plen_var=paste("plen_",i,sep="")
    ben_var=paste("bene_",i,sep="")  
    d[ben_var] <- with(d, actual_benefits*(get(plen_var)/particip_length))
    d[ben_var] <- with(d, ifelse(is.na(get(ben_var)),0,get(ben_var)))
    
    # benefits for PFL, DI leave types
    if (i=='own'|i=='matdis') {
      d['bene_DI'] <- with(d, bene_DI+ get(ben_var))
    }
    
    if (i=='bond'|i=='illspouse'|i=='illparent'|i=='illchild') {
      d['bene_PFL'] <- with(d, bene_PFL + get(ben_var))
    }
    
    # create ptake_* vars 
    # dummies for those that took a given type of leave, and collected non-zero benefits for it
    take_var=paste("take_",i,sep="")
    ptake_var=paste("ptake_",i,sep="")
    d[ptake_var] <- with(d, ifelse(get(plen_var)>0 & get(take_var)>0,1,0))
    
    # dummies for PFL, DI leave types
    if (i=='own'|i=='matdis') {
      d['ptake_DI'] <- with(d, ifelse(get(plen_var)>0 & get(take_var)>0,1,ptake_DI))  
    }
    
    if (i=='bond'|i=='illspouse'|i=='illparent'|i=='illchild') {
      d['ptake_PFL'] <- with(d, ifelse(get(plen_var)>0 & get(take_var)>0,1,ptake_PFL))  
    }
    # create taker and needer vars
    d['taker'] <- 0
    d['needer'] <- 0
    take_var <- paste0('take_',i)
    need_var <- paste0('need_',i)
    d <- d %>% mutate(taker=ifelse(get(take_var)==1,1,taker))
    d <- d %>% mutate(needer=ifelse(get(need_var)==1,1,needer))
    d$needer[is.na(d$needer)] <- 0
    
    # calculate squo and counterfactual employer pay
    len_var=paste("length_",i,sep="")
    squo_var=paste("squo_length_",i,sep="")
    d[paste0('emppay_',i)] <- with(d, actual_leave_pay*(get(len_var)/total_length))
    d[paste0('squo_emppay_',i)] <- with(d, squo_leave_pay*(get(squo_var)/squo_total_length))
  }
  return(d)
}

# ============================ #
# 12A. check_caps
# ============================ #
# see function 6A.
