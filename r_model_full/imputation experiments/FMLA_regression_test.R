# testing FMLA regressions to ensure R and Python produce same results.
library(plyr)
library(dplyr)
library(survey)
library("rjson")
options(error=recover)

source("0_master_execution_function.R")
source("3_impute_functions.R")
source("4_post_impute_functions.R")
makelog <<- FALSE

#d_fmla <- readRDS('R_dataframes/d_fmla.rds')
d_fmla <- read.csv('csv_inputs/fmla_clean_2012_py.csv')

xvars = c('widowed', 'divorced', 'separated', 'nevermarried',
      'female', 'age','agesq',
      'ltHS', 'someCol', 'BA', 'GradSch',
      'black', 'other', 'asian','native','hisp','nochildren','faminc')

w = 'weight'

# transform FMLA xvars 
adj_xvars <- c()
for (i in xvars) {
   # fill in missing values with means 
   if (is.numeric(d_fmla[,i]) & any(unique(d_fmla[is.na(d_fmla[,i])==FALSE,i])!=c(0,1))) {
      d_fmla[is.na(d_fmla[,i]), i] <- mean(d_fmla[,i], na.rm = TRUE)  
   }
   # if it is a dummy var, then take a random draw with probability = to the non-missing mean
   if (is.numeric(d_fmla[,i]) & all(unique(d_fmla[is.na(d_fmla[,i])==FALSE,i])==c(0,1))) {
      prob <- mean(d_fmla[,i], na.rm = TRUE)
      d_fmla['rand']=runif(nrow(d_fmla))
      d_fmla[i] <- with(d_fmla, ifelse(is.na(get(i)),ifelse(rand>prob,0,1),get(i)))    
      d_fmla['rand'] <- NULL
   }
}

d_filt <-d_fmla[complete.cases(d_fmla[,xvars]),]

# standardize vars
adj_xvars <- c()
for (i in xvars) {
   d_filt[paste0(i,'_std')] <- (d_filt[i] - mean(d_filt[, i]))/sd(d_filt[, i])
   adj_xvars <- c(adj_xvars, paste0(i,'_std'))
}

yvars <- c(take_own = "take_own", 
           take_matdis = "take_matdis",
           take_bond = "take_bond",
           take_illchild = "take_illchild",
           take_illparent = "take_illparent",
           take_illspouse = "take_illspouse",
           need_own = "need_own", 
           need_matdis = "need_matdis",
           need_bond = "need_bond",
           need_illchild = "need_illchild",
           need_illparent = "need_illparent",
           need_illspouse = "need_illspouse",
           resp_len= "resp_len")

# filters: logical conditionals always applied to filter vraiable imputation 
filts <- c(take_own = "TRUE",
           take_matdis = "female == 1 & nochildren == 0 & age < 50",
           take_bond = "nochildren == 0 & age < 50",
           take_illchild = "TRUE",
           take_illparent = "TRUE",
           take_illspouse = "nevermarried == 0 & divorced == 0",
           need_own = "TRUE",
           need_matdis = "female == 1 & nochildren == 0 & age < 50",
           need_bond = "nochildren == 0 & age < 50",
           need_illchild = "TRUE",
           need_illparent = "TRUE",
           need_illspouse = "nevermarried == 0 & divorced == 0",
           resp_len="TRUE")

# weight: if method uses FMLA weights, the weight variable to use
weights <- c(take_own = "~ weight",
             take_matdis = "~ weight",
             take_bond = "~ weight",
             take_illchild = "~ weight",
             take_illparent = "~ weight",
             take_illspouse = "~ weight",
             need_own = "~ weight",
             need_matdis = "~ weight",
             need_bond = "~ weight",
             need_illchild = "~ weight",
             need_illparent = "~ weight",
             need_illspouse = "~ weight",
             resp_len = "~ weight")

# set up regressions for each variable
# generate formulas for logistic regression
# need formula strings to look something like "take_own ~ age + agesq + male + ..." 
formulas=c()
for (i in yvars) { 
   # remove female as an xvar if a matdis var first 
   if (i=='take_matdis' | i=='need_matdis'){
      new_formula=paste(i, "~",  paste(adj_xvars[1],'+', paste(adj_xvars[2:length(adj_xvars)] , collapse=" + ")))
      new_formula=gsub(' female_std +','',new_formula, fixed=TRUE)
   } else {
      new_formula=paste(i, "~",  paste(adj_xvars[1],'+', paste(adj_xvars[2:length(adj_xvars)] , collapse=" + ")))
   }
   formulas= c(formulas, new_formula)
}

# define logit estimation function
logit_est <- function(data, formula, filt, weight, varname) {
   des <- svydesign(id = ~1,  weights = as.formula(weight), data = data %>% filter_(filt))
   complete <- svyglm(as.formula(formula), data = data %>% filter_(filt),
                      family = "quasibinomial", design = des)
   return(complete$coefficients)
}

# apply logit estimation for all yvars
results <-  mapply(logit_est, varname=yvars, formula = formulas, filt=filts, weight = weights, 
                MoreArgs=list(data=d_filt), SIMPLIFY = FALSE)

# store all coeffs in a single dataframe
d_coeffs <- data.frame()
for (i in results) {
   d_coeffs <- rbind(d_coeffs,i)
   names(d_coeffs) <- names(i)
}
d_coeffs['yvar'] <- yvars
write.csv(d_coeffs, file='output/fmla_test_logit_coeffs.csv')

# load ACS data set
d_acs <- readRDS('R_dataframes/work_states/RI_work.rds')

# transform ACS xvars 
adj_xvars <- c()
for (i in xvars) {
   # fill in missing values with means 
   if (is.numeric(d_acs[,i]) & any(unique(d_acs[,i])!=c(0,1))) {
      d_acs[is.na(d_acs[,i]), i] <- mean(d_acs[,i], na.rm = TRUE)  
   }
   # if it is a dummy var, then take a random draw with probability = to the non-missing mean
   if (is.numeric(d_acs[,i]) & all(unique(d_acs[,i])==c(0,1))) {
      prob <- mean(d_acs[,i], na.rm = TRUE)
      d_acs['rand']=runif(nrow(d_acs))
      d_acs[i] <- with(d_acs, ifelse(is.na(get(i)),ifelse(rand>get(prob),0,1),get(i)))    
      d_acs['rand'] <- NULL
   }
   
   # standardize vars
   d_acs[paste0(i,'_std')] <- (d_acs[i] - mean(d_acs[, i]))/sd(d_acs[, i])
   adj_xvars <- c(adj_xvars, paste0(i,'_std'))
}


# apply estimates
for (yvar in yvars){
   estimate <- results[[yvar]]
   var_prob= paste0(yvar,"_prob")
   d_acs_filt <- d_acs %>% filter_(filts[yvar])
   d_acs_filt[var_prob]=estimate['(Intercept)']
   for (dem in names(estimate)) {
      if (dem !='(Intercept)' & !is.na(estimate[dem])) { 
         d_acs_filt[is.na(d_acs_filt[,dem]),dem]=0
         d_acs_filt[var_prob]= d_acs_filt[,var_prob] + d_acs_filt[,dem]*estimate[dem]
      }
   }
   
   d_acs_filt[var_prob] <- with(d_acs_filt, exp(get(var_prob))/(1+exp(get(var_prob))))
   d_acs_filt <- d_acs_filt[,c(var_prob, 'id')]
   
   # create imputed dummy variables
   d_acs_filt[is.na(d_acs_filt[var_prob]), var_prob] <- 0
   d_acs_filt['rand']=runif(nrow(d_acs_filt))
   d_acs_filt[yvar] <- with(d_acs_filt, ifelse(rand>get(var_prob),0,1))    
   d_acs_filt <- d_acs_filt[,c(yvar, 'id')]
   
   # add imputed variable to acs 
   d_acs[yvar] <- d_acs_filt[match(d_acs$id, d_acs_filt$id), yvar]    
}

# Apply logic control 
d_acs[d_acs['male'] == 1, 'take_matdis'] = 0
d_acs[d_acs['male'] == 1, 'need_matdis'] = 0
d_acs[(d_acs['nevermarried'] == 1) | (d_acs['divorced'] == 1), 'take_illspouse'] = 0
d_acs[(d_acs['nevermarried'] == 1) | (d_acs['divorced'] == 1), 'need_illspouse'] = 0
d_acs[d_acs['nochildren'] == 1, 'take_bond'] = 0
d_acs[d_acs['nochildren'] == 1, 'need_bond'] = 0
d_acs[d_acs['nochildren'] == 1, 'take_matdis'] = 0
d_acs[d_acs['nochildren'] == 1, 'need_matdis'] = 0
d_acs[d_acs['age'] > 50, 'take_matdis'] = 0
d_acs[d_acs['age'] > 50, 'need_matdis'] = 0
d_acs[d_acs['age'] > 50, 'take_bond'] = 0
d_acs[d_acs['age'] > 50, 'need_bond'] = 0


# just create some dummy values so this runs 
d_acs$prop_pay= 0
d_acs$dual_receiver= 1
d_acs$exhausted_by= 0
d_acs$wage_rr=.6
leave_types <<- c("own","matdis","bond","illchild","illspouse","illparent")

# extend leaves
d_acs <- impute_leave_length(d_fmla, d_acs, ext_resp_len=TRUE, len_method='rand', rr_sensitive_leave_len=TRUE,wage_rr=.6,maxlen_DI=150,maxlen_PFL=20)


# add in eligibility flags
d_acs <- ELIGIBILITYRULES(d_acs, earnings=3840, weeks=NULL, ann_hours=NULL, minsize=NULL, 
   base_bene_level=.6, week_bene_min=89, formula_prop_cuts=NULL, formula_value_cuts=NULL,
   formula_bene_levels=NULL, elig_rule_logic= '(earnings & weeks & ann_hours & minsize)', 
   FEDGOV=FALSE, STATEGOV=FALSE, LOCALGOV=FALSE, SELFEMP=FALSE,PRIVATE=TRUE, dual_receiver=1)

# add in participation values 
d_acs <-UPTAKE(d_acs, own_uptake= .0822, matdis_uptake=.0274, bond_uptake=.0104, illparent_uptake=.0009,
                  illspouse_uptake=.0015, illchild_uptake=.0006, full_particip=FALSE, wait_period=5,
               maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
               maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150,dual_receiver=1)

# check caps 
d_acs <- check_caps(d_acs, maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                    maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150)

# tabulate values of yvars in imputed data set 
d_agg <- data.frame(row.names=yvars)
d_agg['count'] <- NA
for (yvar in c(yvars,'eligworker')) {
   d_agg[yvar,'count'] <- sum(d_acs[yvar], na.rm=TRUE)
}
write.csv(d_agg, file='output/test_ACS_yvar_counts.csv')

# reorder some vars for ease of reading
for (i in seq(1,80)) {
   d_acs[paste0('PWGTP',i)] <- NULL
}
for (stub in c('take_','need_','squo_length_','mnl_','length_','plen_','takes_up_')) {
   for (type in leave_types) {
      var <- paste0(stub,type)
      d_acs <- d_acs %>% select(-var,var)      
   }
}


# save csv of final data set 
write.csv(d_acs, file='output/test_ACS_RI.csv')


