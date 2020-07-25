# 9/30/19 update to model simulation tests; rerunning all tests with exclusive participation disabled, 
# and from state of work rather than state of residence. Also added xg_boost imputation method.

# test executions on full state data
# for report, testing each method

#rm(list=ls())
cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)
#options(error=NULL)

# sample master execution function for testing code
source("0_master_execution_function.R")
#methods <- c('random','logit','random_forest','ridge_class','xg_boost','KNN1') # 'KNN_multi's 'Naive_Bayes'
methods <- c('logit')
for (meth in methods) {
  print(meth)
  #=================================
  #Rhode Island
  #=================================
  
  timestart <<- Sys.time()
  ri <- policy_simulation(saveCSV=TRUE,
                          state='RI',
                          makelog=TRUE,
                          base_bene_level=.6,
                          impute_method=meth,
                          place_of_work = TRUE,
                          dual_receiver = 1,
                          ext_resp_len = TRUE, 
                          fmla_protect=FALSE,
                          ext_base_effect=FALSE,
                          rr_sensitive_leave_len=TRUE,
                          full_particip = FALSE,
                          bene_effect=FALSE, wait_period=5, clone_factor=1, week_bene_cap=759, week_bene_min=89,
                          dependent_allow = 10,
                          alpha=1,
                          xvars = c('widowed', 'divorced', 'separated', 'nevermarried',
                                    'female', 'age','agesq',
                                    'ltHS', 'someCol', 'BA', 'GradSch',
                                    'black', 'other', 'asian','native','hisp','nochildren','faminc'),
                          sens_var='resp_len',
                          own_uptake= .0723, matdis_uptake=.0241, bond_uptake=.0104, illchild_uptake=.0006,
                          illspouse_uptake=.0015,  illparent_uptake=.0009,
                          maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                          maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=3840,output=paste0("RI_",meth,"full_test"),
                          output_stats=c('state_compar'),  random_seed=12312)
  timeend <<- Sys.time()
  print(timeend - timestart)
}  

for (i in leave_types) {
  plen_var <- paste0("plen_",i)
  length_var <- paste0("length_",i)
  print('-------------------------------------------------')
  print(paste0('leave type ',i))
  print('plen (cpl) mean')
  print(weighted.mean(ri[ri[,plen_var]>0,plen_var]), w=ri[ri[,plen_var]>0,'PWGTP'],na.rm=TRUE)
  print('length (cfl) mean')
  print(weighted.mean(ri[ri[,length_var]>0,length_var]), w=ri[ri[,length_var]>0,'PWGTP'],na.rm=TRUE)
  print('Is plen (cpl) <= length (cfl) var?')
  print(table(ri[,plen_var]<=ri[,length_var]))
}
print('-------------------------------------------------')
