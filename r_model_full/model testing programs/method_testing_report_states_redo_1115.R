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
  #methods <- c('Naive_Bayes','random','KNN1','logit','random_forest','ridge_class','xg_boost') # 'KNN_multi's
  methods <- c('logit')
  for (meth in methods) {
    print(meth)
  #=================================
  #Rhode Island
  #=================================
  
    timestart <<- Sys.time()
    d_ri <- policy_simulation(saveCSV=TRUE,
                              state='RI',
                              makelog=TRUE,
                              base_bene_level=.6,
                              impute_method=meth,
                              place_of_work = TRUE,
                              dual_receiver = 1,
                              ext_resp_len = TRUE, 
                              ext_base_effect=TRUE,
                              rr_sensitive_leave_len=FALSE,
                              bene_effect=FALSE, wait_period=5, full_particip=FALSE, clone_factor=1, week_bene_cap=795, week_bene_min=89,
                              dependent_allow = 10,
                              own_uptake=.0704, matdis_uptake=.0235, bond_uptake=.0092, illparent_uptake=.0008,
                              illspouse_uptake=.0014, illchild_uptake=.0005,
                              maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                              maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=3840,output=paste0("RI_",meth,"_method_resid_1115"),
                              output_stats=c('state_compar'),  random_seed=12312)
  
  timeend <<- Sys.time()
  print(timeend - timestart)
  }
  
  d_len <- c()
  # output leave lengths
  for (i in leave_types) {
    len_var <-paste0('plen_',i)
    d_filt <- d_ri[d_ri[,len_var]>0,]
    #d_filt <- d_ri
     d_len[i] <- weighted.mean(d_filt %>% select(len_var),w=d_filt %>% select(PWGTP))
  }
  
  
  
  #=================================♠
  #California
  #=================================
  
  #for (meth in methods) {  
    # clear up memory
  #   rm(list=ls()[ls()!='methods' & ls()!='policy_simulation' & ls()!='meth'])
  #   start_time <- Sys.time()
  #   policy_simulation(state='CA',
  #                     makelog=TRUE,
  #                     xvars=c("widowed", "divorced", "separated", "nevermarried", "female", 
  #                             'age_cat', "ltHS", "someCol", "BA", "GradSch", "black", 
  #                             "other", "asian",'native', "hisp","nochildren",'faminc_cat','coveligd'),
  #                     base_bene_level=.55,
  #                     impute_method = meth,
  #                     sample_prop = .1,
  #                     place_of_work = TRUE,
  #                     exclusive_particip = FALSE
                        # ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                        # bene_effect=TRUE, full_particip_needer=TRUE, wait_period=5, clone_factor=1, week_bene_cap=1216,
                        # week_bene_min=50, dependent_allow = 10,
                        # own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                        # illspouse_uptake=.25, illchild_uptake=.25,
                        # maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent =30, 
                        # maxlen_PFL= 30, maxlen_DI=260, maxlen_total=260,
                        # maxlen_illspouse =30, maxlen_illchild =30,earnings=300, output=paste0("CA_",meth,"_method_POW_alt"),
                        # output_stats=c('state_compar'),  random_seed=123)
  #   
  #   end_time <- Sys.time()
  #   print(end_time - start_time)
  # }
  #=================================
  # New Jersey
  #=================================
  # New Jersey has reduced TDI take up due to many employers offering qualifying private plans 
  # with a robust private insurance market for TDI (though not for PFL). Adjusting down TDI uptake by 30% as a result
  #http://lims.dccouncil.us/Download/34613/B21-0415-Economic-and-Policy-Impact-Statement-UPLAA3.pdf
  # 
  
#   
# for (meth in methods) {  
#   start_time <- Sys.time()
#   d_nj <- policy_simulation(state='NJ',
#                             makelog=TRUE,
#                             xvars=c("widowed", "divorced", "separated", "nevermarried", "female", 
#                                     'age_cat', "ltHS", "someCol", "BA", "GradSch", "black", 
#                                     "other", "asian",'native', "hisp","nochildren",'faminc_cat','coveligd'),
#                             base_bene_level=.66,
#                             impute_method = meth,
#                             place_of_work = FALSE,
#                             exclusive_particip = TRUE,
#                             ext_resp_len = TRUE,
#                             ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=0, topoff_minlength=10,
#                             bene_effect=TRUE,  wait_period=5, clone_factor=1, week_bene_cap=594,
#                             own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
#                             illspouse_uptake=.25, illchild_uptake=.25,
#                             own_elig_adj=.7, matdis_elig_adj=.7,
#                             maxlen_own =130, maxlen_matdis =130, maxlen_bond =30, maxlen_illparent =30, 
#                             maxlen_PFL= 30, maxlen_DI=130, maxlen_total=130,
#                             maxlen_illspouse =30, maxlen_illchild =30,earnings=8400,output=paste0("NJ_",meth,"_method_resid_redo"), output_stats=c('state_compar'), random_seed=123)
#   
#   
#   end_time <- Sys.time()
#   print(end_time - start_time)
# }