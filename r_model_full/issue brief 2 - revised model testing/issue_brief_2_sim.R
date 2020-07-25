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
  methods <- c('random','logit','random_forest','ridge_class','xg_boost','KNN1') # 'KNN_multi's 'Naive_Bayes'
  #methods <- c('logit')
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
                            rr_sensitive_leave_len=FALSE,
                            topoff_rate=.06, topoff_minlength=20, 
                            bene_effect=FALSE, wait_period=5, clone_factor=1, week_bene_cap=795, week_bene_min=89,
                            dependent_allow = 10,
                            sens_var='resp_len',
                            own_uptake= .08, matdis_uptake=.03, bond_uptake=.007, illparent_uptake=.001,
                            illspouse_uptake=.002, illchild_uptake=.001,
                            maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                            maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=3840,output=paste0("RI_",meth,"issue_brief_1"),
                            output_stats=c('state_compar'),  random_seed=12312)
    timeend <<- Sys.time()
    print(timeend - timestart)
  }  
  
  
  #=================================
  #California
  #=================================
  
  for (meth in methods) {
    timestart <<- Sys.time()
    policy_simulation(saveCSV=TRUE,
                      state='CA',
                      FEDGOV=TRUE,
                      STATEGOV=TRUE,
                      LOCALGOV=TRUE,
                      makelog=TRUE,
                      base_bene_level=.55,
                      impute_method=meth,
                      place_of_work = TRUE,
                      dual_receiver = 1,
                      ext_resp_len = TRUE,
                      fmla_protect=FALSE,
                      ext_base_effect=TRUE,
                      rr_sensitive_leave_len=FALSE,
                      bene_effect=FALSE, wait_period=5, clone_factor=1, week_bene_cap=1216, week_bene_min=50,
                      dependent_allow = 10,
                      sens_var='resp_len',
                      topoff_rate=.06, topoff_minlength=20, 
                      own_uptake=.04, matdis_uptake=.01, bond_uptake=.02, illparent_uptake=.01,
                      illspouse_uptake=.01, illchild_uptake=.01,
                      maxlen_PFL= 30, maxlen_DI=260, maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent=30,
                      maxlen_illspouse =30, maxlen_illchild =30, maxlen_total=260, earnings=300,output=paste0("CA_",meth,"issue_brief_1"),
                      output_stats=c('state_compar'),  random_seed=12312)
    timeend <<- Sys.time()
    print(timeend - timestart)
  }
  #=================================
  # New Jersey
  #=================================
  # New Jersey has reduced TDI take up due to many employers offering qualifying private plans 
  # with a robust private insurance market for TDI (though not for PFL). Adjusting down TDI uptake by 30% as a result
  #http://lims.dccouncil.us/Download/34613/B21-0415-Economic-and-Policy-Impact-Statement-UPLAA3.pdf
  # 
  

for (meth in methods) {
  timestart <<- Sys.time()
  policy_simulation(saveCSV=TRUE,
                    state='NJ',
                    makelog=TRUE,
                    base_bene_level=.66,
                    impute_method=meth,
                    place_of_work = TRUE,
                    dual_receiver = 1,
                    ext_resp_len = TRUE,
                    ext_base_effect=TRUE,
                    fmla_protect=FALSE,
                    topoff_rate=.06, topoff_minlength=40, 
                    rr_sensitive_leave_len=FALSE,
                    bene_effect=FALSE, wait_period=5, clone_factor=1, week_bene_cap=594, week_bene_min=0,
                    dependent_allow = 10,
                    sens_var='resp_len',
                    own_uptake=.03, matdis_uptake=.01, bond_uptake=.01, illparent_uptake=.001,
                    illspouse_uptake=.001, illchild_uptake=.001,
                    maxlen_PFL= 30, maxlen_DI=130, maxlen_own =130, maxlen_matdis =130, maxlen_bond =30, maxlen_illparent=30,
                    maxlen_illspouse =30, maxlen_illchild =30, maxlen_total=130, earnings=8400,output=paste0("NJ_",meth,"issue_brief_1"),
                    output_stats=c('state_compar'),  random_seed=12312)
  timeend <<- Sys.time()
  print(timeend - timestart)
}