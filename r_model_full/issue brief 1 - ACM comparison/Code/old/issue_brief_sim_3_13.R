  # code for issue brief 1 - simulating 2012 leave program in RI 
  
  #rm(list=ls())
  cat("\014")  
  basepath <- rprojroot::find_rstudio_root_file()
  setwd(basepath)
  options(error=recover)
  #options(error=NULL)
  
  # sample master execution function for testing code
  source("0_master_execution_function.R")
  meth <- 'logit'
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
                            ext_base_effect=TRUE,
                            rr_sensitive_leave_len=FALSE,
                            topoff_rate=.06, topoff_minlength=20, 
                            bene_effect=FALSE, wait_period=5, clone_factor=1, week_bene_cap=795, week_bene_min=89,
                            dependent_allow = 10,
                            alpha=.5,
                            sens_var='resp_len',
                            own_uptake= .0723, matdis_uptake=.0241, bond_uptake=.0104, illchild_uptake=.0006,
                            illspouse_uptake=.0015, illparent_uptake=.0009,
                            maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                            maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=3840,output=paste0("RI_",meth,"issue_brief_1_3_13"),
                            output_stats=c('state_compar'),  random_seed=12312)
  timeend <<- Sys.time()
  print(timeend - timestart)

  #=================================
  # New Jersey
  #=================================
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
                    own_uptake=.0235, matdis_uptake=.0083, bond_uptake=.008, 
                    illchild_uptake=.0004, illspouse_uptake=.0005, illparent_uptake=.0007,
                    maxlen_PFL= 30, maxlen_DI=130, maxlen_own =130, maxlen_matdis =130, maxlen_bond =30, maxlen_illparent=30,
                    maxlen_illspouse =30, maxlen_illchild =30, maxlen_total=130, earnings=8400,output=paste0("NJ_",meth,"issue_brief_1_3_13"),
                    output_stats=c('state_compar'),  random_seed=12312)
  timeend <<- Sys.time()
  print(timeend - timestart)

  #=================================
  # California
  #=================================

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
                            own_uptake=.0363, matdis_uptake=.0127, bond_uptake=.0152, 
                            illchild_uptake=.0004, illspouse_uptake=.0007, illparent_uptake=.0007,
                            maxlen_PFL= 30, maxlen_DI=260, maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent=30,
                            maxlen_illspouse =30, maxlen_illchild =30, maxlen_total=260, earnings=300,output=paste0("CA_",meth,"issue_brief_1_3_13"),
                            output_stats=c('state_compar'),  random_seed=12312)
  timeend <<- Sys.time()
  print(timeend - timestart)
