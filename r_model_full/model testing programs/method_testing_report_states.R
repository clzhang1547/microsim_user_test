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
  methods <- c('Naive_Bayes','random','KNN1','logit', 'KNN_multi','random_forest','ridge_class','xg_boost')
  
  for (meth in methods) {
  
  #=================================
  #Rhode Island
  #=================================
  
  timestart <<- Sys.time()
    d_ri <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                              acs_person_csv="ss16pri.csv",
                              acs_house_csv="ss16hri.csv",
                              cps_csv="CPS2014extract.csv",
                              useCSV=TRUE,
                              saveDF=FALSE,
                              makelog=TRUE,
                              xvars=c("widowed", "divorced", "separated", "nevermarried", "female", 
                                      'age_cat', "ltHS", "someCol", "BA", "GradSch", "black", 
                                      "other", "asian",'native', "hisp","nochildren",'faminc_cat','coveligd'),
                              base_bene_level=.6,
                              impute_method=meth,
                              ext_resp_len = TRUE, sens_var = 'resp_len', progalt_post_or_pre ='post',
                              ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                              bene_effect=TRUE, wait_period=5, full_particip_needer=TRUE, clone_factor=0, week_bene_cap=795, week_bene_min=89,
                              dependent_allow = 10,
                              own_uptake=.95, matdis_uptake=.95, bond_uptake=.75, illparent_uptake=.1,
                              illspouse_uptake=.2, illchild_uptake=.2,
                              exclusive_particip=FALSE,
                              maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                              maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=11520,output=paste0("RI_",meth,"_method_alt_no_exclude"),
                              output_stats=c('state_compar_no_exclude'),  random_seed=123)
  
  timeend <<- Sys.time()
  print(timeend - timestart)
  }
  #methods <- c('random','KNN1','logit', 'KNN_multi','random_forest','Naive_Bayes', 'ridge_class')
  #methods <- c('KNN1','logit', 'KNN_multi','random_forest','Naive_Bayes', 'ridge_class')
  methods <- c('Naive_Bayes', 'ridge_class')
  for (meth in methods) {  
    #=================================
    #California
    #=================================
    # clear up memory
    rm(list=ls()[ls()!='methods' & ls()!='policy_simulation' & ls()!='meth'])
    start_time <- Sys.time()
    policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                      acs_person_csv="ss16pca.csv",
                      acs_house_csv="ss16hca.csv",
                      cps_csv="CPS2014extract.csv",
                      useCSV=TRUE,
                      saveDF=FALSE,
                      makelog=TRUE,
                      xvars=c("widowed", "divorced", "separated", "nevermarried", "female", 
                              'age_cat', "ltHS", "someCol", "BA", "GradSch", "black", 
                              "other", "asian",'native', "hisp","nochildren",'faminc_cat','coveligd'),
                      base_bene_level=.55,
                      impute_method = meth,
                      sample_prop = .1,
                      ext_resp_len = TRUE, sens_var = 'resp_len', progalt_post_or_pre ='post',
                      ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                      bene_effect=TRUE, full_particip_needer=TRUE, wait_period=5, clone_factor=0, week_bene_cap=1216,
                      week_bene_min=50, dependent_allow = 10,
                      own_uptake=.95, matdis_uptake=.95, bond_uptake=.75, illparent_uptake=.1,
                      illspouse_uptake=.2, illchild_uptake=.2,
                      maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent =30, 
                      maxlen_PFL= 30, maxlen_DI=260, maxlen_total=260,
                      maxlen_illspouse =30, maxlen_illchild =30,earnings=300, output=paste0("CA_",meth,"_method_alt"),
                      output_stats=c('state_compar'),  random_seed=123)
    
    end_time <- Sys.time()
    print(end_time - start_time)
  }
  #=================================
  # New Jersey
  #=================================
  # New Jersey has reduced TDI take up due to many employers offering qualifying private plans 
  # with a robust private insurance market for TDI (though not for PFL). Adjusting down TDI uptake by 30% as a result
  #http://lims.dccouncil.us/Download/34613/B21-0415-Economic-and-Policy-Impact-Statement-UPLAA3.pdf
  # 
  
  
for (meth in methods) {  
  start_time <- Sys.time()
  d_nj <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                            acs_person_csv="ss16pnj.csv",
                            acs_house_csv="ss16hnj.csv",
                            cps_csv="CPS2014extract.csv",
                            useCSV=TRUE,
                            saveDF=FALSE,
                            makelog=TRUE,
                            xvars=c("widowed", "divorced", "separated", "nevermarried", "female", 
                                    'age_cat', "ltHS", "someCol", "BA", "GradSch", "black", 
                                    "other", "asian",'native', "hisp","nochildren",'faminc_cat','coveligd'),
                            base_bene_level=.66,
                            impute_method = meth,
                            ext_resp_len = TRUE, sens_var = 'resp_len', progalt_post_or_pre ='post',
                            ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=0, topoff_minlength=10,
                            bene_effect=TRUE,  wait_period=5, clone_factor=0, week_bene_cap=594,
                            own_uptake=.95, matdis_uptake=.95, bond_uptake=.75, illparent_uptake=.1,
                            illspouse_uptake=.2, illchild_uptake=.2,
                            own_elig_adj=.7, matdis_elig_adj=.7,
                            maxlen_own =130, maxlen_matdis =130, maxlen_bond =30, maxlen_illparent =30, 
                            maxlen_PFL= 30, maxlen_DI=130, maxlen_total=130,
                            maxlen_illspouse =30, maxlen_illchild =30,earnings=8400,output=paste0("NJ_",meth,"_method"), output_stats=c('state_compar'), random_seed=123)
  
  
  end_time <- Sys.time()
  print(end_time - start_time)
}