  # Some alternative  executions on full state data
# for report, testing each method

#rm(list=ls())
cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)
#options(error=NULL)

# sample master execution function for testing code
source("0_master_execution_function.R")

methods <- c('random','logit','KNN1', 'KNN_multi','random_forest','Naive_Bayes', 'ridge_class')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Leave taking with no intra-fmla imputation, but uptake params to match ACM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (meth in methods) {
  
  #=================================
  #Rhode Island
  #=================================
  
  timestart <<- Sys.time()
  d_ri <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                            acs_person_csv="xRI_2012-2016p.csv",
                            acs_house_csv="ss16hri.csv",
                            cps_csv="CPS2014extract.csv",
                            useCSV=TRUE,
                            saveDF=FALSE,
                            makelog=TRUE,
                            base_bene_level=.6,
                            impute_method=meth,
                            state = 'RI',
                            place_of_work = TRUE,
                            ext_resp_len = TRUE, 
                            intra_impute =FALSE,
                            ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                            bene_effect=TRUE, wait_period=5, full_particip_needer=TRUE, clone_factor=0, week_bene_cap=795, week_bene_min=89,
                            dependent_allow = 10,
                            own_uptake=.4, matdis_uptake=.95, bond_uptake=.75, illparent_uptake=.1,
                            illspouse_uptake=.2, illchild_uptake=.2,
                            maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                            maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=11520,output=paste0("RI_",meth,"_no_intra_impute"),
                            output_stats=c('state_compar'),  random_seed=123)
  
  timeend <<- Sys.time()
  print(timeend - timestart)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Place of Work instead of Place of Residence
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  for (meth in methods) {
  
  #=================================
  #Rhode Island
  #=================================
  
  timestart <<- Sys.time()
    d_ri <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                              acs_person_csv="xRI_2012-2016p.csv",
                              acs_house_csv="ss16hri.csv",
                              cps_csv="CPS2014extract.csv",
                              useCSV=TRUE,
                              saveDF=FALSE,
                              makelog=TRUE,
                              base_bene_level=.6,
                              impute_method=meth,
                              state = 'RI',
                              place_of_work = TRUE,
                              ext_resp_len = TRUE, 
                              ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                              bene_effect=TRUE, wait_period=5, full_particip_needer=TRUE, clone_factor=0, week_bene_cap=795, week_bene_min=89,
                              dependent_allow = 10,
                              own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                              illspouse_uptake=.25, illchild_uptake=.25,
                              maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                              maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=11520,output=paste0("RI_",meth,"_method_POW"),
                              output_stats=c('state_compar'),  random_seed=123)
  
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
    start_time <- Sys.time()
    d_nj <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                              acs_person_csv="xNJ_2012-2016p.csv",
                              acs_house_csv="ss16hnj.csv",
                              cps_csv="CPS2014extract.csv",
                              useCSV=TRUE,
                              saveDF=FALSE,
                              makelog=TRUE,
                              base_bene_level=.66,
                              impute_method = meth,
                              state = 'NJ',
                              place_of_work = TRUE,
                              ext_resp_len = TRUE,
                              ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=0, topoff_minlength=10,
                              bene_effect=TRUE,  wait_period=5, clone_factor=0, week_bene_cap=594,
                              own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                              illspouse_uptake=.25, illchild_uptake=.25,
                              own_elig_adj=.7, matdis_elig_adj=.7,
                              maxlen_own =130, maxlen_matdis =130, maxlen_bond =30, maxlen_illparent =30, 
                              maxlen_PFL= 30, maxlen_DI=130, maxlen_total=130,
                              maxlen_illspouse =30, maxlen_illchild =30,earnings=8400,output=paste0("NJ_",meth,"_method_POW"), output_stats=c('state_compar'), random_seed=123)
    
    
    end_time <- Sys.time()
    print(end_time - start_time)
  }
  #methods <- c('random','KNN1','logit', 'KNN_multi','random_forest','Naive_Bayes', 'ridge_class')
  #methods <- c('KNN1','logit', 'KNN_multi','random_forest','Naive_Bayes', 'ridge_class')
  #methods <- c('Naive_Bayes', 'ridge_class')
  for (meth in methods) {  
    #=================================
    #California
    #=================================
    # clear up memory
    rm(list=ls()[ls()!='methods' & ls()!='policy_simulation' & ls()!='meth'])
    start_time <- Sys.time()
    policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                      acs_person_csv="xCA_2012-2016p.csv",
                      acs_house_csv="ss16hca.csv",
                      cps_csv="CPS2014extract.csv",
                      useCSV=TRUE,
                      saveDF=FALSE,
                      makelog=TRUE,
                      base_bene_level=.55,
                      impute_method = meth,
                      state = 'CA',
                      place_of_work = TRUE,
                      ext_resp_len = TRUE,
                      sample_prop = .1,
                      ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                      bene_effect=TRUE, full_particip_needer=TRUE, wait_period=5, clone_factor=0, week_bene_cap=1216,
                      week_bene_min=50, dependent_allow = 10,
                      own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                      illspouse_uptake=.25, illchild_uptake=.25,
                      maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent =30, 
                      maxlen_PFL= 30, maxlen_DI=260, maxlen_total=260,
                      maxlen_illspouse =30, maxlen_illchild =30,earnings=300, output=paste0("CA_",meth,"_method_POW"),
                      output_stats=c('state_compar'),  random_seed=123)
    
    end_time <- Sys.time()
    print(end_time - start_time)
  }

  