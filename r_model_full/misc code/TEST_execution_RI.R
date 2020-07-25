# test out cost estimation of different imputation methods on RI data set

#rm(list=ls())
cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)
#options(error=NULL)

# sample master execution function for testing code
source("0_master_execution_function.R")

meth <- 'logit'

timestart <<- Sys.time()
d <- policy_simulation(state='RI',
                          makelog=TRUE,
                          xvars=c("widowed", "divorced", "separated", "nevermarried", "female", 
                                  'age_cat', "ltHS", "someCol", "BA", "GradSch", "black", 
                                  "other", "asian",'native', "hisp","nochildren",'faminc_cat','coveligd'),
                          base_bene_level=.6,
                          impute_method=meth,
                          place_of_work = TRUE,
                          exclusive_particip = FALSE,
                          ext_resp_len = TRUE, 
                          ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                          bene_effect=TRUE, wait_period=5, full_particip_needer=TRUE, clone_factor=0, week_bene_cap=795, week_bene_min=89,
                          dependent_allow = 10,
                          own_uptake=.95, matdis_uptake=.95, bond_uptake=.75, illparent_uptake=.1,
                          illspouse_uptake=.2, illchild_uptake=.2,
                          maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                          maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=11520,output=paste0("RI_",meth,"_method_resid_redo"),
                          output_stats=c('state_compar'),  random_seed=123)

timeend <<- Sys.time()
print(timeend - timestart)

