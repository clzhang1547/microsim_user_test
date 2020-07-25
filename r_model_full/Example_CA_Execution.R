


# load master execution function for testing code
source("0_master_execution_function.R")

# subsequent times, can run from saved r data frames to save time.

timestart <<- Sys.time()
policy_simulation(saveCSV=TRUE,
                  state='CA',
                  FEDGOV=TRUE,
                  STATEGOV=TRUE,
                  LOCALGOV=TRUE,
                  makelog=TRUE,
                  base_bene_level=.55,
                  impute_method='logit',
                  place_of_work = TRUE,
                  dual_receiver = 1,
                  ext_resp_len = TRUE,
                  fmla_protect=FALSE,
                  ext_base_effect=TRUE,
                  rr_sensitive_leave_len=FALSE,
                  bene_effect=FALSE, wait_period=5, clone_factor=1, week_bene_cap=1216, week_bene_min=50,
                  dependent_allow = 10,
                  alpha=0,
                  sens_var='resp_len',
                  topoff_rate=.06, topoff_minlength=20, 
                  own_uptake=.0363, matdis_uptake=.0127, bond_uptake=.0152, 
                  illchild_uptake=.0004, illspouse_uptake=.0007, illparent_uptake=.0007,
                  maxlen_PFL= 30, maxlen_DI=260, maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent=30,
                  maxlen_illspouse =30, maxlen_illchild =30, maxlen_total=260, earnings=300,output=paste0("CA_example_exec"),
                  output_stats=c('state_compar'),  random_seed=12312)
timeend <<- Sys.time()
print(timeend - timestart)