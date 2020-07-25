cat("\014")  
options(error=recover)

# load execution function
source("0_master_execution_function.R")

# simulate a Maryland paid family leave program with Rhode Island's rules
d <- policy_simulation(state='MD',
                  saveCSV=TRUE,
                  makelog=TRUE,
                  base_bene_level=.6,
                  impute_method='logit',
                  place_of_work = FALSE,
                  ext_resp_len = TRUE, 
                  ext_base_effect=TRUE,
                  bene_effect=FALSE, wait_period=5, full_particip=FALSE, week_bene_cap=795, week_bene_min=89,
                  dependent_allow = 10,
                  own_uptake=.0435, matdis_uptake=.0143, bond_uptake=.0097, illparent_uptake=.0018,
                  illspouse_uptake=.0020, illchild_uptake=.0012,
                  maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                  maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=3840,output="MD_simulation",
                  output_stats=c('state_compar'),
                  random_seed=123)


