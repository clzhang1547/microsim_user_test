
#rm(list=ls())
cat("\014")  
#basepath <- rprojroot::find_rstudio_root_file()
#setwd(basepath)
options(error=recover)
#options(error=NULL)

# sample master execution function for testing code
source("0_master_execution_function.R")

#First time, preload cleaned data sets to speed up testing
# policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
#                    acs_person_csv="ss16pri_short.csv",
#                    acs_house_csv="ss16hri_short.csv",
#                    cps_csv="CPS2014extract.csv",
#                    useCSV=TRUE,
#                    saveDF=TRUE,
#                    GOVERNMENT=TRUE,
#                    SELFEMP=TRUE)


# subsequent times, can run from saved r data frames to save time.
timestart <<- Sys.time()
d <- policy_simulation(
                  saveCSV=TRUE,
                  xvars=c("widowed", "divorced", "separated", "nevermarried", "female", 
                          'age_cat', "ltHS", "someCol", "BA", "GradSch", "black", 
                          "other", "asian",'native', "hisp","nochildren",'faminc_cat','coveligd'),
                  base_bene_level=.55,
                  impute_method="logit",
		              makelog = FALSE,
                  sample_prop=1,
		              state='WY',
		              SELFEMP=FALSE,
		              place_of_work = FALSE,
		              exclusive_particip = TRUE,
		              SMOTE=FALSE, 
                  ext_resp_len = FALSE, len_method = 'mean', sens_var = 'unaffordable', progalt_post_or_pre ='post',
		              ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                  bene_effect=TRUE, full_particip_needer=FALSE, wait_period=5, clone_factor=0, week_bene_cap=1216,
                  own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                  illspouse_uptake=.25, illchild_uptake=.25,
                  maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent =30, 
                  maxlen_PFL= 30, maxlen_DI=260, maxlen_total=260,
                  maxlen_illspouse =30, maxlen_illchild =30,earnings=30000, own_elig_adj= .75,
                  formula_value_cuts=c(20000, 50000, 100000), formula_bene_levels=c(.4,.5,.6,.7),
                  output='test_execution', output_stats=c('state_compar'), random_seed=NULL)

timeend <<- Sys.time()
print(timeend - timestart)