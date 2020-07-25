
#rm(list=ls())0
cat("\014")  
#basepath <- rprojroot::find_rstudio_root_file()
#setwd(basepath)
options(error=recover)
options(warn=1)
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
meth <- 'Logistic Regression GLM'
timestart <<- Sys.time()
d <- policy_simulation(
                  saveCSV=TRUE,
                  base_bene_level=.55,
                  impute_method = meth,
		              makelog = FALSE,
                  sample_prop=1,
		              state='WY',
		              SELFEMP=FALSE,
		              place_of_work = TRUE,
		              dual_receiver = .75,
		              alpha=1,
                  ext_resp_len = TRUE, sens_var = 'resp_len', progalt_post_or_pre ='post',
		              ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                  bene_effect=FALSE, full_particip=FALSE, wait_period=5, clone_factor=1, week_bene_cap=1216,
                  own_uptake=.01, matdis_uptake=.01, bond_uptake=.01, illparent_uptake=.01,
                  illspouse_uptake=.01, illchild_uptake=.01,
                  maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent =30, 
                  maxlen_PFL= 30, maxlen_DI=260, maxlen_total=260,
                  maxlen_illspouse =30, maxlen_illchild =30,earnings=30000, own_elig_adj= .75,
                  formula_value_cuts=c(20000, 50000, 100000), formula_bene_levels=c(.4,.5,.6,.7), random_seed=NULL)

timeend <<- Sys.time()
print(timeend - timestart)