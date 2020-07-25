# code for issue brief 1 - simulating 2012 leave program in RI 

#rm(list=ls())
cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)
#options(error=NULL)

# sample master execution function for testing code
source("0_master_execution_function.R")
#=================================
#Rhode Island
#=================================
meth <- 'Logistic Regression GLM'
timestart <<- Sys.time()
ri <- policy_simulation(saveCSV=TRUE,
                        minsize=1,
                        ann_hours=1,
                        state='RI',
                        makelog=TRUE,
                        base_bene_level=.6,
                        place_of_work = TRUE,
                        dual_receiver = 1,
                        ext_resp_len = TRUE, 
                        fmla_protect=FALSE,
                        ext_base_effect=FALSE,
                        rr_sensitive_leave_len=TRUE,
                        topoff_rate=0, topoff_minlength=0, 
                        bene_effect=FALSE, wait_period=5, clone_factor=1, week_bene_cap=804, week_bene_min=0,
                        dependent_allow = c(0.07,0.07,0.07,0.07,0.07),
                        alpha=0,
                        sens_var='resp_len',
                        own_uptake= .0809, matdis_uptake=.027, bond_uptake=.0102, illchild_uptake=.0006,
                        illspouse_uptake=.0015, illparent_uptake=.0009,
                        maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                        maxlen_illspouse =20, maxlen_illchild =20,
                        earnings=3840,output=paste0("RI_",meth,"_gui_compar"),
                        output_stats=c('state_compar'),  random_seed=12312)
timeend <<- Sys.time()
print(timeend - timestart)
