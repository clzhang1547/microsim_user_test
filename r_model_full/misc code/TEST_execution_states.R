# test executions on full state data

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
timestart <<- Sys.time()
d_ri <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                          acs_person_csv="ss16pri.csv",
                          acs_house_csv="ss16hri.csv",
                          cps_csv="CPS2014extract.csv",
                          useCSV=TRUE,
                          saveDF=FALSE,
                          saveCSV=TRUE,
                          makelog=TRUE,
                          base_bene_level=.6,
                          impute_method='KNN1',
                          ext_resp_len = TRUE, 
                          ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                          bene_effect=TRUE, wait_period=5, full_particip_needer=TRUE, clone_factor=0, week_bene_cap=795, week_bene_min=89,
                          dependent_allow = 10,
                          own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                          illspouse_uptake=.25, illchild_uptake=.25,
                          maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
                          maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=11520,output=paste0("RI_",'KNN1',"_method"),
                          output_stats=c('state_compar'),  random_seed=123)

timeend <<- Sys.time()
print(timeend - timestart)

#=================================
#California
#=================================
start_time <- Sys.time()
d_ca <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                          acs_person_csv="ss16pca.csv",
                          acs_house_csv="ss16hca.csv",
                          cps_csv="CPS2014extract.csv",
                          useCSV=TRUE,
                          saveDF=FALSE,
                          leaveprogram=TRUE,
                          base_bene_level=.55,
                          ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
                          bene_effect=TRUE, full_particip_needer=TRUE, wait_period=5, clone_factor=0, week_bene_cap=1216,
                          week_bene_min=50, dependent_allow = 10,
                          own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                          illspouse_uptake=.25, illchild_uptake=.25,
                          maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent =30, 
                          maxlen_PFL= 30, maxlen_DI=260, maxlen_total=260,
                          maxlen_illspouse =30, maxlen_illchild =30,earnings=300, output="CA",
                          output_stats=c('state_compar'),  random_seed=123)

end_time <- Sys.time()
print(end_time - start_time)

#=================================
# New Jersey
#=================================
# New Jersey has reduced TDI take up due to many employers offering qualifying private plans 
# with a robust private insurance market for TDI (though not for PFL). Adjusting down TDI uptake by 30% as a result
#http://lims.dccouncil.us/Download/34613/B21-0415-Economic-and-Policy-Impact-Statement-UPLAA3.pdf
# 



start_time <- Sys.time()
d_nj <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
                          acs_person_csv="ss16pnj.csv",
                          acs_house_csv="ss16hnj.csv",
                          cps_csv="CPS2014extract.csv",
                          useCSV=TRUE,
                          saveDF=FALSE,
                          leaveprogram=TRUE,
                          base_bene_level=.66,
                          ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=0, topoff_minlength=10,
                          bene_effect=TRUE,  wait_period=5, clone_factor=0, week_bene_cap=594,
                          own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
                          illspouse_uptake=.25, illchild_uptake=.25,
                          own_elig_adj=.7, matdis_elig_adj=.7,
                          maxlen_own =130, maxlen_matdis =130, maxlen_bond =30, maxlen_illparent =30, 
                          maxlen_PFL= 30, maxlen_DI=130, maxlen_total=130,
                          maxlen_illspouse =30, maxlen_illchild =30,earnings=8400,output="NJ", output_stats=c('state_compar'), random_seed=124)


end_time <- Sys.time()
print(end_time - start_time)

#----------------------------
# Massachusetts
#----------------------------
#under construction 

# policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
#                   acs_person_csv="ss15pma.csv",
#                   acs_house_csv="ss15hma.csv",
#                   cps_csv="CPS2014extract.csv",
#                   useCSV=TRUE,
#                   saveDF=TRUE)
# 
# 
# # Logit test for comparison with Chris
# # Parameters not tailored to Mass yet
# d <- policy_simulation(fmla_csv="fmla_2012_employee_restrict_puf.csv",
#                        acs_person_csv="ss16pma_short.csv",
#                        acs_house_csv="ss16ma_short.csv",
#                        cps_csv="CPS2014extract.csv",
#                        useCSV=FALSE,
#                        saveDF=FALSE,
#                        leaveprogram=TRUE,
#                        base_bene_level=.55,
#                        impute_method="logit",
#                        GOVERNMENT=TRUE,
#                        ext_base_effect=TRUE, extend_prob=.01, extend_days=1, extend_prop=1.01, topoff_rate=.01, topoff_minlength=10,
#                        bene_effect=TRUE, full_particip_needer=1, wait_period=5, clone_factor=0, week_bene_cap=1216,
#                        own_uptake=.25, matdis_uptake=.25, bond_uptake=.25, illparent_uptake=.25,
#                        illspouse_uptake=.25, illchild_uptake=.25,
#                        maxlen_own =260, maxlen_matdis =260, maxlen_bond =30, maxlen_illparent =30, 
#                        maxlen_PFL= 30, maxlen_DI=260, maxlen_total=260,
#                        maxlen_illspouse =30, maxlen_illchild =30,earnings=300, random_seed=123)