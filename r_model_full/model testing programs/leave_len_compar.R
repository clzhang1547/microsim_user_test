# compare leave length extension effects of each method of leave extension

# house keeping
cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)
#options(error=NULL)

# sample master execution function for testing code
source("0_master_execution_function.R")

# first set a base set of params 
default_params <- list(
  state='RI',
  makelog=TRUE,
  base_bene_level=.6,
  impute_method='logit',
  place_of_work = TRUE,
  dual_receiver = 1,
  bene_effect=FALSE, 
  wait_period=5, full_particip=FALSE, clone_factor=1, week_bene_cap=795, week_bene_min=89,
  dependent_allow = 10,
  own_uptake=.0704, matdis_uptake=.0235, bond_uptake=.0092, illparent_uptake=.0008,
  illspouse_uptake=.0014, illchild_uptake=.0005,
  maxlen_PFL= 20, maxlen_DI=150, maxlen_own =150, maxlen_matdis =150, maxlen_bond =20, maxlen_illparent=20, 
  maxlen_illspouse =20, maxlen_illchild =20, maxlen_total=150, earnings=3840, random_seed=12345
)

# baseline sim -> no leave lengths should change in this scenario
addl_params <- list(
  rr_sensitive_leave_len=FALSE,
  ext_base_effect=FALSE,
  ext_resp_len = FALSE
)
run_params <- append(default_params, addl_params)
d1 <- do.call(policy_simulation,run_params)


# ACM sim -> only turn on the ACM base effect 
addl_params <- list(
  rr_sensitive_leave_len=FALSE,
  ext_base_effect=TRUE,
  ext_resp_len = FALSE
)
run_params <- append(default_params, addl_params)
d2 <- do.call(policy_simulation,run_params)

# resp len sim -> only turn on the conditional draw of constrained vs unconstrained 
addl_params <- list(
  rr_sensitive_leave_len=FALSE,
  ext_base_effect=FALSE,
  ext_resp_len = TRUE
)
run_params <- append(default_params, addl_params)
d3 <- do.call(policy_simulation,run_params)

# rr sensitivity  sim -> also turn on extension of leave sensitivity to rr, 
addl_params <- list(
  rr_sensitive_leave_len=TRUE,
  ext_base_effect=FALSE,
  ext_resp_len = TRUE
)
run_params <- append(default_params, addl_params)
d4 <- do.call(policy_simulation,run_params)

# all sim -> turn on all options, 
addl_params <- list(
  rr_sensitive_leave_len=TRUE,
  ext_base_effect=TRUE,
  ext_resp_len = TRUE
)
run_params <- append(default_params, addl_params)
d5 <- do.call(policy_simulation,run_params)

# store results together in a vector
results <- list(
  base=d1,
  ACM=d2,
  resp_len=d3,
  rr_sen=d4,
  all=d5
)
# start a data frame to calculate the leave taking extensions of each 
leave_types <- c("own","matdis","bond","illchild","illspouse","illparent")
mean_lens <-data.frame(row.names = c('fmla',names(results)))
for (j in leave_types) {
  mean_lens[j] <- 0
}

# first, load FMLA data, and get empirical sample of differences in leave lengths
fmla <- readRDS(paste0("./restricted_data/","d_fmla_restrict.rds"))
for (j in leave_types) {
  take_var <- paste0('take_',j)
  d_squo <- fmla %>% filter(recStatePay==0 & get(take_var)==1)
  d_cfact <- fmla %>% filter(recStatePay==1 & get(take_var)==1)
  mean_lens['fmla',j] <-(weighted.mean(d_cfact$length,weight=d_cfact$weight,na.rm=TRUE)-
    weighted.mean(d_squo$length,weight=d_squo$weight,na.rm=TRUE))/
    weighted.mean(d_squo$length,weight=d_squo$weight,na.rm=TRUE)
}

# then append the synthetic simulation increases
for (i in names(results)) {
  result <- results[[i]]
  for (j in leave_types) {
    squo_var <- paste0('squo_length_',j)
    cfact_var <- paste0('length_',j)
    d <- result %>% filter(get(squo_var)>0)
    mean_lens[i,j] <-(weighted.mean(d[,cfact_var],weight=d$PWGTP)-
        weighted.mean(d[,squo_var],weight=d$PWGTP))/
        weighted.mean(d[,squo_var],weight=d$PWGTP)
  }
}