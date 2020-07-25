
# """
# File: 0_NEW_Master_execution_function
#
# Master execution function that calls all other functions in order to execute the simulation
# 
# 9 Sept 2018
# Luke
# 
# """

# ----------Helper function for measuring time elapsed
time_elapsed <<- function(msg) {
  print(msg)
  print(Sys.time()-timestart)
  browser()
}  

# ----------master function that sets policy scenario, and calls functions to alter FMLA data to match policy scenario---------------------
# see parameter documentation for details on arguments

policy_simulation <- function(
                              # input data parameters
                              loadRDS=FALSE,
                              loadCSV=TRUE,
                              acs_dir='../data/acs',
                              cps_dir='../data/cps/',
                              fmla_file='../data/fmla/fmla_2012/fmla_2012_employee_revised_puf.csv',
                              fmla_year=2012,
                              acs_year=2016,
                              # output/logging params
                              makelog=TRUE,
                              log_directory='./log/',
                              progress_file=NULL,
                              fulloutput=FALSE,
                              saveCSV=TRUE,
                              out_dir="../output/",
                              output=NULL,
                              output_stats=NULL,
                              
                              # simulation runtime modification params
                              sample_prop=NULL, 
                              sample_num=NULL,
                              clone_factor=1,  
                              progalt_post_or_pre ='post',
                              random_seed=123,
                              engine_type='Main',
                              runtime_measure=0,
                              
                              # program eligibility params
                              FEDGOV=FALSE, 
                              STATEGOV=FALSE,
                              LOCALGOV=FALSE,
                              SELFEMP=FALSE, 
                              PRIVATE=TRUE,
                              wait_period=0,
                              wait_period_recollect=FALSE,
                              place_of_work = FALSE,
                              state = '',
                              dual_receiver = 1,
                              earnings=NULL,
                              weeks= NULL,
                              ann_hours=NULL,
                              minsize= NULL,
                              elig_rule_logic= '(earnings & weeks & ann_hours & minsize)',
                              
                              # program benefit params
                              base_bene_level=1, 
                              dependent_allow=0,
                              maxlen_own =60,
                              maxlen_matdis =60,
                              maxlen_bond =60,
                              maxlen_illparent =60,
                              maxlen_illspouse =60,
                              maxlen_illchild =60,
                              maxlen_PFL=max(maxlen_illparent,maxlen_illspouse,maxlen_illchild,maxlen_bond), 
                              maxlen_DI=max(maxlen_bond,maxlen_matdis),
                              maxlen_total=max(maxlen_DI,maxlen_PFL), 
                              week_bene_cap=1000000, 
                              week_bene_min=0, 
                              week_bene_cap_prop=NULL,
                              formula_prop_cuts=NULL,
                              formula_value_cuts=NULL,
                              formula_bene_levels=NULL,
                              
                              # FMLA -> ACS imputation params
                              impute_method="Logistic Regression GLM",
                              kval= 5,
                              xvars=NULL,
                              # default xvar weights is 1's for everything
                              xvar_wgts = rep(1,length(xvars)),
                              
                              # behavioral simulation params
                              topoff_rate=0, 
                              topoff_minlength=0, 
                              bene_effect=FALSE, 
                              full_particip=FALSE, 
                              own_uptake=.01, 
                              matdis_uptake=.01, 
                              bond_uptake=.01, 
                              illparent_uptake=.01, 
                              illspouse_uptake=.01, 
                              illchild_uptake=.01, 
                              own_elig_adj=1, 
                              illspouse_elig_adj=1, 
                              illchild_elig_adj=1, 
                              illparent_elig_adj=1, 
                              matdis_elig_adj=1, 
                              bond_elig_adj=1,
                              sens_var = 'resp_len',
                              ext_resp_len = TRUE,  
                              rr_sensitive_leave_len=TRUE,
                              min_takeup_cpl = 5,
                              alpha=0,
                              ext_base_effect=FALSE,
                              extend_prob=0,
                              extend_days=0,
                              extend_prop=1,
                              fmla_protect=FALSE,
                              
                              # ABF Params
                              ABF_enabled=TRUE,
                              ABF_elig_size=earnings,
                              ABF_max_tax_earn=0,
                              ABF_bene_tax=TRUE,
                              ABF_avg_state_tax=0,
                              ABF_payroll_tax=0,
                              ABF_bene=NULL,
                              ABF_detail_out=FALSE
                            ) {
  
  
  ####################################
  # Parameter standardization
  ####################################
  # read in state codes data set
  d_states <- read.csv("../data/ACS_state_codes.csv")
  # make two digit abbreviation of year
  year_2dig <- toString(acs_year - 2000)
  if (nchar(year_2dig)==1) {
    year_2dig <- paste0('0',year_2dig)
  }
  # set derived parameters
  weightfactor=1/clone_factor
  # create required folders for logs and output, if they don't exist
  dir.create(log_directory, showWarnings = FALSE)
  dir.create(out_dir, showWarnings = FALSE)
  #  if output name is null, set a standard name to match python
  
  if (is.null(output)) {
    output <- paste0('acs_sim_all_', format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  
  
  # If xvars is null, set to be defaults based on fmla wave 
  if (is.null(xvars) & fmla_year==2012) {
    xvars <-c("widowed", "divorced", "separated", "nevermarried", "female", 
              "ltHS", "someCol", "BA", "GradSch", "black", 
              "other", "asian",'native', "hisp","nochildren",'fmla_eligible',
              'union','hourly',
              'age',"agesq",'wkhours','faminc')
  }
  if (is.null(xvars) & fmla_year==2018) {
    xvars <-c("widowed", "divorced", "separated", "nevermarried", "female", 
              "ltHS", "someCol", "BA", "GradSch", "black", 
              "other", "asian",'native', "hisp","nochildren",'fmla_eligible',
              'union','noelderly','hourly',
              'age',"agesq",'wkhours','faminc',
              'emp_nonprofit','low_wage','occ_1','occ_2','occ_3','occ_4','occ_5','occ_6'
              ,'occ_7','occ_8','occ_9','occ_10','ind_1','ind_2','ind_3','ind_4','ind_5','ind_6'
              ,'ind_7','ind_8','ind_9','ind_10','ind_11','ind_12','ind_13')
  }
  
  ####################################
  # Option to create execution log
  ####################################
  makelog <<- makelog
  if (makelog==TRUE) {
    # note starting time
    timestart <<- Sys.time()
    
    # save starting parameters
    params <- lapply(ls(), function(x) {return(get(x))})
    names(params) <- ls()[!ls() %in% c('params')]
    params['params'] <- NULL
    
    # create log file and record starting parameters 
    log_name <<- paste0(log_directory, format(Sys.time(), "%Y-%m-%d %H.%M.%S"), '_',output, '.log')
    file.create(log_name)
    cat("==============================", file = log_name, sep="\n")
    cat("Microsim Log File", file = log_name, sep="\n", append = TRUE)
    cat(paste0("Run Title: ", output), file = log_name, sep="\n", append = TRUE)
    cat(paste0("Date/Time: ", Sys.time()), file = log_name, sep="\n", append = TRUE)
    cat("==============================", file = log_name, sep="\n", append = TRUE)
    cat("", file = log_name, sep="\n", append = TRUE)
    cat("------------------------------", file = log_name, sep="\n", append = TRUE)
    cat("Parameters used", file = log_name, sep="\n", append = TRUE)
    cat("------------------------------", file = log_name, sep="\n", append = TRUE)
    lapply(names(params), function(x) cat(paste(x, ":", params[x]), file = log_name, sep="\n", append = TRUE))
    cat("", file = log_name, sep="\n", append = TRUE)
    
  }
  # create parameter meta file 
  meta_params <- c(
    # 2 letter postal abbreviation of state
    'State'=state,
    'Year'= acs_year, 
    'Place of Work' =place_of_work , 
    'Minimum Annual Wage' = earnings, 
    'Minimum Annual Work Weeks' = weeks, 
    'Minimum Annual Work Hours' = ann_hours, 
    'Minimum Employer Size' = minsize, 
    'Proposed Wage Replacement Ratio' = base_bene_level, 
    'Weekly Benefit Cap' = week_bene_cap, 
    'Include Private Employees' = PRIVATE, 
    'Include Goverment Employees, Federal' = FEDGOV, 
    'Include Goverment Employees, State' = STATEGOV, 
    'Include Goverment Employees, Local' = LOCALGOV, 
    'Include Self-employed' = SELFEMP, 
    'Simulation Method' = impute_method, 
    'Share of Dual Receivers' = dual_receiver, 
    'Alpha' = alpha, 
    'Minimum Leave Length Applied' = min_takeup_cpl, 
    'Waiting Period' = wait_period, 
    'Recollect Benefits of Waiting Period' = wait_period_recollect, 
    'Minimum Leave Length for Recollection' = wait_period, 
    'Dependent Allowance' = is.null(dependent_allow) == FALSE & all(dependent_allow!=0), 
    'Dependent Allowance Profile: Increments of Replacement Ratio by Number of Dependants' = dependent_allow, 
    'Clone Factor' = clone_factor, 
    'Random Seed' = random_seed
  )
  meta_params <- data.frame(meta_params)
 
  write.table(meta_params, sep=',', file=paste0(out_dir, '/prog_para_',format(Sys.time(), "%Y%m%d_%H%M%S"),'.csv'), col.names = FALSE)
  bene_take <- data.frame(row.names=c('Maximum Week of Benefit Receiving','Take Up Rates'))
  leave_types <- c("own","matdis","bond","illchild","illspouse","illparent")
  for (i in leave_types) {
    bene_take[i] <- NA
    bene_take['Maximum Week of Benefit Receiving',i] <- get(paste0('maxlen_',i))/5
    bene_take['Take Up Rates',i] <- get(paste0(i,'_uptake'))
  }
  write.table(bene_take, sep=',', file=paste0(out_dir, '/prog_para_',format(Sys.time(), "%Y%m%d_%H%M%S"),'.csv'), append=TRUE)
  
  ####################################
  # global libraries used everywhere #
  ####################################
  pkgTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
    return("OK")
  }
  
  global.libraries <- c('readr','tibble','DMwR','xgboost','bnclassify', 'randomForest','magick','stats', 'rlist', 'MASS', 'plyr', 'dplyr', 
                        'survey', 'class', 'dummies', 'varhandle', 'oglmx', 
                        'foreign', 'ggplot2', 'reshape2','e1071','pander','ridge')
  
  
  results <- sapply(as.list(global.libraries), pkgTest)
  
  # run files that define functions
  source("1_cleaning_functions.R"); source("2_pre_impute_functions.R"); source("3_impute_functions.R"); 
  source("4_post_impute_functions.R"); source("5_ABF_functions.R"); source("6_output_analysis_functions.R")
  
  # set random seed option
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }
 
  #========================================
  # 1. Cleaning 
  #========================================
  # make sure either loadRDS or loadCSV are specified 
  stopifnot(loadRDS | loadCSV)
  if (loadRDS) {
    # if loadRDS, then load files from a dataframe
    d_fmla <- readRDS(paste0("./R_dataframes/","d_fmla.rds"))
    d_cps <- readRDS(paste0("./R_dataframes/","d_cps.rds"))
    # load from residence ACS if state is a 
    if (place_of_work==TRUE) {
      d_acs <- readRDS(paste0("./R_dataframes/work_states/",state,"_work.rds"))  
    }
    else {
      d_acs <- readRDS(paste0("./R_dataframes/resid_states/",state,"_resid.rds"))  
    }
  }
  if (loadCSV) {
    # if loadCSV, look for csvs and conduct cleaning
    # Load and clean CPS
    d_cps <- read.csv(paste0(cps_dir,'cps_clean_',acs_year-2,'.csv'))
    #d_cps <- clean_cps(d_cps)
  
    # Load and clean FMLA 
    d_fmla <- read.csv(fmla_file)
    stopifnot(fmla_year == 2012 | fmla_year == 2018)
    if (fmla_year == 2012) {
      d_fmla <- clean_fmla(d_fmla)
    }
    if (fmla_year == 2018) {
      d_fmla <- clean_fmla_2018(d_fmla)
    }
    
    # Load and clean ACS
    st_code <- d_states[d_states$state_abbr==toupper(state),'ST']
    if (place_of_work==TRUE) {
      d_acs_hh <- read.csv(paste0(acs_dir,'/',acs_year,'/pow_household_files/h',st_code,'_',tolower(state),'_pow.csv'))
      d_acs_p <- read.csv(paste0(acs_dir,'/',acs_year,'/pow_person_files/p',st_code,'_',tolower(state),'_pow.csv')) 
    }
    else {
      d_acs_hh <- read.csv(paste0(acs_dir,'/',acs_year,'/pow_household_files/ss',year_2dig,'p',tolower(state),'.csv'))
      d_acs_p <- read.csv(paste0(acs_dir,'/',acs_year,'/pow_person_files/ss',year_2dig,'h',tolower(state),'.csv')) 
    }
    
    d_acs <- clean_acs(d_acs_p, d_acs_hh, fmla_year, save_csv=FALSE)
    # Impute hourly worker, weeks worked, and firm size variables from CPS into ACS. 
    # These are needed for leave program eligibility determination
    d_acs <- impute_cps_to_acs(d_acs, d_cps)
    
  }
  
  # sample ACS
  # user option to sample ACS data
  if (!is.null(sample_prop) & is.null(sample_num)) {
    d_acs <- sample_acs(d_acs, sample_prop=sample_prop, sample_num=NULL)  
  }
  if (is.null(sample_prop) & !is.null(sample_num)) {
    d_acs <- sample_acs(d_acs, sample_prop=NULL, sample_num=sample_num)  
  }
  if (!is.null(sample_prop) & !is.null(sample_num)) {
    d_acs <- sample_acs(d_acs, sample_prop=sample_prop, sample_num=sample_num)  
  }
  
  if (!is.null(progress_file)) {
    message <- paste0('{"type": "message", "engine": "', engine_type, '", "value": "Cleaned data files before CPS imputation."}')
    cat(message, file = progress_file, sep = "\n", append = TRUE)
    message <- paste0('{"type": "progress", "engine": "', engine_type, '", "value": 10}')
    cat(message, file = progress_file, sep = "\n", append = TRUE)
  }
   
  #========================================
  # 2. Pre-imputation 
  #========================================
  
  # General philosophy behind split between pre-ACS imputation and post-ACS imputation modifications:
  # Do as much as possible post-imputation, and impute as few FMLA variables as possible.
  
  # Pre-imputation: Alterations that are specific to FMLA variables. Mostly leave taking behavior.
  # Post-imputation: Alterations specific to ACS variables, or require both FMLA and ACS variables. 
  #                  Uptake behavior and benefit calculations.
  
  # define global values for use across functions
  leave_types <<- c("own","matdis","bond","illchild","illspouse","illparent")
  
  # preserve original copy of FMLA survey
  d_fmla_orig <- d_fmla 

  # adjust for program's base behavioral effect on leave taking
  # INPUT: FMLA data
  # In presence of program, apply leave-taking behavioral updates
  if (progalt_post_or_pre == 'pre') {
    d_fmla <-LEAVEPROGRAM(d_fmla, sens_var,dual_receiver)
  }

  if (!is.null(progress_file)) {
    message <- paste0('{"type": "message", "engine": "', engine_type, '", "value": "Applied leave-taking behavioral updates."}')
    cat(message, file = progress_file, sep = "\n", append = TRUE)
    message <- paste0('{"type": "progress", "engine": "', engine_type, '", "value": 20}')
    cat(message, file = progress_file, sep = "\n", append = TRUE)
  }
  # OUTPUT: FMLA data with adjusted take_leave columns to include 1s
  #         for those that would have taken leave if they could afford it
  
  #-----FMLA to ACS Imputation-----
  # filter/modify ACS data based on user specifications
  # INPUT: ACS Data
  d_acs <- acs_filtering(d_acs, weightfactor, place_of_work, state)
  # OUTPUT: Filtered ACS data
  # default is just simple nearest neighbor, K=1 
  # This is the big beast of getting leave behavior into the ACS.
  # INPUT: cleaned acs/fmla data, method for imputation, dependent variables 
  #         used for imputation
  d_acs_imp <- impute_fmla_to_acs(d_fmla,d_acs, impute_method, xvars, kval, xvar_wgts)  
  
  # OUTPUT: ACS data with imputed values for a) leave taking and needing, b) proportion of pay received from
  #         employer while on leave, and c) whether leave needed was not taken due to unaffordability 
  if (runtime_measure==1){
    time_elapsed('finished fmla to acs imputation')
  }

  if (!is.null(progress_file)) {
    message <- paste0('{"type": "message", "engine": "', engine_type, '", "value": "Finished FMLA to ACS imputation."}')
    cat(message, file = progress_file, sep = "\n", append = TRUE)
    message <- paste0('{"type": "progress", "engine": "', engine_type, '", "value": 80}')
    cat(message, file = progress_file, sep = "\n", append = TRUE)
  }

  # -------------Post-imputation functions-----------------
  # adjust for program's base behavioral effect on leave taking
  # INPUT: FMLA data
  # In presence of program, apply leave-taking behavioral updates
  if (progalt_post_or_pre == 'post') {
    d_acs_imp <-LEAVEPROGRAM(d_acs_imp, sens_var,dual_receiver)
  }
  
  if (runtime_measure==1){
    time_elapsed('finished LEAVEPROGRAM function')
  }
  
  # OUTPUT: FMLA data with adjusted take_leave columns to include 1s
  #         for those that would have taken leave if they could afford it
  
  # ---------------------------------------------------------------------------------------------------------
  # Impute Days of Leave Taken
  # ---------------------------------------------------------------------------------------------------------
  # INPUTS: unmodified FMLA survey as the training data, ACS as test data,
  #         and conditionals for filter the two sets. Using unmodified survey for leave lengths,
  #         since modified FMLA survey contains no new information about leave lengths, and 
  #         the intra-FMLA imputed leave lengths a random draw from that imputed data
  #         would produced a biased 
  #         estimate of leave length
  d_acs_imp <- impute_leave_length(d_fmla_orig, d_acs_imp, ext_resp_len,
                                   rr_sensitive_leave_len,base_bene_level,maxlen_DI,maxlen_PFL)
  # OUTPUT: ACS data with lengths for leaves imputed
  if (runtime_measure==1){
    time_elapsed('finished imputing leave length')
  }

  if (!is.null(progress_file)) {
    message <- paste0('{"type": "message", "engine": "', engine_type, '", "value": "Imputed leave lengths"}')
    cat(message, file = progress_file, sep = "\n", append = TRUE)
    message <- paste0('{"type": "progress", "engine": "', engine_type, '", "value": 90}')
    cat(message, file = progress_file, sep = "\n", append = TRUE)
  }
  # function interactions description (may not be complete, just writing as they come to me):
  # ELIGIBILITYRULES: eligibility rules defined, and participation initially set 
  # UPTAKE: Uptake probability values applied
  # BENEFITS: base values for benefits to be modified by other functions
  # BENEFITEFFECT: Overrides UPTAKE participation, but there are some classes of participants not affected by this
  # TOPOFF: TOPOFF overrides participation behavior of BENEFITEFFECT
  # DEPENDENTALLOWANCE: independent of other functions
  
  # Allow for users to clone ACS individuals 
  # INPUT: ACS file
  d_acs_imp <- CLONEFACTOR(d_acs_imp, clone_factor)
  if (runtime_measure==1){
    time_elapsed('finished clonefactor')
  }
  
  # OUTPUT: ACS file with user-specifed number of cloned records

  # Assign employer pay schedule for duration of leaves via imputation from Westat 2001 survey probabilities
  # Then, flag those who will have exhausted employer benefits with leave remaining, and will apply 
  # to leave program for remainder of their leave
  # INPUT: ACS file
  d_acs_imp <- PAY_SCHEDULE(d_acs_imp)
  if (runtime_measure==1){
    time_elapsed('applying pay schedule')
  }
  
  # OUTPUT: ACS file with imputed pay schedule, and date of benefit exhaustion for those with partial pay
  
  # Program eligibility and uptake functions
  # INPUT: ACS file
  d_acs_imp <-ELIGIBILITYRULES(d_acs_imp, earnings, weeks, ann_hours, minsize, base_bene_level, week_bene_min,
                               formula_prop_cuts, formula_value_cuts, formula_bene_levels, elig_rule_logic,
                               FEDGOV, STATEGOV, LOCALGOV, SELFEMP,PRIVATE,dual_receiver) 
  # OUTPUT: ACS file with program eligibility and base program take-up indicators
  if (runtime_measure==1){
    time_elapsed('finished applying eligbility rules')
  }
  
  # Option to extend leaves under leave program 
    # INPUT: ACS file
    d_acs_imp <- EXTENDLEAVES(d_fmla, d_acs_imp, wait_period, ext_base_effect, 
                              extend_prob, extend_days, extend_prop, fmla_protect)  
  if (runtime_measure==1){
    time_elapsed('finished applying ACM extended leaves')
  }
  
    # OUTPUT: ACS file with leaves extended based on user specifications

  # INPUT: ACS file
  d_acs_imp <-UPTAKE(d_acs_imp, own_uptake, matdis_uptake, bond_uptake, illparent_uptake, 
                     illspouse_uptake, illchild_uptake, full_particip, wait_period, wait_period_recollect,
                     maxlen_own, maxlen_matdis, maxlen_bond, maxlen_illparent, maxlen_illspouse, maxlen_illchild,
                     maxlen_total,maxlen_DI,maxlen_PFL,dual_receiver, min_takeup_cpl,alpha)
  # OUTPUT: ACS file with modified leave program variables based on user-specified program restrictions
  #         on maximum participation length and user-specified take-up rate assumptions
  
  if (runtime_measure==1){
    time_elapsed('finished applying uptake rates')
  }
  # benefit parameter functions
  # INPUT: ACS file
  d_acs_imp <- BENEFITS(d_acs_imp)
  if (runtime_measure==1){
    time_elapsed('finished calculating benefits')
  }
  
  # OUTPUT: ACS file with base employer pay and program benefit calculation variables

  # INPUT: ACS file
  d_acs_imp <- BENEFITEFFECT(d_acs_imp, bene_effect)
  # OUTPUT: ACS file with leave taking variables modified to account for behavioral cost of applying to program
  if (runtime_measure==1){
    time_elapsed('finished calculating benefit effect')
  }
  
  # INPUT: ACS file
  d_acs_imp <- TOPOFF(d_acs_imp,topoff_rate, topoff_minlength)
  # OUTPUT: ACS file with leave taking variables modified to account for employer top-off effects
  if (runtime_measure==1){
    time_elapsed('finished applying topoff effect')
  }
  
  # INPUT: ACS file
  d_acs_imp <- DEPENDENTALLOWANCE(d_acs_imp,dependent_allow)
  # OUTPUT: ACS file with program benefit amounts including a user-specified weekly dependent allowance

  # Apply type-specific elig adjustments 
  d_acs_imp <- DIFF_ELIG(d_acs_imp, own_elig_adj, illspouse_elig_adj, illchild_elig_adj,
                         illparent_elig_adj, matdis_elig_adj, bond_elig_adj)

  
  # final clean up 
  # INPUT: ACS file
  d_acs_imp <- CLEANUP(d_acs_imp, week_bene_cap,week_bene_cap_prop,week_bene_min, maxlen_own, maxlen_matdis, maxlen_bond, 
                       maxlen_illparent, maxlen_illspouse, maxlen_illchild, maxlen_total,maxlen_DI,maxlen_PFL)
  # OUTPUT: ACS file with finalized leave taking, program uptake, and benefits received variables
 
  # Running ABF module
  if (ABF_enabled==TRUE) {
    d_acs_imp <- run_ABF(d_acs_imp, ABF_elig_size, ABF_max_tax_earn, ABF_bene_tax, ABF_avg_state_tax, 
                     ABF_payroll_tax, ABF_bene, output,place_of_work,ABF_detail_out)
  }
  
  if (runtime_measure==1){
    time_elapsed('finished clean up')
  }
  
  
   # -----------obsolete code, now that we impute state of work ------
  # if using POW, adjust weights up by .02 because there are some missing POW
  # if (place_of_work==TRUE){
  #   replicate_weights <- paste0('PWGTP',seq(1,80))
  #   d_acs_imp['PWGTP'] <- d_acs_imp['PWGTP']*1.02
  #   for (i in replicate_weights) {
  #     d_acs_imp[i] <- d_acs_imp[i]*1.02
  #   }  
  # }
  
  # create meta file
  create_meta_file(d_acs_imp,out_dir,place_of_work)
  
  # final output options
  for (i in output_stats) {
    if (i=='standard') {
      standard_summary_stats(d_acs_imp,output, out_dir,place_of_work)
    }
    
    if (i=='state_compar') {
      state_compar_stats(d_acs_imp, output, out_dir,place_of_work)
    }
    
    if (i=='take_compar') {
      take_compar(d_acs_imp, output, out_dir,place_of_work)
    }  
  }
  
  # Options to output final data
  if (!is.null(output) & saveCSV==TRUE & fulloutput==TRUE) {
    write.csv(d_acs_imp, file=file.path(out_dir, paste0(output,'.csv'), fsep = .Platform$file.sep))
  }
  
  # Clean up vars for Python compatibility
  # drop intermediate, unneeded variables 
  for (i in c('ST.y','DI_plen', 'FER', 'OCC', 'PFL_plen', 'WKHP', 
              'actual_leave_pay', 'age_cat', 'base_benefits', 'base_leave_pay', 'bene_DI', 'bene_PFL', 'bene_effect_flg', 
              'benefit_prop', 'emppay_bond', 'emppay_illchild', 'emppay_illparent', 'emppay_illspouse', 'emppay_matdis', 
              'emppay_own', 'exhausted_by', 'extend_flag', 'faminc_cat', 'fem_c617', 'fem_cu6', 'fem_cu6and617', 'fem_nochild', 
              'fmla_constrain_flag', 'id', 'longer_leave', 'ndep_old', 'num_emp', 'orig_len_bond', 'orig_len_illchild', 'orig_len_illparent', 
              'orig_len_illspouse', 'orig_len_matdis', 'orig_len_own', 'pay_schedule', 'ptake_DI', 'ptake_PFL', 'squo_emppay_bond', 
              'squo_emppay_illchild', 'squo_emppay_illparent', 'squo_emppay_illspouse', 'squo_emppay_matdis', 'squo_emppay_own', 
              'squo_leave_pay', 'squo_take_bond', 'squo_take_illchild', 'squo_take_illparent', 'squo_take_illspouse', 'squo_take_matdis', 
              'squo_take_own', 'squo_taker', 'squo_total_length', 'state_abbr', 'state_name', 'topoff_flg', 'total_leaves', 'weeks_worked_cat',
              'takes_up_bond', 'takes_up_illchild', 'takes_up_illparent', 'takes_up_illspouse', 'takes_up_matdis', 'takes_up_own')) {
    if (i %in% names(d_acs_imp)){
      d_acs_imp[i] <- NULL
    }
  }
  
  
  # rename variables to be consistent with Python
  d_acs_imp <- d_acs_imp %>% rename(
    ST = ST.x, 
    annual_benefit_all = actual_benefits, 
    cfl_bond=length_bond ,
    cfl_illchild=length_illchild ,
    cfl_illparent=length_illparent ,
    cfl_illspouse=length_illspouse ,
    cfl_matdis=length_matdis ,
    cfl_own=length_own ,
    ln_faminc=lnfaminc ,
    takeup_any=particip ,
    cpl_all=particip_length ,
    cpl_bond=plen_bond ,
    cpl_illchild=plen_illchild ,
    cpl_illparent=plen_illparent ,
    cpl_illspouse=plen_illspouse ,
    cpl_matdis=plen_matdis ,
    cpl_own=plen_own ,
    takeup_bond=ptake_bond ,
    takeup_illchild=ptake_illchild ,
    takeup_illparent=ptake_illparent ,
    takeup_illspouse=ptake_illspouse ,
    takeup_matdis=ptake_matdis ,
    takeup_own=ptake_own ,
    len_bond=squo_length_bond ,
    len_illchild=squo_length_illchild ,
    len_illparent=squo_length_illparent ,
    len_illspouse=squo_length_illspouse ,
    len_matdis=squo_length_matdis ,
    len_own=squo_length_own ,
    cfl_all=total_length ,
  )
  
  if (!is.null(output) & saveCSV==TRUE) {
    write.csv(d_acs_imp, file=file.path(out_dir, paste0(output,'_py_compatible.csv'), fsep = .Platform$file.sep))
  }  
  
  
  print('=====================================')
  print('Simulation successfully completed')
  print('=====================================')
  if (!is.null(progress_file)) {
    message <- paste0('{"type": "message", "engine": "', engine_type, '", "value": "Output saved"}')
    cat(message, file = progress_file, sep = "\n", append = TRUE)
    message <- paste0('{"type": "progress", "engine": "', engine_type, '", "value": 100}')
    cat(message, file = progress_file, sep = "\n", append = TRUE)
    message <- paste0('{"type": "done", "engine": "', engine_type, '", "value": None}')
    cat(message, file = progress_file, sep = "\n", append = TRUE)
  }
  return(d_acs_imp)
}

