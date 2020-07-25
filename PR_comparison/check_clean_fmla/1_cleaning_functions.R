
# """
# File: 1_NEW_cleaning_functions
#
# These functions load in the raw ACS and FMLA files, creates the necessary variables
# for the simulation and saves a master dataset to be used in the simulations.
# 
# 9 Sept 2018
# Luke
# 
#
# """

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table of Contents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. clean_fmla
# 2. clean_acs
# 3. clean_cps
# 4. impute_cps_to_acs
# Subfunctions:    
  # 4a. runLogitEstimate - see 3_impute_functions.R, function 1Ba
  # 4b. runOrdinalEstimate
# 5. sample_acs


# ============================ #
# 1.clean_fmla
# ============================ #

clean_fmla <-function(d_fmla, save_csv=FALSE) {
  
  # --------------------------------------------------------------------
  # demographic characteristics
  # --------------------------------------------------------------------
  
  # FMLA eligible worker
  d_fmla <- d_fmla %>% mutate(fmla_eligworker = NA)
  d_fmla <- d_fmla %>% mutate(fmla_eligworker = ifelse(E13 == 1 & (E14 == 1 | (E15_CAT >= 5 & E15_CAT <= 8)),1,NA))
  
  # FMLA ineligible workers
  # E13 same job past year fails
  d_fmla <- d_fmla %>% mutate(fmla_eligworker = ifelse(is.na(E13)==FALSE & E13!=1, 0, fmla_eligworker))
  # E14 (FT) and E15 (hrs) fails
  d_fmla <- d_fmla %>% mutate(fmla_eligworker = ifelse(is.na(E14)==FALSE & E14!=1 & is.na(E15_CAT)==FALSE & (E15_CAT<5 | E15_CAT>8), 0, fmla_eligworker))
  
  # covered workplace
  d_fmla <- d_fmla %>% mutate(covwrkplace = ifelse(E11 == 1 | (E12 >= 6 & E12 <=9),1,0))
  d_fmla <- d_fmla %>% mutate(covwrkplace = ifelse(is.na(covwrkplace) ==  1,0,covwrkplace),
                                                cond1 = ifelse(is.na(E11) == 1 & is.na(E12) == 1,1,0),
                                                cond2 = ifelse(E11 == 2 & is.na(E11) == 0 & is.na(E12) == 1,1,0),
                                                miscond = ifelse(cond1 == 1 | cond2 == 1,1,0))
  d_fmla <- d_fmla %>% mutate(covwrkplace = ifelse(miscond == 1,NA,covwrkplace))
  
  # covered and eligible 
  d_fmla <-  d_fmla %>% mutate(coveligd = ifelse(covwrkplace == 1 & fmla_eligworker == 1,1,0))
  
  # hourly worker
  d_fmla <- d_fmla %>% mutate(hourly = ifelse(E9_1 == 2,1,0))
  
  # Hours worked per week at midpoint of category
  d_fmla <- d_fmla %>% mutate(wkhours = ifelse(E15_CAT_REV == 1,4,0))
  d_fmla <- d_fmla %>% mutate(wkhours = ifelse(E15_CAT_REV == 2,11.5,wkhours))
  d_fmla <- d_fmla %>% mutate(wkhours = ifelse(E15_CAT_REV == 3,17,wkhours))
  d_fmla <- d_fmla %>% mutate(wkhours = ifelse(E15_CAT_REV == 4,21.5,wkhours))
  d_fmla <- d_fmla %>% mutate(wkhours = ifelse(E15_CAT_REV == 5,26.5,wkhours))
  d_fmla <- d_fmla %>% mutate(wkhours = ifelse(E15_CAT_REV == 6,32,wkhours))
  d_fmla <- d_fmla %>% mutate(wkhours = ifelse(E15_CAT_REV == 7,37.5,wkhours))
  d_fmla <- d_fmla %>% mutate(wkhours = ifelse(E15_CAT_REV == 8,45,wkhours))
  
  # union member
  d_fmla <- d_fmla %>% mutate(union = ifelse(D3 == 1,1,0))
  
  # age at midpoint of category
  d_fmla <- d_fmla %>% mutate(age = ifelse(AGE_CAT == 1,21,NA))
  d_fmla <- d_fmla %>% mutate(age = ifelse(AGE_CAT == 2,27,age))
  d_fmla <- d_fmla %>% mutate(age = ifelse(AGE_CAT == 3,32,age))
  d_fmla <- d_fmla %>% mutate(age = ifelse(AGE_CAT == 4,37,age))
  d_fmla <- d_fmla %>% mutate(age = ifelse(AGE_CAT == 5,42,age))
  d_fmla <- d_fmla %>% mutate(age = ifelse(AGE_CAT == 6,47,age))
  d_fmla <- d_fmla %>% mutate(age = ifelse(AGE_CAT == 7,52,age))
  d_fmla <- d_fmla %>% mutate(age = ifelse(AGE_CAT == 8,57,age))
  d_fmla <- d_fmla %>% mutate(age = ifelse(AGE_CAT == 9,63.5,age))
  d_fmla <- d_fmla %>% mutate(age = ifelse(AGE_CAT == 10,70,age))
  d_fmla <- d_fmla %>% mutate(agesq = age^2)
  
  # make a coarser categorical age var
  d_fmla <- d_fmla %>% mutate(age_cat = ifelse(AGE_CAT >= 1 & AGE_CAT <= 4 , 1,NA))
  d_fmla <- d_fmla %>% mutate(age_cat = ifelse(AGE_CAT >= 5 & AGE_CAT <= 7, 2,age_cat))
  d_fmla <- d_fmla %>% mutate(age_cat = ifelse(AGE_CAT >= 8, 3,age_cat))
                                
  # government employment
  d_fmla <- d_fmla %>% mutate(empgov_fed = ifelse(D2 == 1,1,0))
  d_fmla <- d_fmla %>% mutate(empgov_st = ifelse(D2 == 2,1,0))
  d_fmla <- d_fmla %>% mutate(empgov_loc = ifelse(D2 == 3,1,0))
  
  # sex
  d_fmla <- d_fmla %>% mutate(male = ifelse(GENDER_CAT == 1,1,0),
                                                female = ifelse(GENDER_CAT == 2,1,0))
  
  # dependents
  d_fmla <- d_fmla %>% mutate(ndep_kid = D7_CAT)
  d_fmla <- d_fmla %>% mutate(ndep_old = D8_CAT)
  
  # no children
  d_fmla <- d_fmla %>% mutate(nochildren = ifelse(D7_CAT==0,1,0))  
  
  # educational level
  d_fmla <- d_fmla %>% mutate(ltHS = ifelse(D1_CAT == 1,1,0),
                                                someHS = ifelse(D1_CAT == 2,1,0),
                                                HSgrad = ifelse(D1_CAT == 3,1,0),
                                                someCol = ifelse(D1_CAT == 5,1,0),
                                                BA = ifelse(D1_CAT == 6,1,0),
                                                GradSch = ifelse(D1_CAT == 7,1,0),
                                                noHSdegree = ifelse(ltHS == 1 | someHS == 1,1,0),
                                                BAplus = ifelse((BA == 1 | GradSch == 1),1,0))
  
  # family income using midpoint of category
  d_fmla <- d_fmla %>% mutate(faminc = ifelse(D4_CAT == 3,15000,NA))
  d_fmla <- d_fmla %>% mutate(faminc = ifelse(D4_CAT == 4,25000,faminc))
  d_fmla <- d_fmla %>% mutate(faminc = ifelse(D4_CAT == 5,32500,faminc))
  d_fmla <- d_fmla %>% mutate(faminc = ifelse(D4_CAT == 6,37500,faminc))
  d_fmla <- d_fmla %>% mutate(faminc = ifelse(D4_CAT == 7,45000,faminc))
  d_fmla <- d_fmla %>% mutate(faminc = ifelse(D4_CAT == 8,62500,faminc))
  d_fmla <- d_fmla %>% mutate(faminc = ifelse(D4_CAT == 9,87500,faminc))
  d_fmla <- d_fmla %>% mutate(faminc = ifelse(D4_CAT == 10,130000,faminc))
  d_fmla <- d_fmla %>% mutate(lnfaminc = log(faminc))
  
  # Make more coarse categores
  d_fmla <- d_fmla %>% mutate(faminc_cat = ifelse(D4_CAT >= 3 & D4_CAT <= 5 , 1,NA))
  d_fmla <- d_fmla %>% mutate(faminc_cat = ifelse(D4_CAT >= 6 & D4_CAT <= 8, 2,faminc_cat))
  d_fmla <- d_fmla %>% mutate(faminc_cat = ifelse(D4_CAT >= 9, 3,faminc_cat))
  
  # marital status
  d_fmla <- d_fmla %>%  mutate(married = ifelse(D10 == 1,1,0),
                                                 partner = ifelse(D10 == 2,1,0),
                                                 separated = ifelse(D10 == 3,1,0),
                                                 divorced = ifelse(D10 == 4,1,0),
                                                 widowed = ifelse(D10 == 5,1,0),
                                                 nevermarried = ifelse(D10 == 6,1,0))
  
  # race/ethnicity
  d_fmla <- d_fmla %>% mutate(raceth = ifelse(is.na(D5) == 0 & D5 == 1,7,D6_1_CAT),
                                                native = ifelse(raceth == 1,1,0),
                                                asian = ifelse(raceth == 2,1,0),
                                                black = ifelse(raceth == 4,1,0),
                                                white = ifelse(raceth == 5,1,0),
                                                other = ifelse(raceth == 6,1,0),
                                                hisp = ifelse(raceth == 7,1,0))
  # id var
  d_fmla$id <- as.numeric(rownames(d_fmla))
  d_fmla <- d_fmla[order(d_fmla$id),]
  
  # --------------------------------------------------------------------
  # leave characteristics
  # --------------------------------------------------------------------
  
  # number of reasons leaves taken
  d_fmla <- d_fmla %>% mutate(num_leaves_take=A4a_CAT)
  d_fmla <- d_fmla %>% mutate(num_leaves_take=ifelse(is.na(num_leaves_take)== TRUE, 0 ,num_leaves_take))
  
  # number of reasons leaves needed
  d_fmla <- d_fmla %>% mutate(num_leaves_need=B5_CAT)
  d_fmla <- d_fmla %>% mutate(num_leaves_need=ifelse(is.na(num_leaves_need)== TRUE, 0 ,num_leaves_need))
  
  # leave reason for most recent leave
  d_fmla <- d_fmla %>% mutate(reason_take = ifelse(is.na(A20) == FALSE & A20 == 2,A5_2_CAT,A5_1_CAT))
  
    # leave reason for longest leave
  # 10/9, Luke: changing to be longest leave, regardless of if it is different from most recent leave or not
  d_fmla <- d_fmla %>% mutate(long_reason = A5_1_CAT)
  
  # old longest leave reason var
  #d_fmla <- d_fmla %>% mutate(long_reason = ifelse(is.na(A20) == FALSE & A20 == 2,A5_1_CAT,NA))
  #d_fmla <- d_fmla %>% mutate(long_reason = ifelse(long_reason==reason_take,NA,long_reason))
  
  # taken doctor
  d_fmla <- d_fmla %>% mutate(YNdoctor_take = ifelse(is.na(A20) == FALSE & A20 == 2,A11_2,A11_1),
                                                doctor_take = ifelse(YNdoctor_take == 1,1,0))
  d_fmla <- d_fmla %>% mutate(doctor_take = ifelse(is.na(YNdoctor_take), NA, doctor_take))
  
  # taken hospital
  d_fmla <- d_fmla %>% mutate(YNhospital_take = ifelse(is.na(A20) == FALSE & A20 == 2, A12_2, A12_1),
                                                hospital_take = ifelse(YNhospital_take == 1, 1, 0))
  d_fmla <- d_fmla %>% mutate(hospital_take = ifelse(is.na(YNhospital_take), NA, hospital_take))
  d_fmla <- d_fmla %>% mutate(hospital_take = ifelse(is.na(hospital_take) == TRUE & doctor_take == 0, 0, hospital_take))
  
  # need doctor
  d_fmla <- d_fmla %>% mutate(doctor_need = ifelse(B12_1 == 1, 1, 0))
  d_fmla <- d_fmla %>% mutate(doctor_need = ifelse(is.na(B12_1), NA, doctor_need))
  
  # need hospital
  d_fmla <- d_fmla %>% mutate(hospital_need = ifelse(B13_1 == 1, 1, 0))
  d_fmla <- d_fmla %>% mutate(hospital_need = ifelse(is.na(B13_1), NA, hospital_need))
  d_fmla <- d_fmla %>% mutate(hospital_need = ifelse(is.na(hospital_need) == TRUE & doctor_need == 0, 0, hospital_need))
  
  # taken or needed doctor or hospital for leave category
  d_fmla <- d_fmla %>% mutate(doctor = pmax(doctor_need, doctor_take, na.rm=TRUE))
  d_fmla <- d_fmla %>% mutate(doctor1 = ifelse(is.na(LEAVE_CAT) == FALSE & LEAVE_CAT == 2, doctor_need, doctor_take))
  d_fmla <- d_fmla %>% mutate(doctor2 = ifelse(is.na(LEAVE_CAT) == FALSE & (LEAVE_CAT == 2 | LEAVE_CAT == 4), doctor_need, doctor_take))  
  
  d_fmla <- d_fmla %>% mutate(hospital = pmax(hospital_need, hospital_take, na.rm=TRUE))
  d_fmla <- d_fmla %>% mutate(hospital1 = ifelse(is.na(LEAVE_CAT) == FALSE & LEAVE_CAT == 2, hospital_need, hospital_take))
  d_fmla <- d_fmla %>% mutate(hospital2 = ifelse(is.na(LEAVE_CAT) == FALSE & (LEAVE_CAT == 2 | LEAVE_CAT == 4), hospital_need, hospital_take))  
  
  # length of leave for most recent leave
  # take mid points in days of categorical leave length questions 
  # round up/down on tie breaks in alternating fashion
  d_fmla <- d_fmla %>% mutate(A19_1_vals=0)
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==1,1,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==2,2,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==3,3,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==4,4,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==5,5,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==6,6,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==7,7,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==8,8,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==9,9,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==10,10,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==11,12,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==12,13,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==13,15,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==14,18,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==15,20,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==16,22,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==17,27,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==18,30,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==19,33,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==20,38,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==21,43,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==22,48,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==23,53,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==24,58,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==25,66,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==26,80,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==27,106,A19_1_vals))
  d_fmla <- d_fmla %>% mutate(A19_1_vals=ifelse(is.na(A19_1_CAT)==0 & A19_1_CAT==28,191,A19_1_vals))
  
  d_fmla <- d_fmla %>% mutate(A19_2_vals=0)
  d_fmla <- d_fmla %>% mutate(A19_2_vals=ifelse(is.na(A19_2_vals)==0 & A19_2_vals==1,1,A19_2_vals))
  d_fmla <- d_fmla %>% mutate(A19_2_vals=ifelse(is.na(A19_2_vals)==0 & A19_2_vals==2,2,A19_2_vals))
  d_fmla <- d_fmla %>% mutate(A19_2_vals=ifelse(is.na(A19_2_vals)==0 & A19_2_vals==3,3,A19_2_vals))
  d_fmla <- d_fmla %>% mutate(A19_2_vals=ifelse(is.na(A19_2_vals)==0 & A19_2_vals==4,4,A19_2_vals))
  d_fmla <- d_fmla %>% mutate(A19_2_vals=ifelse(is.na(A19_2_vals)==0 & A19_2_vals==5,5,A19_2_vals))
  d_fmla <- d_fmla %>% mutate(A19_2_vals=ifelse(is.na(A19_2_vals)==0 & A19_2_vals==6,8,A19_2_vals))
  d_fmla <- d_fmla %>% mutate(A19_2_vals=ifelse(is.na(A19_2_vals)==0 & A19_2_vals==7,10,A19_2_vals))
  d_fmla <- d_fmla %>% mutate(A19_2_vals=ifelse(is.na(A19_2_vals)==0 & A19_2_vals==8,15,A19_2_vals))
  d_fmla <- d_fmla %>% mutate(A19_2_vals=ifelse(is.na(A19_2_vals)==0 & A19_2_vals==9,41,A19_2_vals))
  d_fmla <- d_fmla %>% mutate(A19_2_vals=ifelse(is.na(A19_2_vals)==0 & A19_2_vals==10,90,A19_2_vals))
  
  d_fmla <- d_fmla %>% mutate(length = ifelse(is.na(A20) == FALSE & A20 == 2, A19_2_vals, A19_1_vals))
  # 10/9, Luke: changing to be longest leave, regardless of if it is different from most recent leave or not
  d_fmla <- d_fmla %>% mutate(long_length = A19_1_vals)
  # top code length to be 261 days - number of days in a working year
  d_fmla <- d_fmla %>% mutate(length= ifelse(length>261,261,length))
  d_fmla <- d_fmla %>% mutate(long_length= ifelse(long_length>261,261,long_length))
  
  
  # old longest leave length code
  # d_fmla <- d_fmla %>% mutate(long_length = ifelse(is.na(A20) == FALSE & A20 == 2, A19_1_CAT_rev, NA))
  # d_fmla <- d_fmla %>% mutate(long_length = ifelse(long_reason==reason_take,NA,long_length))
  
  d_fmla <- d_fmla %>% mutate(lengthsq = length^2,
                                                lnlength = log(length),
                                                lnlengthsq = lnlength^2)
  
  # --------------------------
  # Benefits and pay received
  # --------------------------
  
  # fully paid
  d_fmla <- d_fmla %>% mutate(fullyPaid = ifelse(A49 == 1, 1, 0))
  
  # longer leave if more pay
  d_fmla <- d_fmla %>% mutate(longerLeave = ifelse(A55 == 1, 1, 0))
  
  # could not afford to take leave
  d_fmla <- d_fmla %>% mutate(unaffordable = ifelse(B15_1_CAT == 5, 1, 0))
  
  # Resp_len - more nuanced unaffordable variable used in the Python implementation
  # LEAVE_CAT: employed only
  # EMPLOYED ONLY workers have no need and take no leave, would not respond anyway
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse(LEAVE_CAT == 3, 0, NA))
  
  # A55 asks if worker would take longer leave if paid?
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & A55 == 2, 0, resp_len))
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & A55 == 1, 1, resp_len))
  
  # The following variables indicate whether leave was cut short for financial issues
  # A23c: unable to afford unpaid leave due to leave taking
  # A53g: cut leave time short to cover lost wages
  # A62a: return to work because cannot afford more leaves
  # B15_1_CAT, B15_2_CAT: can't afford unpaid leave
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & A23c == 1, 1, resp_len))
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & A53g == 1, 1, resp_len))
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & A62a == 1, 1, resp_len))
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & B15_1_CAT == 5, 1, resp_len))
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & B15_2_CAT == 5, 1, resp_len))

  # B15_1_CAT and B15_2_CAT only has one group (cod = 5) identified as constrained by unaffordability
  # These financially-constrained workers were assigned resp_len=1 above, all other cornered workers would not respond
  # Check reasons of no leave among rest: d[d['resp_len'].isna()].B15_1_CAT.value_counts().sort_index()
  # all reasons unsolved by replacement generosity
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & A23c == 2, 0, resp_len))
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & A53g == 2, 0, resp_len))
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & A62a == 2, 0, resp_len))
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & (!is.na(B15_1_CAT)), 0, resp_len))
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & (!is.na(B15_2_CAT)), 0, resp_len))
  
  # Assume all takers/needers with ongoing condition are 'cornered' and would respond with longer leaves
  # A10_1, A10_2: regular/ongoing condition, takers and dual
  # B11_1, B11_2: regular/ongoing condition, needers and dual
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)) & ((
    A10_1 == 2) | (A10_1 == 3) | (B11_1 == 2) | (B11_1 == 3)), 1, resp_len))


  # Check LEAVE_CAT of rest: 
  #table(d_fmla$resp_len, d_fmla$LEAVE_CAT, useNA = 'always')
  # 3 takers, 3 needers still remain
  # for set to sensitive to be conservative
  d_fmla <- d_fmla %>% mutate(resp_len = ifelse((is.na(resp_len)), 1, resp_len))
  
  # any pay received
  d_fmla <- d_fmla %>% mutate(anypay = ifelse(A45 == 1, 1, 0))
  
  # proportion of pay received from employer (mid point of ranges provided in FMLA)
  d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(A50 == 1, .125, NA))
  d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(A50 == 2, .375, prop_pay))
  d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(A50 == 3, .5, prop_pay))
  d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(A50 == 4, .625, prop_pay))
  d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(A50 == 5, .875, prop_pay))
  d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(A49 == 1, 1, prop_pay))
  d_fmla <- d_fmla %>% mutate(prop_pay = ifelse(A45 == 2, NA, prop_pay))
  
  # Adding values in leave program variables for starting condition (absence of program)
  # Leave Program Participation
  # baseline is absence of program, so this will start as a nonparticipant
  d_fmla  <- d_fmla  %>% mutate(particip = 0)
  
  # Benefits received as proportion of pay
  # baseline is employer-provided pay: starting at 0, will be imputed
  d_fmla  <- d_fmla  %>% mutate(benefit_prop = 0)
  
  # Cost to program as proportion of pay
  # baseline is 0
  d_fmla  <- d_fmla  %>% mutate(cost_prop = 0)
  
  # state program
  d_fmla <- d_fmla %>% mutate(recStateFL = ifelse(A48b == 1, 1, 0))
  d_fmla <- d_fmla %>% mutate(recStateFL = ifelse(is.na(recStateFL) == TRUE & anypay == 0, 0, recStateFL))
  
  d_fmla <- d_fmla %>% mutate(recStateDL = ifelse(A48c == 1, 1, 0))
  d_fmla <- d_fmla %>% mutate(recStateDL = ifelse(is.na(recStateDL) == TRUE & anypay == 0, 0, recStateDL))
  
  d_fmla <- d_fmla %>% mutate(recStatePay = ifelse(recStateFL == 1 | recStateDL == 1, 1, 0))
  
  # weights
  w_emp <- d_fmla %>% filter(LEAVE_CAT == 3) %>% summarise(w_emp = mean(weight))
  w_leave <- d_fmla %>% filter(LEAVE_CAT != 3) %>% summarise(w_leave = mean(weight))
  
  d_fmla <- d_fmla %>% mutate(fixed_weight = ifelse(LEAVE_CAT == 3, w_emp, w_leave),
                                                freq_weight = round(weight))
  
  d_fmla <- d_fmla %>% mutate(fixed_weight = unlist(fixed_weight))
  
  # --------------------------
  # dummies for leave type 
  # -------------------------- 
  
  # there are four variables for each leave type for most recent leave:
  # (1) taking a leave - take_*
  # (2) needing a leave - need_*
  # (3) taking or needing a leave - type_*
  # (4) length of most recent leave - length_*
  
  # also need to know if the longest leave is that type - long_*
  # Length of above leave - longlength_*
  
  
  # maternity disability
  d_fmla <- d_fmla %>% mutate(take_matdis = ifelse((A5_1_CAT == 21 & A11_1 == 1 & GENDER_CAT == 2) & (A20 != 2 | is.na(A20) == TRUE) , 1, 0))
  d_fmla <- d_fmla %>% mutate(take_matdis = ifelse((A5_2_CAT == 21 & A11_2 == 1 & GENDER_CAT == 2) & (A20 == 2 & is.na(A20)== FALSE) , 1, take_matdis))
  d_fmla <- d_fmla %>% mutate(take_matdis = ifelse(is.na(take_matdis) == 1,0,take_matdis))
  d_fmla <- d_fmla %>% mutate(take_matdis = ifelse(is.na(A5_1_CAT) == 1 & is.na(A5_2_CAT) == 1, NA, take_matdis))
  d_fmla <- d_fmla %>% mutate(take_matdis = ifelse(is.na(take_matdis) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0, take_matdis))
  d_fmla <- d_fmla %>% mutate(take_matdis = ifelse(is.na(take_matdis) == 1 & GENDER_CAT == 1,0, take_matdis))
  
  d_fmla <- d_fmla %>% mutate(long_matdis = ifelse(A5_1_CAT == 21 & A11_1 == 1 & GENDER_CAT == 2, 1, 0))
  d_fmla <- d_fmla %>% mutate(long_matdis = ifelse(is.na(long_matdis) == 1,0,long_matdis))
  d_fmla <- d_fmla %>% mutate(long_matdis = ifelse(is.na(A5_1_CAT) == 1 & is.na(A5_2_CAT) == 1, NA, long_matdis))
  d_fmla <- d_fmla %>% mutate(long_matdis = ifelse(is.na(long_matdis) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0, long_matdis))
  
  d_fmla <- d_fmla %>% mutate(need_matdis = ifelse(B6_1_CAT == 21 & B12_1 == 1 & GENDER_CAT == 2, 1, 0))
  d_fmla <- d_fmla %>% mutate(need_matdis = ifelse(is.na(need_matdis) == 1,0,need_matdis))
  d_fmla <- d_fmla %>% mutate(need_matdis = ifelse(is.na(B6_1_CAT) == 1, NA, need_matdis))
  d_fmla <- d_fmla %>% mutate(need_matdis = ifelse(is.na(need_matdis) == 1 & (LEAVE_CAT == 1 | LEAVE_CAT == 3),0, need_matdis))
  d_fmla <- d_fmla %>% mutate(need_matdis = ifelse(is.na(need_matdis) == 1 & GENDER_CAT == 1,0, need_matdis))
  
  d_fmla <- d_fmla %>% mutate(type_matdis = ifelse((take_matdis == 1 | need_matdis == 1),1,0))
  d_fmla <- d_fmla %>% mutate(type_matdis = ifelse((is.na(take_matdis) == 1 | is.na(need_matdis) == 1),NA,type_matdis))
  
  d_fmla <- d_fmla %>% mutate(length_matdis = ifelse(take_matdis==1,length, 0))
  d_fmla <- d_fmla %>% mutate(longlength_matdis = ifelse(long_matdis==1,long_length, 0))
  
  # new child/bond
  # d_fmla <- d_fmla %>% mutate(take_bond = ifelse(A5_1_CAT==21 & (is.na(A11_1) == 1 | GENDER_CAT == 1 | (GENDER_CAT==2 & A11_1==2 & A5_1_CAT_rev!=32))
  #                                                & (A20 != 2|is.na(A20)==TRUE),1,0))
  # d_fmla <- d_fmla %>% mutate(take_bond = ifelse(A5_2_CAT==21 & (is.na(A11_2) == 1 | GENDER_CAT == 1 | (GENDER_CAT==2 & A11_2==2 & A5_2_CAT_REV!=32)) 
  #                                                & (A20 == 2 & is.na(A20)==FALSE),1,take_bond))
  d_fmla <- d_fmla %>% mutate(take_bond = ifelse((is.na(take_matdis)==1 | take_matdis == 0) & A5_1_CAT == 21 & (is.na(A11_1) == 1 | A11_1==2) & (is.na(A20)==1 | A20 != 2),1,0))
  d_fmla <- d_fmla %>% mutate(take_bond = ifelse((is.na(take_matdis)==1 | take_matdis == 0) & (is.na(A5_2_CAT)==0 & A5_2_CAT == 21) & 
                                                  (is.na(A11_2) == 1 | A11_2==2) & (is.na(A20)==0 | A20 == 2),1,take_bond))
  
  d_fmla <- d_fmla %>% mutate(take_bond = ifelse(is.na(A5_1_CAT) == 1, NA, take_bond))
  d_fmla <- d_fmla %>% mutate(take_bond = ifelse(is.na(take_bond) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0, take_bond))
  
  d_fmla <- d_fmla %>% mutate(long_bond = ifelse((is.na(long_matdis)==1 | long_matdis == 0) & A5_1_CAT==21 & (is.na(A11_1) == 1 | A11_1==2),1,0))
  d_fmla <- d_fmla %>% mutate(long_bond = ifelse(is.na(A5_1_CAT) == 1, NA, long_bond))
  d_fmla <- d_fmla %>% mutate(long_bond = ifelse(is.na(long_bond) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0, long_bond))
  
  d_fmla <- d_fmla %>% mutate(need_bond = ifelse((is.na(need_matdis) | need_matdis == 0) & (B6_1_CAT == 21 & B12_1 == 2),1,0))
  d_fmla <- d_fmla %>% mutate(need_bond = ifelse(is.na(B6_1_CAT) == 1, NA, need_bond))
  d_fmla <- d_fmla %>% mutate(need_bond = ifelse(is.na(need_bond) == 1 & (LEAVE_CAT == 1 | LEAVE_CAT == 3),0, need_bond))
  
  d_fmla <- d_fmla %>% mutate(type_bond = ifelse((take_bond == 1 | need_bond == 1),1,0))
  d_fmla <- d_fmla %>% mutate(type_bond = ifelse((is.na(take_bond) == 1 | is.na(need_bond) == 1),NA,type_bond))
  
  d_fmla <- d_fmla %>% mutate(length_bond = ifelse(take_bond==1,length, 0))
  d_fmla <- d_fmla %>% mutate(longlength_bond = ifelse(long_bond==1,long_length, 0))
  
  #odie = d_fmla %>% select(take_bond)
  # own health
  d_fmla <- d_fmla %>% mutate(take_own = ifelse(reason_take == 1,1,0))
  d_fmla <- d_fmla %>% mutate(take_own = ifelse(is.na(take_own) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,take_own))
  
  d_fmla <- d_fmla %>% mutate(long_own = ifelse(long_reason == 1,1,0))
  d_fmla <- d_fmla %>% mutate(long_own = ifelse(is.na(long_own) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,long_own))
  
  d_fmla <- d_fmla %>% mutate(need_own = ifelse(B6_1_CAT == 1,1,0))
  # # some more needers of this type are in the 2nd loop
  # d_fmla <- d_fmla %>% mutate(need_own = ifelse(B6_2_CAT == 1,1,need_own))
  d_fmla <- d_fmla %>% mutate(need_own = ifelse(is.na(need_own)==1 & (LEAVE_CAT == 1 | LEAVE_CAT == 3),0,need_own))
  
  d_fmla <- d_fmla %>% mutate(type_own = ifelse((take_own == 1 | need_own == 1),1,0))
  d_fmla <- d_fmla %>% mutate(type_own = ifelse((is.na(take_own) == 1 | is.na(need_own) == 1),NA,type_own))
  
  d_fmla <- d_fmla %>% mutate(length_own = ifelse(take_own==1,length, 0))
  d_fmla <- d_fmla %>% mutate(longlength_own = ifelse(long_own==1,long_length, 0))
  
  #ill child
  d_fmla <- d_fmla %>% mutate(take_illchild = ifelse(reason_take == 11,1,0))
  d_fmla <- d_fmla %>% mutate(take_illchild = ifelse(is.na(take_illchild) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,take_illchild))
  
  d_fmla <- d_fmla %>% mutate(long_illchild = ifelse(long_reason == 11,1,0))
  d_fmla <- d_fmla %>% mutate(long_illchild = ifelse(is.na(long_illchild) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,long_illchild))
  
  d_fmla <- d_fmla %>% mutate(need_illchild = ifelse(B6_1_CAT == 11,1,0))
  # # some more needers of this type are in the 2nd loop
  # d_fmla <- d_fmla %>% mutate(need_illchild = ifelse(B6_2_CAT == 11,1,need_illchild))
  d_fmla <- d_fmla %>% mutate(need_illchild = ifelse(is.na(need_illchild) == 1 & (LEAVE_CAT == 1 | LEAVE_CAT == 3),0,need_illchild))
  
  d_fmla <- d_fmla %>% mutate(type_illchild = ifelse((take_illchild == 1 | need_illchild == 1),1,0))
  d_fmla <- d_fmla %>% mutate(type_illchild = ifelse((is.na(take_illchild) == 1 | is.na(need_illchild) == 1),NA,type_illchild))
  
  d_fmla <- d_fmla %>% mutate(length_illchild = ifelse(take_illchild==1,length, 0))
  d_fmla <- d_fmla %>% mutate(longlength_illchild = ifelse(long_illchild==1,long_length, 0))
  
  #ill spouse
  d_fmla <- d_fmla %>% mutate(take_illspouse = ifelse(reason_take == 12,1,0))
  d_fmla <- d_fmla %>% mutate(take_illspouse = ifelse(is.na(take_illspouse) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,take_illspouse))
  d_fmla <- d_fmla %>% mutate(take_illspouse = ifelse(is.na(take_illspouse) == 1 & (nevermarried == 1 | 
                                                                                    separated == 1 |
                                                                                    divorced == 1 |
                                                                                    widowed == 1),0,take_illspouse))
  
  d_fmla <- d_fmla %>% mutate(long_illspouse = ifelse(long_reason == 12,1,0))
  d_fmla <- d_fmla %>% mutate(long_illspouse = ifelse(is.na(long_illspouse) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,long_illspouse))
  
  d_fmla <- d_fmla %>% mutate(need_illspouse = ifelse(B6_1_CAT == 12,1,0))
  d_fmla <- d_fmla %>% mutate(need_illspouse = ifelse(is.na(need_illspouse) == 1 & (LEAVE_CAT == 1 | LEAVE_CAT == 3),0,need_illspouse))
  d_fmla <- d_fmla %>% mutate(need_illspouse = ifelse(is.na(need_illspouse) == 1 & (nevermarried == 1 | 
                                                                                      separated == 1 |
                                                                                      divorced == 1 |
                                                                                      widowed == 1),0,need_illspouse))
  
  d_fmla <- d_fmla %>% mutate(type_illspouse = ifelse((take_illspouse == 1 | need_illspouse == 1),1,0))
  d_fmla <- d_fmla %>% mutate(type_illspouse = ifelse((is.na(take_illspouse) == 1 | is.na(need_illspouse) == 1),NA,type_illspouse))
  
  d_fmla <- d_fmla %>% mutate(length_illspouse = ifelse(take_illspouse==1,length, 0))
  d_fmla <- d_fmla %>% mutate(longlength_illspouse = ifelse(long_illspouse==1,long_length, 0))
  
  #ill parent
  d_fmla <- d_fmla %>% mutate(take_illparent = ifelse(reason_take == 13,1,0))
  d_fmla <- d_fmla %>% mutate(take_illparent = ifelse(is.na(take_illparent) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,take_illparent))
  
  d_fmla <- d_fmla %>% mutate(long_illparent = ifelse(long_reason == 13,1,0))
  d_fmla <- d_fmla %>% mutate(long_illparent = ifelse(is.na(long_illparent) == 1 & (LEAVE_CAT == 2 | LEAVE_CAT == 3),0,long_illparent))
  
  d_fmla <- d_fmla %>% mutate(need_illparent = ifelse(B6_1_CAT == 13,1,0))
  d_fmla <- d_fmla %>% mutate(need_illparent = ifelse(is.na(need_illparent) == 1 & (LEAVE_CAT == 1 | LEAVE_CAT == 3),0,need_illparent))
  
  d_fmla <- d_fmla %>% mutate(type_illparent = ifelse((take_illparent == 1 | need_illparent == 1),1,0))
  d_fmla <- d_fmla %>% mutate(type_illparent = ifelse((is.na(take_illparent) == 1 | is.na(need_illparent) == 1),NA,type_illparent))
  
  d_fmla <- d_fmla %>% mutate(length_illparent = ifelse(take_illparent==1,length, 0))
  d_fmla <- d_fmla %>% mutate(longlength_illparent = ifelse(long_illparent==1,long_length, 0))
  
  # taking or needing any leave
  leave_types <- c("own","illspouse","illchild","illparent","matdis","bond")
  d_fmla['taker']=rowSums(d_fmla[,paste('take',c("own","illspouse","illchild","illparent","matdis","bond"),sep="_")], na.rm=TRUE)
  d_fmla['needer']=rowSums(d_fmla[,paste('need',c("own","illspouse","illchild","illparent","matdis","bond"),sep="_")], na.rm=TRUE)
  d_fmla <- d_fmla %>% mutate(taker=ifelse(taker>=1, 1, 0))
  d_fmla <- d_fmla %>% mutate(needer=ifelse(needer>=1, 1, 0))
  
  # saving data
  if (save_csv==TRUE) {
    write.csv(d_fmla, file = "fmla_clean_2012.csv", row.names = FALSE)  
  }

  return(d_fmla)
}

# ============================ #
# 2. clean_acs
# ============================ #


# -------------------------- #
# ACS Household File
# -------------------------- #
clean_acs <-function(d,d_hh,save_csv=FALSE) {

  # create variables
  
  d_hh$nochildren <- as.data.frame(dummy("FPARC",d_hh))$FPARC4
  # adjust to 2012 dollars to conform with FMLA 2012 data
  # don't multiple ADJINC directly to faminc to avoid integer overflow issue in R
  d_hh$adjinc_2012 <- d_hh$ADJINC / 1042852 
  d_hh$faminc <- d_hh$FINCP * d_hh$adjinc_2012 
  d_hh <- d_hh %>% mutate(faminc=ifelse(is.na(faminc)==FALSE & faminc<=0, 0.01, faminc)) # force non-positive income to be epsilon to get meaningful log-income
  d_hh$lnfaminc <- log(d_hh$faminc)
  
  # number of dependents
  d_hh$ndep_kid <- d_hh$NOC
  d_hh$ndep_old <- d_hh$R65
  
  # cut down vars to save on memory
  d_hh <- d_hh[c("SERIALNO","nochildren","lnfaminc","faminc", "PARTNER","ndep_kid","ndep_old",'NPF')]
  
  # -------------------------- #
  # ACS Person File
  # -------------------------- #

  # merge with household level vars 
  d <- merge(d,d_hh, by="SERIALNO")
  rm(d_hh)
  # rename ACS vars to be consistent with FMLA data
  d$age <- d$AGEP
  d$a_age <- d$AGEP
  
  # create new ACS vars
  
  
  # marital status
  # marital status
  d <- d %>% mutate(married=ifelse(MAR==1, 1, 0))
  d <- d %>% mutate(widowed=ifelse(MAR==2, 1, 0))
  d <- d %>% mutate(divorced=ifelse(MAR==3, 1, 0))
  d <- d %>% mutate(separated=ifelse(MAR==4, 1, 0))
  d <- d %>% mutate(nevermarried=ifelse(MAR==5, 1, 0))
  
  # use PARTNER in household data to tease out unmarried partners
  d <- d %>% mutate(PARTNER=ifelse(is.na(PARTNER),0,PARTNER)) 
  d <- d %>% mutate(partner=ifelse(PARTNER==1 | PARTNER==2 | PARTNER==3 | PARTNER==4, 1, 0))
  d <- d %>% mutate(married=ifelse(partner==1, 0, married))
  d <- d %>% mutate(widowed=ifelse(partner==1, 0, widowed))
  d <- d %>% mutate(divorced=ifelse(partner==1, 0, divorced))
  d <- d %>% mutate(separated=ifelse(partner==1, 0, separated))
  d <- d %>% mutate(nevermarried=ifelse(partner==1, 0, nevermarried))
  
  #gender
  d <- d %>% mutate(male=ifelse(SEX==1, 0, nevermarried))
  d$female <- 1-d$male
  
  #age
  d$agesq <- d$age ** 2
  
  # coarse age categories
  d <- d %>% mutate(age_cat = ifelse(age <= 39,1,NA))
  d <- d %>% mutate(age_cat = ifelse(age <= 59 & age >= 40,2,age_cat))
  d <- d %>% mutate(age_cat = ifelse(age >= 60, 3,age_cat))
  # d <- d %>% mutate(age_cat = ifelse(age <= 18 & age >= 24,21,NA))
  # d <- d %>% mutate(age_cat = ifelse(age <= 25 & age >=29,27,age_cat))
  # d <- d %>% mutate(age_cat = ifelse(age <= 30 & age >=34,32,age_cat))
  # d <- d %>% mutate(age_cat = ifelse(age <= 35 & age >=39,37,age_cat))
  # d <- d %>% mutate(age_cat = ifelse(age <= 40 & age >=44,42,age_cat))
  # d <- d %>% mutate(age_cat = ifelse(age <= 45 & age >=49,47,age_cat))
  # d <- d %>% mutate(age_cat = ifelse(age <= 50 & age >=54,52,age_cat))
  # d <- d %>% mutate(age_cat = ifelse(age <= 55 & age >=59,57,age_cat))
  # d <- d %>% mutate(age_cat = ifelse(age <= 60 & age >=67,63.5,age_cat))
  # d <- d %>% mutate(age_cat = ifelse(age >= 68 ,70,age_cat))
  
  # ed level
  d <- d %>% mutate(SCHL=ifelse(is.na(SCHL),0,SCHL)) 
  d <- d %>% mutate(ltHS=ifelse(SCHL<=15,1,0)) 
  d <- d %>% mutate(HSgrad=ifelse(SCHL==16 | SCHL==17 ,1,0)) 
  d <- d %>% mutate(someCol=ifelse(SCHL>=18 & SCHL<=20,1,0)) 
  d <- d %>% mutate(BA =ifelse(SCHL==21,1,0)) 
  d <- d %>% mutate(GradSch=ifelse(SCHL>=22,1,0)) 
  # coarser ed groups
  d <- d %>% mutate(noHSdegree=ifelse(SCHL<=15,1,0))
  d <- d %>% mutate(BAplus=ifelse(SCHL>=21,1,0))
  
  #race
  d <- d %>% mutate(hisp=ifelse(HISP>=2,1,0)) 
  d <- d %>% mutate(black=ifelse(RAC1P==2 & hisp==0,1,0)) 
  d <- d %>% mutate(white=ifelse(RAC1P==1 & hisp==0,1,0)) 
  d <- d %>% mutate(asian=ifelse(RAC1P==6 & hisp==0,1,0))
  d <- d %>% mutate(native=ifelse((RAC1P==3 | RAC1P==4 | RAC1P==5 | RAC1P==7) & hisp==0,1,0))
  d <- d %>% mutate(other=ifelse(white==0 & black==0 & asian==0 & native==0 & hisp==0,1,0))
  
  # Employement, and government employement
  d <- d %>% mutate(employed=ifelse(ESR==1 | ESR==2 | ESR==4 | ESR==5, 1, 0))
  d <- d %>% mutate(employed=ifelse(is.na(ESR)==TRUE, NA, employed))
  d <- d %>% mutate(empgov_fed=ifelse(COW==5, 1, 0))
  d <- d %>% mutate(empgov_fed=ifelse(is.na(COW)==TRUE, NA, empgov_fed))
  d <- d %>% mutate(empgov_st=ifelse(COW==4, 1, 0))
  d <- d %>% mutate(empgov_st=ifelse(is.na(COW)==TRUE, NA, empgov_st))  
  d <- d %>% mutate(empgov_loc=ifelse(COW==3, 1, 0))
  d <- d %>% mutate(empgov_loc=ifelse(is.na(COW)==TRUE, NA, empgov_loc))
  
  # occupation
  # since there is an actual OCCP variable in ACS file, going to use OCC as our varname going forward
  
  if (!is.null(d$OCCP)) {
    d <- d %>% mutate(OCC = OCCP)  
  }
  
  
  if (!is.null(d$OCCP10)) {
    d <- d %>% mutate(OCC = OCCP10)  
  }
  
  if (!is.null(d$OCCP12)) {
    d <- d %>% mutate(OCC = OCCP12)  
  }
  

  if (is.factor(d['OCC'])) {
    d['OCC'] <- unfactor(d['OCC'])
  }

  d <- d %>% mutate(occ_1 = ifelse(OCC>=10 & OCC<=950,1,0),
                    occ_2 = ifelse(OCC>=1000 & OCC<=3540,1,0),
                    occ_3 = ifelse(OCC>=3600 & OCC<=4650,1,0),
                    occ_4 = ifelse(OCC>=4700 & OCC<=4965,1,0),
                    occ_5 = ifelse(OCC>=5000 & OCC<=5940,1,0),
                    occ_6 = ifelse(OCC>=6000 & OCC<=6130,1,0),
                    occ_7 = ifelse(OCC>=6200 & OCC<=6940,1,0),
                    occ_8 = ifelse(OCC>=7000 & OCC<=7630,1,0),
                    occ_9 = ifelse(OCC>=7700 & OCC<=8965,1,0),
                    occ_10 = ifelse(OCC>=9000 & OCC<=9750,1,0))
  
  #Class of Government workers
  d <- d %>% mutate(empgov_fed = ifelse(COW == 5,1,0))
  d <- d %>% mutate(empgov_st = ifelse(COW == 4,1,0))
  d <- d %>% mutate(empgov_loc = ifelse(COW == 3,1,0))
  
  # industry
  d <- d %>% mutate(  ind_1 = ifelse(INDP>=170 & INDP<=290 ,1,0),
                      ind_2 = ifelse(INDP>=370 & INDP<=490 ,1,0),
                      ind_3 = ifelse(INDP==770 ,1,0),
                      ind_4 = ifelse(INDP>=1070 & INDP<=3990 ,1,0),
                      ind_5 = ifelse(INDP>=4070 & INDP<=5790 ,1,0),
                      ind_6 = ifelse(INDP>=6070 & INDP<=6390 ,1,0),
                      ind_7 = ifelse((INDP>=6470 & INDP<=6780)|(INDP>=570 & INDP<=690) ,1,0),
                      ind_8 = ifelse(INDP>=6870 & INDP<=7190 ,1,0),
                      ind_9 = ifelse(INDP>=7270 & INDP<=7790 ,1,0),
                      ind_10 = ifelse(INDP>=7860 & INDP<=8470 ,1,0),
                      ind_11 = ifelse(INDP>=8560 & INDP<=8690 ,1,0),
                      ind_12 = ifelse(INDP>=8770 & INDP<=9290 ,1,0),
                      ind_13 = ifelse(INDP>=9370 & INDP<=9590 ,1,0))
  
  # Hours per week
  d$wkhours <- d$WKHP
  
  # Weeks worked
  # simply taking midpoint of range for now
  # gets imputed later on from CPS
  d <- d %>% mutate(weeks_worked_cat=ifelse(WKW==1,'50-52 weeks',NA))
  d <- d %>% mutate(weeks_worked_cat=ifelse(WKW==2,'48-49 weeks',weeks_worked_cat))
  d <- d %>% mutate(weeks_worked_cat=ifelse(WKW==3,'40-47 weeks',weeks_worked_cat))
  d <- d %>% mutate(weeks_worked_cat=ifelse(WKW==4,'27-39 weeks',weeks_worked_cat))
  d <- d %>% mutate(weeks_worked_cat=ifelse(WKW==5,'14-26 weeks',weeks_worked_cat))
  d <- d %>% mutate(weeks_worked_cat=ifelse(WKW==6,'13 weeks or less',weeks_worked_cat))
  
  d <- d %>% mutate(weeks_worked=ifelse(WKW==1,51,0))
  d <- d %>% mutate(weeks_worked=ifelse(WKW==2,48.5,weeks_worked))
  d <- d %>% mutate(weeks_worked=ifelse(WKW==3,43.5,weeks_worked))
  d <- d %>% mutate(weeks_worked=ifelse(WKW==4,33,weeks_worked))
  d <- d %>% mutate(weeks_worked=ifelse(WKW==5,20,weeks_worked))
  d <- d %>% mutate(weeks_worked=ifelse(WKW==6,7.5,weeks_worked))
  d <- d %>% mutate(weeks_worked=ifelse(is.na(weeks_worked),0,weeks_worked))
  
  d <- d %>% mutate(wkw_min=ifelse(WKW==1,50,0))
  d <- d %>% mutate(wkw_min=ifelse(WKW==2,48,wkw_min))
  d <- d %>% mutate(wkw_min=ifelse(WKW==3,40,wkw_min))
  d <- d %>% mutate(wkw_min=ifelse(WKW==4,27,wkw_min))
  d <- d %>% mutate(wkw_min=ifelse(WKW==5,14,wkw_min))
  d <- d %>% mutate(wkw_min=ifelse(WKW==6,0,wkw_min))
  d <- d %>% mutate(wkw_min=ifelse(is.na(wkw_min),0,wkw_min))
  
  d <- d %>% mutate(wkw_max=ifelse(WKW==1,52,0))
  d <- d %>% mutate(wkw_max=ifelse(WKW==2,49,wkw_max))
  d <- d %>% mutate(wkw_max=ifelse(WKW==3,47,wkw_max))
  d <- d %>% mutate(wkw_max=ifelse(WKW==4,39,wkw_max))
  d <- d %>% mutate(wkw_max=ifelse(WKW==5,26,wkw_max))
  d <- d %>% mutate(wkw_max=ifelse(WKW==6,13,wkw_max))
  d <- d %>% mutate(wkw_max=ifelse(is.na(wkw_max),0,wkw_max))
  
  # Health Insurance from employer
  d <- d %>% mutate(hiemp=ifelse(HINS1==1,1,0))
  
  # log earnings
  d <- d %>% mutate(wage12=WAGP*(ADJINC/1056030))
  d <- d %>% mutate(lnearn=ifelse(wage12>0, log(wage12), NA))
  
  # family income
  # Make more coarse categores
  d <- d %>% mutate(faminc_cat = ifelse(faminc <= 34999,1,NA))
  d <- d %>% mutate(faminc_cat = ifelse(faminc <= 74999 & faminc >= 35000,2,faminc_cat))
  d <- d %>% mutate(faminc_cat = ifelse(faminc >= 74999, 3,faminc_cat))
  
  # presence of children
  d <- d %>% mutate(fem_cu6= ifelse(PAOC==1,1,0))
  d <- d %>% mutate(fem_c617= ifelse(PAOC==2,1,0))
  d <- d %>% mutate(fem_cu6and617= ifelse(PAOC==3,1,0))
  d <- d %>% mutate(fem_nochild= ifelse(PAOC==4,1,0))
  
  # strip to only required variables to save memory
  # use select
  replicate_weights <- paste0('PWGTP',seq(1,80))
  d <- d[c('SERIALNO',"nochildren", "lnfaminc", "faminc", "lnearn","fem_cu6","fem_c617","fem_cu6and617","fem_nochild",
           "age", "a_age", "widowed", "divorced", "hiemp",
           "separated", "nevermarried", "male", "female", "agesq", "ltHS", "someCol", "BA", 
           "GradSch", "black", "white", "asian", "other",'native', "hisp", "OCC", "occ_1", "occ_2", "occ_3", 
           "occ_4", "occ_5", "occ_6", "occ_7", "occ_8", "occ_9", "occ_10", "ind_1", "ind_2", "ind_3", "ind_4", 
           "ind_5", "ind_6", "ind_7", "ind_8", "ind_9", "ind_10", "ind_11", "ind_12", "ind_13", "weeks_worked",
           "WAGP",'wage12',"WKHP","PWGTP", replicate_weights,"FER", "WKW","COW","ESR",'NPF',"partner","ndep_kid",
           "ndep_old",'empgov_fed','empgov_st', 'wkhours', 'empgov_loc', 'ST','POWSP','age_cat','faminc_cat','employed',
           'married','HSgrad','BAplus')]

  # id variable
  d$id <- as.numeric(rownames(d))
  d <- d[order(d$id),]
  

  
  # -------------------------- #
  # Remove ineligible workers
  # -------------------------- #
  
  # Restrict dataset to civilian employed workers
  d <- d %>% filter(ESR==1|ESR==2)
  
  #  Include self-employed and gov't workers unless user specifies otherwise
  d <- d %>% filter(COW<=7)
  

  # -------------------------- #
  # Save the resulting dataset
  # -------------------------- #
  
  if (save_csv==TRUE) {
    write.csv(d, file = filename, row.names = FALSE)  
  }
  return(d)
}

# ============================ #
# 3. clean_cps
# ============================ #
  
clean_cps <-function(d_cps) {

  #Create dummies for logit regressions
  # Gender
  d_cps <- d_cps %>% mutate(male = ifelse(a_sex == 1,1,0),
                            female = ifelse(a_sex == 2,1,0))
  
  # Education
  d_cps <- d_cps %>% mutate(ltHS = ifelse(a_hga <= 38,1,0),
                            someCol = ifelse(a_hga >= 40 & a_hga<=42,1,0),
                            BA = ifelse(a_hga == 43,1,0),
                            GradSch = ifelse(a_hga >= 44,1,0))
  # Race
  d_cps <- d_cps %>% mutate(black = ifelse(prdtrace==2 & pehspnon==2,1,0),
                            asian = ifelse(prdtrace==4 & pehspnon==2,1,0),
                            other = ifelse(((prdtrace==3)|((prdtrace>=5)&(prdtrace<=26)))&(pehspnon==2),1,0),
                            hisp = ifelse(pehspnon==1,1,0))
  
  # age squared
  d_cps <- d_cps %>% mutate(age = a_age)
  d_cps <- d_cps %>% mutate(agesq = a_age*a_age)
  
  # occupation
  d_cps <- d_cps %>% mutate(occ_1 = ifelse(a_mjocc == 1,1,0),
                            occ_2 = ifelse(a_mjocc == 2,1,0),
                            occ_3 = ifelse(a_mjocc == 3,1,0),
                            occ_4 = ifelse(a_mjocc == 4,1,0),
                            occ_5 = ifelse(a_mjocc == 5,1,0),
                            occ_6 = ifelse(a_mjocc == 6,1,0),
                            occ_7 = ifelse(a_mjocc == 7,1,0),
                            occ_8 = ifelse(a_mjocc == 8,1,0),
                            occ_9 = ifelse(a_mjocc == 9,1,0),
                            occ_10 = ifelse(a_mjocc == 10,1,0))
  d_cps <- d_cps %>% mutate(occ_1 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_1),
                            occ_2 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_2),
                            occ_3 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_3),
                            occ_4 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_4),
                            occ_5 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_5),
                            occ_6 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_6),
                            occ_7 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_7),
                            occ_8 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_8),
                            occ_9 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_9),
                            occ_10 = ifelse(a_mjocc == 0|a_mjocc == 11,NA,occ_10))
  
  # industry
  d_cps <- d_cps %>% mutate(ind_1 = ifelse(a_mjind == 1,1,0),
                            ind_2 = ifelse(a_mjind == 2,1,0),
                            ind_3 = ifelse(a_mjind == 3,1,0),
                            ind_4 = ifelse(a_mjind == 4,1,0),
                            ind_5 = ifelse(a_mjind == 5,1,0),
                            ind_6 = ifelse(a_mjind == 6,1,0),
                            ind_7 = ifelse(a_mjind == 7,1,0),
                            ind_8 = ifelse(a_mjind == 8,1,0),
                            ind_9 = ifelse(a_mjind == 9,1,0),
                            ind_10 = ifelse(a_mjind == 10,1,0),
                            ind_11 = ifelse(a_mjind == 11,1,0),
                            ind_12 = ifelse(a_mjind == 12,1,0),
                            ind_13 = ifelse(a_mjind == 13,1,0))
  d_cps <- d_cps %>% mutate(ind_1 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_1),
                            ind_2 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_2),
                            ind_3 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_3),
                            ind_4 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_4),
                            ind_5 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_5),
                            ind_6 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_6),
                            ind_7 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_7),
                            ind_8 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_8),
                            ind_9 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_9),
                            ind_10 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_10),
                            ind_11 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_11),
                            ind_12 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_12),
                            ind_13 = ifelse(a_mjind == 0|a_mjind == 14,NA,ind_13))
  
  # hourly pay
  d_cps <- d_cps %>% mutate(paid_hrly= ifelse(prerelg == 1, 0,NA))
  d_cps <- d_cps %>% mutate(paid_hrly= ifelse(prerelg == 1 & a_hrlywk == 1, 1, paid_hrly))
  
  # Making zero/negative earnings into NaN so we can take natural log
  d_cps <- d_cps %>% mutate(lnearn = ifelse(pearnval<=0,NA,pearnval))  
  d_cps <- d_cps %>% mutate(lnearn = log(lnearn))  
  # Making other values 0 per ACM code
  d_cps <- d_cps %>% mutate(lnearn = ifelse(pearnval<=0, 0, lnearn))  
  
  # employer provided health insurance
  d_cps$hiemp <- as.numeric(d_cps$hiemp)
  d_cps <- d_cps %>% mutate(hiemp = ifelse(hiemp == 0, NA, hiemp))  
  d_cps <- d_cps %>% mutate(hiemp = ifelse(hiemp == 2, 0, hiemp))  
  
  # weeks worked 
  d_cps <- d_cps %>% mutate(wks_cat= ifelse(wkswork>=50 & wkswork<=52, 1,NA))
  d_cps <- d_cps %>% mutate(wks_cat= ifelse(wkswork>=48 & wkswork<=49, 2,wks_cat))
  d_cps <- d_cps %>% mutate(wks_48_49= ifelse(wkswork==49, 1,0))
  d_cps <- d_cps %>% mutate(wks_cat= ifelse(wkswork>=40 & wkswork<=47, 3,wks_cat))
  d_cps <- d_cps %>% mutate(wks_cat= ifelse(wkswork>=27 & wkswork<=39, 4,wks_cat))
  d_cps <- d_cps %>% mutate(wks_cat= ifelse(wkswork>=14 & wkswork<=26, 5,wks_cat))
  d_cps <- d_cps %>% mutate(wks_cat= ifelse(wkswork>=0 & wkswork<=13, 6,wks_cat))
  
  # presence of children
  d_cps <- d_cps %>% mutate(fem_cu6= ifelse(pextra1==2,1,0))
  d_cps <- d_cps %>% mutate(fem_c617= ifelse(pextra1==3,1,0))
  d_cps <- d_cps %>% mutate(fem_cu6and617= ifelse(pextra1==4,1,0))
  d_cps <- d_cps %>% mutate(fem_nochild= ifelse(pextra1==5,1,0))
  
  # employer size
  d_cps <- d_cps %>% mutate(emp_size=noemp) 
  
  #write.csv(d_cps, file = "CPS_extract_clean.csv", row.names = FALSE)
  return(d_cps)
}

# ============================ #
# 4. impute_cps_to_acs
# ============================ #

# This program cleans CPS data and runs a number of logit and ordinal logit
# regressions to produce coefficient estimates to impute some variables into ACS.

# everything in this program is a candidate for modular imputation

impute_cps_to_acs <- function(d_acs, d_cps){
  
  # ---------------------------------------------------------------------------------------------------------
  # Run models
  # ---------------------------------------------------------------------------------------------------------
  
  # logit for hourly paid regression
  varname= 'paid_hrly'
  formula = paste("paid_hrly ~ female + black + a_age + agesq + BA",
            "+ GradSch + occ_1 + occ_3 + occ_5 + occ_7 + occ_8",
            "+ occ_9 + occ_10 + ind_5 + ind_8 + ind_11 + ind_12")
  filt = c(paid_hrly= "TRUE")
  weight = c(paid_hrly = "~ marsupwt")
  
  # INPUTS: CPS (training) data set, logit regression model specification, training filter condition, weight to use
  d_filt <- runLogitEstimate(d_train=d_cps,d_test=d_acs, formula=formula, test_filt=filt, train_filt=filt, 
                            weight=weight, varname=varname, create_dummies=TRUE)
  d_acs <- merge(d_filt, d_acs, by='id', all.y=TRUE)
  # OUTPUT: Dataframe with two columns: id and imputed paid hourly variable
  
  # ordered logit for number of employers
  varname= 'num_emp'
  formula = paste("factor(phmemprs) ~  age + agesq + asian + hisp",
            "+ ltHS + someCol + BA + GradSch + lnearn",
            "+ hiemp + ind_4 + ind_5 + ind_6 + ind_8",
            "+ ind_13 + occ_1 + occ_6 + occ_7 + occ_9 + occ_10")
  filt =  "TRUE"
  
  # INPUTS: CPS (training) data set, ordinal regression model specification, filter conditions, var to create 
  d_filt <- runOrdinalEstimate(d_train=d_cps,d_test=d_acs, formula=formula,test_filt=filt,
                              train_filt=filt, varname=varname)
  d_acs <- merge(d_filt, d_acs, by='id', all.y=TRUE)
  # OUTPUTS: ACS data with imputed number of employers variable

  # ordered logit for weeks worked categories
  formulas= c(wks_50_52="factor(wkswork) ~ age + agesq +  black + hisp + lnearn",
              wks_40_47="factor(wkswork) ~ age + lnearn" ,
              wks_27_39="factor(wkswork) ~ age + fem_cu6 + fem_c617 + fem_cu6and617 + female + lnearn" ,
              wks_14_26="factor(wkswork) ~ age + hisp + lnearn" ,
              wks_0_13="factor(wkswork) ~ age + agesq + female + lnearn" )
  train_filts= c(wks_50_52="wks_cat==1" ,
                 wks_40_47="wks_cat==3" ,
                 wks_27_39="wks_cat==4" ,
                 wks_14_26="wks_cat==5" ,
                 wks_0_13="wks_cat==6" )
  test_filts= c(wks_50_52="WKW==1",
                wks_40_47="WKW==3" ,
                wks_27_39="WKW==4" ,
                wks_14_26="WKW==5" ,
                wks_0_13="WKW==6" )
  varnames= c (wks_50_52='wks_50_52',
               wks_40_47='wks_40_47' ,
               wks_27_39='wks_27_39' ,
               wks_14_26='wks_14_26' ,
               wks_0_13='wks_0_13' )            
  
  
  sets <- mapply(runOrdinalEstimate, formula=formulas,test_filt=test_filts,
                                    train_filt=train_filts, varname=varnames,
                                    MoreArgs=list(d_train=d_cps,d_test=d_acs),
                                    SIMPLIFY=FALSE)
  for (i in sets) {
    d_acs <- merge(i, d_acs, by='id', all.y=TRUE)
  }
          
  # one category only has 2 categories, so using a logit 
  varname= 'wks_48_49'
  formula = "wks_48_49 ~ age + agesq + lnearn"
  train_filt = "wks_cat==2"
  test_filt= "WKW==2"
  d_filt <- runLogitEstimate(d_train=d_cps,d_test=d_acs, formula=formula, test_filt=test_filt, 
                             train_filt=train_filt, weight=weight, varname=varname, create_dummies=TRUE)
  d_acs <- merge(d_filt, d_acs, by='id', all.y=TRUE)
  
  # create single weeks worked var
  d_acs <- d_acs %>% mutate (iweeks_worked= ifelse(!is.na(wks_50_52),wks_50_52+49,0)) %>% 
    mutate (iweeks_worked= ifelse(!is.na(wks_48_49),wks_48_49+48,iweeks_worked)) %>% # "+48" is intentaional, this is a 0/1 var
    mutate (iweeks_worked= ifelse(!is.na(wks_40_47),wks_40_47+39,iweeks_worked)) %>%
    mutate (iweeks_worked= ifelse(!is.na(wks_27_39),wks_27_39+26,iweeks_worked)) %>%
    mutate (iweeks_worked= ifelse(!is.na(wks_14_26),wks_14_26+13,iweeks_worked)) %>%
    mutate (iweeks_worked= ifelse(!is.na(wks_0_13),wks_0_13,iweeks_worked))
  
  # Ordered logit employer size categories
  varname = 'emp_size'
  formula = paste("factor(emp_size) ~ a_age + black + ltHS + someCol + BA + GradSch + lnearn",
                 "  + hiemp + ind_1 + ind_3 + ind_5 + ind_6 + ind_8 + ind_9",
                 "+ ind_11 + ind_12 + ind_13 + occ_1 + occ_4 + occ_5 + occ_6 + occ_7 + occ_9")
  filt = "TRUE"
  weight = "marsupwt"
  d_filt <- runOrdinalEstimate(d_train=d_cps,d_test=d_acs, formula=formula,test_filt=filt,
                               train_filt=filt, varname=varname)
  d_acs <- cbind(d_acs, d_filt['emp_size'])
  
  # then do random draw within assigned size range
  d_acs <- d_acs %>% mutate(temp_size=ifelse(emp_size==1,sample(1:9, nrow(d_acs), replace=T),0)) %>%
    mutate(temp_size=ifelse(emp_size==2,sample(10:49, nrow(d_acs), replace=T),temp_size)) %>%
    mutate(temp_size=ifelse(emp_size==3,sample(50:99, nrow(d_acs), replace=T),temp_size)) %>%
    mutate(temp_size=ifelse(emp_size==4,sample(100:499, nrow(d_acs), replace=T),temp_size)) %>%
    mutate(temp_size=ifelse(emp_size==5,sample(500:999, nrow(d_acs), replace=T),temp_size)) %>%
    mutate(temp_size=ifelse(emp_size==6,sample(1000:99999, nrow(d_acs), replace=T),temp_size)) %>%
    mutate(emp_size=temp_size) %>%
  # clean up weeks worked variables
    mutate(weeks_worked_cat=weeks_worked) %>%
    mutate(weeks_worked=iweeks_worked) %>%
  # create dummy for one employer worked for
    mutate(oneemp=ifelse(num_emp==1,1,0))
  
  # generate FMLA coverage eligibility based on these vars:
  d_acs <- d_acs %>% mutate(coveligd=ifelse(WKHP>=25 & weeks_worked>=40 & num_emp==1 & emp_size>=50,1,0))
  
  # clean up vars
  d_acs <- d_acs[, !(names(d_acs) %in% c('rand','temp_size','iweeks_worked',
                                         "wks_0_13", "wks_14_26", "wks_27_39", "wks_40_47", 
                                         "wks_48_49", "wks_50_52" ))]
  
  return(d_acs)
}

# ============================ #
# 4A. runLogitEstimate
# ============================ #
# see 3_impute_functions.R, function 1Ba

# ============================ #
# 4B. runOrdinalEstimate
# ============================ #
# see 3_impute_functions.R, function 1Bb


# ============================ #
# 5. sample_acs
# ============================ #
# user option to sample ACS data, either by number of observations or proportion of obs
sample_acs <- function(d, sample_prop, sample_num) {
  # user option to sample ACS data
  # by proportion
  if (!is.null(sample_prop)) {
    samp=round(nrow(d)*sample_prop,digits=0)
    d$PWGTP=d$PWGTP/sample_prop
    d <- sample_n(d, samp)
    # also adjust replicate weights
    replicate_weights <- paste0('PWGTP',seq(1,80))
    for (i in replicate_weights) {
      d[i] = d[i]/sample_prop
    }
  }
  # by absolute value
  if (!is.null(sample_num)) {
    d$PWGTP=d$PWGTP*(nrow(d)/sample_num)
    d <- sample_n(d, sample_num)
    # also adjust replicate weights
    replicate_weights <- paste0('PWGTP',seq(1,80))
    for (i in replicate_weights) {
      d[i] = d[i]*(nrow(d)/sample_num)
    }
  }
  return(d)
}