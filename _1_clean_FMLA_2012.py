"""
This program takes the FMLA data and cleans it into a format to be used
for behavioral estimation
"""

import pandas as pd
import numpy as np
import sklearn.linear_model
import mord
import json
from time import time
from _5a_aux_functions import fillna_binary, get_multiple_leave_vars

class DataCleanerFMLA:

    def __init__(self, fp_fmla_in, fp_fmla_out, random_state):
        '''

        :param fp_fmla_in: file path to original FMLA data
        :param fp_fmla_out: file path to cleaned FMLA data, before CPS simulation
        '''

        self.fp_fmla_in = fp_fmla_in
        self.fp_fmla_out = fp_fmla_out
        self.random_state = random_state

    def clean_data(self):

        # Read in FMLA data
        d = pd.read_csv(self.fp_fmla_in, low_memory=False)

        # Make empid to follow 0-order to be consistent with Python standard (e.g. indices output from kNN)
        d['empid'] = d['empid'] - 1

        # FMLA eligibility
        d['fmla_eligible'] = np.where((d['E13']==1) & # one employer past year
                                      ((d['E14'] == 1) | (d['E15_CAT'].isin([5,6,7,8]))) & # FT or wkhour>=25/week
                                      (d['E12'].isin([6,7,8,9])) # emp size over 50 within 75 miles
                                      , 1, 0)
        d['fmla_eligible'] = np.where((d['E13'].isna()) |
                                      ((d['E14'].isna()) & (d['E15_CAT'].isna())) |
                                      (d['E13'].isna())
                                      , np.nan, d['fmla_eligible'])

        # Hourly worker
        d['hourly'] = np.where(d['E9_1'] == 2, 1, 0)
        d['hourly'] = np.where(np.isnan(d['E9_1']), np.nan, d['hourly'])

        # Union member
        d['union'] = np.where(d['D3'] == 1, 1, 0)
        d['union'] = np.where(np.isnan(d['D3']), np.nan, d['union'])

        # Employment Status
        d['employed'] = np.where(d['E1'] == 1, 1, 0)
        d['employed'] = np.where(np.isnan(d['E1']), np.nan, d['employed'])

        # Hours per week
        # a dict to map code into average numeric hours per week
        dict_wkhours = {
            1: 4,
            2: 11.5,
            3: 17,
            4: 21.5,
            5: 26.5,
            6: 32,
            7: 37.5,
            8: 45,
            'nan': np.nan
        }
        d['wkhours'] = d['E15_CAT_REV'].map(dict_wkhours)

        # Employment at government
        # all rows should be valid for empgov_[] indicators, given FMLA sample
        d['emp_gov'] = np.where(d['D2'].isin([1,2,3]), 1, 0)
        d['empgov_fed'] = np.where(d['D2'] == 1, 1, 0)
        d['empgov_st'] = np.where(d['D2'] == 2, 1, 0)
        d['empgov_loc'] = np.where(d['D2'] == 3, 1, 0)

        # Age at midpoint of category
        conditions = [(d['AGE_CAT'] == 1), (d['AGE_CAT'] == 2),
                      (d['AGE_CAT'] == 3), (d['AGE_CAT'] == 4),
                      (d['AGE_CAT'] == 5), (d['AGE_CAT'] == 6),
                      (d['AGE_CAT'] == 7), (d['AGE_CAT'] == 8),
                      (d['AGE_CAT'] == 9), (d['AGE_CAT'] == 10)]
        choices = [21, 27, 32, 37, 42, 47, 52, 57, 63.5, 70]
        d['age'] = np.select(conditions, choices, default=np.nan)
        d['agesq'] = np.array(d['age']) ** 2

        # Sex
        d['male'] = np.where(d['GENDER_CAT'] == 1, 1, 0)
        d.loc[d['GENDER_CAT'].isna(), 'male'] = np.nan
        d['female'] = np.where(d['GENDER_CAT'] == 2, 1, 0)
        d.loc[d['GENDER_CAT'].isna(), 'female'] = np.nan

        # No children
        d['nochildren'] = np.where(d['D7_CAT'] == 0, 1, 0)
        d['nochildren'] = np.where(np.isnan(d['D7_CAT']), np.nan, d['nochildren'])

        # No spouse
        d['nospouse'] = np.where(d['D10'].isin([3, 4, 5, 6]), 1, 0)
        d['nospouse'] = np.where(np.isnan(d['D10']), np.nan, d['nospouse'])

        # No elderly dependent
        d['noelderly'] = np.where(d['D8_CAT'] == 0, 1, 0)
        d['noelderly'] = np.where(d['D8_CAT'].isna(), np.nan, d['noelderly'])

        # Number of dependents
        d['ndep_kid'] = d.D7_CAT
        d['ndep_old'] = d.D8_CAT

        # Educational level
        d['ltHS'] = np.where(d['D1_CAT'] == 1, 1, 0)
        d['ltHS'] = np.where(np.isnan(d['D1_CAT']), np.nan, d['ltHS'])

        d['someHS'] = np.where(d['D1_CAT'] == 2, 1, 0)
        d['someHS'] = np.where(np.isnan(d['D1_CAT']), np.nan, d['someHS'])

        d['HSgrad'] = np.where(d['D1_CAT'] == 3, 1, 0)
        d['HSgrad'] = np.where(np.isnan(d['D1_CAT']), np.nan, d['HSgrad'])

        d['someCol'] = np.where(d['D1_CAT'] == 5, 1, 0)
        d['someCol'] = np.where(np.isnan(d['D1_CAT']), np.nan, d['someCol'])

        d['BA'] = np.where(d['D1_CAT'] == 6, 1, 0)
        d['BA'] = np.where(np.isnan(d['D1_CAT']), np.nan, d['BA'])

        d['GradSch'] = np.where(d['D1_CAT'] == 7, 1, 0)
        d['GradSch'] = np.where(np.isnan(d['D1_CAT']), np.nan, d['GradSch'])

        d['noHSdegree'] = np.where((d['ltHS'] == 1) | (d['someHS'] == 1), 1, 0)
        d['noHSdegree'] = np.where(np.isnan(d['ltHS']) & np.isnan(d['someHS']), np.nan, d['noHSdegree'])

        d['BAplus'] = np.where((d['BA'] == 1) | (d['GradSch'] == 1), 1, 0)
        d['BAplus'] = np.where(np.isnan(d['BA']) & np.isnan(d['GradSch']), np.nan, d['BAplus'])

        # Family income using midpoint of category
        conditions = [(d['D4_CAT'] == 3), (d['D4_CAT'] == 4),
                      (d['D4_CAT'] == 5), (d['D4_CAT'] == 6),
                      (d['D4_CAT'] == 7), (d['D4_CAT'] == 8),
                      (d['D4_CAT'] == 9), (d['D4_CAT'] == 10)]
        choices = [15, 25, 32.5, 37.5, 45, 62.5, 87.5, 130]
        d['faminc'] = 1000*np.select(conditions, choices, default=np.nan)
        d.loc[(d['faminc'] <= 0.01) & (~d['faminc'].isna()), 'faminc'] = 0.01  # set to 0.01 for any reported income <=0.01
        # Log income - set to log(0.01) for any reported income <=0.01, set to NA if income NA
        d['ln_faminc'] = [np.log(x) if not np.isnan(x) else x for x in d['faminc']]

        # Marital status
        d['married'] = np.where(d['D10'] == 1, 1, 0)
        d['married'] = np.where(np.isnan(d['D10']), np.nan, d['married'])

        d['partner'] = np.where(d['D10'] == 2, 1, 0)
        d['partner'] = np.where(np.isnan(d['D10']), np.nan, d['partner'])

        d['separated'] = np.where(d['D10'] == 3, 1, 0)
        d['separated'] = np.where(np.isnan(d['D10']), np.nan, d['separated'])

        d['divorced'] = np.where(d['D10'] == 4, 1, 0)
        d['divorced'] = np.where(np.isnan(d['D10']), np.nan, d['divorced'])

        d['widowed'] = np.where(d['D10'] == 5, 1, 0)
        d['widowed'] = np.where(np.isnan(d['D10']), np.nan, d['widowed'])

        d['nevermarried'] = np.where(d['D10'] == 6, 1, 0)
        d['nevermarried'] = np.where(np.isnan(d['D10']), np.nan, d['nevermarried'])

        # Race/ethnicity
        d['raceth'] = np.where((np.isnan(d['D5']) == False) & (d['D5'] == 1), 7, d['D6_1_CAT'])

        d['native'] = np.where(d['raceth'] == 1, 1, 0)
        d['native'] = np.where(np.isnan(d['raceth']), np.nan, d['native'])

        d['asian'] = np.where(d['raceth'] == 2, 1, 0)
        d['asian'] = np.where(np.isnan(d['raceth']), np.nan, d['asian'])

        d['black'] = np.where(d['raceth'] == 4, 1, 0)
        d['black'] = np.where(np.isnan(d['raceth']), np.nan, d['black'])

        d['white'] = np.where(d['raceth'] == 5, 1, 0)
        d['white'] = np.where(np.isnan(d['raceth']), np.nan, d['white'])

        d['other'] = np.where(d['raceth'] == 6, 1, 0)
        d['other'] = np.where(np.isnan(d['raceth']), np.nan, d['other'])

        d['hisp'] = np.where(d['raceth'] == 7, 1, 0)
        d['hisp'] = np.where(np.isnan(d['raceth']), np.nan, d['hisp'])


        ## Use below for getting mid-point approx length distribution from public FMLA (prone to error, not recom'd)
        # length of leave for most recent leave
        # # cap at 365-52*2 = 261 work days a year
        # a dict from leave length cat to length in days (take mid-point of category, if need tiebreak, take larger/smaller alternating)
        # dct = {}
        # ks = list(range(1, 29))
        # vs = list(range(1, 11)) + [12, 13, 15, 18, 20, 22, 27, 30, 33, 38, 43, 48, 53, 58, 66, 80, 106, 191]
        # dct['A19_1_CAT'] = dict(zip(ks, vs))
        # ks = list(range(1, 11))
        # vs = list(range(1, 6)) + [8, 10, 15, 41, 90] # heuristic: for 6 rows with A19_2_CAT=10=61+days, scale-up by 1.5x
        # dct['A19_2_CAT'] = dict(zip(ks, vs))
        #
        # # get approx days for A19_1_CAT, A19_2_CAT
        # d['A19_1_CAT_days'] = [dct['A19_1_CAT'][x] if not np.isnan(x) else np.nan for x in d['A19_1_CAT']]
        # d['A19_2_CAT_days'] = [dct['A19_2_CAT'][x] if not np.isnan(x) else np.nan for x in d['A19_2_CAT']]
        #
        #
        # d['length'] = np.where((np.isnan(d['A20']) == False) & (d['A20'] == 2), d['A19_2_CAT_days'], d['A19_1_CAT_days'])
        # d['length'] = [min(x, 261) for x in d['length']]

        # Use below for getting exact length distribution from restricted FMLA
        # length of leave for most recent leave
        # cap at 365-52*2 = 261 work days a year
        # d['length'] = np.where((np.isnan(d['A20']) == False) & (d['A20'] == 2), d['A19_2_CAT_rev'], d['A19_1_CAT_rev'])
        # d['length'] = [min(x, 261) for x in d['length']]


        # --------------------------
        # dummies for leave type
        # --------------------------

        # there are three variables for each leave type:
        # (1) taking a leave
        # (2) needing a leave
        # (3) taking or needing a leave

        # leave reason for most recent leave
        d['reason_take'] = np.where((np.isnan(d['A20']) == False) & (d['A20'] == 2), d['A5_2_CAT'], d['A5_1_CAT'])

        # maternity disability
        d['take_matdis'] = np.where(
            (((d['A5_1_CAT'] == 21) & (d['A11_1'] == 1) & (d['GENDER_CAT'] == 2)) #  | (d['A5_1_CAT_rev'] == 32)
             ) & (
                (d['A20'] != 2) | (d['A20'].isna())), 1, 0)
            # follow makes no change for FMLA 2012 since A5_2_CAT = 21 has 0 case but include code for later FMLA data
        d['take_matdis'] = np.where(
            (((d['A5_2_CAT'] == 21) & (d['A11_1'] == 1) & (d['GENDER_CAT'] == 2)) # | (d['A5_1_CAT_rev'] == 32)
             ) & (
                d['A20'] == 2), 1, d['take_matdis'])

        d['take_matdis'] = np.where(np.isnan(d['take_matdis']), 0, d['take_matdis'])
        d['take_matdis'] = np.where((np.isnan(d['A5_1_CAT'])) & (np.isnan(d['A5_2_CAT'])), np.nan, d['take_matdis'])
        d['take_matdis'] = np.where(np.isnan(d['take_matdis']) & ((d['LEAVE_CAT'] == 2) | (d['LEAVE_CAT'] == 3)), 0,
                                    d['take_matdis'])
        d['take_matdis'] = np.where(np.isnan(d['take_matdis']) & (d['male'] == 1), 0, d['take_matdis'])
        # # make sure take_matdis = 0 for matdis taker whose most recent leave is not matdis
        d['take_matdis'] = np.where((d['take_matdis'] == 1) &
                                    (d['A20'] == 2) &
                                    (d['A5_1_CAT'] == 21) &
                                    (d['A5_2_CAT'] != 21), 0, d['take_matdis'])

        d['need_matdis'] = np.where(
            ((d['B6_1_CAT'] == 21) & (d['B12_1'] == 1) & (d['GENDER_CAT'] == 2)) # | (d['B6_1_CAT_rev'] == 32)
            , 1, 0)
        d['need_matdis'] = np.where(np.isnan(d['need_matdis']), 0, d['need_matdis'])
        d['need_matdis'] = np.where(np.isnan(d['B6_1_CAT']), np.nan, d['need_matdis'])
        d['need_matdis'] = np.where(np.isnan(d['need_matdis']) & ((d['LEAVE_CAT'] == 1) | (d['LEAVE_CAT'] == 3)), 0,
                                    d['need_matdis'])
        d['need_matdis'] = np.where(np.isnan(d['need_matdis']) & (d['male'] == 1), 0, d['need_matdis'])

        d['type_matdis'] = np.where((d['take_matdis'] == 1) | (d['need_matdis'] == 1), 1, 0)
        d['type_matdis'] = np.where(np.isnan(d['take_matdis']) | np.isnan(d['need_matdis']), np.nan, d['type_matdis'])

        # new child/bond
        d['take_bond'] = np.where(((d['take_matdis']==0) | (d['take_matdis'].isna())) &
                                  ( # (d['A5_1_CAT_rev']==31) |
                                      (d['A5_1_CAT']==21)  # & (d['A5_1_CAT_rev']!=32)
                                  ) &
                                  ((d['A11_1'].isna()) | (d['A11_1']==2)) &
                                  ((d['A20']!=2) | (d['A20'].isna())), 1, 0)
        d['take_bond'] = np.where(((d['take_matdis']==0) | (d['take_matdis'].isna())) &
                                  ((d['A5_2_CAT'].isna()) & (d['A5_2_CAT']==21)) &
                                  ((d['A11_2'].isna()) | (d['A11_2']==2)) &
                                  ((d['A20'].isna()) | (d['A20']==2))
                                  , 1, d['take_bond'])
        d['take_bond'] = np.where(np.isnan(d['A5_1_CAT']), np.nan, d['take_bond'])
        d['take_bond'] = np.where(np.isnan(d['take_bond']) & ((d['LEAVE_CAT'] == 2) | (d['LEAVE_CAT'] == 3)), 0, d['take_bond'])

        d['need_bond'] = np.where(((d['need_matdis']==0) | (d['need_matdis'].isna())) &
                                  ( # (d['B6_1_CAT_rev']==31) |
                                      (d['B6_1_CAT']==21) #  & (d['B6_1_CAT_rev']!=32)
                                  ) &
                                  (d['B12_1']==2), 1, 0)
        d['need_bond'] = np.where(d['B12_1'].isna(), np.nan, d['need_bond'])
        d['need_bond'] = np.where(np.isnan(d['B6_1_CAT']), np.nan, d['need_bond'])
        d['need_bond'] = np.where(np.isnan(d['need_bond']) & ((d['LEAVE_CAT'] == 1) | (d['LEAVE_CAT'] == 3)), 0, d['need_bond'])


        d['type_bond'] = np.where((d['take_bond'] == 1) | (d['need_bond'] == 1), 1, 0)
        d['type_bond'] = np.where(np.isnan(d['take_bond']) | np.isnan(d['need_bond']), np.nan, d['type_bond'])

        # own health
        d['take_own'] = np.where(d['reason_take'] == 1, 1, 0)
        d['take_own'] = np.where((d['reason_take'].isna()), np.nan, d['take_own'])
        d['take_own'] = np.where((np.isnan(d['take_own'])) & ((d['LEAVE_CAT'] == 2) | (d['LEAVE_CAT'] == 3)), 0, d['take_own'])

        # d['need_own'] = np.where((d['B6_1_CAT']==1) | (d['B6_2_CAT']==1),1,0)
        d['need_own'] = np.where(d['B6_1_CAT'] == 1, 1, 0)
        d['need_own'] = np.where(d['B6_1_CAT'].isna(), np.nan, d['need_own'])
        # multiple leaves - if one leave is Own the other NA, need_own is NA
        d['need_own'] = np.where(np.isnan(d['need_own']) & ((d['LEAVE_CAT'] == 1) | (d['LEAVE_CAT'] == 3)), 0, d['need_own'])

        d['type_own'] = np.where((d['take_own'] == 1) | (d['need_own'] == 1), 1, 0)
        d['type_own'] = np.where(np.isnan(d['take_own']) | np.isnan(d['need_own']), np.nan, d['type_own'])

        # ill child
        d['take_illchild'] = np.where(d['reason_take'] == 11, 1, 0)
        d['take_illchild'] = np.where(d['reason_take'].isna(), np.nan, d['take_illchild'])
        d['take_illchild'] = np.where(np.isnan(d['take_illchild']) & ((d['LEAVE_CAT'] == 2) | (d['LEAVE_CAT'] == 3)), 0,
                                      d['take_illchild'])
        # d['need_illchild'] = np.where((d['B6_1_CAT']==11) | (d['B6_2_CAT']==11),1,0)
        d['need_illchild'] = np.where(d['B6_1_CAT'] == 11, 1, 0)
        d['need_illchild'] = np.where(np.isnan(d['B6_1_CAT']), np.nan, d['need_illchild'])

        # multiple leaves - if one leave is not Illchild the other NA, need_illchild is NA
        d['need_illchild'] = np.where(np.isnan(d['need_illchild']) & ((d['LEAVE_CAT'] == 1) | (d['LEAVE_CAT'] == 3)), 0,
                                      d['need_illchild'])

        d['type_illchild'] = np.where((d['take_illchild'] == 1) | (d['need_illchild'] == 1), 1, 0)
        d['type_illchild'] = np.where(np.isnan(d['take_illchild']) | np.isnan(d['need_illchild']), np.nan, d['type_illchild'])

        # ill spouse
        d['take_illspouse'] = np.where(d['reason_take'] == 12, 1, 0)
        d['take_illspouse'] = np.where(d['reason_take'].isna(), np.nan, d['take_illspouse'])
        d['take_illspouse'] = np.where(np.isnan(d['take_illspouse']) & ((d['LEAVE_CAT'] == 2) | (d['LEAVE_CAT'] == 3)), 0,
                                       d['take_illspouse'])
        d['take_illspouse'] = np.where(np.isnan(d['take_illspouse']) & ((d['nevermarried'] == 1) |
                                                                        (d['separated'] == 1) |
                                                                        (d['divorced'] == 1) |
                                                                        (d['widowed'] == 1)), 0, d['take_illspouse'])

        d['need_illspouse'] = np.where(d['B6_1_CAT'] == 12, 1, 0)
        d['need_illspouse'] = np.where(d['B6_1_CAT'].isna(), np.nan, d['need_illspouse'])
        d['need_illspouse'] = np.where(np.isnan(d['need_illspouse']) & ((d['LEAVE_CAT'] == 1) | (d['LEAVE_CAT'] == 3)), 0,
                                       d['need_illspouse'])
        d['need_illspouse'] = np.where(np.isnan(d['need_illspouse']) & ((d['nevermarried'] == 1) |
                                                                        (d['separated'] == 1) |
                                                                        (d['divorced'] == 1) |
                                                                        (d['widowed'] == 1)), 0, d['need_illspouse'])

        d['type_illspouse'] = np.where((d['take_illspouse'] == 1) | (d['need_illspouse'] == 1), 1, 0)
        d['type_illspouse'] = np.where(np.isnan(d['take_illspouse']) | np.isnan(d['need_illspouse']), np.nan,
                                       d['type_illspouse'])

        # ill parent
        d['take_illparent'] = np.where(d['reason_take'] == 13, 1, 0)
        d['take_illparent'] = np.where(d['reason_take'].isna(), np.nan, d['take_illparent'])
        d['take_illparent'] = np.where(np.isnan(d['take_illparent']) & ((d['LEAVE_CAT'] == 2) | (d['LEAVE_CAT'] == 3)), 0,
                                       d['take_illparent'])

        d['need_illparent'] = np.where(d['B6_1_CAT'] == 13, 1, 0)
        d['need_illparent'] = np.where(d['B6_1_CAT'].isna(), np.nan, d['need_illparent'])
        d['need_illparent'] = np.where(np.isnan(d['need_illparent']) & ((d['LEAVE_CAT'] == 1) | (d['LEAVE_CAT'] == 3)), 0,
                                       d['need_illparent'])

        d['type_illparent'] = np.where((d['take_illparent'] == 1) | (d['need_illparent'] == 1), 1, 0)
        d['type_illparent'] = np.where(np.isnan(d['take_illparent']) | np.isnan(d['need_illparent']), np.nan,
                                       d['type_illparent'])

        # overall taker/needer status, any of 6 types
        types = ['own','matdis','bond','illchild','illspouse','illparent']
        d['taker'] = [max(x) for x in d[['take_%s' % t for t in types]].values]
        d['needer'] = [max(x) for x in d[['need_%s' % t for t in types]].values]

        # most recent take/need leave type in 1 col

        d['take_type'] = np.nan
        for t in types:
            d.loc[(d['take_type'].isna()) & (d['take_%s' % t] == 1), 'take_type'] = t
        d['need_type'] = np.nan
        for t in types:
            d.loc[(d['need_type'].isna()) & (d['need_%s' % t] == 1), 'need_type'] = t

        # following variables all refer to most recent leave, as indicated by FMLA 2012 questionnaire
        # any pay received
        d['anypay'] = np.where(d['A45'] == 1, 1, 0)
        d['anypay'] = np.where(np.isnan(d['A45']), np.nan, d['anypay'])

        # proportion of pay received from employer (mid point of ranges provided in FMLA)
        d['prop_pay_employer'] = np.where(d['A50'] == 1, 0.125, np.nan)
        d['prop_pay_employer'] = np.where(d['A50'] == 2, 0.375, d['prop_pay_employer'])
        d['prop_pay_employer'] = np.where(d['A50'] == 3, 0.5, d['prop_pay_employer'])
        d['prop_pay_employer'] = np.where(d['A50'] == 4, 0.625, d['prop_pay_employer'])
        d['prop_pay_employer'] = np.where(d['A50'] == 5, 0.875, d['prop_pay_employer'])
        d['prop_pay_employer'] = np.where(d['A49'] == 1, 1, d['prop_pay_employer'])
        d['prop_pay_employer'] = np.where(d['A45'] == 2, 0, d['prop_pay_employer'])

        # receive any pay from state program
        d['recStateFL'] = np.where(d['A48b'] == 1, 1, 0)
        d['recStateFL'] = np.where(np.isnan(d['A48b']), np.nan, d['recStateFL'])
        d['recStateFL'] = np.where(np.isnan(d['recStateFL']) & (d['anypay'] == 0), 0, d['recStateFL'])

        d['recStateDL'] = np.where(d['A48c'] == 1, 1, 0)
        d['recStateDL'] = np.where(np.isnan(d['A48c']), np.nan, d['recStateDL'])
        d['recStateDL'] = np.where(np.isnan(d['recStateDL']) & (d['anypay'] == 0), 0, d['recStateDL'])

        d['recStatePay'] = np.where((d['recStateFL'] == 1) | (d['recStateDL'] == 1), 1, 0)
        d['recStatePay'] = np.where(np.isnan(d['recStateFL']) | np.isnan(d['recStateDL']), np.nan, d['recStatePay'])


        # --------------------------
        # Indicator of cannot afford taking more leaves when needed
        # a direct indicator from FMLA data
        # --------------------------
        d['unaffordable'] = np.where(d['B15_1_CAT'] == 5, 1, 0)
        d['unaffordable'] = np.where(np.isnan(d['B15_1_CAT']), np.nan, d['unaffordable'])

        # -------------
        # resp_len
        # resp_len will flag 0/1 for a worker that will take longer leave if offered financially more generous leave policy
        # -------------

        # Initiate
        d['resp_len'] = np.nan

        # LEAVE_CAT: employed only
        # EMPLOYED ONLY workers have no need and take no leave, would not respond anyway
        d.loc[d['LEAVE_CAT'] == 3, 'resp_len'] = 0

        # A55 asks if worker would take longer leave if paid?
        d.loc[(d['resp_len'].isna()) & (d['A55'] == 2), 'resp_len'] = 0
        d.loc[(d['resp_len'].isna()) & (d['A55'] == 1), 'resp_len'] = 1

        # The following variables indicate whether leave was cut short for financial issues
        # A23c: unable to afford unpaid leave due to leave taking
        # A53g: cut leave time short to cover lost wages
        # A62a: return to work because cannot afford more leaves
        # B15_1_CAT, B15_2_CAT: can't afford unpaid leave
        d.loc[(d['resp_len'].isna()) & ((d['A23c'] == 1) |
                                        (d['A53g'] == 1) |
                                        (d['A62a'] == 1) |
                                        (d['B15_1_CAT'] == 5) |
                                        (d['B15_2_CAT'] == 5)), 'resp_len'] = 1
        # Assign 0s to A23c, A53g, A62a among remaining workers who indicate reasons other than unaffordability
        # B15_1_CAT and B15_2_CAT only has one group (cod = 5) identified as constrained by unaffordability
        # These financially-constrained workers were assigned resp_len=1 above, all other cornered workers would not respond
        # Check reasons of no leave among rest: d[d['resp_len'].isna()].B15_1_CAT.value_counts().sort_index()
        # all reasons unsolved by replacement generosity
        d.loc[(d['resp_len'].isna()) & (d['A23c'] == 2), 'resp_len'] = 0
        d.loc[(d['resp_len'].isna()) & (d['A53g'] == 2), 'resp_len'] = 0
        d.loc[(d['resp_len'].isna()) & (d['A62a'] == 2), 'resp_len'] = 0
        d.loc[(d['resp_len'].isna()) & (d['B15_1_CAT'].notna()), 'resp_len'] = 0
        d.loc[(d['resp_len'].isna()) & (d['B15_2_CAT'].notna()), 'resp_len'] = 0

        # Assume all takers/needers with ongoing condition are 'cornered' and would respond with longer leaves
        # A10_1, A10_2: regular/ongoing condition, takers and dual
        # B11_1, B11_2: regular/ongoing condition, needers and dual
        d.loc[(d['resp_len'].isna()) &
              ((d['A10_1'] == 2) | (d['A10_1'] == 3) | (d['B11_1'] == 2) | (d['B11_1'] == 3)), 'resp_len'] = 1

        # Check LEAVE_CAT of rest: d[d['resp_len'].isna()]['LEAVE_CAT'].value_counts().sort_index()
        # 267 takers and 3 needers
        # with no evidence in data of need solvable / unsolvable by $, assume solvable to be conservative
        d.loc[d['resp_len'].isna(), 'resp_len'] = 1

        # optional - get multiple leave type variables
        #d = get_multiple_leave_vars(d, types)

        # Save data
        d.to_csv(self.fp_fmla_out, index=False)
        print('File saved: clean FMLA data file before CPS imputation.')
        return None

    def impute_fmla_cps(self, fp_cps_in, fp_cps_out):
        '''

        :param fp_cps: file path to original CPS data
        '''
        t0 = time()
        # Read in processed FMLA data and raw CPS data, and clean CPS data
        d = pd.read_csv(self.fp_fmla_out, low_memory=False)
        cps = pd.read_csv(fp_cps_in)

        cps['female'] = np.where(cps['a_sex'] == 2, 1, 0)
        cps['female'] = np.where(cps['a_sex'].isna(), np.nan, cps['female'])
        cps['black'] = np.where(cps['prdtrace'] == 2, 1, 0)
        cps['black'] = np.where(cps['prdtrace'].isna(), np.nan, cps['black'])
        cps['age'] = cps['a_age']
        cps['agesq'] = cps['age'] * cps['age']
        cps['BA'] = np.where(cps['a_hga'] == 43, 1, 0)
        cps['BA'] = np.where(cps['a_hga'].isna(), np.nan, cps['BA'])
        cps['GradSch'] = np.where((cps['a_hga'] <= 46) & (cps['a_hga'] >= 44), 1, 0)
        cps['GradSch'] = np.where(cps['a_hga'].isna(), np.nan, cps['GradSch'])
        for i in cps['a_mjind'].value_counts().sort_index().index:
            cps['ind_%s' % i] = np.where(cps['a_mjind'] == i, 1, 0)
            cps['ind_%s' % i] = np.where(cps['a_mjind'].isna(), np.nan, cps['ind_%s' % i])
        for i in cps['a_mjocc'].value_counts().sort_index().index:
            cps['occ_%s' % i] = np.where(cps['a_mjocc'] == i, 1, 0)
            cps['occ_%s' % i] = np.where(cps['a_mjocc'].isna(), np.nan, cps['occ_%s' % i])

        cps['hourly'] = np.where(cps['a_hrlywk'] == 1, 1, 0)
        cps['hourly'] = np.where((cps['a_hrlywk'] == 0) | (cps['a_hrlywk'].isna()), np.nan, cps['hourly'])
        cps['empsize'] = cps['noemp']
        cps['oneemp'] = np.where(cps['phmemprs'] == 1, 1, 0)
        cps['oneemp'] = np.where(cps['phmemprs'].isna(), np.nan, cps['oneemp'])

        cps = cps[['female', 'black', 'age', 'agesq', 'BA', 'GradSch'] +
                  ['ind_%s' % x for x in range(1, 14)] +
                  ['occ_%s' % x for x in range(1, 11)] +
                  ['hourly', 'empsize', 'oneemp', 'wkswork', 'marsupwt']]  # remove armed forces code from ind/occ
        cps = cps.dropna(how='any')
        '''
        do logit below:
        y: paid hourly, employer size, single employer last year, weeks worked (subject to FMLA weeks worked categories if any)
        x: chars - female, black, age, agesq, BA, GradSch, all occ codes, all ind codes

        '''

        # logit regressions
        X = cps[['female', 'black', 'age', 'agesq', 'BA', 'GradSch']]
        w = cps['marsupwt']
        Xd = fillna_binary(d[X.columns], self.random_state)
        # cps based hourly paid indicator
        y = cps['hourly']
        clf = sklearn.linear_model.LogisticRegression(solver='liblinear').fit(X, y, sample_weight=w)
        d['hourly_fmla'] = pd.Series(clf.predict(Xd))
        # one employer last year
        y = cps['oneemp']
        clf = sklearn.linear_model.LogisticRegression(solver='liblinear').fit(X, y, sample_weight=w)
        d['oneemp_fmla'] = pd.Series(clf.predict(Xd))
        # ordered logit - TODO: mord package not ideal for handling empsize, cannot impute all 6 levels, best is LogisticAT().
        # employer size
        y = cps['empsize']
        clf = mord.LogisticAT().fit(X, y)
        d['empsize_fmla'] = pd.Series(clf.predict(Xd))
        # regression
        # weeks worked
        y = cps['wkswork']
        clf = sklearn.linear_model.LinearRegression().fit(X, y, sample_weight=w)
        d['wkswork_fmla'] = pd.Series(clf.predict(Xd))

        # Save files
        d.to_csv(self.fp_fmla_out, index=False)
        cps.to_csv(fp_cps_out, index=False)
        progress = 'CPS cleaned and CPS variable imputation done for FMLA. Time elapsed = %s seconds' % (round(time() - t0, 2))
        print(progress)
        print('File saved: clean FMLA data file after CPS imputation.')
        return progress

    def get_length_distribution(self, fp_length_distribution_out):
        '''

        :param fp_length_distribution_out: file path to json outfile of leave length distribution estimated from FMLA
        '''
        # Read in cleaned FMLA data
        d = pd.read_csv(self.fp_fmla_out, low_memory=False)

        # A dictionary to store results
        dct = {}
        types = ['own', 'matdis', 'bond', 'illchild', 'illspouse', 'illparent']
        # For PFL states
        dct_PFL = {}
        for t in types:
            xps = d[(d['recStatePay'] == 1) & (d['take_type'] == t)][['weight', 'length']].groupby('length').sum()
            xps = (xps / xps.sum()).sort_index()
            xps.columns = ['']
            xps = [(i, v.values[0]) for i, v in xps.iterrows()]
            dct_PFL[t] = xps
        dct['PFL'] = dct_PFL
        # For non-PFL states
        dct_NPFL = {}
        for t in types:
            xps = d[(d['recStatePay'] == 0) & (d['take_type'] == t)][['weight', 'length']].groupby('length').sum()
            xps = (xps / xps.sum()).sort_index()
            xps.columns = ['']
            xps = [(i, v.values[0]) for i, v in xps.iterrows()]
            dct_NPFL[t] = xps
        dct['non-PFL'] = dct_NPFL

        # Save as json

        with open(fp_length_distribution_out, 'w') as f:
            json.dump(dct, f, sort_keys=True, indent=4)
            f.close()

        print('File saved: leave distribution estimated from FMLA data.')
        return None





