"""
This program loads in the raw ACS files, creates the necessary variables
for the simulation and saves a master dataset to be used in the simulations.

chris zhang 2/28/2019
"""
# -------------------------- #
# Housekeeping
# -------------------------- #

import pandas as pd
import numpy as np
from _5a_aux_functions import fillna_df, get_wquantile
import sklearn.linear_model
import mord
from time import time
from Utils import STATE_CODES
import os


class DataCleanerACS:

    def __init__(self, st, yr, fp_h, fp_p, fp_out, state_of_work, random_state, yr_adjinc,
                 private, self_emp, gov_fed, gov_st, gov_loc):
        '''
        :param st: state name, e.g.'ma'
        :param yr: end year of 5-year ACS, e.g. 2016
        :param fp_h: file path to ACS 5-year state household data, excl. CSV file name
        :param fp_p: file path to ACS 5-year state person data, excl. CSV file name
        :param fp_out: file path for output, excl. CSV file name
        :param state_of_work: if True then use POW data files, if False use state-of-residence files (original ACS)
        :param yr_adjinc: base year for inflation-adjusting dollars in ACS, 2012 or 2018
        :param worker_class: dict from worker type to eligibility boolean, default is private sector=True only
        '''

        self.st = st
        self.yr = yr
        self.fp_h = fp_h
        self.fp_p = fp_p
        self.fp_out = fp_out
        self.state_of_work = state_of_work
        self.random_state = random_state
        self.yr_adjinc = yr_adjinc
        self.private = private
        self.self_emp = self_emp
        self.gov_fed = gov_fed
        self.gov_st = gov_st
        self.gov_loc = gov_loc
        # a dict from fmla_wave, acs yr to adjinc
        # to get 2012 dollars, convert dollor to lowest ACS year then use ACS12-16 ADJINC
        # to get 2018 dollars, convert dollar to greatest ACS year then use ACS14-18 ADJINC
        self.dct_adjinc = dict(zip([2012, 2018], [{}, {}]))
        self.dct_adjinc[2012] = {
            2016: 1056030, 2017: 1061971*(1056030/1038170), 2018: 1070673*(1056030/1022342)
        }
        self.dct_adjinc[2018] = {
            2016: 1054346/1013097*1000000, 2017: 1035838/1013097*1000000, 2018: 1000000
        }
        # adjinc (7-digit) value to use in inflation adj factor = d['ADJINC']/adjinc
        self.adjinc = self.dct_adjinc[self.yr_adjinc][self.yr]
        # a dict from st to state code (e.g. 'ri' to 44, 'ca' to 6)
        self.dct_st = dict(zip(
            [x.lower() for x in
             ["AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA",
              "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
              "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"]
             ],
            ["01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16", "17", "18", "19", "20", "21", "22",
             "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
             "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"]
                               ))

    def load_hh_data(self, st):
        '''
        load and prepare ACS household data needed for merging to large ACS 5-year person file
        :return: reduced ACS household data for merging to ACS person data
        '''

        # Load ACS household data and create some variables
        # a single state
        if self.st.lower() == 'all' and not self.private and not self.self_emp and \
                (self.gov_fed or self.gov_st or self.gov_loc):
            print('\n\n\n~~~~~~~~ Reading gov worker master data ~~~~~~~~~~~~\n\n\n')
            fp_d_hh = self.fp_h + '/gov_workers_us_household_%s_fed_state_local.csv' % (str(self.yr))
        # national gov workers only
        else:
            fp_d_hh = self.fp_h + '/ss%sh%s.csv' % (str(self.yr)[-2:], st)
            if self.state_of_work:
                fp_d_hh = self.fp_h + '/h%s_%s_pow.csv' % (self.dct_st[st], st)

        d_hh = pd.read_csv(fp_d_hh, low_memory=False)
        # SERIALNO to string - chars in this ID from 2017
        d_hh['SERIALNO'] = d_hh['SERIALNO'].astype(str)
        # Number of dependents
        d_hh['nochildren'] = np.where(d_hh['NOC'] == 0, 1, 0)
        d_hh['nochildren'] = np.where(d_hh['NOC'].isna(), np.nan, d_hh['nochildren'])
        d_hh['noelderly'] = np.where(d_hh['R65'] == 0, 1, 0)
        d_hh['noelderly'] = np.where(d_hh['R65'].isna(), np.nan, d_hh['noelderly'])
        # ndep_kid, ndep_spouse - for dependency allowance in programs
        d_hh['ndep_kid'] = d_hh['NOC']
        d_hh['ndep_spouse'] = np.where(d_hh['FES'].isin([2,3]), 1, 0)
        d_hh['ndep_spouse_kid'] = d_hh['ndep_kid'] + d_hh['ndep_spouse']
        # Family income
        d_hh['faminc'] = d_hh['FINCP'] * d_hh['ADJINC'] / self.adjinc  # adjust to 2012 dollars to conform with FMLA 2012 data
        d_hh.loc[(d_hh['faminc'] < 0.01), 'faminc'] = 0.01  # force non-positive income to be epsilon to get meaningful log-income
        d_hh['ln_faminc'] = np.log(d_hh["faminc"])

        return d_hh[['SERIALNO', 'NPF', 'nochildren', 'noelderly', 'faminc', 'ln_faminc', 'PARTNER'] +
                    ['ndep_kid', 'ndep_spouse', 'ndep_spouse_kid']]
    
    def clean_person_state_data(self, st, cps, chunk_size=100000):
        ## First handle CPS data outside the ACS person data chunk loop
        # fill na in CPS
        cps = fillna_df(cps, self.random_state)
        # identify industry and occupation CPS codes for creating same cats in ACS in chunk loop
        top_ind = [cps[cps.ind_top1==1].a_mjind.value_counts().index[0]]
        top_ind += [cps[cps.ind_top2==1].a_mjind.value_counts().index[0]]
        top_ind += [cps[cps.ind_top3==1].a_mjind.value_counts().index[0]]
        top_occ = [cps[cps.occ_top1==1].a_mjocc.value_counts().index[0]]
        top_occ += [cps[cps.occ_top2==1].a_mjocc.value_counts().index[0]]
        top_occ += [cps[cps.occ_top3==1].a_mjocc.value_counts().index[0]]

        # use cps df to train models for predicting needed yvars in ACS person data chunks
        # fillna for cps - not needed if use cps_clean_year.csv (preprocessed)
        # cps = fillna_df(cps, self.random_state)

        # dict from yvar to impute to xvars
        xvars_in_acs = ['female', 'black', 'asian', 'native', 'other', 'age', 'agesq', 'BA', 'GradSch', 'married']
        xvars_in_acs += ['wage12', 'wkswork', 'wkhours', 'emp_gov']
        xvars_in_acs += ['occ_%s' % x for x in range(1, 11)] + ['ind_%s' % x for x in range(1, 14)]
        # xvars_in_acs += ['ind_top1', 'ind_top2', 'ind_top3', 'ind_other', 'ind_na']
        # xvars_in_acs += ['occ_top1', 'occ_top2', 'occ_top3', 'occ_other', 'occ_na']
        dct_cps_yx = {
            'hourly': xvars_in_acs, # filter rows to prerelg=1 only
            'empsize': xvars_in_acs,
            'oneemp':xvars_in_acs,
            'union': xvars_in_acs + ['hourly', 'empsize', 'oneemp'] # filter rows to prerelg=1 only
           }
        # dict to store CPS-based classifiers
        dct_cps_clfs = {}
        # empsize - mord
        y = cps['empsize']
        X = cps[dct_cps_yx['empsize']]
        clf = mord.LogisticAT().fit(X, y)
        dct_cps_clfs['empsize'] = clf
        # paid hourly, oneemp, union - logit
        for col_y in ['hourly', 'oneemp', 'union']:
            ix_train_cps = cps.index
            if col_y in ['hourly', 'union']: # too many NAs for hourly, union (prerelg=0), use non-NA rows to train
                ix_train_cps = cps[cps['prerelg']==1].index
            y = cps.loc[ix_train_cps, col_y]
            X = cps.loc[ix_train_cps, dct_cps_yx[col_y]]
            w = cps.loc[ix_train_cps, 'marsupwt']
            clf = sklearn.linear_model.LogisticRegression(solver='liblinear',
                                                          random_state=self.random_state).fit(X, y, sample_weight=w)
            dct_cps_clfs[col_y] = clf

        ## Get household data for merging to person data chunks
        d_hh = self.load_hh_data(st)

        ## Work on ACS person data in chunks
        # initiate output master person data to store cleaned up chunks
        dout = pd.DataFrame([])

        # clean person data by chunks
        ichunk = 1
        print('Cleaning ACS data. State chosen = %s. Chunk size = %s ACS rows' % (st.upper(), chunk_size))

        # set file path to person file, per state_of_work and worker_class filter values
        # a single state
        if self.st.lower() == 'all' and not self.private and not self.self_emp and \
                (self.gov_fed or self.gov_st or self.gov_loc):
            print('\n\n\n~~~~~~~~ Reading gov worker master data ~~~~~~~~~~~~\n\n\n')
            fp_d_p = self.fp_p + '/gov_workers_us_person_%s_fed_state_local.csv' % (str(self.yr))
        # national gov workers only
        else:
            fp_d_p = self.fp_p + "/ss%sp%s.csv" % (str(self.yr)[-2:], st)
            if self.state_of_work:
                fp_d_p = self.fp_p + '/p%s_%s_pow.csv' % (self.dct_st[st], st)

        # process person data by chunk
        for d in pd.read_csv(fp_d_p, chunksize=chunk_size, low_memory=False):
            # Convert SERIALNO (merge key) to string - chars in this ID from 2017
            d['SERIALNO'] = d['SERIALNO'].astype(str)
            # Merge with the household level variables
            d = pd.merge(d, d_hh, on='SERIALNO', how='left')

            # -------------------------- #
            # Remove ineligible workers, include only
            # civilian employed (ESR=1/2), and
            # have paid work (COW=1/2/3/4/5), including self-employed(COW=6/7)
            # -------------------------- #
            d = d[((d['ESR'] == 1) | (d['ESR'] == 2)) &
                  ((d['COW'] == 1) | (d['COW'] == 2) | (d['COW'] == 3) | (d['COW'] == 4) | (d['COW'] == 5) |
                   (d['COW'] == 6) | (d['COW'] == 7))]
            # Rename ACS variables to be consistent with FMLA data
            dct_rename = {'AGEP': 'age'}
            d = d.rename(columns=dct_rename)

            # duplicating age column for meshing with CPS estimate output
            d['a_age'] = d['age']

            # Create new ACS Variables
            mar_dummies = pd.get_dummies(d["MAR"])
            d['married'] = mar_dummies[1]
            d["widowed"] = mar_dummies[2]
            d["divorced"] = mar_dummies[3]
            d["separated"] = mar_dummies[4]
            d["nevermarried"] = mar_dummies[5]
            # use PARTNER in household data to tease out unmarried partners
            d['partner'] = np.where(
                (d['PARTNER'] == 1) | (d['PARTNER'] == 2) | (d['PARTNER'] == 3) | (d['PARTNER'] == 4), 1, 0)
            for m in ['married', 'widowed', 'divorced', 'separated', 'nevermarried']:
                d.loc[d['partner'] == 1, m] = 0

            d["male"] = pd.get_dummies(d["SEX"])[1]
            d["female"] = 1 - d["male"]
            d["agesq"] = d["age"] ** 2

            # Educational level
            d['sku'] = np.where(d['SCHL'].isna(), 0, d['SCHL'])
            d['ltHS'] = np.where(d['sku'] <= 15, 1, 0)
            d['HSgrad'] = np.where((d['sku'] >= 16) & (d['sku'] <= 17), 1, 0)
            d['someCol'] = np.where((d['sku'] >= 18) & (d['sku'] <= 20), 1, 0)
            d["BA"] = np.where(d['sku'] == 21, 1, 0)
            d["GradSch"] = np.where(d['sku'] >= 22, 1, 0)

            d["noHSdegree"] = np.where(d['sku'] <= 15, 1, 0)
            d["BAplus"] = np.where(d['sku'] >= 21, 1, 0)
            # variables for imputing hourly status, using CPS estimates from original model
            d["maplus"] = np.where(d['sku'] >= 22, 1, 0)
            d["ba"] = d['BA']

            # race
            d['hisp'] = np.where(d['HISP'] >= 2, 1, 0)
            d['white'] = np.where((d['hisp'] == 0) & (d['RAC1P'] == 1), 1, 0)
            d['black'] = np.where((d['hisp'] == 0) & (d['RAC1P'] == 2), 1, 0)
            d['asian'] = np.where((d['hisp'] == 0) & (d['RAC1P'] == 6), 1, 0)
            d['native'] = np.where((d['hisp'] == 0) & ((d['RAC1P'] == 3) |
                                                       (d['RAC1P'] == 4) |
                                                       (d['RAC1P'] == 5) |
                                                       (d['RAC1P'] == 7)), 1, 0)
            d['other'] = np.where(
                (d['hisp'] == 0) & (d['white'] == 0) & (d['black'] == 0) & (d['asian'] == 0) & (d['native'] == 0), 1, 0)

            # Employed
            d['employed'] = np.where((d['ESR'] == 1) |
                                     (d['ESR'] == 2) |
                                     (d['ESR'] == 4) |
                                     (d['ESR'] == 5),
                                     1, 0)
            d['employed'] = np.where(np.isnan(d['ESR']), np.nan, d['employed'])

            # Hours per week, total working weeks
            d['wkhours'] = d['WKHP']
            # Weeks worked - average of categories
            dict_wks = {
                1: 51,
                2: 48.5,
                3: 43.5,
                4: 33,
                5: 20,
                6: 7
            }
            d['wkswork'] = d['WKW'].map(dict_wks)
            d['wkswork'] = np.where(d['wkswork'].isna(), 0, d['wkswork'])
            # Weeks worked - WKW category bounds, for bounding imputed weeks worked from CPS
            dict_wkwBounds = {
                1: (50, 52),
                2: (48, 49),
                3: (40, 47),
                4: (27, 39),
                5: (14, 26),
                6: (1, 13)
            }
            # d['wkw_min'] = d['WKW'].apply(lambda x: dict_wkwBounds[x][0] if not np.isnan(x) else np.nan)
            # d['wkw_max'] = d['WKW'].apply(lambda x: dict_wkwBounds[x][1] if not np.isnan(x) else np.nan)
            d['wkw_min'] = [dict_wkwBounds[x][0] if not np.isnan(x) else np.nan for x in d['WKW']]
            d['wkw_max'] = [dict_wkwBounds[x][1] if not np.isnan(x) else np.nan for x in d['WKW']]

            # Total wage past 12m, adjusted to 2012, and its log
            d['wage12'] = d['WAGP'] * d['ADJINC'] / self.adjinc
            d['lnearn'] = np.nan
            # d.loc[d['wage12'] > 0, 'lnearn'] = d.loc[d['wage12'] > 0, 'wage12'].apply(lambda x: np.log(x))
            d.loc[d['wage12'] > 0, 'lnearn'] = [np.log(x) for x in d.loc[d['wage12'] > 0, 'wage12']]

            # hourly wage estimate
            d['wage_hourly'] = d['wage12']/d['wkswork']/d['wkhours']
            # low wage indicator
            d['low_wage'] = np.where(d['wage_hourly']<15, 1, 0)
            d['low_wage'] = np.where(d['wage_hourly'].isna(), np.nan, d['low_wage'])

            # health insurance from employer
            d['hiemp'] = np.where(d['HINS1'] == 1, 1, 0)
            d['hiemp'] = np.where(d['HINS1'].isna(), np.nan, d['hiemp'])

            # Employment at government
            # missing = age<16, or NILF over 5 years, or never worked
            d['emp_gov'] = np.where(d['COW'].isin([3,4,5]), 1, 0)
            d['emp_gov'] = np.where(d['COW'].isna(), np.nan, d['emp_gov'])
            d['empgov_fed'] = np.where(d['COW'] == 5, 1, 0)
            d['empgov_fed'] = np.where(np.isnan(d['COW']), np.nan, d['empgov_fed'])
            d['empgov_st'] = np.where(d['COW'] == 4, 1, 0)
            d['empgov_st'] = np.where(np.isnan(d['COW']), np.nan, d['empgov_st'])
            d['empgov_loc'] = np.where(d['COW'] == 3, 1, 0)
            d['empgov_loc'] = np.where(np.isnan(d['COW']), np.nan, d['empgov_loc'])

            # Employment at non-profit
            d['emp_nonprofit'] = np.where(d['COW']==2, 1, 0)
            d['emp_nonprofit'] = np.where(d['COW'].isna(), np.nan, d['emp_nonprofit'])

            # Occupation
            # use numeric OCCP = OCCP12 if ACS 2011-2015, or OCCP = OCCP if ACS 2012-2016
            if self.yr == 2015:
                if 'N.A.' in d['OCCP12'].value_counts().index:
                    d.loc[d['OCCP12'] == 'N.A.', 'OCCP12'] = np.nan
                d.loc[d['OCCP12'].notna(), 'OCCP12'] = [int(x) for x in d.loc[d['OCCP12'].notna(), 'OCCP12']]

                if 'N.A.' in d['OCCP10'].value_counts().index:
                    d.loc[d['OCCP10'] == 'N.A.', 'OCCP10'] = np.nan
                d.loc[d['OCCP10'].notna(), 'OCCP10'] = [int(x) for x in d.loc[d['OCCP10'].notna(), 'OCCP10']]

                d['OCCP'] = np.nan
                d['OCCP'] = np.where(d['OCCP12'].notna(), d['OCCP12'], d['OCCP'])
                d['OCCP'] = np.where((d['OCCP'].isna()) & (d['OCCP12'].isna()) & (d['OCCP10'].notna()), d['OCCP10'],
                                     d['OCCP'])
            elif self.yr == 2016:
                if 'N.A.' in d['OCCP'].value_counts().index:
                    d.loc[d['OCCP'] == 'N.A.', 'OCCP'] = np.nan
                d.loc[d['OCCP'].notna(), 'OCCP'] = [int(x) for x in d.loc[d['OCCP'].notna(), 'OCCP']]

            for c in range(1, 11):
                d['occ_%s' % c] = 0
            d.loc[(d['OCCP'] >= 10) & (d['OCCP'] <= 950), 'occ_1'] = 1
            d.loc[(d['OCCP'] >= 1000) & (d['OCCP'] <= 3540), 'occ_2'] = 1
            d.loc[(d['OCCP'] >= 3600) & (d['OCCP'] <= 4650), 'occ_3'] = 1
            d.loc[(d['OCCP'] >= 4700) & (d['OCCP'] <= 4965), 'occ_4'] = 1
            d.loc[(d['OCCP'] >= 5000) & (d['OCCP'] <= 5940), 'occ_5'] = 1
            d.loc[(d['OCCP'] >= 6000) & (d['OCCP'] <= 6130), 'occ_6'] = 1
            d.loc[(d['OCCP'] >= 6200) & (d['OCCP'] <= 6940), 'occ_7'] = 1
            d.loc[(d['OCCP'] >= 7000) & (d['OCCP'] <= 7630), 'occ_8'] = 1
            d.loc[(d['OCCP'] >= 7700) & (d['OCCP'] <= 8965), 'occ_9'] = 1
            d.loc[(d['OCCP'] >= 9000) & (d['OCCP'] <= 9750), 'occ_10'] = 1
            # make sure occ_x gets nan if OCCP code is nan
            for x in range(1, 11):
                d.loc[d['OCCP'].isna(), 'occ_%s' % x] = np.nan
            # set maj_occ in 1 col
            d['maj_occ'] = 0
            for x in range(1, 11):
                d.loc[d['occ_%s' % x]==1, 'maj_occ'] = x
            # get cols of top-3 occ codes, other, NA
            for rank in range(1, 4):
                d['occ_top%s' % rank] = np.where(d['occ_%s' % top_occ[rank-1]]==1, 1, 0)
            d['occ_other'] = np.where((~d['maj_occ'].isin(top_occ)), 1, 0)
            d['occ_na'] = np.where(d['maj_occ'].isna(), 1, 0)
            
            # Industry
            for c in range(1, 14):
                d['ind_%s' % c] = 0
            d.loc[(d['INDP'] >= 170) & (d['INDP'] <= 290), 'ind_1'] = 1
            d.loc[(d['INDP'] >= 370) & (d['INDP'] <= 490), 'ind_2'] = 1
            d.loc[(d['INDP'] == 770), 'ind_3'] = 1
            d.loc[(d['INDP'] >= 1070) & (d['INDP'] <= 3990), 'ind_4'] = 1
            d.loc[(d['INDP'] >= 4070) & (d['INDP'] <= 5790), 'ind_5'] = 1
            d.loc[((d['INDP'] >= 6070) & (d['INDP'] <= 6390)) | ((d['INDP'] >= 570) & (d['INDP'] <= 690)), 'ind_6'] = 1
            d.loc[(d['INDP'] >= 6470) & (d['INDP'] <= 6780), 'ind_7'] = 1
            d.loc[(d['INDP'] >= 6870) & (d['INDP'] <= 7190), 'ind_8'] = 1
            d.loc[(d['INDP'] >= 7270) & (d['INDP'] <= 7790), 'ind_9'] = 1
            d.loc[(d['INDP'] >= 7860) & (d['INDP'] <= 8470), 'ind_10'] = 1
            d.loc[(d['INDP'] >= 8560) & (d['INDP'] <= 8690), 'ind_11'] = 1
            d.loc[(d['INDP'] >= 8770) & (d['INDP'] <= 9290), 'ind_12'] = 1
            d.loc[(d['INDP'] >= 9370) & (d['INDP'] <= 9590), 'ind_13'] = 1
            # make sure ind_x gets nan if INDP code is nan
            for x in range(1, 14):
                d.loc[d['INDP'].isna(), 'ind_%s' % x] = np.nan
            # set maj_ind in 1 col
            d['maj_ind'] = 0
            for x in range(1, 14):
                d.loc[d['ind_%s' % x]==1, 'maj_ind'] = x

            # get cols of top-3 ind codes, other, NA
            for rank in range(1, 4):
                d['ind_top%s' % rank] = np.where(d['ind_%s' % top_ind[rank - 1]] == 1, 1, 0)
            d['ind_other'] = np.where((~d['maj_ind'].isin(top_ind)), 1, 0)
            d['ind_na'] = np.where(d['maj_ind'].isna(), 1, 0)

            # -------------------------- #
            # impute ACS vars using CPS-based classifiers built before the chunk loop
            # -------------------------- #
            Xd = fillna_df(d[xvars_in_acs], self.random_state)
            # paid hourly, oneemp, empsize, union
            for c in ['hourly', 'oneemp', 'empsize', 'union']: # imputing union needs previous vars, set union to end
                clf = dct_cps_clfs[c]
                if c=='union':
                    Xd = Xd.join(d[['hourly', 'oneemp', 'empsize']])
                # make prediction
                if c=='empsize': # ordered logit via mord, predict category directly
                    d[c] = pd.Series(clf.predict(Xd), index=d.index)
                else: # simulate from logit phat, not yhat, to avoid loss of predicted minor class (e.g. union=0)
                    ps = clf.predict_proba(Xd)  # get prob vector ps where each p = (p0, p1)
                    ps = ps[:, 1]  # get prob=1 for each row
                    us = np.random.rand(len(ps))  # random number
                    d[c] = [int(x) for x in ps > us]  # flag 1 if p1 > random number

            # get fmla_eligible col based on imputed vars above
            d['fmla_eligible'] = np.where((d['oneemp'] == 1) &
                                          (d['wkhours'] * d['wkswork'] >= 1250) &
                                          (d['empsize'] >= 3), 1, 0)
            
            # # categorize numerical cols in person files, still in chunk so cannot get percentiles/terciless
            # # use hard value as cutoffs
            # dct_cuts = {
            #     'age': [35, 65], # not needed for agesq - cuts already accounts for non-linearity
            #     'wkhours': [20, 35]
            # }
            # for k, v in dct_cuts.items():
            #     d[k + '_grp1'] = np.where(d[k] < v[0], 1, 0)
            #     d[k + '_grp2'] = np.where((d[k] < v[1]) & (d[k] >= v[0]), 1, 0)
            #     d[k + '_grp3'] = np.where(d[k] >= v[1], 1, 0)
            #     for z in [1, 2, 3]:
            #         d[k + '_grp%s' % z] = np.where(d[k].isna(), np.nan, d[k + '_grp%s' % z])
            # -------------------------- #
            # Save the resulting dataset
            # -------------------------- #
            cols = ['SERIALNO', 'SPORDER', 'PWGTP', 'ST', 'POWSP', 'NPF',
                    'employed', 'emp_gov', 'emp_nonprofit', 'empgov_fed', 'empgov_st', 'empgov_loc',
                    'wkhours', 'wkswork', 'wage12', 'low_wage', 'lnearn', 'hiemp',
                    'a_age', 'age', 'agesq',
                    'male', 'female',
                    'nochildren', 'noelderly', 'ndep_kid', 'ndep_spouse', 'ndep_spouse_kid',
                    'ltHS', 'HSgrad', 'someCol', 'BA', 'GradSch', 'noHSdegree', 'BAplus',
                    'faminc', 'ln_faminc',
                    'married', 'partner', 'separated', 'divorced', 'widowed', 'nevermarried',
                    'asian', 'black', 'white', 'native', 'other', 'hisp',
                    'ESR', 'COW'  # original ACS vars for restricting samples later
                    ]
            # hard-cutoff group cols
            # cols += [item for sublist in [[x + '_grp%s' % z for z in range(1, 4)] for x in dct_cuts.keys()]
            #          for item in sublist]
            # cols += ['faminc_grp1', 'faminc_grp2', 'faminc_grp3']

            cols += ['INDP'] + ['ind_%s' % x for x in range(1, 14)]
            cols += ['OCCP'] + ['occ_%s' % x for x in range(1, 11)]
            cols += ['hourly', 'oneemp', 'empsize', 'fmla_eligible', 'union']
            cols += ['WAGP', 'WKW'] #  'wkw_min', 'wkw_max'
            cols += ['PWGTP%s' % x for x in range(1, 81)]
            # reduced ACS chunk to be appended to output df dout
            d_reduced = d[cols]
            dout = dout.append(d_reduced)

            print('ACS data cleaned for chunk %s of person data...' % ichunk)
            ichunk += 1
        return dout

    def clean_person_data(self, cps_fp, chunk_size=100000):
        '''
        clean large ACS 5-year person file
        :return:
        '''
        t0 = time()

        # Load CPS data from impute_fmla_cps in FMLA cleaning class
        # set CPS year as mid-year of ACS5
        cps = pd.read_csv(cps_fp)  # set CPS year as mid-year of ACS5
        # Process ACS data
        # a single state
        if self.st.lower() != 'all':
            dout = self.clean_person_state_data(self.st, cps, chunk_size=chunk_size)
            dout.to_csv(self.fp_out + "ACS_cleaned_forsimulation_%s_%s.csv" % (self.yr, self.st), index=False,
                        header=True)
        # TODO: GUI's default_params.private etc. not updated per user input, not passed to worker_class in GUI
        # TODO: temp solution below - if st=ALL, do it for US Fed gov workers... need a formal fix
        else:
            dout = self.clean_person_state_data(self.st, cps, chunk_size=chunk_size)
            dout.to_csv(self.fp_out + "ACS_cleaned_forsimulation_%s_%s_gov.csv" % (self.yr, self.st), index=False,
                        header=True)
        # # all states, private workers eligible
        # elif self.worker_class['private']:
        #     for i, st in enumerate(STATE_CODES):
        #         dout = self.clean_person_state_data(st.lower(), self.worker_class, cps, chunk_size=chunk_size)
        #         if i == 0:
        #             dout.to_csv(self.fp_out + "ACS_cleaned_forsimulation_%s_%s.csv" % (self.yr, self.st), index=False,
        #                         header=True)
        #         else:
        #             dout.to_csv(self.fp_out + "ACS_cleaned_forsimulation_%s_%s.csv" % (self.yr, self.st), index=False,
        #                         mode='a', header=False)
        # # all states, private workers ineligible, self-emp ineligible, any gov workers eligible
        # elif not self.worker_class['self_emp'] and \
        #     (self.worker_class['gov_fed'] or self.worker_class['gov_st'] or self.worker_class['gov_loc']):
        #     dout = self.clean_person_state_data(self.st, self.worker_class, chunk_size=chunk_size)
        #     dout.to_csv(self.fp_out + "ACS_cleaned_forsimulation_%s_%s_gov.csv" % (self.yr, self.st), index=False,
        #                 header=True)


        t1 = time()
        message = 'ACS data cleaning finished for state %s. Time elapsed = %s seconds' % (self.st.upper(), round((t1 - t0), 0))
        print(message)
        return message
