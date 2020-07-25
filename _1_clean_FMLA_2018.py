"""
This program takes the FMLA 2018 data and cleans it into a format to be used
for behavioral estimation
"""

import pandas as pd
import numpy as np
import sklearn.linear_model
import mord
import json
from time import time
from _5a_aux_functions import fillna_binary, fillna_df, get_multiple_leave_vars
import math

# TODO: resp_len defined exactly in wave18, so get 433 NA/4037 valid. Forced NA=1 in wave12. NT fix code for 12.
# TODO: remove occ = military from wage 18 sample when training/validating. MUST reset index for cleaned FMLA!

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

        ## Read in FMLA data
        d = pd.read_csv(self.fp_fmla_in, low_memory=False)
        # make all col name lower case
        d.columns = [x.lower() for x in d.columns]

        ## Create variables
        # Make empid to follow 0-order to be consistent with Python standard (e.g. indices output from kNN)
        d['empid'] = d['empid'] - 1

        # rename weight column
        d = d.rename(columns={'combo_trimmed_weight': 'weight'})
        # FMLA eligibility - use fmla_eligible, same as coveligd in FMLA 2012

        # Union status - in CPS but not ACS, can sim in ACS if FMLA data shows necessary
        d['union'] = np.where(d['d3']==1, 1, 0)
        d['union'] = np.where(d['d3'].isna(), np.nan, d['union'])

        # Hours per week (main job if multiple jobs, mid-point)
        # a common dict_wkhours for e0b_cat, e0g_cat
        dict_wkhours = {
            0: 0,
            1: 2.5,
            2: 7,
            3: 12,
            4: 17,
            5: 22,
            6: 27,
            7: 32,
            8: 37,
            9: 42,
            10: 47,
            11: 52,
            12: 57,  # boundary cat for e0b_cat, so approximation
            13: 62,
            14: 67,
            15: 77,
            16: 85,  # boundary cat for e0g_cat, so approximation
            np.nan: np.nan
        }
        d['wkhours'] = [dict_wkhours[x] if not np.isnan(x) else np.nan for x in d['e0b_cat']]
        d['wkhours_m'] = [dict_wkhours[x] if not np.isnan(x) else np.nan for x in
                          d['e0g_cat']]  # main job hours if 2+ jobs
        d['wkhours'] = np.where((d['wkhours'].isna()) & (d['wkhours_m'].notna()), d['wkhours_m'], d['wkhours'])
        del d['wkhours_m']

        # Employment at government - use govt_emp=1 (no fed/state/local info in FMLA 2018)
        d['emp_gov'] = np.where(d['govt_emp'] == 1, 1, 0)
        d['emp_gov'] = np.where(d['govt_emp'].isna(), np.nan, d['emp_gov'])

        # Employment at non-profit - use govt_emp=3
        d['emp_nonprofit'] = np.where(d['govt_emp'] == 3, 1, 0)
        d['emp_nonprofit'] = np.where(d['govt_emp'].isna(), np.nan, d['emp_nonprofit'])

        # Age
        dct_age = {
            1: 21,
            2: 27,
            3: 32,
            4: 37,
            5: 42,
            6: 47,
            7: 52,
            8: 57,
            9: 63,
            10: 70  # boundary category
        }
        d['age'] = [dct_age[x] if not np.isnan(x) else x for x in d['age_cat']]
        d['agesq'] = [x ** 2 if not np.isnan(x) else x for x in d['age']]

        # Sex
        d['female'] = np.where(d['gender_cat'] == 2, 1, 0)
        d['female'] = np.where(d['gender_cat'].isna(), np.nan, d['female'])

        # No children
        d['nochildren'] = np.where(d['d7_cat'] == 0, 1, 0)
        d['nochildren'] = np.where(np.isnan(d['d7_cat']), np.nan, d['nochildren'])

        # No spouse
        d['nospouse'] = np.where(d['d10'].isin([3, 4, 5, 6]), 1, 0)
        d['nospouse'] = np.where(np.isnan(d['d10']), np.nan, d['nospouse'])

        # No elderly dependent
        d['noelderly'] = np.where(d['d8_cat'] == 0, 1, 0)
        d['noelderly'] = np.where(np.isnan(d['d8_cat']), np.nan, d['noelderly'])

        # Number of dependents categories
        for x in range(5):
            d['ndep_kid_%s' % x] = np.where(d['d7_cat'] == x, 1, 0)
            d['ndep_kid_%s' % x] = np.where(np.isnan(d['d7_cat']), np.nan, d['ndep_kid_%s' % x])
        for x in range(4):
            d['ndep_old_%s' % x] = np.where(d['d7_cat'] == x, 1, 0)
            d['ndep_old_%s' % x] = np.where(np.isnan(d['d7_cat']), np.nan, d['ndep_old_%s' % x])

        # Educational level
        d['ltHS'] = np.where(d['educ_cat'] == 1, 1, 0)
        d['ltHS'] = np.where(np.isnan(d['educ_cat']), np.nan, d['ltHS'])

        d['someHS'] = np.where(d['educ_cat'] == 2, 1, 0)
        d['someHS'] = np.where(np.isnan(d['educ_cat']), np.nan, d['someHS'])

        d['HSgrad'] = np.where(d['educ_cat'] == 3, 1, 0)
        d['HSgrad'] = np.where(np.isnan(d['educ_cat']), np.nan, d['HSgrad'])

        d['someCol'] = np.where(d['educ_cat'].isin([5, 6]), 1, 0)  # some college/Associate's degree as in wave 2012
        d['someCol'] = np.where(np.isnan(d['educ_cat']), np.nan, d['someCol'])

        d['BA'] = np.where(d['educ_cat'] == 7, 1, 0)
        d['BA'] = np.where(np.isnan(d['educ_cat']), np.nan, d['BA'])

        d['GradSch'] = np.where(d['educ_cat'] == 8, 1, 0)
        d['GradSch'] = np.where(np.isnan(d['educ_cat']), np.nan, d['GradSch'])

        d['noHSdegree'] = np.where(d['educ_cat'].isin([1, 2]), 1, 0)
        d['noHSdegree'] = np.where(np.isnan(d['educ_cat']), np.nan, d['noHSdegree'])

        d['BAplus'] = np.where(d['educ_cat'].isin([7, 8]), 1, 0)
        d['BAplus'] = np.where(np.isnan(d['educ_cat']), np.nan, d['BAplus'])

        # Family income using midpoint of category
        dct_inc = dict(zip(range(1, 37),
                           [2500, 7500, 12500, 17500,
                            22500, 27500, 32500, 37500,
                            42500, 47500, 52500, 57500,
                            62500, 67500, 72500, 77500,
                            82500, 87500, 92500, 97500,
                            105000, 115000, 125000, 135000,
                            145000, 150000, 165000, 175000,
                            185000, 195000, 225000, 275000,
                            325000, 375000, 425000, 500000]))  # boundary cat is 450k+, appox by 500k
        d['faminc'] = [dct_inc[x] if not np.isnan(x) else x for x in d['nd4_cat']]
        d.loc[(d['faminc'] <= 0.01) & (~d['faminc'].isna()), 'faminc'] = 0.01  # set to 0.01 for any income <=0.01
        # Log income - set to log(0.01) for any reported income <=0.01, set to NA if income NA
        d['ln_faminc'] = [np.log(x) if not np.isnan(x) else x for x in d['faminc']]

        # Marital status
        d['married'] = np.where(d['d10'] == 1, 1, 0)
        d['married'] = np.where(np.isnan(d['d10']), np.nan, d['married'])

        d['partner'] = np.where(d['d10'] == 2, 1, 0)
        d['partner'] = np.where(np.isnan(d['d10']), np.nan, d['partner'])

        d['separated'] = np.where(d['d10'] == 3, 1, 0)
        d['separated'] = np.where(np.isnan(d['d10']), np.nan, d['separated'])

        d['divorced'] = np.where(d['d10'] == 4, 1, 0)
        d['divorced'] = np.where(np.isnan(d['d10']), np.nan, d['divorced'])

        d['widowed'] = np.where(d['d10'] == 5, 1, 0)
        d['widowed'] = np.where(np.isnan(d['d10']), np.nan, d['widowed'])

        d['nevermarried'] = np.where(d['d10'] == 6, 1, 0)
        d['nevermarried'] = np.where(np.isnan(d['d10']), np.nan, d['nevermarried'])

        # Race/ethnicity
        d['raceth'] = np.where((~np.isnan(d['d5'])) & (d['d5'] == 1), 7, d['race_cat'])

        d['native'] = np.where(d['raceth'] == 4, 1, 0)
        d['native'] = np.where(np.isnan(d['raceth']), np.nan, d['native'])

        d['asian'] = np.where(d['raceth'] == 3, 1, 0)
        d['asian'] = np.where(np.isnan(d['raceth']), np.nan, d['asian'])

        d['black'] = np.where(d['raceth'] == 2, 1, 0)
        d['black'] = np.where(np.isnan(d['raceth']), np.nan, d['black'])

        d['white'] = np.where(d['raceth'] == 1, 1, 0)
        d['white'] = np.where(np.isnan(d['raceth']), np.nan, d['white'])

        d['other'] = np.where(d['raceth'] == 5, 1, 0)
        d['other'] = np.where(np.isnan(d['raceth']), np.nan, d['other'])

        d['hisp'] = np.where(d['raceth'] == 7, 1, 0)
        d['hisp'] = np.where(np.isnan(d['raceth']), np.nan, d['hisp'])

        # leave length for most recent leave (approx by cat mid-point)
        dct_len = dict(zip(range(1, 19),
                           [1, 2, 3, 4, 5,
                            8, 13, 18, 23, 28, 33,
                            40, 48, 55, 65, 80, 105, 150]))  # boundary cat is 120+, appox by 150
        d['length'] = [dct_len[x] if not np.isnan(x) else x for x in d['a19_mr_cat']]

        # residence in paid leave state - use paid_leave_state
        # this replaces recStatePay derived from wave 2012

        # dummies for take_type, need_type, where type = own/matdis/bond/illchild/illspouse/illparent
        types = ['own', 'matdis', 'bond', 'illchild', 'illspouse', 'illparent']
        dct_take = dict(zip(types, [[1], [3, 20], [5, 8, 21], [11], [12, 16], [13]]))
        for k, v in dct_take.items():
            d['take_%s' % k] = np.where(d['a5_mr_cat'].isin(v), 1, 0)
            d['take_%s' % k] = np.where(d['a5_mr_cat'].isna(), np.nan, d['take_%s' % k])
            d['take_%s' % k] = np.where(d['leave_cat'].isin([2, 3]), 0, d['take_%s' % k])

        dct_need = dict(zip(types, [[1], [3, 20], [5, 8, 9, 21], [11], [12, 16], [13]]))
        for k, v in dct_need.items():
            d['need_%s' % k] = np.where(d['b6_cat'].isin(v), 1, 0)
            d['need_%s' % k] = np.where(d['b6_cat'].isna(), np.nan, d['need_%s' % k])
            d['need_%s' % k] = np.where(d['leave_cat'].isin([1, 3]), 0, d['need_%s' % k])


        # taker/needer status
        d['taker'] = [max(x) for x in d[['take_%s' % t for t in types]].values]
        d['needer'] = [max(x) for x in d[['need_%s' % t for t in types]].values]

        # most recent take/need leave type in 1 col
        d['take_type'] = np.nan
        for t in types:
            d.loc[(d['take_type'].isna()) & (d['take_%s' % t] == 1), 'take_type'] = t
        d['need_type'] = np.nan
        for t in types:
            d.loc[(d['need_type'].isna()) & (d['need_%s' % t] == 1), 'need_type'] = t

        # any pay received during leave
        d['anypay'] = np.where((d['a43'] == 1) | ((d['a43'] == 2) & (d['a43a'] == 2)), 1, 0)
        d['anypay'] = np.where(np.isnan(d['a43']), np.nan, d['anypay'])
        d['anypay'] = np.where(d['a43c'] == 4, 0, d['anypay'])

        # proportion of pay received from employer (mid point of ranges provided in FMLA)
        d['prop_pay_employer'] = np.nan
        # set prop=0 for rows
        d.loc[d['anypay'] == 0, 'prop_pay_employer'] = 0
        # set prop=1 for rows
        d['receive_no_state_benefit'] = np.where((d['paid_leave_state'] == 0) |  # in no-program state
                                                 ((d['a43i_d_cat'].isna()) & (d['a43i_e_cat'].isna()))
                                                 # no st benefit received
                                                 , 1, np.nan)
        d.loc[(d['receive_no_state_benefit'] == 1) & (d['a43c'] == 1), 'prop_pay_employer'] = 1
        # set prop in (0,1) for rows that has valid a43g_cat
        # get recent leave length with full pay (only for rows with valid a43g_cat)
        dct_lfp = dict(
            zip(range(17), [1, 2, 3, 4, 5, 8, 13, 18, 23, 28, 33, 38, 43, 48, 55, 75, 120]))  # boundary cat 90+
        d.loc[d['a43g_cat'].notna(), 'length_full_pay'] \
            = [dct_lfp[x] if not np.isnan(x) else x for x in d.loc[d['a43g_cat'].notna(), 'a43d_cat']]
        # get recent leave length with partial pay (only for rows with valid a43g_cat)
        dct_lpp = dict(zip(range(14), [1, 2, 3, 5, 8, 13, 18, 23, 28, 35, 43, 53, 75, 120]))  # boundary cat 90+
        d.loc[d['a43g_cat'].notna(), 'length_partial_pay'] \
            = [dct_lpp[x] if not np.isnan(x) else x for x in d.loc[d['a43g_cat'].notna(), 'a43f_cat']]
        # if length < length full + length partial, allocate proportionally between full/partial days
        d['allocate'] = np.where(
            (d['a43g_cat'].notna()) & (d['length'] < d['length_full_pay'] + d['length_partial_pay']),
            1, np.nan)
        d.loc[d['allocate'] == 1, 'length_full_pay'] \
            = [math.floor(x[0] * x[1] / (x[1] + x[2])) for x in d.loc[d['allocate'] == 1,
                                                                      ['length', 'length_full_pay',
                                                                       'length_partial_pay']].values]
        d.loc[d['allocate'] == 1, 'length_partial_pay'] \
            = [x[0] - x[1] for x in d.loc[d['allocate'] == 1, ['length', 'length_full_pay']].values]
        # estimate prop pay for rows with valid a43g_cat
        dct_rre = dict(zip(range(1, 10), [0.125, 0.375, 0.5, 0.55, 0.635, 0.685, 0.73, 0.78, 0.9]))  # boundary cat 0.8+
        d.loc[d['a43g_cat'].notna(), 'prop_pay_employer'] \
            = [(x[1] + x[2] * dct_rre[x[3]]) / x[0] for x in d.loc[d['a43g_cat'].notna(),
                                                                   ['length', 'length_full_pay', 'length_partial_pay',
                                                                    'a43g_cat']].values]

        # resp_len
        # resp_len will flag 0/1 for a worker that will take longer leave if offered financially more generous leave policy
        d['resp_len'] = np.nan
        # employed only workers have no need and take no leave, would not respond anyway
        d.loc[d['leave_cat'] == 3, 'resp_len'] = 0
        # a10_mr: health condition re leave - code 2/3/4
        d.loc[d['a10_mr'].isin([2, 3, 4]), 'resp_len'] = 1
        # a10_long_cat: health condition re leave - code 2/3
        d.loc[d['a10_long_cat'].isin([2, 3, 4]), 'resp_len'] = 1
        # a53g: forced to cut leave time short due to wage loss
        d.loc[d['a53g'] == 1, 'resp_len'] = 1
        # a55: would take longer leave if more pay
        d.loc[d['a55'] == 1, 'resp_len'] = 1
        # na62b: return to work due to leaves running out
        d.loc[d['na62b'] == 1, 'resp_len'] = 1
        # na62f: return to work due to no longer needing leaves (resp_len=0)
        d.loc[d['na62f'] == 1, 'resp_len'] = 0
        # b15e: couldn't afford to take unpaid leave
        d.loc[d['b15e'] == 1, 'resp_len'] = 1

        # FMLA coverage - use fmla_eligible
        # do not use self-perception of eligiblity (ne6) due to possible misunderstanding by worker

        # tenure at job - <1yr, [1, 3), [3, 5), [5, 10), 10yr+
        # ten0->e0a_cat (single job), ten1->e0f_cat(main if 1+ job), ten2->e0i_cat(second job if 1+ job)
        dct_tenure = {'ten0': {}, 'ten1': {}, 'ten2': {}}
        dct_tenure['ten0'] = dict(zip(list(range(1, 3)) +  # <1yr,
                                      list(range(3, 27)) +  # [1, 3),
                                      list(range(27, 51)) +  # [3, 5),
                                      list(range(51, 57)) +  # [5, 10),
                                      list(range(57, 85)),  # 10yr+
                                      ['0_1'] * 2 + ['1_3'] * 24 + ['3_5'] * 24 + ['5_10'] * 6 + ['10_up'] * 28))
        dct_tenure['ten1'] = dict(zip(list(range(1, 3)) +  # <1yr,
                                      list(range(3, 6)) +  # [1, 3),
                                      list(range(6, 8)) +  # [3, 5),
                                      list(range(8, 13)) +  # [5, 10),
                                      list(range(13, 27)),  # 10yr+
                                      ['0_1'] * 2 + ['1_3'] * 3 + ['3_5'] * 2 + ['5_10'] * 5 + ['10_up'] * 14))
        dct_tenure['ten2'] = dict(zip(list(range(1, 3)) +  # <1yr,
                                      list(range(3, 6)) +  # [1, 3),
                                      list(range(6, 8)) +  # [3, 5),
                                      list(range(8, 13)) +  # [5, 10),
                                      list(range(13, 23)),  # 10yr+
                                      ['0_1'] * 2 + ['1_3'] * 3 + ['3_5'] * 2 + ['5_10'] * 5 + ['10_up'] * 10))
        d['job_tenure'] = np.nan
        d.loc[d['e0a_cat'].notna(), 'job_tenure'] = [dct_tenure['ten0'][x] for x in
                                                     d.loc[d['e0a_cat'].notna(), 'e0a_cat']]
        d.loc[(d['job_tenure'].isna()) & (d['e0f_cat'].notna()), 'job_tenure'] \
            = [dct_tenure['ten1'][x] for x in d.loc[(d['job_tenure'].isna()) & (d['e0f_cat'].notna()), 'e0f_cat']]
        d.loc[(d['job_tenure'].isna()) & (d['e0i_cat'].notna()), 'job_tenure'] \
            = [dct_tenure['ten2'][x] for x in d.loc[(d['job_tenure'].isna()) & (d['e0i_cat'].notna()), 'e0i_cat']]
        tenure_cats = pd.get_dummies(d['job_tenure'], 'job_tenure', dummy_na=True)
        for c in tenure_cats.columns.drop('job_tenure_nan'):
            tenure_cats.loc[tenure_cats['job_tenure_nan'] == 1, c] = np.nan
        del tenure_cats['job_tenure_nan']
        d = d.join(tenure_cats)

        # paid hourly
        d['hourly'] = np.where(d['e9_cat'] == 2, 1, 0)
        d['hourly'] = np.where(d['e9_cat'].isna(), np.nan, d['hourly'])

        # occupation - map to CPS code (a_mjocc)
        dct_occ = dict(zip(list(range(1, 12)), [[11, 13],
                                                [15, 17, 19, 21, 23, 25, 27, 29],
                                                [31, 33, 35, 37, 39]] +
                           [[x] for x in range(41, 57, 2)]))
        for k, v in dct_occ.items():
            d['occ_%s' % k] = np.where(d['ne16_coded'].isin(dct_occ[k]), 1, 0)
            d['occ_%s' % k] = np.where(d['ne16_coded'].isna(), np.nan, d['occ_%s' % k])

        # industry - map to CPS code (a_mjind). Note: wave 18 data uses string for ind code
        dct_ind = dict(
            zip(list(range(1, 14)), [['11'], ['21'], ['23'], ['31-33'], ['42', '44-45'], ['22', '48-49'], ['51'],
                                     ['52'], ['53', '54', '55'], ['56', '61'], ['62', '71'], ['81'], ['92']]))
        for k, v in dct_ind.items():
            d['ind_%s' % k] = np.where(d['ne15_coded'].isin(dct_ind[k]), 1, 0)
            d['ind_%s' % k] = np.where(d['ne15_coded'].isin(['DR', 'S', '99999']), np.nan, d['ind_%s' % k])

        # categorize numerical cols
        # use hard value as cutoffs (must match with ACS cutoffs)
        dct_cuts = {
            'faminc': [40000, 70000], # no needed for ln_faminc, monotonic
            'age': [35, 65],  # not needed for agesq - cuts already accounts for non-linearity
            'wkhours': [20, 35]
        }
        for k, v in dct_cuts.items():
            d[k + '_grp1'] = np.where(d[k] < v[0], 1, 0)
            d[k + '_grp2'] = np.where((d[k] < v[1]) & (d[k] >= v[0]), 1, 0)
            d[k + '_grp3'] = np.where(d[k] >= v[1], 1, 0)
            for z in [1, 2, 3]:
                d[k + '_grp%s' % z] = np.where(d[k].isna(), np.nan, d[k + '_grp%s' % z])

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
        # note: read FMLA is only for FMLA 2012 which needs CPS imputation
        d = pd.read_csv(self.fp_fmla_out, low_memory=False)
        cps = pd.read_csv(fp_cps_in, low_memory=False)

        # CPS-imputation for new vars needed in FMLA
        # no new vars needed so far for FMLA 2018

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
            xps = d[(d['paid_leave_state'] == 1) & (d['take_type'] == t)][['weight', 'length']].groupby('length').sum()
            xps = (xps / xps.sum()).sort_index()
            xps.columns = ['']
            xps = [(i, v.values[0]) for i, v in xps.iterrows()]
            dct_PFL[t] = xps
        dct['PFL'] = dct_PFL
        # For non-PFL states
        dct_NPFL = {}
        for t in types:
            xps = d[(d['paid_leave_state'] == 0) & (d['take_type'] == t)][['weight', 'length']].groupby('length').sum()
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





