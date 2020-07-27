"""
main simulation engine
chris zhang 2/14/2020
"""
# CHANGES 11/14/2019
# Made dual receiver as proportionate parameter of those individuals with >0 prop_pay_employer.
# Removed imputation of doctor/hospital variables
# Removed multiple leave types imputation
# Temporary - removed link between cp-len and generosity. Now set cp-len = mn-len for all types,
# and dual receiver status becomes irrelevant - may add this link back later
# Modified take up rate to randomly draw rows until target take up population is created
# Used common 'denom' ACS data for takeup flags - eligible workers after excl. per wage12/wkweek/wksworked/empsize
# Used take up rates = official case load data / eligible worker pop. This is indep from sim,
# skipping the hard-to-estimate taker/needer pops
# Finding - logit sim fewest takers/needers, ridge most, rest methods in between.
# but these do not cause huge diff in cost est, given the small empirical takeup ratios
# logit does not sim enough takers/needers to reach required takeup in RI. Other methods okay
# Applied logic control - age range of taker/needer of matdis, bond (set max to 50)


# TODO: make a note in doc about 1.02 POW factor if user tries to create pop est / needers_full_part override

import pandas as pd
import numpy as np
import bisect
import json
from time import time
from _5a_aux_functions import get_columns, get_sim_col, get_weighted_draws, get_na_count
import sklearn.linear_model
import sklearn.naive_bayes
import sklearn.neighbors
import sklearn.tree
import sklearn.ensemble
import sklearn.gaussian_process
import sklearn.svm
import xgboost
from datetime import datetime
import matplotlib.pyplot as plt
import os
import csv
from _1_clean_FMLA_2018 import DataCleanerFMLA as dcf18
from _1_clean_FMLA_2012 import DataCleanerFMLA as dcf12
from _4_clean_ACS import DataCleanerACS
from Utils import check_dependency, get_sim_name, create_cost_chart, STATE_CODES


class SimulationEngine:
    def __init__(self, st, yr, fmla_wave, fps_in, fps_out, clf_name='Logistic Regression Regularized',
                 state_of_work=True, random_state=None, pow_pop_multiplier=1.02, q=None):
        """
        :param st: state name, 'ca', 'ma', etc.
        :param yr: end year of 5-year ACS
        :param fmla_wave: wave of FMLA data, 2012 or 2018
        :param fps_in: filepaths of infiles (FMLA, ACS h, ACS p, CPS)
        :param fps_out: filepaths of outfiles files (FMLA, CPS, ACS, length distribution, master ACS post sim)
        :param clf_name: classifier name
        """

        self.st = st
        self.yr = yr
        self.fmla_wave = fmla_wave
        self.fp_fmla_in = fps_in[0]
        self.fp_cps_in = fps_in[1]
        self.fp_acsh_in = fps_in[2]  # directory only for ACS household file
        self.fp_acsp_in = fps_in[3]  # directory only for ACS person file
        self.fp_dir_out = fps_out[0]
        self.fp_fmla_out = fps_out[1]
        self.fp_cps_out = fps_out[2]
        self.fp_acs_out = fps_out[3]  # directory only for cleaned ACS file
        # fp to length distributions in days estimated from restricted FMLA
        self.fp_length_distribution_out = fps_out[4]
        self.clf_name = clf_name
        self.state_of_work = state_of_work
        self.random_seed = random_state
        print('Random seed:', self.random_seed)
        self.random_state = np.random.RandomState(self.random_seed)

        # leave types
        self.types = ['own', 'matdis', 'bond', 'illchild', 'illspouse', 'illparent']

        # a dict from clf_name to clf
        self.d_clf = {
            'Logistic Regression GLM': ['logit glm',
                                        sklearn.linear_model.LogisticRegression(solver='liblinear',
                                                                                multi_class='auto',
                                                                                random_state=self.random_state)],
            'Logistic Regression Regularized': sklearn.linear_model.LogisticRegression(solver='liblinear', multi_class='auto',
                                                                           random_state=self.random_state),
            'Ridge Classifier': sklearn.linear_model.RidgeClassifier(random_state=self.random_state),
            'Naive Bayes': sklearn.naive_bayes.MultinomialNB(),
            'Support Vector Machine': sklearn.svm.SVC(probability=True, gamma='auto', random_state=self.random_state),
            'Random Forest': sklearn.ensemble.RandomForestClassifier(random_state=self.random_state),
            'K Nearest Neighbor': sklearn.neighbors.KNeighborsClassifier(),
            'XGBoost': xgboost.XGBClassifier(objective='binary:logistic') # = multi:softmax as needed in get_sim_col
        }

        # out id for creating unique out folder to store all model outputs
        self.out_id = datetime.now().strftime('%Y%m%d_%H%M%S')
        self.prog_para = []
        self.sim_count = 0
        self.output_directories = []

        self.updates = []
        self.progress = 0
        self.figure = None
        self.progress = 0
        self.q = q

        # POW population weight multiplier
        self.pow_pop_multiplier = pow_pop_multiplier  # based on 2012-2016 ACS, see project acs_all

    def set_simulation_params(self, elig_wage12, elig_wkswork, elig_yrhours, elig_empsize, rrp, wkbene_cap, d_maxwk,
                              d_takeup, incl_private, incl_empgov_fed, incl_empgov_st, incl_empgov_loc, incl_empself,
                              needers_fully_participate, clone_factor, dual_receivers_share, alpha,
                              min_takeup_cpl, wait_period, recollect, min_cfl_recollect,
                              dependency_allowance, dependency_allowance_profile, leave_types=None, sim_num=None):
        params = {
            'elig_wage12': elig_wage12,
            'elig_wkswork': elig_wkswork,
            'elig_yrhours': elig_yrhours,
            'elig_empsize': elig_empsize,
            'rrp': rrp,
            'wkbene_cap': wkbene_cap,
            'd_maxwk': d_maxwk,
            'd_takeup': d_takeup,
            'incl_private': incl_private,
            'incl_empgov_fed': incl_empgov_fed,
            'incl_empgov_st': incl_empgov_st,
            'incl_empgov_loc': incl_empgov_loc,
            'incl_empself': incl_empself,
            'needers_fully_participate': needers_fully_participate,
            'clone_factor': clone_factor,
            'dual_receivers_share': dual_receivers_share,
            'alpha': alpha,
            'min_takeup_cpl': min_takeup_cpl,
            'wait_period': wait_period,
            'recollect': recollect,
            'min_cfl_recollect': min_cfl_recollect,
            'dependency_allowance': dependency_allowance,
            'dependency_allowance_profile': dependency_allowance_profile,
            'leave_types': leave_types if leave_types is not None else self.types
        }

        if type(sim_num) == int and -1 < sim_num < self.sim_count:
            self.prog_para[sim_num] = params
        else:
            self.prog_para.append(params)
            sim_name = get_sim_name(self.sim_count).lower()
            self.output_directories.append(os.path.join(self.fp_dir_out, 'output_%s_%s' % (self.out_id, sim_name)))
            self.sim_count += 1

    def delete_simulation_params(self, sim_num):
        del self.prog_para[sim_num]
        self.sim_count -= 1

    def save_program_parameters(self, sim_num):
        # create output folder
        params = self.prog_para[sim_num]
        output_directory = self.output_directories[sim_num]

        if not os.path.isdir(output_directory):
            os.makedirs(output_directory)

        # save meta file of program parameters
        para_labels = ['State', 'Year', 'Place of Work', 'Minimum Annual Wage', 'Minimum Annual Work Weeks',
                       'Minimum Annual Work Hours', 'Minimum Employer Size', 'Proposed Wage Replacement Ratio',
                       'Weekly Benefit Cap', 'Include Private Employees',
                       'Include Goverment Employees, Federal',
                       'Include Goverment Employees, State', 'Include Goverment Employees, Local',
                       'Include Self-employed', 'Simulation Method', 'Share of Dual Receivers',
                       'Alpha', 'Minimum Leave Length Applied',
                       'Waiting Period', 'Recollect Benefits of Waiting Period', 'Minimum Leave Length for Recollection',
                       'Dependent Allowance',
                       'Dependent Allowance Profile: Increments of Replacement Ratio by Number of Dependants',
                       'Clone Factor','Random Seed']
        para_labels_m = ['Maximum Week of Benefit Receiving',
                         'Take Up Rates']  # type-specific parameters

        para_values = [self.st.upper(), self.yr, self.state_of_work, params['elig_wage12'],
                       params['elig_wkswork'], params['elig_yrhours'], params['elig_empsize'], params['rrp'],
                       params['wkbene_cap'], params['incl_private'],
                       params['incl_empgov_fed'], params['incl_empgov_st'],
                       params['incl_empgov_loc'], params['incl_empself'], self.clf_name, params['dual_receivers_share'],
                       params['alpha'], params['min_takeup_cpl'],
                       params['wait_period'], params['recollect'], params['min_cfl_recollect'],
                       params['dependency_allowance'], params['dependency_allowance_profile'],
                       params['clone_factor'], self.random_seed]
        para_values_m = [params['d_maxwk'], params['d_takeup']]

        d = pd.DataFrame(para_values, index=para_labels)
        dm = pd.DataFrame.from_dict(para_values_m)
        dm = dm.rename(index=dict(zip(dm.index, para_labels_m)))

        with open('%s/prog_para_%s.csv' % (output_directory, self.out_id), 'w', newline='') as f:
            writer = csv.writer(f)
            for idx, row in d.iterrows():
                writer.writerow((idx, row[0]))
            writer.writerow([])
            writer.writerow([''] + list(dm.columns))
            for idx, row in dm.iterrows():
                writer.writerow(([idx] + list(row.values)))
        return None

    def prepare_data(self):
        if self.fmla_wave == 2018:
            dcf = dcf18(self.fp_fmla_in, self.fp_fmla_out, self.random_state)
        elif self.fmla_wave == 2012:
            dcf = dcf12(self.fp_fmla_in, self.fp_fmla_out, self.random_state)
        dcf.clean_data()
        self.__put_queue({'type': 'progress', 'engine': None, 'value': 10})
        self.__put_queue({'type': 'message', 'engine': None,
                          'value': 'File saved: clean FMLA data file before CPS imputation.'})
        # message = dcf.impute_fmla_cps(self.fp_cps_in, self.fp_cps_out)
        # self.__put_queue({'type': 'message', 'engine': None, 'value': message})
        # self.__put_queue({'type': 'progress', 'engine': None, 'value': 20})
        # self.__put_queue({'type': 'message', 'engine': None,
        #                   'value': 'File saved: clean FMLA data file after CPS imputation.'})
        if self.fmla_wave == 2018: # if 2012, length distribution is input data based on restricted FMLA PUF
            dcf.get_length_distribution(self.fp_length_distribution_out)
        self.__put_queue({'type': 'progress', 'engine': None, 'value': 25})
        self.__put_queue({'type': 'message', 'engine': None,
                          'value': 'File saved: leave distribution estimated from FMLA data.'})

        self.__put_queue({'type': 'message', 'engine': None,
                          'value': 'Cleaning ACS data. State chosen = %s. Chunk size = 100000 ACS rows' % self.st})
        # set yr_adjinc = self.fmla_wave to inflation-adjust
        dca = DataCleanerACS(self.st, self.yr, self.fp_acsh_in, self.fp_acsp_in, self.fp_acs_out, self.state_of_work,
                             self.random_state, self.fmla_wave, self.prog_para[0]['incl_private'],
                             self.prog_para[0]['incl_empgov_fed'], self.prog_para[0]['incl_empgov_st'],
                             self.prog_para[0]['incl_empgov_loc'], self.prog_para[0]['incl_empself'])
        message = dca.clean_person_data(self.fp_cps_in)
        self.__put_queue({'type': 'progress', 'engine': None, 'value': 50})
        self.__put_queue({'type': 'message', 'engine': None, 'value': message})
        self.progress = 50
        return None

    def get_acs_simulated(self, sim_num, chunksize=100000):

        tsim = time()
        params = self.prog_para[sim_num]
        pfl = 'non-PFL'  # status of PFL as of ACS sample period
        d = pd.read_csv(self.fp_fmla_out, low_memory=False)
        with open(self.fp_length_distribution_out) as f:
            flen = json.load(f)

        acs_fp_in = os.path.join(self.fp_acs_out, 'ACS_cleaned_forsimulation_%s_%s.csv' % (self.yr, self.st))
        acs_fp_out = '%s/acs_sim_%s_%s.csv' % (self.output_directories[sim_num], self.st, self.out_id)
        append = False

        # Read in cleaned ACS and FMLA data, and FMLA-based length distribution
        ichunk = 1
        n_eligible_workers = 0 # number of eligible workers (acs.PWGTP.sum()), to be update in chunk loop
        # set clf, and set chunksize large if clf needs standardization (so cannot chunk)
        clf = self.d_clf[self.clf_name]
        if isinstance(clf, (sklearn.linear_model.LogisticRegression,
                            sklearn.linear_model.RidgeClassifier,
                            sklearn.neighbors.KNeighborsClassifier,
                            sklearn.svm.SVC)):
            chunksize = 10**7
        # get sim cols by chunks
        for acs in pd.read_csv(acs_fp_in, chunksize=chunksize):
            # Sample restriction - reduce to eligible workers (all elig criteria indep from simulation below)

            # drop government and self-employed workers based on user input
            if not params['incl_private']:
                acs = acs.drop(acs[(acs['COW'] == 1) | (acs['COW'] == 2)].index)
            if not params['incl_empgov_fed']:
                acs = acs.drop(acs[acs['empgov_fed'] == 1].index)
            if not params['incl_empgov_st']:
                acs = acs.drop(acs[acs['empgov_st'] == 1].index)
            if not params['incl_empgov_loc']:
                acs = acs.drop(acs[acs['empgov_loc'] == 1].index)
            if not params['incl_empself']:
                acs = acs.drop(acs[(acs['COW'] == 6) | (acs['COW'] == 7)].index)

            # check other program eligibility
            acs['elig_prog'] = 0
            elig_empsizebin = 0
            if 1 <= params['elig_empsize'] < 10:
                elig_empsizebin = 1
            elif 10 <= params['elig_empsize'] <= 49:
                elig_empsizebin = 2
            elif 50 <= params['elig_empsize'] <= 99:
                elig_empsizebin = 3
            elif 100 <= params['elig_empsize'] <= 499:
                elig_empsizebin = 4
            elif 500 <= params['elig_empsize'] <= 999:
                elig_empsizebin = 5
            elif params['elig_empsize'] >= 1000:
                elig_empsizebin = 6
            acs.loc[(acs['wage12'] >= params['elig_wage12']) &
                    (acs['wkswork'] >= params['elig_wkswork']) &
                    (acs['wkswork'] * acs['wkhours'] >= params['elig_yrhours']) &
                    (acs['empsize'] >= elig_empsizebin), 'elig_prog'] = 1
            # drop ineligible workers (based on wage/work/empsize)
            acs = acs.drop(acs[acs['elig_prog'] != 1].index)

            # Expand ACS if clone factor > 1
            # shrink all weights by factor
            if params['clone_factor'] > 1:
                for wt in ['PWGTP'] + ['PWGTP' + str(x) for x in range(1, 81)]:
                    acs[wt] = acs[wt] / params['clone_factor']
                # then expand acs by factor
                acs = pd.concat([acs] * params['clone_factor'])

            # Train models using FMLA, and simulate on ACS workers
            t0 = time()
            gov_workers_only = False
            if not params['incl_private'] and not params['incl_empself']:
                gov_workers_only = True
            col_Xs, col_ys, col_w =get_columns(self.fmla_wave, params['leave_types'], gov_workers_only=gov_workers_only)
            X = d[col_Xs]
            w = d[col_w]
            Xa = acs[X.columns]

            for c in col_ys:
                tt = time()
                y = d[c]
                if c in ['take_matdis', 'need_matdis']:  # restricted sample for matdis, join using indexed simcol
                    simcol_indexed = get_sim_col(X, y, w, Xa, clf, self.random_state)
                    simcol_indexed = pd.Series(simcol_indexed, index=Xa[(Xa['female'] == 1) &
                                                                        (Xa['nochildren'] == 0) &
                                                                        (Xa['age'] <= 50)].index, name=c)
                    acs = acs.join(simcol_indexed)
                elif c in ['take_bond', 'need_bond']:  # restricted sample for bond
                    simcol_indexed = get_sim_col(X, y, w, Xa, clf, self.random_state)
                    simcol_indexed = pd.Series(simcol_indexed, index=Xa[(Xa['nochildren'] == 0) &
                                                                        (Xa['age'] <= 50)].index, name=c)
                    acs = acs.join(simcol_indexed)
                elif c in ['take_illspouse', 'need_illspouse']:  # restricted sample for illspouse
                    simcol_indexed = get_sim_col(X, y, w, Xa, clf, self.random_state)
                    simcol_indexed = pd.Series(simcol_indexed, index=Xa[(Xa['nevermarried'] == 0) &
                                                                        (Xa['divorced'] == 0)].index, name=c)
                    acs = acs.join(simcol_indexed)
                else:  # sim col same length as acs
                    acs[c] = get_sim_col(X, y, w, Xa, clf, self.random_state)
                print('Simulation of col %s done for chunk %s. Time elapsed = %s' % (c, ichunk, (time() - tt)))
            print('6 take_type variables, 6 need_type variables, and resp_len simulated for chunk %s. '
                  'Time elapsed = %s' % (ichunk, (time() - t0)))

            # Post-simluation logic control
            acs.loc[acs['male'] == 1, 'take_matdis'] = 0
            acs.loc[acs['male'] == 1, 'need_matdis'] = 0
            acs.loc[(acs['nevermarried'] == 1) | (acs['divorced'] == 1), 'take_illspouse'] = 0
            acs.loc[(acs['nevermarried'] == 1) | (acs['divorced'] == 1), 'need_illspouse'] = 0
            acs.loc[acs['nochildren'] == 1, 'take_bond'] = 0
            acs.loc[acs['nochildren'] == 1, 'need_bond'] = 0
            acs.loc[acs['nochildren'] == 1, 'take_matdis'] = 0
            acs.loc[acs['nochildren'] == 1, 'need_matdis'] = 0
            acs.loc[acs['age'] > 50, 'take_matdis'] = 0
            acs.loc[acs['age'] > 50, 'need_matdis'] = 0
            acs.loc[acs['age'] > 50, 'take_bond'] = 0
            acs.loc[acs['age'] > 50, 'need_bond'] = 0

            # Conditional simulation - anypay for taker/needer sample
            acs['taker'] = [max(z) for z in acs[['take_%s' % t for t in params['leave_types']]].values]
            acs['needer'] = [max(z) for z in acs[['need_%s' % t for t in params['leave_types']]].values]
            X = d[(d['taker'] == 1) | (d['needer'] == 1)][col_Xs]
            w = d.loc[X.index][col_w]
            Xa = acs[(acs['taker'] == 1) | (acs['needer'] == 1)][X.columns]
            if len(Xa) == 0:
                print('Warning: Neither leave taker nor leave needer present in simulated ACS persons. '
                      'Simulation gives degenerate scenario of zero leaves for all workers.')
            else:
                for c in ['anypay']:
                    y = d.loc[X.index][c]
                    simcol_indexed = get_sim_col(X, y, w, Xa, clf, self.random_state)
                    simcol_indexed = pd.Series(simcol_indexed, index=Xa.index, name=c)
                    acs = acs.join(simcol_indexed)

            # Conditional simulation - prop_pay_employer for anypay=1 sample
            X = d[(d['anypay'] == 1) & (d['prop_pay_employer'].notna())][col_Xs]
            w = d.loc[X.index][col_w]
            Xa = acs[acs['anypay'] == 1][X.columns]
            # a dict from prop_pay_employer int category to numerical prop_pay_employer value
            # int category used for phat 'p_0', etc. in get_sim_col
            v = d.prop_pay_employer.value_counts().sort_index().index
            k = range(len(v))
            d_prop = dict(zip(k, v))
            D_prop = dict(zip(v, k))

            if len(Xa) == 0:
                pass
            else:
                y = [D_prop[x] for x in d.loc[X.index]['prop_pay_employer']]
                yhat = get_sim_col(X, y, w, Xa, clf, self.random_state)
                # prop_pay_employer labels are from 1 to 6, get_sim_col() vectorization sum gives 0~5, increase label by 1
                yhat = pd.Series(data=[x + 1 for x in yhat], index=Xa.index, name='prop_pay_employer')
                acs = acs.join(yhat)
                acs.loc[acs['prop_pay_employer'].notna(), 'prop_pay_employer'] = [d_prop[x] for x in
                                                                acs.loc[acs['prop_pay_employer'].notna(), 'prop_pay_employer']]

            # Sample restriction - reduce to simulated takers/needers, append dropped rows later before saving post-sim acs
            acs_neither_taker_needer = acs[(acs['taker'] == 0) & (acs['needer'] == 0)]
            acs = acs.drop(acs_neither_taker_needer.index)

            # Draw status-quo leave length for each type
            # Without-program lengths - draw from FMLA-based distribution (pfl indicator = 0)
            # note: here, cumsum/bisect is 20% faster than np/choice.
            # But when simulate_wof applied as lambda to df, np/multinomial is 5X faster!
            # t0 = time()
            for t in params['leave_types']:
                acs['len_%s' % t] = 0
                n_lensim = len(acs.loc[acs['take_%s' % t] == 1])  # number of acs workers who need length simulation
                ps = [x[1] for x in flen[pfl][t]]  # prob vector of length of type t
                cs = np.cumsum(ps)
                lens = []  # initiate list of lengths
                for i in range(n_lensim):
                    lens.append(flen[pfl][t][bisect.bisect(cs, self.random_state.random_sample())][0])
                acs.loc[acs['take_%s' % t] == 1, 'len_%s' % t] = np.array(lens)

            # Max needed lengths (mnl) - draw from simulated without-program length distribution
            # conditional on max length >= without-program length
            for t in params['leave_types']:
                # t0 = time()
                acs['mnl_%s' % t] = 0
                # resp_len = 0 workers' mnl = status-quo length
                acs.loc[acs['resp_len'] == 0, 'mnl_%s' % t] = acs.loc[acs['resp_len'] == 0, 'len_%s' % t]
                # resp_len = 1 workers' mnl draw from length distribution conditional on new length > sq length
                # dict from sq length to possible greater length value, and associated weight of worker who provides the
                # length
                dct_vw = {}
                x_max = acs['len_%s' % t].max()
                for x in acs['len_%s' % t].value_counts().index:
                    if x < x_max:
                        dct_vw[x] = acs[(acs['len_%s' % t] > x)][['len_%s' % t, 'PWGTP']].groupby(by='len_%s' % t)[
                            'PWGTP'].sum().reset_index()
                        mx = len(acs[(acs['resp_len'] == 1) & (acs['len_%s' % t] == x)])
                        vxs = self.random_state.choice(dct_vw[x]['len_%s' % t], mx,
                                                       p=dct_vw[x]['PWGTP'] / dct_vw[x]['PWGTP'].sum())
                        acs.loc[(acs['resp_len'] == 1) & (acs['len_%s' % t] == x), 'mnl_%s' % t] = vxs
                    else:
                        acs.loc[(acs['resp_len'] == 1) & (acs['len_%s' % t] == x), 'mnl_%s' % t] = int(x * 1.25)

            # logic control of mnl
            acs.loc[acs['male'] == 1, 'mnl_matdis'] = 0
            acs.loc[(acs['nevermarried'] == 1) | (acs['divorced'] == 1), 'mnl_illspouse'] = 0
            acs.loc[acs['nochildren'] == 1, 'mnl_bond'] = 0
            acs.loc[acs['nochildren'] == 1, 'mnl_matdis'] = 0
            acs.loc[acs['age'] > 50, 'mnl_matdis'] = 0
            acs.loc[acs['age'] > 50, 'mnl_bond'] = 0

            # check if sum of mnl hits max = 52*5 = 260. If so, use max=260 to distribute prop to mnl of 6 types
            acs['mnl_all'] = [x.sum() for x in acs[['mnl_%s' % x for x in params['leave_types']]].values]
            for t in params['leave_types']:
                acs.loc[acs['mnl_all'] > 260, 'mnl_%s' % t] = [int(x) for x in
                                                               acs.loc[acs['mnl_all'] > 260, 'mnl_%s' % t] /
                                                               acs.loc[acs['mnl_all'] > 260, 'mnl_all'] * 260]
            # the mnl-capped workers would must have sq-len no larger than mn-len
            for t in params['leave_types']:
                acs.loc[acs['mnl_all'] > 260, 'len_%s' % t] = acs.loc[acs['mnl_all'] > 260, 'mnl_%s' % t]

            # If do following, then ignores link between generosity (rrp) and cf-len, cp-len
            # # set covered-by-program leave lengths (cp-len) as maximum needed leave lengths (mn-len) for each type
            # for t in params['leave_types:
            #     acs['cpl_%s' % t] = acs['mnl_%s' % t]

            # Given fraction of dual receiver x among anypay=1, simulate dual/single receiver status among anypay=1
            acs['dual_receiver'] = 0
            ws = acs[acs['anypay'] == 1]['PWGTP']  # weights of anypay=1
            acs.loc[acs['anypay'] == 1, 'dual_receiver'] = get_weighted_draws(ws, params['dual_receivers_share'],
                                                                              self.random_state)
            # check if target pop is achieved among anypay=1
            s_dual_receiver = acs[(acs['anypay'] == 1) &
                                  (acs['dual_receiver'] == 1)]['PWGTP'].sum() / acs[acs['anypay'] == 1]['PWGTP'].sum()
            s_dual_receiver = round(s_dual_receiver, 2)
            print('Specified share of dual-receiver = %s. Post-sim weighted share = %s' %
                  (params['dual_receivers_share'], s_dual_receiver))

            # Simulate counterfactual leave lengths (cf-len) for dual receivers
            # First get col of effective rrp for each person, subject to adding any dependency allowance, up to 1
            dependency_allowance = params['dependency_allowance']
            dependency_allowance_profile = params[
                'dependency_allowance_profile']  # rrp increment by ndep, len of this is max ndep allowed
            cum_profile = np.cumsum(np.array(dependency_allowance_profile))
            acs['effective_rrp'] = params['rrp']
            if dependency_allowance:
                acs['effective_rrp'] += [
                    cum_profile[int(min(x, len(cum_profile))) - 1]  # ndep-1 to get index in cum_profile
                    if x > 0 else 0 for x in acs['ndep_spouse_kid']]
                acs['effective_rrp'] = [min(x, 1) for x in acs['effective_rrp']]

            # Given cf-len, get cp-len
            # With program, effective rr =min(rre+rrp, 1), assuming responsiveness diminishes if full replacement attainable
            for t in params['leave_types']:
                acs['cfl_%s' % t] = np.nan
                # Get cf-len for dual receivers among anypay=1
                # use [(rre, sql), (1, mnl)] to interpolate cfl at rre+rrp, regardless rre+rrp<1 or not
                acs.loc[acs['dual_receiver'] == 1, 'cfl_%s' % t] = \
                    acs.loc[acs['dual_receiver'] == 1, 'len_%s' % t] + \
                    (acs.loc[acs['dual_receiver'] == 1, 'mnl_%s' % t] - acs.loc[acs['dual_receiver'] == 1, 'len_%s' % t]) * \
                    acs.loc[acs['dual_receiver'] == 1, 'effective_rrp'] / (
                                1 - acs.loc[acs['dual_receiver'] == 1, 'prop_pay_employer'])
                # if rre+rrp>=1, set cfl = mnl
                acs.loc[(acs['dual_receiver'] == 1) & (acs['prop_pay_employer'] >= 1 - acs['effective_rrp']), 'cfl_%s' % t] = \
                    acs.loc[(acs['dual_receiver'] == 1) & (acs['prop_pay_employer'] >= 1 - acs['effective_rrp']), 'mnl_%s' % t]
                # Get covered-by-program leave lengths (cp-len) for dual receivers among anypay=1
                # subtract wait period, down to 0
                # wait period benefit recollection indicator, min cfl needed for recollection
                # RI = [False, nan], NJ = [True, 15]
                wait_period = params['wait_period']
                recollect, min_cfl_recollect = params['recollect'], params['min_cfl_recollect']

                acs.loc[acs['dual_receiver'] == 1, 'cpl_%s' % t] = \
                    [max(x, 0) for x in (acs.loc[acs['dual_receiver'] == 1, 'cfl_%s' % t] - wait_period).values]
                if recollect:
                    acs.loc[(acs['dual_receiver'] == 1) & (acs['cfl_%s' % t] >= min_cfl_recollect), 'cpl_%s' % t] = \
                        acs.loc[(acs['dual_receiver'] == 1) & (acs['cfl_%s' % t] >= min_cfl_recollect), 'cfl_%s' % t]
                # later will apply cap of coverage period (in weeks)

            # Simulate cf-len for single receivers among anypay=1
            # Given cf-len, get cp-len
            for t in params['leave_types']:
                # single receiver, rrp>rre. Assume will use state program benefit to replace employer benefit
                acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] < acs['effective_rrp']), 'cfl_%s' % t] = \
                    acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] < acs['effective_rrp']), 'len_%s' % t] + \
                    (acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] < acs['effective_rrp']), 'mnl_%s' % t] -
                     acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] < acs['effective_rrp']), 'len_%s' % t]) * \
                    (acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] < acs['effective_rrp']), 'effective_rrp']
                     - acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] < acs['effective_rrp']), 'prop_pay_employer']) \
                    / (1 - acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] < acs['effective_rrp']), 'prop_pay_employer'])
                # single receiver, rrp<=rre. Assume will not use any state program benefit
                # so still using same employer benefit as status-quo, thus cf-len = sq-len
                acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] >= acs['effective_rrp']), 'cfl_%s' % t] = \
                    acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] >= acs['effective_rrp']), 'len_%s' % t]
                # Get covered-by-program leave lengths (cp-len) for single receivers
                # if rrp<=rre, cp-len = 0
                acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] >= acs['effective_rrp']), 'cpl_%s' % t] = 0
                # if rrp>rre, cp-len = cf-len - wait period (assume covered by company), down to 0
                acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] < acs['effective_rrp']), 'cpl_%s' % t] = \
                    [max(x, 0) for x in
                     (acs.loc[(acs['dual_receiver'] == 0) &
                              (acs['prop_pay_employer'] < acs['effective_rrp']), 'cfl_%s' % t] - wait_period).values]
                # recollect if any
                if recollect:
                    acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] < acs['effective_rrp']) &
                            (acs['cfl_%s' % t] >= min_cfl_recollect), 'cpl_%s' % t] = \
                        acs.loc[(acs['dual_receiver'] == 0) & (acs['prop_pay_employer'] < acs['effective_rrp']) &
                                (acs['cfl_%s' % t] >= min_cfl_recollect), 'cfl_%s' % t]
                # later will apply cap of coverage period (in weeks)

            # Simulate cf-len for anypay=0 workers
            # Given cf-len, get cp-len
            for t in params['leave_types']:
                # anypay=0 workers extends leave by (mnl-sql)*rrp
                acs.loc[(acs['anypay'] == 0), 'cfl_%s' % t] = \
                    acs.loc[(acs['anypay'] == 0), 'len_%s' % t] + \
                    (acs.loc[(acs['anypay'] == 0), 'mnl_%s' % t] -
                     acs.loc[(acs['anypay'] == 0), 'len_%s' % t]) * acs['effective_rrp']
                # anypay=0 workers' cp-len is just cf-len - wait_period, down to 0, as they don't have company benefits
                acs.loc[(acs['anypay'] == 0), 'cpl_%s' % t] = \
                    [max(x, 0) for x in (acs.loc[(acs['anypay'] == 0), 'cfl_%s' % t] - wait_period).values]
                # recollect if any
                if recollect:
                    acs.loc[(acs['anypay'] == 0) & (acs['cfl_%s' % t] >= min_cfl_recollect), 'cpl_%s' % t] = \
                        acs.loc[(acs['anypay'] == 0) & (acs['cfl_%s' % t] >= min_cfl_recollect), 'cfl_%s' % t]

            # Set cp-len = 0 if missing
            for t in params['leave_types']:
                acs.loc[acs['cpl_%s' % t].isna(), 'cpl_%s' % t] = 0

            # Apply cap of coverage period (in weeks) to cpl_type (in days) for each leave type
            for t in params['leave_types']:
                acs.loc[acs['cpl_%s' % t] >= 0, 'cpl_%s' % t] = [min(x, 5 * params['d_maxwk'][t]) for x in
                                                                 acs.loc[acs['cpl_%s' % t] >= 0, 'cpl_%s' % t]]
            # acs now is taker/needer only, append acs_neither_taker_needer to get all eligible workers
            acs = acs.append(acs_neither_taker_needer, sort=True)

            # Save ACS data after finishing simulation
            if not append:
                acs.to_csv(acs_fp_out, index=False)
                n_eligible_workers = acs['PWGTP'].sum()
                append = True
            else:
                acs.to_csv(acs_fp_out, mode='a', index=False, header=False)
                n_eligible_workers += acs['PWGTP'].sum()

            # end of chunk loop, update ichunk
            ichunk +=1

        message = 'Leaves simulated for 5-year ACS %s-%s in state %s. Time needed = %s seconds. ' % \
                  ((self.yr-4), self.yr, self.st.upper(), round(time()-tsim, 0))
        message += '\nEstimate of total eligible workers in state = %s' % (n_eligible_workers*self.pow_pop_multiplier)
        print(message)

        self.progress += 40 / len(self.prog_para)
        self.__put_queue({'type': 'progress', 'engine': sim_num, 'value': self.progress})
        self.__put_queue({'type': 'message', 'engine': sim_num, 'value': message})

    def get_acs_with_takeup_flags(self, acs_taker_needer, acs_neither_taker_needer, col_w, params):

        # get 0/1 takeup flag using post-sim acs with only takers/needers
        # col_w = weight column, PWGTP for main, or PWGTPx for x-th rep weight in ACS data

        # We first append acs_neither_taker_needer back to post-sim acs, so we'll work with a common population
        # for new cols not in acs_neither_taker_needer, will create nan
        acs = acs_taker_needer.append(acs_neither_taker_needer, sort=True)
        # drop takeup flag cols if any
        for c in ['takeup_%s' % x for x in params['leave_types']]:
            if c in acs.columns:
                del acs[c]

        # Then perform a weighted random draw using user-specified take up rate until target pop is reached
        # set min cpl (covered-by-program length) for taking up program
        # TODO: validate min_takeup_cpl: min must = 1, cannot be 0
        min_takeup_cpl = params['min_takeup_cpl']
        alpha = params['alpha']
        for t in params['leave_types']:
            # set cpl = 0 if cpl less than min_takeup_cpl
            acs.loc[acs['cpl_%s' % t] < min_takeup_cpl, 'cpl_%s' % t] = 0
            # cap user-specified take up for type t by max possible takeup = s_positive_cpl, in pop per sim results
            s_positive_cpl = acs[acs['cpl_%s' % t] >= min_takeup_cpl][col_w].sum() / acs[col_w].sum()
            # init takeup_type = 0 for all rows
            acs['takeup_%s' % t] = 0
            # if share of positive cpl (cpl>min_takeup_cpl) is positive, draw program takers
            if s_positive_cpl > 0:
                # display warning for unable to reach target pop from simulated positive cpl_type pop
                if col_w == 'PWGTP':
                    if params['d_takeup'][t] > s_positive_cpl:
                        print('Warning: User-specified take up for type -%s- is capped '
                              'by maximum possible take up rate (share of positive covered-by-program length) '
                              'based on simulation results, at %s.' % (t, s_positive_cpl))
                takeup = min(s_positive_cpl, params['d_takeup'][t])
                p_draw = takeup / s_positive_cpl  # need to draw w/ prob=p_draw from cpl>=min_takeup_cpl subpop, to get desired takeup
                # get take up indicator for type t - weighted random draw from cpl_type>=min_takeup_cpl until target is reached
                if alpha >0:
                    draws = get_weighted_draws(acs[acs['cpl_%s' % t] >= min_takeup_cpl][col_w], p_draw, self.random_state,
                                               shuffle_weights=(acs[acs['cpl_%s' % t] >= min_takeup_cpl]['cpl_%s' % t])**alpha)
                elif alpha==0:
                    draws = get_weighted_draws(acs[acs['cpl_%s' % t] >= min_takeup_cpl][col_w], p_draw, self.random_state,
                                               shuffle_weights=None)
                else:
                    print('ERROR: alpha (exponent) of shuffle_weights should be non-negative. Please check!')
                acs.loc[acs['cpl_%s' % t] >= min_takeup_cpl, 'takeup_%s' % t] = draws

                # for main weight, check if target pop is achieved among eligible ACS persons
                if col_w == 'PWGTP':
                    s_takeup = acs[acs['takeup_%s' % t] == 1][col_w].sum() / acs[col_w].sum()
                    s_takeup = round(s_takeup, 4)
                    print('Specified takeup for type %s = %s. '
                          'Effective takeup = %s. '
                          'Post-sim weighted share = %s' % (t, params['d_takeup'][t], takeup, s_takeup))

        # takeup_any = 1 if any takeup_type = 1
        acs['takeup_any'] = [int(x.sum() > 0) for x in acs[['takeup_%s' % x for x in params['leave_types']]].values]

        # return ACS with all eligible workers (regardless of taker/needer status), with takeup_type flags sim'ed
        return acs


    def get_summary(self, sim_num):
        ## Get takeup cols for main and rep weights, then get costs/progtakers and costs_rep, progtakers_rep se, and ci
        # no chunking - otherwise will not be using sum of weights for entire acs sample

        ## Update post-sim ACS with takeup flags and annual benefits by type
        # read simulated ACS, and reduce to takers/needers
        fp_acs_sim = '%s/acs_sim_%s_%s.csv' % (self.output_directories[sim_num], self.st, self.out_id)
        output_directory = self.output_directories[sim_num]
        params = self.prog_para[sim_num]

        acs = pd.read_csv(fp_acs_sim)
        acs_taker_needer = acs[(acs['taker'] == 1) | (acs['needer'] == 1)]
        acs_neither_taker_needer = acs.drop(acs_taker_needer.index)

        # get takeup flag using main weight
        wt = 'PWGTP'
        acs = self.get_acs_with_takeup_flags(acs_taker_needer, acs_neither_taker_needer, wt, params)
        acs_taker_needer = acs[(acs['taker'] == 1) | (acs['needer'] == 1)]

        # get benefit received for each worker
        # apply take up flag and weekly benefit cap, and annual benefit for each worker, 6 types
        for t in params['leave_types']:
            # v = capped weekly benefit of leave type
            v = [min(x, params['wkbene_cap']) for x in
                 ((acs_taker_needer['wage12'] / acs_taker_needer['wkswork'] * acs_taker_needer['effective_rrp']))]
            # get annual benefit for leave type t - sumprod of capped benefit, and takeup flag for each ACS row
            acs_taker_needer['annual_benefit_%s' % t] = (v * acs_taker_needer['cpl_%s' % t] / 5 *
                                                         acs_taker_needer['takeup_%s' % t])
        # append acs_neither_taker_needer
        acs = acs_taker_needer.append(acs_neither_taker_needer, sort=True)
        # Save to same fp_acs_sim, updated with takeup flags and annual benefits by type
        acs.to_csv(fp_acs_sim, index=False)
        message = 'File saved: post-sim ACS with take-up flags and annual benefits simulated.'
        print(message)

        # apply take up flag and weekly benefit cap, and compute total cost, progtaker, 6 types
        costs, progtakers = {}, {}
        # v is capped weekly benefit (same for all types, assuming no multiple types within 1 week)
        v = [min(x, params['wkbene_cap']) for x in
             ((acs_taker_needer['wage12'] / acs_taker_needer['wkswork'] * acs_taker_needer['effective_rrp']))]
        v = np.array(v)
        # w is inflated weight for missing POW
        # for each leave type, get program outlay, progtaker
        for t in params['leave_types']:
            # get program cost for leave type t - sumprod of capped benefit, weight, and takeup flag for each ACS row
            w = acs_taker_needer['PWGTP'] * self.pow_pop_multiplier
            costs[t] = (v * acs_taker_needer['cpl_%s' % t] / 5 * w * acs_taker_needer['takeup_%s' % t]).sum()
            progtakers[t] = int(round((w * acs_taker_needer['takeup_%s' % t]).sum(), 0))
        costs['total'] = sum(list(costs.values()))
        progtakers['any'] = int(round((w * acs_taker_needer['takeup_any']).sum(), 0))
        print('Completed cost estimation - costs[total] = %s' % costs['total'])
        print('Completed program taker count estimation - progtakers[any] = %s' % progtakers['any'])

        # get takeup flag using 80 rep weights, and get se, ci
        t0_se = time()
        print('Computing standard errors for cost and progtaker estimates...')
        # suffices of rep wt col name PWGTPx x=1~80
        rep_wt_ixs = list(range(1, 81))
        # initialize costs dict from rep weight index to cost profile
        costs_rep = {}
        progtakers_rep = {}
        for wt in ['PWGTP%s' % x for x in rep_wt_ixs]:
            costs_rep_wt = {}
            progtakers_rep_wt = {}
            # get takeup_type flags for acs under current rep weight
            # acs_taker_needer contains takeup cols from main weight, but get_acs_with_takeup_flags \
            # has a del step to remove takeup cols
            acs = self.get_acs_with_takeup_flags(acs_taker_needer, acs_neither_taker_needer, wt, params)
            acs_taker_needer = acs[(acs['taker'] == 1) | (acs['needer'] == 1)]
            # v is capped weekly benefit (same for all types, assuming no multiple types within 1 week)
            v = [min(x, params['wkbene_cap']) for x in
                 ((acs_taker_needer['wage12'] / acs_taker_needer['wkswork'] * acs_taker_needer['effective_rrp']))]
            v = np.array(v)
            # w is inflated weight for missing POW
            w = acs_taker_needer[wt] * self.pow_pop_multiplier
            for t in params['leave_types']:
                # get program cost for leave type t - sumprod of capped benefit, weight, and takeup flag
                costs_rep_wt[t] = (v * acs_taker_needer['cpl_%s' % t] / 5 * w * acs_taker_needer['takeup_%s' % t]).sum()
                # get program taker counts for leave type t - sumprod of weight, and takeup flag
                progtakers_rep_wt[t] = int(round((w * acs_taker_needer['takeup_%s' % t]).sum(), 0))
            costs_rep_wt['total'] = sum(list(costs_rep_wt.values()))
            progtakers_rep_wt['any'] = int(round((w * acs_taker_needer['takeup_any']).sum(), 0))
            # update cost_rep
            costs_rep[wt] = costs_rep_wt
            progtakers_rep[wt] = progtakers_rep_wt
        # end of wt loop

        # compute standard error using replication weights, then compute confidence interval (lower bound at 0)
        # methodology reference: https://usa.ipums.org/usa/repwt.shtml

        # costs
        sesq = dict(zip(costs.keys(), [0] * len(costs.keys())))
        for wt in ['PWGTP%s' % x for x in rep_wt_ixs]:
            for k in costs_rep[wt].keys():
                sesq[k] += 4 / 80 * (costs[k] - costs_rep[wt][k]) ** 2
        for k, v in sesq.items():
            sesq[k] = v ** 0.5
        ci_costs = {}
        for k, v in sesq.items():
            ci_costs[k] = (max(costs[k] - 1.96 * sesq[k], 0), costs[k] + 1.96 * sesq[k])
        t_se = round((time()-t0_se), 0)

        # progtakers
        sesq = dict(zip(progtakers.keys(), [0] * len(progtakers.keys())))
        for wt in ['PWGTP%s' % x for x in rep_wt_ixs]:
            for k in progtakers_rep[wt].keys():
                sesq[k] += 4 / 80 * (progtakers[k] - progtakers_rep[wt][k]) ** 2
        for k, v in sesq.items():
            sesq[k] = v ** 0.5
        ci_progtakers = {}
        for k, v in sesq.items():
            ci_progtakers[k] = (int(round(max(progtakers[k] - 1.96 * sesq[k], 0), 0)),
                                int(round(progtakers[k] + 1.96 * sesq[k], 0)))
        t_se = round((time()-t0_se), 0)
        # print a timing message for summary statistics (costs, progtakers)
        print('Completed computing standard errors for summary estimates. Time needed = %s seconds' % t_se)

        # Save output
        # cost
        varname_summary = 'cost'
        fp_out_summary = '%s/program_%s_%s_%s.csv' % (output_directory, varname_summary, self.st, self.out_id)
        out = self.save_summary(costs, ci_costs, varname_summary, fp_out_summary)
        message = 'Output saved - cost summary. Total cost = $%s million %s dollars' % \
                  (round(out.loc[out['type']=='total', 'cost'].values[0]/1000000, 1), self.fmla_wave)
        print(message)

        # progtaker
        varname_summary = 'progtaker'
        fp_out_summary = '%s/program_%s_%s_%s.csv' % (output_directory, varname_summary, self.st, self.out_id)
        out = self.save_summary(progtakers, ci_progtakers, varname_summary, fp_out_summary)
        message = 'Output saved - progtaker summary. Total progtakers = $%s' % \
                  (round(out.loc[out['type']=='any', 'progtaker'].values[0]/1))
        print(message)


        # out_costs = pd.DataFrame.from_dict(costs, orient='index')
        # out_costs = out_costs.reset_index()
        # out_costs.columns = ['type', 'cost']
        #
        # out_ci = pd.DataFrame.from_dict(ci, orient='index')
        # out_ci = out_ci.reset_index()
        # out_ci.columns = ['type', 'ci_lower', 'ci_upper']
        #
        # out = pd.merge(out_costs, out_ci, how='left', on='type')
        #
        # d_tix = {'own': 1, 'matdis': 2, 'bond': 3, 'illchild': 4, 'illspouse': 5, 'illparent': 6, 'total': 7}
        # out['tix'] = [d_tix[x] for x in out['type']]
        #
        # out = out.sort_values(by='tix')
        # del out['tix']
        #
        # out.to_csv('%s/program_cost_%s_%s.csv' % (output_directory, self.st, self.out_id), index=False)
        # message = 'Output saved. Total cost = $%s million %s dollars' % \
        #           (round(out.loc[out['type']=='total', 'cost'].values[0]/1000000, 1), self.fmla_wave)
        # print(message)
        self.progress += 10 / len(self.prog_para)
        self.__put_queue({'type': 'progress', 'engine': sim_num, 'value': self.progress})
        self.__put_queue({'type': 'message', 'engine': sim_num, 'value': message})
        return out  # df of leave type specific costs and total cost, along with ci's

    def save_summary(self, dct_summary, dct_ci, varname_summary, fp_out_summary):
        '''

        :param dct_summary: dict of summary statistics, like costs
        :param dct_ci: dict of confidence interval
        :param varname_summary:  variable name string, like 'cost', 'progtaker', TB used in out col label
        :param fp_out_summary: fp to save summary table
        :return:
        '''
        # save output summary table (costs, progtakers)
        out_level = pd.DataFrame.from_dict(dct_summary, orient='index')
        out_level = out_level.reset_index()
        out_level.columns = ['type', varname_summary]

        out_ci = pd.DataFrame.from_dict(dct_ci, orient='index')
        out_ci = out_ci.reset_index()
        out_ci.columns = ['type', 'ci_lower', 'ci_upper']

        out = pd.merge(out_level, out_ci, how='left', on='type')
        if varname_summary=='cost':
            d_tix = {'own': 1, 'matdis': 2, 'bond': 3, 'illchild': 4, 'illspouse': 5, 'illparent': 6, 'total': 7}
        if varname_summary=='progtaker':
            d_tix = {'own': 1, 'matdis': 2, 'bond': 3, 'illchild': 4, 'illspouse': 5, 'illparent': 6, 'any': 7}
        out['tix'] = [d_tix[x] for x in out['type']]

        out = out.sort_values(by='tix')
        del out['tix']

        # out.to_csv('%s/program_%s_%s_%s.csv' % (varname_summary, fp_out_summary, self.st, self.out_id), index=False)
        out.to_csv(fp_out_summary, index=False)

        return out



    def create_chart(self, out, sim_num):
        output_directory = self.output_directories[sim_num]
        # Plot costs and ci
        fig = create_cost_chart(out, self.st)
        plt.savefig('%s/total_cost_%s_%s_%s' % (output_directory, self.yr, self.st, self.out_id),
                    facecolor='#333333', edgecolor='white')
        self.figure = fig
        return fig

    def run(self):
        t0 = time()
        self.progress = 0
        try:
            self.check_dependencies()
            self.prepare_data()
        except Exception as e:
            self.__put_queue({'type': 'error', 'engine': None, 'value': e})
            raise

        for sim_num in range(len(self.prog_para)):
            try:
                self.save_program_parameters(sim_num)
                self.get_acs_simulated(sim_num)
                self.get_summary(sim_num)
            except Exception as e:
                self.__put_queue({'type': 'error', 'engine': sim_num, 'value': e})
                raise

        self.progress = 100
        self.__put_queue({'type': 'progress', 'engine': self.sim_count - 1, 'value': self.progress})
        print('Total runtime = %s seconds.' % (round(time() - t0, 0)))

    def check_dependencies(self):
        dependency_versions = {
            'matplotlib': '2.2.3',
            'mord': '0.5',
            'pandas': '0.23.0',
            'sklearn': '0.20.1'
        }

        for dependency, version in dependency_versions.items():
            if not check_dependency(dependency, version):
                self.__put_queue({'type': 'warning', 'engine': None, 'value': (dependency, version)})

    def __put_queue(self, obj):
        if self.q is not None:
            self.q.put(obj)

    def get_progress(self):
        result = self.progress, self.updates
        self.updates = []
        return result

    def get_results_file(self, sim_num):
        return '%s/acs_sim_%s_%s.csv' % (self.output_directories[sim_num], self.st, self.out_id)

    def get_results_files(self):
        return [self.get_results_file(i) for i in range(self.sim_count)]

    def get_results(self, sim_num):
        return pd.read_csv(self.get_results_file(sim_num))

    def get_cost_df(self, sim_num):
        return pd.read_csv('%s/program_cost_%s_%s.csv' % (self.output_directories[sim_num], self.st, self.out_id))

    def get_population_analysis_results(self, sim_num):
        # read in simulated acs, this is just df returned from get_acs_simulated()
        output_directory = self.output_directories[sim_num]
        params = self.prog_para[sim_num]
        d = pd.read_csv('%s/acs_sim_%s.csv' % (output_directory, self.out_id))
        # restrict to taker/needer only (workers with neither status have cpl_type = nan)
        # d = d[(d['taker']==1) | (d['needer']==1)]

        # restrict to workers who take up the program
        d['takeup_any'] = [int(x.sum() > 0) for x in d[['takeup_%s' % x for x in params['leave_types']]].values]
        d = d[d['takeup_any'] == 1]

        # make sure cpl_type is non-missing
        for t in params['leave_types']:
            d.loc[d['cpl_%s' % t].isna(),  'cpl_%s' % t] = 0

        # total covered-by-program length
        d['cpl'] = [sum(x) for x in d[['cpl_%s' % t for t in params['leave_types']]].values]
        # keep needed vars for population analysis plots
        columns = ['PWGTP', 'cpl', 'female', 'age', 'wage12', 'nochildren', 'asian', 'black', 'white', 'native',
                   'other', 'hisp']
        d = d[columns]
        return d