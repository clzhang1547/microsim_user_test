'''
model validation for Issue Brief 2 (Summary of Model Testing Memo)

chris zhang 2/6/2020
'''
# TODO: for take/leave/resp_len, cross-validate using only rows with valid outvars, don't fillna. See if results change.
# TODO: in IB2 writeup - explain why ML outperforms random draw mostly for leave needs but not leave taking. own/matdis
# TODO: in writeup - pop level results overshoot is good - expected because sim design w. indep logit.
# Overshooted pop level results (total leavers/takers, total # leaves taken) should not be an issue for
# policy simulation given low take up. We just have a larger pool of takers/needers to choose from. Bias introduced by
# this larger pool is limited, as shown by random draw VS ML results that a big part of leave taking/need cannot be
# properly predicted via model, likely unobservables (health, intra-hh bargain, time preference, career choice, etc.)

import pandas as pd
pd.set_option('max_colwidth', 100)
pd.set_option('display.max_columns', 999)
pd.set_option('display.width', 200)
import numpy as np
import json
from time import time
from _5a_aux_functions import *
import sklearn.linear_model, sklearn.naive_bayes, sklearn.neighbors, sklearn.tree, sklearn.ensemble, \
    sklearn.gaussian_process, sklearn.svm
import xgboost
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import precision_score, recall_score, f1_score
import matplotlib.pyplot as plt
import matplotlib
from Utils import format_chart
import os
from os import listdir
from os.path import isfile, join

# a function to get yhat via k-fold train-test split (stratified) and prediction
def get_kfold_yhat(X, y, w, clf, fold, random_state):
    '''
    :param y: must be pd.Series with name
    '''

    yhat = pd.Series([],name=y.name + '_hat')
    skf = StratifiedKFold(n_splits=fold)
    for train_index, test_index in skf.split(X, y):
        Xtr, Xts = X.loc[train_index, ], X.loc[test_index, ]
        ytr, yts = y.loc[train_index, ], y.loc[test_index, ]
        wtr, wts = w[Xtr.index], w[Xts.index]
        # save a copy of Xts - get_sim_col() will mutate Xts['age'] etc under Naive Bayes
        # which would make logic control infeasible
        Xts_copy = Xts.copy()
        # get sim col
        yhat_fold = get_sim_col(Xtr,ytr, wtr, Xts, clf, random_state=random_state)
        if 'matdis' in y.name:
            yhat_fold = pd.Series(yhat_fold, index=Xts[(Xts['female']==1)
                                                       & (Xts['nochildren']==0)
                                                       & (Xts['age']<=50)].index, name=yhat.name)
        elif 'bond' in y.name:
            yhat_fold = pd.Series(yhat_fold, index=Xts[(Xts['nochildren']==0)
                                                       & (Xts['age']<=50)].index, name=yhat.name)
        elif 'spouse' in y.name:
            yhat_fold = pd.Series(yhat_fold, index=Xts[(Xts['nevermarried']==0)
                                                       & (Xts['divorced']==0)].index, name=yhat.name)
        else:
            yhat_fold = pd.Series(yhat_fold, index=yts.index, name=yhat.name)

        # logic control - see simulation engine code
        if 'matdis' in yhat_fold.name:
            yhat_fold.loc[Xts['female']!=1] = 0
        if 'illspouse' in yhat_fold.name:
            yhat_fold.loc[(Xts['nevermarried'] == 1) | (Xts['divorced'] == 1)] = 0
        if 'bond' in yhat_fold.name:
            yhat_fold.loc[Xts['nochildren'] == 1] = 0
        if 'matdis' in yhat_fold.name:
            yhat_fold.loc[Xts['nochildren'] == 1] = 0
        if 'bond' in yhat_fold.name:
            yhat_fold.loc[Xts_copy['age']>50] = 0
        if 'matdis' in yhat_fold.name:
            yhat_fold.loc[Xts_copy['age']>50] = 0

        yhat = yhat.append(yhat_fold)

    yhat = yhat.sort_index()
    return yhat

# a function to use given classifier to generate kfold prediction cols
def update_df_with_kfold_cols(d, col_Xs, col_ys, col_w, clf, fold, random_state):
    X, ys, w = d[col_Xs], d[col_ys], d[col_w]
    for col in col_ys:
        y = ys[col]
        yhat = get_kfold_yhat(X, y, w, clf, fold, random_state)
        if yhat.name in d.columns:
            del d[yhat.name]
        d = d.join(yhat)
    ## Get derived cols, both true/predicted
    types = ['own', 'matdis', 'bond', 'illchild', 'illspouse', 'illparent']
    d['taker'] = [max(x) for x in d[['take_%s' % x for x in types]].values]
    d['taker_hat'] = [max(x) for x in d[['take_%s_hat' % x for x in types]].values]
    d['needer'] = [max(x) for x in d[['need_%s' % x for x in types]].values]
    d['needer_hat'] = [max(x) for x in d[['need_%s_hat' % x for x in types]].values]
    d['num_leave_reasons_taken'] = [np.nansum(x) for x in d[['take_%s' % x for x in types]].values]
    d['num_leave_reasons_taken_hat'] = [np.nansum(x) for x in d[['take_%s_hat' % x for x in types]].values]

    return d

## Population level results of clf-based pred cols
def get_population_level_results(d, clf_name):
    # clf_name: str name of classifier
    out = {}
    out['true'], out['pred'] = {}, {}
    out['true']['n_takers'] = d[d['taker']==1]['weight'].sum()
    out['true']['n_needers'] = d[d['needer']==1]['weight'].sum()
    out['true']['n_reasons_taken'] = (d['num_leave_reasons_taken']*d['weight']).sum()
    # out['true']['n_reasons_taken'] = (d_raw['a4_cat']*d_raw['weight']).sum() # directly refer to raw FMLA var
    out['pred']['n_takers'] = d[d['taker_hat']==1]['weight'].sum()
    out['pred']['n_needers'] = d[d['needer_hat']==1]['weight'].sum()
    out['pred']['n_reasons_taken'] = (d['num_leave_reasons_taken_hat']*d['weight']).sum()
    return (clf_name, out)

## Individual level results of clf-based pred cols
def get_individual_level_results(d, clf_name, weighted_test=True):
    # clf_name: str name of classifier
    # weigthed_test: if True then weight test sample using FMLA weights, default=False
    out = {}
    out['precision'], out['recall'], out['f1'] = {}, {}, {}
    for v in ['taker', 'needer', 'resp_len'] + ['take_own', 'need_own', 'take_matdis', 'need_matdis']:
        dd = d.copy() # make a copy of original df. dd updates depending on v (removes yhat=NA rows from logic control)
        if 'matdis' in v:
            dd = dd[dd[v + '_hat'].notna()] # reduce d to rows with valid yhat (NA rows from logic control)
            dd = dd.reset_index(drop=True)
        # get weight col from dd
        sample_weight = np.ones(dd.shape[0])
        if weighted_test:
            sample_weight = dd['weight']
        # update output dict with performance scores
        out['precision'][v] = precision_score(dd[v], dd['%s_hat' % v], sample_weight=sample_weight)
        out['recall'][v] = recall_score(dd[v], dd['%s_hat' % v], sample_weight=sample_weight)
        out['f1'][v] = f1_score(dd[v], dd['%s_hat' % v], sample_weight=sample_weight)
    return (clf_name, out)

# ------------------------
## Plot functions
# ------------------------

# a function to plot pop level results - worker counts
def plot_pop_level_worker_counts(dp, clf_class_names_plot, clf_profile, savefig=None):
    '''
    :param: clf_class_names: col of dp except 'true', numbers will follow this order in plot
    '''
    # get clfs with cv results (ready for plot), dct_clf, and clf_class_names from clf_profile
    clfs, clf_class_names_plot, dct_clf = clf_profile
    # get clf class names, and corresponding labels for plot, must align for proper plotting
    clf_class_names_plot = [type(z).__name__ for z in clfs]
    clf_labels_plot = tuple([dct_clf[x] for x in clf_class_names_plot])
    # make plot
    title = 'Population Level Validation Results - Worker Counts'
    fig, ax = plt.subplots(figsize=(8, 6), dpi=100)
    dp = dp[clf_class_names_plot + ['true']]
    ys = dp.drop(columns=['true']).loc['n_takers'].values
    zs = dp.drop(columns=['true']).loc['n_needers'].values
    ind = np.arange(len(ys))
    width = 0.2
    bar1 = ax.bar(ind - width / 2, ys, width, align='center', capsize=5, color='indianred', ecolor='grey')
    bar2 = ax.bar(ind + width / 2, zs, width, align='center', capsize=5, color='tan', ecolor='grey')
    ax.set_ylabel('Millions of workers')
    ax.set_xticks(ind)
    ax.set_xticklabels(clf_labels_plot)
    ax.yaxis.grid(False)
    ax.legend((bar1, bar2), ('Leave Takers', 'Leave Needers'))
    ax.ticklabel_format(style='plain', axis='y')
    ax.get_yaxis().set_major_formatter(matplotlib.ticker.FuncFormatter(lambda x, p: format(int(x), ',')))
    format_chart(fig, ax, title, bg_color='white', fg_color='k')
    # add horizontal bar for true numbers
    n_takers_true = dp['true']['n_takers']
    plt.axhline(y=n_takers_true, color='indianred', linestyle='--')
    hline_offset = 1.025
    hline_text = 'Actual Number of Takers: %s million' % (round(n_takers_true, 1))
    plt.text(2, n_takers_true * hline_offset, hline_text, horizontalalignment='center', color='k')
    n_needers_true = dp['true']['n_needers']
    plt.axhline(y=n_needers_true, color='tan', linestyle='--')
    hline_offset = 1.025
    hline_text = 'Actual Number of Needers: %s million' % (round(n_needers_true, 1))
    plt.text(2, n_needers_true * hline_offset, hline_text, horizontalalignment='center', color='k')
    if savefig is not None:
        dir_out, suffix = savefig
        # save
        plt.savefig(dir_out + 'pop_level_workers%s.png' % suffix, facecolor='white', edgecolor='grey')  #
    return None

# a function to plot pop level results - leave counts
def plot_pop_level_leave_counts(dp, clf_class_names_plot, clf_profile, savefig=None):
    '''
    :param: clf_class_names: col of dp except 'true', numbers will follow this order in plot
    '''
    # get clfs with cv results (ready for plot), dct_clf, and clf_class_names from clf_profile
    clfs, clf_class_names_plot, dct_clf = clf_profile
    # get clf class names, and corresponding labels for plot, must align for proper plotting
    clf_class_names_plot = [type(z).__name__ for z in clfs]
    clf_labels_plot = tuple([dct_clf[x] for x in clf_class_names_plot])
    # make plot
    title = 'Population Level Validation Results - Leaves Taken'
    fig, ax = plt.subplots(figsize=(8, 6), dpi=100)
    dp = dp[clf_class_names_plot + ['true']]
    ys = dp.drop(columns=['true']).loc['n_reasons_taken'].values
    ind = np.arange(len(ys))
    width = 0.2
    bar1 = ax.bar(ind, ys, width, align='center', capsize=5, color='indianred', ecolor='grey')
    ax.set_ylabel('Number of Leaves')
    ax.set_xticks(ind)
    ax.set_xticklabels(clf_labels_plot)
    ax.yaxis.grid(False)
    ax.ticklabel_format(style='plain', axis='y')
    ax.get_yaxis().set_major_formatter(matplotlib.ticker.FuncFormatter(lambda x, p: format(int(x), ',')))
    format_chart(fig, ax, title, bg_color='white', fg_color='k')
    # add horizontal bar for true numbers - leave count based on num_leave_reasons_taken
    n_leaves_true = dp['true']['n_reasons_taken']
    plt.axhline(y=n_leaves_true, color='indianred', linestyle='--')
    hline_offset = 1.025
    hline_text = 'Actual Number of Recent Leaves: %s million' % (round(n_leaves_true, 1))
    plt.text(2, n_leaves_true * hline_offset, hline_text, horizontalalignment='center', color='k')
    # add horizontal bar for true numbers - leave count based on a4_cat (incl. multiple leavers, reasons beyond 6 types)
    n_leaves_true_a4 = 37.6
    plt.axhline(y=n_leaves_true_a4, color='tan', linestyle='--')
    hline_offset = 1.025
    hline_text = 'Actual Number of Total Leaves: %s million' % (round(n_leaves_true_a4, 1))
    plt.text(2, n_leaves_true_a4 * hline_offset, hline_text, horizontalalignment='center', color='k')
    if savefig is not None:
        dir_out, suffix = savefig
        # save
        plt.savefig(dir_out + 'pop_level_leaves%s.png' % suffix, facecolor='white', edgecolor='grey')  #
    return None

# a function to plot ind level results
def plot_ind_level(di, clf_class_names_plot, clf_profile, yvar, savefig=None):
    # get clfs with cv results (ready for plot), dct_clf, and clf_class_names from clf_profile
    clfs, clf_class_names_plot, dct_clf = clf_profile
    # get clf class names, and corresponding labels for plot, must align for proper plotting
    clf_class_names_plot = [type(z).__name__ for z in clfs]
    clf_labels_plot = tuple([dct_clf[x] for x in clf_class_names_plot])
    # make plot
    title = 'Individual Level Validation Results - Performance Measures\nOutcome = %s' % yvar
    fig, ax = plt.subplots(figsize=(8, 6), dpi=100)
    di = di[clf_class_names_plot] # order to ensure
    ys = [x[yvar] for x in di.loc['precision'].values]
    zs = [x[yvar] for x in di.loc['recall'].values]
    ws = [x[yvar] for x in di.loc['f1'].values]
    ind = np.arange(len(ys))
    width = 0.2
    bar1 = ax.bar(ind - width, ys, width, align='center', capsize=5, color='indianred', ecolor='grey')
    bar2 = ax.bar(ind, zs, width, align='center', capsize=5, color='tan', ecolor='grey')
    bar3 = ax.bar(ind + width, ws, width, align='center', capsize=5, color='slategray', ecolor='grey')
    ax.set_ylabel('Performance Measure')
    ax.set_xticks(ind)
    ax.set_xticklabels(clf_labels_plot)
    ax.yaxis.grid(False)
    ax.legend((bar1, bar2, bar3), ('Precision', 'Recall', 'F1'))
    ax.ticklabel_format(style='plain', axis='y')
    # ax.get_yaxis().set_major_formatter(matplotlib.ticker.FuncFormatter(lambda x, p: format(int(x), ',')))
    format_chart(fig, ax, title, bg_color='white', fg_color='k')
    if savefig is not None:
        dir_out, suffix = savefig
        # save
        plt.savefig(dir_out + 'ind_level%s.png' % suffix, facecolor='white', edgecolor='grey')  #
    return None

# a function to get cross validation results for pop and ind levels, based on number of folds, and list of seeds
def get_cv_results(d, fmla_wave, fold, seeds, fp_dir_out):
    for random_seed in seeds:
        print('--- Seed = %s ---' % random_seed)
        random_state = np.random.RandomState(random_seed)
        ## Based on seed, set up classifiers to be validated
        # classifiers
        clfs = []
        clfs += [sklearn.dummy.DummyClassifier(strategy='stratified')]
        clfs += [
            sklearn.linear_model.LogisticRegression(solver='liblinear', multi_class='ovr', random_state=random_state)]
        clfs += [sklearn.neighbors.KNeighborsClassifier(n_neighbors=5)]
        clfs += [sklearn.naive_bayes.MultinomialNB()]
        clfs += [sklearn.ensemble.RandomForestClassifier(random_state=random_state)]
        clfs += [xgboost.XGBClassifier(objective='binary:logistic')]
        clfs += [sklearn.linear_model.RidgeClassifier()]
        clfs += [sklearn.svm.SVC(probability=True, gamma='auto', random_state=random_state)]

        # dict from classifier class names to readable labels
        clf_class_names = ['DummyClassifier', 'LogisticRegression', 'KNeighborsClassifier',
                           'MultinomialNB', 'RandomForestClassifier', 'XGBClassifier', 'RidgeClassifier', 'SVC']
        clf_labels = ['Random Draw', 'Logit', 'KNN', 'Naive Bayes', 'Random Forest', 'XGB', 'Ridge', 'SVC']
        dct_clf = dict(zip(clf_class_names, clf_labels))
        # classifier plot labels
        clf_class_names_plot = [type(z).__name__ for z in clfs]
        clf_labels_plot = tuple([dct_clf[x] for x in clf_class_names_plot])

        ## Read in FMLA data, get cols, fillna
        # drop any military workers and reset index
        try:
            d = d.drop(d[d['occ_11'] == 1].index)
            d = d.reset_index(drop=True)
        except KeyError:
            pass

        d_raw = d.copy()
        types = ['own', 'matdis', 'bond', 'illchild', 'illspouse', 'illparent']
        col_Xs, col_ys, col_w = get_columns(fmla_wave, types)
        # part_size = int(round(len(d) / fold, 0))

        ## Get output
        out_pop, out_ind = {}, {}
        for clf in clfs:
            # print clf name
            if type(clf) != str:  # only str clf is 'random draw'
                clf_name = clf.__class__.__name__
            else:
                clf_name = clf
            # get results
            print('Getting results for clf = %s' % clf_name)
            t0 = time()

            d = d[col_Xs + col_ys + [col_w]]
            d = fillna_df(d, random_state=random_state)
            X, ys = d[col_Xs], d[col_ys]

            d = update_df_with_kfold_cols(d, col_Xs, col_ys, col_w, clf, fold, random_state=random_state)
            print('Time needed to get results = %s' % round((time() - t0), 0))
            clf_name, out_pop_clf = get_population_level_results(d, clf_name)
            clf_name, out_ind_clf = get_individual_level_results(d, clf_name)
            # if isinstance(clf, sklearn.neighbors.KNeighborsClassifier):
            #     clf_name += str(int(clf.get_params()['n_neighbors']))
            out_pop[clf_name] = out_pop_clf
            out_ind[clf_name] = out_ind_clf

        # save as json
        dir_out = fp_dir_out + 'seed_%s/' % random_seed
        if not os.path.exists(dir_out):
            os.mkdir(dir_out)
        fp_out_json_pop = dir_out + 'pop_level_%s_k%s.json' % (fmla_wave, fold)
        fp_out_json_ind = dir_out + 'ind_level_%s_k%s.json' % (fmla_wave, fold)
        with open(fp_out_json_pop, 'w') as f:
            json.dump(out_pop, f, sort_keys=True, indent=4)
        with open(fp_out_json_ind, 'w') as f:
            json.dump(out_ind, f, sort_keys=True, indent=4)

    clf_profile = (clfs, clf_class_names_plot, dct_clf)
    return clf_profile



    # ## Plots for a given seed
    #
    # # outfile suffix
    # suffix = '_%s_k%s_%s_%s' % (fmla_wave, fold, 'SimpleImputer', 'XtsWeighted')
    #
    # # clean up pop and ind level results into df
    # # pop level
    # with open(fp_out_json_pop, 'r') as j:
    #     dp = json.load(j)
    # dp_raw = dp.copy()
    # dp = {k: v['pred'] for k, v in dp_raw.items()}
    # dp = pd.DataFrame.from_dict(dp)
    # dp = dp[clf_class_names_plot]
    # for c in dp.columns:
    #     dp[c] = [x / 10 ** 6 for x in dp[c]]
    # # add in the col with true numbers
    # true_counts = pd.Series(dp_raw['DummyClassifier']['true'], name='true')
    # true_counts = true_counts/10**6
    # dp = dp.join(true_counts)
    # # ind level
    # with open(fp_out_json_ind, 'r') as j:
    #     di = json.load(j)
    # di = pd.DataFrame.from_dict(di)
    # di = di[clf_class_names_plot]
    #
    # # make plots
    # # Pop level results - worker counts
    # plot_pop_level_worker_counts(dp, clf_class_names_plot, suffix=suffix)
    # # Pop level results - leave counts
    # plot_pop_level_leave_counts(dp, clf_class_names_plot, suffix=suffix)
    # # Individual Level Results - focus on a given yvar
    # for yvar in ['taker', 'needer', 'resp_len', 'take_own', 'need_own', 'take_matdis', 'need_matdis']:
    #     plot_ind_level(di, yvar, suffix=suffix)

## Get average within-FMLA validation results across seeds
def get_avg_out_pop(dir_results, seeds, true_numbers=False):
    '''

    :param dir_results: directory storing seed-specific result folders, e.g. ./seeds_12345_12354_noSVC
    :param seeds: list of seeds used, corresponding to seed folders in dir_results
    :param true_numbers: if True, then add a col 'true' to avg_out_p showing true pop-level numbers
    :return: avg_out_p, a df with rows=yvars at pop level, and cols=clf class names
    '''
    avg_out_p = None
    # random_seed = 12345
    for s in seeds:
        # load json, convert to df, norm to millions, and norm by number of seeds for avg
        with open(dir_results + 'seed_%s' % s + '/pop_level_%s_k%s.json' % (fmla_wave, fold)) as f:
            out_p = json.load(f)
        out_p_df = pd.DataFrame.from_dict(
                dict((k, dict((kk, vv/len(seeds)) for kk, vv in v['pred'].items())) for k, v in out_p.items())
            )
        out_p_df = out_p_df/10**6
        # update avg_out_p with pop-level prediction numbers from a seed
        if avg_out_p is None:
            avg_out_p = out_p_df
        else:
            avg_out_p += out_p_df
    if true_numbers:
        # add in the col with true numbers
        true_counts = pd.Series(out_p['DummyClassifier']['true'],
                                name='true')  # get true numbers from out_p of last random_seed in loop
        true_counts = true_counts / 10 ** 6
        avg_out_p = avg_out_p.join(true_counts)
    return avg_out_p

def get_avg_out_ind(dir_results, seeds):
    '''

    :param dir_results: directory storing seed-specific result folders, e.g. ./seeds_12345_12354_noSVC
    :param seeds: list of seeds used, corresponding to seed folders in dir_results
    :param true_numbers: if True, then add a col 'true' to avg_out_p showing true pop-level numbers
    :return: avg_ind_p, a df with rows= , and cols=clf class names
    '''
    avg_out_i = None
    # random_seed = 12345
    for s in seeds:
        # load json, convert to df, norm by number of seeds for avg
        with open(dir_results + 'seed_%s' % s + '/ind_level_%s_k%s.json' % (fmla_wave, fold)) as f:
            out_i = json.load(f)
        out_i_df = pd.DataFrame.from_dict(out_i)
        for col in out_i_df.columns:
            for ix in out_i_df.index:
                dct_scores = out_i_df[col][ix]
                out_i_df[col][ix] = {k: v/len(seeds) for k, v in dct_scores.items()}

        # update avg_out_i with pop-level prediction numbers from a seed
        if avg_out_i is None:
            avg_out_i = out_i_df
        else:
            for col in avg_out_i.columns:
                for ix in avg_out_i.index:
                    avg_out_i[col][ix] = {k: (avg_out_i[col][ix][k] + out_i_df[col][ix][k]) for k, v in
                                          avg_out_i[col][ix].items()}

    return avg_out_i



#######################################
# End of within-FMLA validation
# - for program outlay validation results, run model in GUI and save to respective models
#######################################

# Program Outlay Results - use GUI to run model by state-classifier, name output folder as 'st_clf', e.g. 'ca_knn'
# Make a df of total program costs

# a function to get cost estimates from multiple output folders named in format 'st_clf'
def get_sim_costs(fp_dir_out, sts, methods):
    '''

    :param fp_dir_out: parent dir that stores output folders for each sim run
    :param sts: state names in output folder
    :param methods: method names in output folder
    :return: df of cost estimates, by st by method
    '''
    costs = {}
    for st in sts:
        costs[st] = {}
        for method in methods:
            fp = fp_dir_out + '%s_%s/' % (st, method)
            fs = [f for f in listdir(fp) if isfile(join(fp, f))]
            f_dc = [f for f in fs if 'program_cost' in f][0]
            dc = pd.read_csv(fp + f_dc)
            for c in dc.columns:
                if c != 'type':
                    dc[c] = [x / 10 ** 6 for x in dc[c]]
            costs[st][method] = dc.loc[dc['type'] == 'total', 'cost'].values[0]
    costs = pd.DataFrame.from_dict(costs)
    costs = costs.reindex(methods)
    costs = costs[sts]
    return costs

# plot
def plot_sim_costs(costs, savefig=None):
    title = 'Program Outlay Validation Results'
    fig, ax = plt.subplots(figsize=(8, 6), dpi=100)
    ys = costs['ri']
    zs = costs['nj']
    ws = costs['ca']
    dct_color = dict(zip(sts, ['indianred', 'slategray', 'tan']))

    ind = np.arange(len(ys))
    width = 0.2
    bar1 = ax.bar(ind-width, ys, width, align='center', capsize=5, color=dct_color['ri'], ecolor='grey')
    bar2 = ax.bar(ind, zs, width, align='center', capsize=5, color=dct_color['nj'], ecolor='grey')
    bar3 = ax.bar(ind+width, ws, width, align='center', capsize=5, color=dct_color['ca'], ecolor='grey')
    ax.set_ylabel('Program Outlay')
    ax.set_xticks(ind)
    ax.set_xticklabels(('Logit', 'KNN', 'Naive Bayes', 'Random Forest', 'XGB', 'Ridge', 'SVC'))
    ax.yaxis.grid(False)
    ax.legend((bar1, bar2, bar3), ('Rhode Island','New Jersey', 'California' ))
    ax.ticklabel_format(style='plain', axis='y')

    # add horizontal bar for true numbers
    dct_cost = dict(zip(sts, [166.7, 502.2, 5681.7]))
    dct_offset = dict(zip(sts, [1.025, 1.025, 0.975]))
    for st in sts:
        y = dct_cost[st]
        plt.axhline(y=y, color=dct_color[st], linestyle='--')
        hline_offset = dct_offset[st]
        hline_text = 'Actual Program Outlay, %s: %s million' % (st.upper(), y)
        plt.text(2, y * hline_offset, hline_text, horizontalalignment='center', color='k')
    format_chart(fig, ax, title, bg_color='white', fg_color='k')
    if savefig is not None:
        dir_out = savefig
        # save
        plt.savefig(dir_out + 'program_outlay.png', facecolor='white', edgecolor='grey') #
    return None

# Get cross validation results based on number of folds and list of seeds, store results in folders
fmla_wave = 2018
d = pd.read_csv('./data/fmla/fmla_%s/fmla_clean_%s.csv' % (fmla_wave, fmla_wave))
fold = 10  # cannot be more than # minor cases in most imbalanced outvar
n_seeds = 2
seeds = list(range(12345, 12345 + n_seeds))
dir_out = 'C:/workfiles/Microsimulation/draft/issue_briefs/issue_brief_2/results/'
clf_profile = get_cv_results(d, fmla_wave, fold, seeds, dir_out) # clfs, clf_class_names, dct_clf
clfs, clf_class_names_plot, dct_clf = clf_profile
# get avg_out_pop for noSVC results
dir_results = 'C:/workfiles/Microsimulation/draft/issue_briefs/issue_brief_2/results/seeds_12345_12354_noSVC/'
avg_out_p = get_avg_out_pop(dir_results, seeds, true_numbers=True)
# # add in col for SVC
# dir_results = 'C:/workfiles/Microsimulation/draft/issue_briefs/issue_brief_2/results/seeds_12345_12354_SVC/'
# avg_out_p = avg_out_p.join(get_avg_out_pop(dir_results, seeds))
# send 'true' col to end
avg_out_p = avg_out_p[[x for x in avg_out_p.columns if x!='true'] + ['true']]
# get avg_out_ind for noSVC results
avg_out_i = get_avg_out_ind(dir_results, seeds)
# # add in col for SVC
# dir_results = 'C:/workfiles/Microsimulation/draft/issue_briefs/issue_brief_2/results/seeds_12345_12354_SVC/'
# avg_out_i = avg_out_i.join(get_avg_out_ind(dir_results, seeds))

# plot with average results
# Pop level results - worker counts
plot_pop_level_worker_counts(avg_out_p, clf_class_names_plot, clf_profile)
# Pop level results - leave counts
plot_pop_level_leave_counts(avg_out_p, clf_class_names_plot, clf_profile)
# Ind level results
dir_out = 'C:/workfiles/Microsimulation/draft/issue_briefs/issue_brief_2/draft_plot/'
for yvar in ['taker', 'needer', 'resp_len'] + ['take_own', 'need_own', 'take_matdis', 'need_matdis']:
    suffix = '_%s_k%s_%s' % (fmla_wave, fold, yvar)
    savefig = (dir_out, suffix)
    plot_ind_level(avg_out_i, clf_class_names_plot, clf_profile, yvar, savefig=savefig)

# get costs df by states and methods
fp_dir_out = './output/_ib2_v2/'
sts = ['ri', 'nj', 'ca']
methods = ['logit', 'knn', 'nb', 'rf', 'xgb', 'ridge', 'svm']
costs = get_sim_costs(fp_dir_out, sts, methods)

# plot simulated costs by state and methods
dir_out = 'C:/workfiles/Microsimulation/draft/issue_briefs/issue_brief_2/draft_plot/'
plot_sim_costs(costs, savefig=dir_out)


################################################################################################

