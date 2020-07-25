'''
compare results between R and Python

chris zhang 1/7/2020
'''

import pandas as pd
pd.set_option('max_colwidth', 100)
pd.set_option('display.max_columns', 999)
pd.set_option('display.width', 200)
import numpy as np

## Read in post-sim ACS
dr = pd.read_csv("C:\workfiles\Microsimulation\microsim_R-master\output\_test_RI.csv")
dr0 = dr.copy()
dr = dr[dr['eligworker']==1]

p_run_stamp = '20200113_122631'
dp = pd.read_csv('./output/output_%s_Main/acs_sim_%s.csv' % (p_run_stamp, p_run_stamp))

## Check length of DFs
# get diff in eligible workers' IDs between dr and dp
# NOTE: SERIALNO is id for household not person.
ids_r = pd.DataFrame(dr['SERIALNO']).sort_values(by='SERIALNO')
ids_r['zr'] = 1
ids_r = ids_r.groupby(by='SERIALNO').count().reset_index()
ids_p = pd.DataFrame(dp['SERIALNO']).sort_values(by='SERIALNO')
ids_p['zp'] = 1
ids_p = ids_p.groupby(by='SERIALNO').count().reset_index()

# Case 1: find out which hh ids has persons in Python but none in R
ids_pr = pd.merge(ids_p, ids_r, how='left', on='SERIALNO', indicator=True)
ids_p_only = ids_pr[ids_pr['_merge']!='both']
print(ids_p_only)
# Case 2: ids in R but not P
ids_pr = pd.merge(ids_p, ids_r, how='right', on='SERIALNO', indicator=True)
ids_r_only = ids_pr[ids_pr['_merge']!='both']
print(ids_r_only)

# examine >> in dr, WAGP is below RI 3840 line

# Case 3: find out which hh ids has more persons in R than in P (cond' P has any)
ids_pr['dz'] = (ids_pr['zr'] - ids_pr['zp'])
ids_r_more = ids_pr[ids_pr['dz']>0]['SERIALNO'].values
ids_p.loc[ids_p['SERIALNO'].isin(ids_r_more), ]
ids_r.loc[ids_r['SERIALNO'].isin(ids_r_more), ]

# examine >> in dr, WAGP is below RI 3840 line
dr.loc[dr['SERIALNO'].isin(ids_r_more) , 'wage12']

##### read in raw ACS data and check WAGP, ADJINC
acs = pd.read_csv('./data/acs/pow_person_files/p44_ri_pow.csv')
acs = acs.loc[acs['SERIALNO'].isin(ids_r_more), ['SERIALNO', 'ADJINC', 'WAGP']]
acs['wage12'] = acs['WAGP']*acs['ADJINC']/1056030
acs = acs.sort_values(by=['SERIALNO', 'WAGP'])
# check P/R acs persons with ids in ids_r_more
print(acs)
print(dr.loc[dr['SERIALNO'].isin(ids_r_more), ['SERIALNO', 'WAGP', 'wage12']].sort_values(by=['SERIALNO', 'WAGP']))
# TODO: Luke to update wage12=WAGP*(ADJINC/1056030) in R cleaning


## Check take/need vars in DFs
types = ['own', 'matdis', 'bond', 'illchild', 'illspouse', 'illparent']
for t in types:
    print(dr['take_%s' % t].value_counts())
    print(dp['take_%s' % t].value_counts())
    print('-----------------------------')
# TODO: logit gives very diff take vars for R/P.
# checked - sklearn logit regularization does not affect result. Set C=1e8 same diff issue

################################
# check for consistency between
# - Python sklearn logit newton-cg
# - R glm
################################

import sklearn.preprocessing, sklearn.linear_model, sklearn.naive_bayes, sklearn.neighbors, sklearn.tree, sklearn.ensemble, \
    sklearn.gaussian_process, sklearn.svm
## Read in data
import statsmodels.api as sm
import pandas as pd
pd.set_option('max_colwidth', 100)
pd.set_option('display.max_columns', 999)
pd.set_option('display.width', 200)
import numpy as np
from collections import OrderedDict as od

# a function to read in FMLA data and reduce cols
def preprocess_data(fp_fmla):
    # Read in FMLA
    d = pd.read_csv(fp_fmla)
    # id
    id = 'empid'
    # Xs, ys, w
    Xs = ['widowed', 'divorced', 'separated', 'nevermarried',
              'female', 'age','agesq',
              'ltHS', 'someCol', 'BA', 'GradSch',
              'black', 'other', 'asian','native','hisp',
              'nochildren','faminc','coveligd']
    ys = ['take_own', 'take_matdis', 'take_bond', 'take_illchild', 'take_illspouse', 'take_illparent']
    ys += ['need_own', 'need_matdis', 'need_bond', 'need_illchild', 'need_illspouse', 'need_illparent']
    ys += ['resp_len']
    ys += ['anypay', 'prop_pay']
    w = 'weight'
    # reduce cols
    d = d[[id] + Xs + ys+ [w]]

    ## Standardize
    # use N-ddof = N-1 as in R
    # https://stackoverflow.com/questions/27296387/difference-between-r-scale-and-sklearn-preprocessing-scale/27297618

    # TODO: check how fillna works in R - need to be same as Python
    # drop missing rows if any col is missing
    d = d.dropna(subset=Xs)
    # standardize
    for X in Xs:
        d['z_%s' % X] = (d[X] - d[X].mean()) / np.std(d[X], axis=0, ddof=1)
    print(d[['empid', 'age','z_age', 'female', 'z_female']].head())
    # cols to return
    cols = (Xs, ys, w)
    return (d, cols)

## Fit model

# a function to fit logit model using sklearn
def fit_logit_sklearn(d, Xs, w, solver, y, standardized, weighted):
    print('--- sklearn solver = %s -------------------------' % solver)

    # use standardized xvars if opted
    if standardized:
        xvars = ['z_%s' % x for x in Xs]
    else:
        xvars = Xs

    # if matdis, reduce to female only rows, remove female from xvar
    if y in ['take_matdis', 'need_matdis']:
        d = d[d['female']==1]
        if standardized:
            xvars = [x for x in xvars if x!='z_female']
        else:
            xvars = [x for x in xvars if x != 'female']

    # fit model
    if not weighted:
        clf = sklearn.linear_model.LogisticRegression(solver=solver).fit(d[xvars], d[y])
    else:
        clf = sklearn.linear_model.LogisticRegression(solver=solver).fit(d[xvars], d[y], sample_weight=d[w])

    # dict from xvar to coefs, then to Series
    bs = od(list(zip(xvars, [round(x, 6) for x in clf.coef_[0]])))
    bs['const'] = clf.intercept_[0]
    bs.move_to_end('const', last=False)    # print coefs
    bs = pd.Series(bs, index=bs.keys())
    bs.name = y

    # some phats
    phats = [round(x[1], 6) for x in clf.predict_proba(d[xvars])][:10]
    phats = pd.Series(phats, index=d[:10].index)
    phats.name = y
    # print
    # print('const: ', clf.intercept_[0])
    # for k, v in bs.items():
    #     print(k, ': ',v)
    # print(phats)

    return bs, phats

# y = 'take_matdis'
# y = 'take_own'
# bs, phats = fit_logit_sklearn(d, Xs, w, 'newton-cg', y, standardized=True, weighted=True)

# a function to fit logit model using statsmodels
def fit_logit_sm(d, Xs, w, y, standardized, weighted):

    # use standardized xvars if opted
    if standardized:
        xvars = ['z_%s' % x for x in Xs]
    else:
        xvars = Xs

    # if matdis, reduce to female only rows, remove female from xvar
    if y in ['take_matdis', 'need_matdis']:
        d = d[d['female']==1]
        if standardized:
            xvars = [x for x in xvars if x!='z_female']
        else:
            xvars = [x for x in xvars if x != 'female']

    # fit model
    if not weighted:
        logit = sm.GLM(d[y], sm.add_constant(d[xvars]), family=sm.families.Binomial())
    else:
        logit = sm.GLM(d[y], sm.add_constant(d[xvars]), family=sm.families.Binomial(), freq_weights=d[w])
    logit = logit.fit()

    # dict from xvar to coefs
    bs = od(list(zip(logit.params.index, logit.params.values)))
    bs = pd.Series(bs, index=bs.keys())
    bs.name = y

    # some phats
    phats = (logit.predict(sm.add_constant(d[xvars]))).head(10)
    phats.name = y
    # print
    # print(logit.summary())
    # print('---------------------- statsmodel ---')
    # print(logit.params)
    # print((logit.predict(sm.add_constant(d[xvars]))).head(10))

    return bs, phats

# bs, phats = fit_logit_sm(d, Xs, w, y, standardized=True, weighted=False)
#

#######################
# a function to get coefs, phats for 6+6+1, for sklearn/statsmodels
def get_coefs_phats(fitter, args, ys):
    '''

    :param fitter: fit_logit_sklearn(), fit_logit_sm()
    :param args: args of fitter except y - d, Xs, w, solver
    :param ys: yvars
    :return:
    '''
    print('------------ fitting model using statsmodels')
    dbs, dps = pd.DataFrame([]), pd.DataFrame([]) # df for coefs and phats
    for y in ys:
        bs, phats = fitter(*args, y, standardized=True, weighted=True)
        # build up df for coefs
        if dbs.shape[1]==0: # dbs is empty
            dbs[y] = bs
        else: # dbs not empty, merge by index
            dbs = dbs.join(bs)
        dbs = dbs.sort_index()

        # build up df for phats
        if dps.shape[1] == 0:
            dps[y] = phats
        else:
            dps = dps.join(phats)
        dps = dps.sort_index()
    return dbs, dps

# execute

fp_fmla = './data/fmla_2012/fmla_clean_2012.csv'
d, cols = preprocess_data(fp_fmla)
Xs, ys, w = cols

Dbs, Dps = {}, {} # dict dbs dps results
# sklearn
for solver in ['newton-cg', 'liblinear']:
    args = (d, Xs, w, solver)
    dbs, dps = get_coefs_phats(fit_logit_sklearn, args, ys)
    Dbs[solver] = dbs
    Dps[solver] = dps
# sm
args = (d, Xs, w)
dbs, dps = get_coefs_phats(fit_logit_sm, args, ys)
Dbs['sm'] = dbs
Dps['sm'] = dps

# R
def read_R_output(fp):
    df = pd.read_csv(fp)
    df.loc[df['Unnamed: 0']=='(Intercept)', 'Unnamed: 0'] = 'const'
    try: # if integer index from R, reduce 1 as 0-order in Python
        df.index= df['Unnamed: 0'] - 1
    except TypeError:
        df.index = df['Unnamed: 0']
    df.index.name = None
    del df['Unnamed: 0']
    df = df.sort_index()
    return df
fp = './PR_comparison/R_bs.csv'
dbs = read_R_output(fp)
fp = './PR_comparison/R_phats.csv'
dps = read_R_output(fp)
Dbs['R'] = dbs
Dps['R'] = dps
## Check diffs
# newton vs liblinear
Dbs['newton-cg'] - Dbs['liblinear'] # diffs esp for need_type
Dps['newton-cg'] - Dps['liblinear'] # diffs O(1e-6) really small

# liblinear vs sm
Dbs['liblinear'] - Dbs['sm'] # diffs esp for need_type
Dps['liblinear'] - Dps['sm'] # diffs really small

# sm vs R
Dbs['sm'] - Dbs['R'] # very similar overall, except a few cases
Dps['sm'] - Dps['R'] # diffs really small

# liblinear vs R
Dbs['liblinear'] - Dbs['R'] # diffs esp for need_type
Dps['liblinear'] - Dps['R'] # diffs really small

# TODO: newton-cg cannot converge with sample_weights!! Works fine if no weights
# TODO: after ddof=0 (default) standardization, newton-cg and liblinear gives very similar coefs (up to O(1e-5)). Can use liblinear as Python baseline
# TODO: both P/R need hard-coded standardization: (x-mu)/sigma where sigma divisor = (n-1). R psycho package does not affect binary. Python sklearn set divisor = n.
# TODO: done - after hardcoded stand', w or w/o weighting, newton-cg~liblinear~statsmodels=glm, good.
# TODO: note - if not stand' and weighted, sklearn liblinear would give different results
# TODO: done - take_matdis good if restrict to female workers and remove feamle from xvars
# TODO: although coef diffs can be large between newton/liblinear/sm, phat diffs are really small O(1e-6) across 6+6+1

# check 0/1s
# for x in Xs:
#     print('--------------- x = %s --------------' % x)
#     print(d[x].value_counts().sort_index())



####################################
#
# import pandas as pd
# import statsmodels.api as sm
#
# cuse = pd.read_table("http://data.princeton.edu/wws509/datasets/cuse.dat",
#                      sep=" +")
# res = sm.formula.glm("using + notUsing ~ C(age, Treatment('<25')) + "
#                      "education + wantsMore",  family=sm.families.Binomial(),
#                      data=cuse).fit()
# res.summary()
#
# ######
#
#
# res = sm.formula.glm("%s ~ %s" % (y, ' + '.join(xvars)),  family=sm.families.Binomial(),
#                      data=d).fit()
# res.summary()

## MN logit with statsmodel
# set up
fp_fmla = './data/fmla_2012/fmla_clean_2012.csv'
d, cols = preprocess_data(fp_fmla)
Xs, ys, w = cols
y = ['prop_pay']

# two-way dicts from prop_pay values to labels
v = d.prop_pay.value_counts().sort_index().index
k = range(len(v))
d_prop = dict(zip(k, v))
D_prop = dict(zip(v, k))
d = d[d['prop_pay']>0]
d = d[d['prop_pay'].notna()]
mlogit = sm.MNLogit(d[y], d['female']).fit()
