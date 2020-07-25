for t in types:
    print('value counts for need_%s\n' % t, d['need_%s' % t].value_counts())

import numpy as np
from sklearn.impute import KNNImputer
import pandas as pd
pd.set_option('max_colwidth', 100)
pd.set_option('display.max_columns', 999)
pd.set_option('display.width', 200)

X = [[1, 0, np.nan], [3, 4, 0], [np.nan, 6,0], [8, 8, 1]]
X = pd.DataFrame(X)
imputer = KNNImputer(n_neighbors=2)
X = pd.DataFrame(imputer.fit_transform(X))



from xgboost import XGBClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score
from _5a_aux_functions import *

d = pd.read_csv('./data/fmla/fmla_2012/fmla_clean_2012.csv')
d_raw = d.copy()
types = ['own', 'matdis', 'bond', 'illchild', 'illspouse', 'illparent']
col_Xs, col_ys, col_w = get_columns(types)
d = d[col_Xs + col_ys + [col_w]]
X, ys = d[col_Xs], d[col_ys]
y = ys[ys.columns[0]]

idxs = y[y.notna()].index
X, y = X.loc[idxs, ], y.loc[idxs, ]

# split data into train and test sets
seed = 7
test_size = 0.25
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size, random_state=seed)

# fit model no training data
w_train = d.loc[y_train.index, col_w]
model = XGBClassifier()
model.fit(X_train, y_train, sample_weight=w_train)
print(model)


# make predictions for test data
y_pred = model.predict(X_test)
predictions = [round(value) for value in y_pred]

# evaluate predictions
sample_weight = d.loc[y_test.index, col_w]
accuracy = accuracy_score(y_test, predictions, sample_weight=sample_weight)
precision = precision_score(y_test, predictions, sample_weight=sample_weight)
recall = recall_score(y_test, predictions, sample_weight=sample_weight)
f1 = f1_score(y_test, predictions, sample_weight=sample_weight)
print("Accuracy: %.2f%%" % (accuracy * 100.0))
print("Precision: %.2f%%" % (precision * 100.0))
print("Recall: %.2f%%" % (recall * 100.0))
print("F1 score: %.2f%%" % (f1 * 100.0))

#####
import numpy as np
random_seed = 12345
random_state = np.random.RandomState(random_seed)
X, y, w = X_train, y_train, w_train
Xa = X_test
clf = XGBClassifier()
pp = get_sim_col(X, y, w, Xa, clf, random_state)

#################################################
# check standard error of cost by type (NJ got large SE interval with negative lower bound)
#################################################
import numpy as np
import pandas as pd
pd.set_option('max_colwidth', 100)
pd.set_option('display.max_columns', 999)
pd.set_option('display.width', 200)
from _5a_aux_functions import get_acs_with_takeup_flags, get_weighted_draws
## Read in post-sim data
st = 'nj'
tag = '20200523_125352'
acs = pd.read_csv('./output/output_%s_main simulation/acs_sim_%s_%s.csv' % (tag, st, tag))
## params
random_seed = 12345
random_state = np.random.RandomState(random_seed)
types = ['own', 'matdis', 'bond', 'illchild', 'illspouse', 'illparent']
params={
    'leave_types': types,
    'min_takeup_cpl': 5,
    'alpha': 0,
    'd_takeup': dict(zip(types, [0.0219, 0.0077, 0.0081, 0.0005, 0.0005, 0.0006])),
    'wkbene_cap': 594
}
pow_pop_multiplier = 1.02

out_total = None
acs_taker_needer = acs[(acs['taker'] == 1) | (acs['needer'] == 1)]
acs_neither_taker_needer = acs.drop(acs_taker_needer.index)

# apply take up flag and weekly benefit cap, and compute total cost, 6 types
costs = {}
for t in params['leave_types']:
    # v = capped weekly benefit of leave type
    v = [min(x, params['wkbene_cap']) for x in
         ((acs_taker_needer['wage12'] / acs_taker_needer['wkswork'] * acs_taker_needer['effective_rrp']))]
    # inflate weight for missing POW
    w = acs_taker_needer['PWGTP'] * pow_pop_multiplier

    # get program cost for leave type t - sumprod of capped benefit, weight, and takeup flag for each ACS row
    costs[t] = (v * acs_taker_needer['cpl_%s' % t] / 5 * w * acs_taker_needer['takeup_%s' % t]).sum()
costs['total'] = sum(list(costs.values()))

# compute standard error using replication weights, then compute confidence interval (lower bound at 0)
sesq = dict(zip(costs.keys(), [0] * len(costs.keys())))
wt_ixs = list(range(1, 2))
for wt in ['PWGTP%s' % x for x in wt_ixs]:
    # initialize costs dict for current rep weight
    costs_rep = {}
    # get takeup_type flags for acs under current rep weight
    acs = get_acs_with_takeup_flags(acs_taker_needer, acs_neither_taker_needer, wt, params, random_state)

    acs_taker_needer = acs[(acs['taker'] == 1) | (acs['needer'] == 1)]

    for t in params['leave_types']:
        v = [min(x, params['wkbene_cap']) for x in
             ((acs_taker_needer['wage12'] / acs_taker_needer['wkswork'] * acs_taker_needer['effective_rrp']))]
        # inflate weight for missing POW
        w = acs_taker_needer[wt] * pow_pop_multiplier

        # get program cost for leave type t - sumprod of capped benefit, weight, and takeup flag for each ACS
        # row
        costs_rep[t] = (v * acs_taker_needer['cpl_%s' % t] / 5 * w * acs_taker_needer['takeup_%s' % t]).sum()
    costs_rep['total'] = sum(list(costs_rep.values()))
    for k in costs_rep.keys():
        sesq[k] += 4 / len(wt_ixs) * (costs[k] - costs_rep[k]) ** 2

for k, v in sesq.items():
    sesq[k] = v ** 0.5
ci = {}
for k, v in sesq.items():
    ci[k] = (max(costs[k] - 1.96 * sesq[k], 0), costs[k] + 1.96 * sesq[k])

# Save output
out_costs = pd.DataFrame.from_dict(costs, orient='index')
out_costs = out_costs.reset_index()
out_costs.columns = ['type', 'cost']

out_ci = pd.DataFrame.from_dict(ci, orient='index')
out_ci = out_ci.reset_index()
out_ci.columns = ['type', 'ci_lower', 'ci_upper']

out = pd.merge(out_costs, out_ci, how='left', on='type')

d_tix = {'own': 1, 'matdis': 2, 'bond': 3, 'illchild': 4, 'illspouse': 5, 'illparent': 6, 'total': 7}
out['tix'] = [d_tix[x] for x in out['type']]

out = out.sort_values(by='tix')
del out['tix']

if out_total is not None:
    out_total[['cost', 'ci_lower', 'ci_upper']] += out[['cost', 'ci_lower', 'ci_upper']]
else:
    out_total = out





types = ['own', 'matdis', 'bond', 'illchild', 'illspouse', 'illparent']
for t in types:
    print(d['take_%s' % t].value_counts())
for t in types:
    print(d['need_%s' % t].value_counts())

fp = "C:/workfiles/Microsimulation/microsim/data/acs/2018/pow_person_files/p34_nj_pow.csv"

fp2 = "./output/output_20200526_235821_main simulation/acs_sim_nj_20200526_235821.csv"
acs2 = pd.read_csv(fp2)

fp3 = "./data/acs/ACS_cleaned_forsimulation_2018_ri.csv"
acs3 = pd.read_csv(fp3)

# TODO: clean-ACS code fix, see below
# [done] Fix large CI in NJ/NB, check if affects CA/NB underestimation
# takeup flags now drawn by chunks, approx if chunksize and last chunk is large enough (so big pool for draws)
# problematic if not enough takers with min take length>=5 to draw from!!
# we don't need a lot of takers/needers to be 'enough', but NB's underprecition may lead to extremely small
# or even 0 taker/needer in chunk
# this causes insufficient draw of takers to satisfy user-supplied takeup rate, so the chunk does not properly
# contribute its share to cost -> get low costs for that rep wt, -> large SE -> large CI
# solution is to draw takeup flag using entire post-sim ACS for all rep wts just like main weight
# i.e. when draw flags for main weight, draw rest 80 flag cols for 80 rep wts
# not a huge RAM issue when use post-sim ACS without chunking - for CA is only 434MB so okay!

# [done] Fix chunk-based mean/sig computation in clean-ACS code - set large chunksize for clf's need stand'n?

# [done] remove ndep_kid, ndep_old, use nochildren, no elderly binary in clean-ACS code
# [done] for NB classifier, get tercile/group-based binary cols for numerical xvars in clean-ACS code
# [done] numerical xvars - faminc, ln_faminc (tercile); and age, wkhours (group)
# [done] set ADJINC in clean-ACS properly to match ACS/FMLA years
# [done] use ADJINC, adjinc to correct all dollar amounts in clean-ACS
# [for Mike] add a fmla_wave option in GUI (like ACS year) class GeneralParameters()
# [done] add xvar - low_wage

# get xvars consistent in 2012 as 2018

# output a separate CSV for subsample of ineligible workers in ACS (no drop in clean-ACS and sim code)
# ineligible subsample not appended to main post-sim ACS because latter may have been cloned

# wrap up validate_model to produce IB2 results in 1 run
# generate CPS 2015 clean file from raw, test using ACS17

# modify R code to match Py

# residence state CA ACS - person and household file merge, SERIALNO is int in 1 file but object in the other
dh = pd.read_csv('./data/acs/2018/household_files/ss18hca.csv', usecols=['SERIALNO', 'NPF'])
dp = pd.read_csv('./data/acs/2018/person_files/ss18pca.csv', usecols=['SERIALNO', 'WKW'])
#pd.merge(dp, dh, how='left', on='SERIALNO')
for d in pd.read_csv('./data/acs/2018/person_files/ss18pca.csv', usecols=['SERIALNO', 'WKW'], chunksize=100000, low_memory=False):
    pd.merge(d, dh, how='left', on='SERIALNO')
dh['SERIALNO'] = dh['SERIALNO'].astype(str)
dh['idtype'] = [type(x) for x in dh['SERIALNO']]
dp['SERIALNO'] = dp['SERIALNO'].astype(str)
dp['idtype'] = [type(x) for x in dp['SERIALNO']]

# get total case counts in post-sim acs
x = 0
for t in types:
    print('Total case counts for type %s' % t)
    print(acs[(acs['takeup_%s' % t]==1)]['PWGTP'].sum())
    x+=acs[(acs['takeup_%s' % t]==1)]['PWGTP'].sum()
print('Total case counts for all types:')
print(x)

# IB6 - back of envelope calculation of top-coding sum of cpl_matdis and cpl_bond at 60 days, and allocate benefits
fp_acs = 'output/_ib6_v2/output_20200624_222132_main simulation_fed_rf/acs_sim_all_20200624_222132.csv'
fp_acs1250 = 'output/_ib6_v2/output_20200625_155900_main simulation_fed_rf_1250/acs_sim_all_20200625_155900.csv'
acs = pd.read_csv(fp_acs)
acs1250 = pd.read_csv(fp_acs1250)

def get_outlay(acs):
    # all takeup_type = 1 for matdis, bond since assumed takeup rates = 1 for federal parental leave program
    # verify annual_benefit cols add up to GUI results
    outlay_matdis = round(((acs.annual_benefit_matdis)*(acs.PWGTP)).sum()/10**6, 1)
    print('Outlay matdis = $%s million' % outlay_matdis)
    outlay_bond = round(((acs.annual_benefit_bond)*(acs.PWGTP)).sum()/10**6, 1)
    print('Outlay bond = $%s million' % outlay_bond)
    print(' --- Total outlay = %s' % (outlay_matdis + outlay_bond))
    return None
get_outlay(acs)
get_outlay(acs1250)

## below checks why acs and acs1250 merge results in unique rows in BOTH side -
## - caused by ACS-CPS impute logit missing seed control
# cols = ['SERIALNO', 'SPORDER', 'annual_benefit_matdis', 'annual_benefit_bond', 'wage12', 'cpl_matdis', 'cpl_bond', 'wkhours']
# acsm = pd.merge(acs[cols], acs1250[cols], on=['SERIALNO', 'SPORDER'], how='outer', indicator=True)
# acsm[acsm['_merge']=='left_only'].sort_values(by=['SERIALNO', 'SPORDER']).wkhours_x.describe()

# get scale-down factor if total cpl>60
acs['cpl_matdis_bond'] = acs['cpl_matdis'] + acs['cpl_bond']
acs['cpl_matdis_bond_capped'] = [min(x, 60) for x in acs['cpl_matdis_bond']]
acs['scale_down'] = acs['cpl_matdis_bond_capped']/acs['cpl_matdis_bond']
# apply scale-down factor to annual benefit of each type
for t in ['matdis', 'bond']:
    acs['annual_benefit_%s_allocate' % t] = acs['annual_benefit_%s' % t] * acs['scale_down']
# get annual benefit with 60-day cap applied

outlay_matdis_capped = round(((acs.annual_benefit_matdis_allocate)*(acs.PWGTP)).sum()/10**6, 1)
print('Outlay matdis capped = $%s million' % outlay_matdis_capped)
outlay_bond_capped = round(((acs.annual_benefit_bond_allocate)*(acs.PWGTP)).sum()/10**6, 1)
print('Outlay bond capped = $%s million' % outlay_bond_capped)
print(' --- Total outlay = %s' % (outlay_matdis_capped + outlay_bond_capped))
