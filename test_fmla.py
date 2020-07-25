'''
test Py/R cleaned FMLA data

chris zhang 2/28/2020
'''
import pandas as pd
pd.set_option('max_colwidth', 100)
pd.set_option('display.max_columns', 999)
pd.set_option('display.width', 200)
import numpy as np
import pyreadr
from _5a_aux_functions import get_bool_num_cols


# types
types = ['own', 'matdis', 'bond', 'illchild', 'illspouse', 'illparent']

# Xs, ys, w
id = 'empid'
Xs = ['widowed', 'divorced', 'separated', 'nevermarried',
      'female', 'age', 'agesq',
      'ltHS', 'someCol', 'BA', 'GradSch',
      'black', 'other', 'asian', 'native', 'hisp',
      'nochildren', 'faminc', 'coveligd']
ys = ['take_%s' % x for x in types] + ['need_%s' % x for x in types] + ['resp_len']
w = 'weight'
# lens = ['length']
cols = [id] + Xs + ys + [w] + ['recStatePay']  # lens +

fp_p = './data/fmla_2012/fmla_2012_restrict/fmla_clean_2012.csv'
fp_r = './PR_comparison/check_clean_fmla/d_fmla_restrict.rds'

dp = pd.read_csv(fp_p, usecols=cols)
dr = pyreadr.read_r(fp_r)
dr = dr[None]
dr = dr[cols]


## Check bool num cols
bool_cols, num_cols = get_bool_num_cols(dr)
for c in bool_cols:
    if c[:2]!='z_':
        print('----------------- xvar = %s -----------------------' % c)
        print('--- %s - Py ---' % c)
        print(dp[c].value_counts().sort_index())
        print('--- %s - R ---' % c)
        print(dr[c].value_counts().sort_index())

for c in num_cols:
    if c[:2]!='z_':
        print('----------------- xvar = %s -----------------------' % c)
        print('--- %s - Py ---' % c)
        if len(dp[c].isna().value_counts())==2:
            print('nrow missing = %s' % dp[c].isna().value_counts()[True])
        print(dp[c].mean())
        print('--- %s - R ---' % c)
        if len(dr[c].isna().value_counts())==2:
            print('nrow missing = %s' % dr[c].isna().value_counts()[True])
        print(dr[c].mean())

