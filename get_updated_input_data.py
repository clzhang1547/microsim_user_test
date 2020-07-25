'''
get updated input data
- download do/dct file to convert CPS raw dat file to Stata dta file, then read using pandas
https://data.nber.org/data/cps_progs.html

chris zhang 1/31/2020
'''

import pandas as pd
pd.set_option('max_colwidth', 100)
pd.set_option('display.max_columns', 999)
pd.set_option('display.width', 200)
import numpy as np

import urllib.request
...

# get updated CPS data
# cps cols needed -
cols = ['a_sex', 'prdtrace', 'a_age', 'a_hga', 'a_mjind', 'a_mjocc', 'a_hrlywk']
cols += ['a_noemp', 'a_phmemprs', 'marsupwt', 'wkswork']
#cps14 = pd.read_csv('./data/cps/CPS2014extract.csv')
cps15_raw = pd.read_csv('./data/cps/cps2015.csv')
for c in cols:
    if c not in cps15_raw.columns:
        print('-- NOT in 15 -- %s' % c)
    else:
        print('-- in 15 -- %s' % c)

'''
female

'''
cols = ['a_sex', 'prdtrace', 'a_age', 'a_hga', 'a_mjind', 'a_mjocc', 'a_hrlywk']
cols += ['noemp', 'phmemprs', 'marsupwt', 'wkswork']
fp_in = './data/cps/cpsmar2015.dta'
cps15 = pd.read_stata(fp_in, columns=cols, convert_categoricals=False)
cps15.to_csv('./data/cps/CPS2015extract.csv', index=False)

cps16 = pd.read_stata(fp_in, columns=cols, convert_categoricals=False)
cps16.to_csv('./data/cps/CPS2016extract.csv', index=False)
