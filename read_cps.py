'''
read in CPS data prepared from Stata code on NBER
raw data
https://data.nber.org/data/current-population-survey-data.html
Stata code (do/dct files)
https://data.nber.org/data/cps_progs.html

no job_tenure vars in CPS or ACS - will have to remove from FMLA modeling xvars
within-FMLA validation shows limited effect by removing job_tenure from xvars

chris zhang 5/21/2020
'''
import pandas as pd
pd.set_option('max_colwidth', 100)
pd.set_option('display.max_columns', 999)
pd.set_option('display.width', 200)
import numpy as np
from _5a_aux_functions import fillna_df

## Set up file paths, random state
# TODO: let random_seed link to seed specified in model

## Input
year = 2014
fp_in = './data/cps/cpsmar%s.dta' % year
fp_out = './data/cps/cps_clean_%s.csv' % year
random_seed = 12345
random_state = np.random.RandomState(random_seed)

## Read in data
cps = pd.read_stata(fp_in, convert_categoricals=False) # , convert_missing=True works for displaying data but still all NA

## Remove irrelevant rows
# remove children/armed forces, and not in labor force
cps = cps.drop(cps[cps['a_wkstat'].isin([0, 1])].index)
# remove without-pay/never worked
cps = cps.drop(cps[cps['a_clswkr'].isin([0, 7, 8])].index)

## Create vars
# female, race, age, educ, married
cps['female'] = np.where(cps['a_sex'] == 2, 1, 0)
cps['female'] = np.where(cps['a_sex'].isna(), np.nan, cps['female'])
cps['hisp'] = np.where(cps['pehspnon'] == 1, 1, 0)
cps['black'] = np.where((cps['hisp'] == 0) & (cps['prdtrace'] == 2), 1, 0)
cps['black'] = np.where((cps['hisp'] == 0) & (cps['prdtrace'].isna()), np.nan, cps['black'])
cps['asian'] = np.where((cps['hisp'] == 0) & (cps['prdtrace'] == 4), 1, 0)
cps['asian'] = np.where((cps['hisp'] == 0) & (cps['prdtrace'].isna()), np.nan, cps['asian'])
cps['native'] = np.where((cps['hisp'] == 0) & (cps['prdtrace'].isin([3, 5, 14])), 1, 0)
cps['native'] = np.where((cps['hisp'] == 0) & (cps['prdtrace'].isna()), np.nan, cps['native'])
cps['other'] = np.where((cps['hisp'] == 0) & (~cps['prdtrace'].isin([1, 2, 3, 4, 5, 14])), 1, 0)
cps['other'] = np.where((cps['hisp'] == 0) & (cps['prdtrace'].isna()), np.nan, cps['other'])
cps['age'] = cps['a_age']
cps['agesq'] = cps['age'] * cps['age']
cps['BA'] = np.where(cps['a_hga'] == 43, 1, 0)
cps['BA'] = np.where(cps['a_hga'].isna(), np.nan, cps['BA'])
cps['GradSch'] = np.where((cps['a_hga'] <= 46) & (cps['a_hga'] >= 44), 1, 0)
cps['GradSch'] = np.where(cps['a_hga'].isna(), np.nan, cps['GradSch'])
cps['married'] = np.where(cps['a_maritl'].isin([1, 2, 3]), 1, 0)
cps['married'] = np.where(cps['a_maritl'].isna(), np.nan, cps['married'])

# wage
cps['wage12'] = np.where(cps['wsal_val'] > 0, cps['wsal_val'], np.nan)
# work weeks over year - use wkswork
# hours per week
cps['wkhours'] = np.where(cps['hrswk'] > 0, cps['hrswk'], np.nan)
# employment at government
cps['emp_gov'] = np.where(cps['a_clswkr'].isin([2, 3, 4]), 1, 0)
cps['emp_gov'] = np.where(cps['a_clswkr'].isna(), 1, cps['emp_gov'])

# industry - use dummies for each cat
cps = cps.join(pd.get_dummies(cps['a_mjind'], prefix='ind'))
# industry - use 5 cats = top 3 cats, other, NA
# get top 3 cats, make dummy cols
if 0 in cps.a_mjind.value_counts().index:
    ind_top3_cats = list(cps.a_mjind.value_counts().drop(index=[0]).sort_values(ascending=False).index[:3])
    ind_nontop3_cats = list(cps.a_mjind.value_counts().drop(index=[0]).sort_values(ascending=False).index[3:])
else:
    ind_top3_cats = list(cps.a_mjind.value_counts().sort_values(ascending=False).index[:3])
    ind_nontop3_cats = list(cps.a_mjind.value_counts().sort_values(ascending=False).index[3:])
for i, c in enumerate(ind_top3_cats):
    cps['ind_top%s' % (i + 1)] = np.where(cps['a_mjind'] == c, 1, 0)
# make dummy col for other, NA
cps['ind_other'] = np.where(cps['a_mjind'].isin(ind_nontop3_cats), 1, 0)
cps['ind_na'] = np.where(cps['a_mjind'] == 0, 1, 0)

# occupation - use dummies for each cat
cps = cps.join(pd.get_dummies(cps['a_mjocc'], prefix='occ'))
# occupation - use 5 cats = top 3 cats, other, NA
# get top 3 cats, make dummy cols
if 0 in cps.a_mjocc.value_counts().index:
    occ_top3_cats = list(cps.a_mjocc.value_counts().drop(index=[0]).sort_values(ascending=False).index[:3])
    occ_nontop3_cats = list(cps.a_mjocc.value_counts().drop(index=[0]).sort_values(ascending=False).index[3:])
else:
    occ_top3_cats = list(cps.a_mjocc.value_counts().sort_values(ascending=False).index[:3])
    occ_nontop3_cats = list(cps.a_mjocc.value_counts().sort_values(ascending=False).index[:3])
for i, c in enumerate(occ_top3_cats):
    cps['occ_top%s' % (i + 1)] = np.where(cps['a_mjocc'] == c, 1, 0)
# make dummy col for other, NA
cps['occ_other'] = np.where(cps['a_mjocc'].isin(occ_nontop3_cats), 1, 0)
cps['occ_na'] = np.where(cps['a_mjocc'] == 0, 1, 0)

# hourly paid
cps['hourly'] = np.where(cps['a_hrlywk'] == 1, 1, 0)
cps['hourly'] = np.where((cps['a_hrlywk'] == 0) | (cps['a_hrlywk'].isna()), np.nan, cps['hourly'])
# firm size
cps['empsize'] = cps['noemp']
# one employer over past 12 months
cps['oneemp'] = np.where(cps['phmemprs'] == 1, 1, 0)
cps['oneemp'] = np.where(cps['phmemprs'].isna(), np.nan, cps['oneemp'])

# yvars not in ACS, thus need CPS models for imputing

# union
# yvar - a_unmem (defined only for prerelg=1, which marks eligibility for earning edit)
# see https://www.icpsr.umich.edu/web/RCMD/studies/04218/datasets/0001/variables/PRERELG?archive=RCMD
# prerelg=0 means certain vars not available (e.g. union membership, a_unmem)
cps['union'] = np.nan
cps['union'] = np.where(cps['a_unmem'] == 1, 1, cps['union'])
cps['union'] = np.where(cps['a_unmem'] == 2, 0, cps['union'])
# xvars - age, female, race, married, educ, ind, occ, emp_gov, hourly, wkswork, wkhours, empsize, oneemp,

# fmla_eligible - 1 firm over past 12m / 1250 hours worked over past 12m / 50+ employees within 75 miles
# yvar - determined by wkswork, wkhours, empsize, oneemp
cps['fmla_eligible'] = np.where((cps['oneemp'] == 1) &
                                (cps['wkswork'] * cps['wkhours'] >= 1250) &
                                (cps['empsize'].isin([3, 4, 5, 6])), 1, 0)
# xvars - age, female, race, married, educ, ind, occ, emp_gov, hourly
cps = cps[['peridnum', 'marsupwt'] +  # unique person id, weight
          ['female', 'black', 'asian', 'native', 'other', 'age', 'agesq', 'BA', 'GradSch', 'married'] +
          ['wage12', 'wkhours', 'wkswork', 'emp_gov'] +
          ['a_mjind', 'ind_top1', 'ind_top2', 'ind_top3', 'ind_other', 'ind_na'] +
          ['a_mjocc', 'occ_top1', 'occ_top2', 'occ_top3', 'occ_other', 'occ_na'] +
          ['occ_%s' % x for x in range(1, 11)] +
          ['ind_%s' % x for x in range(1, 14)] +
          ['hourly', 'empsize', 'oneemp', 'union', 'fmla_eligible'] +
          ['prerelg', 'a_wkstat', 'a_clswkr', 'a_hrlywk']]  # remove armed forces code from ind/occ

# fillna to get ready for models
# cps = fillna_df(cps, random_state)
# save output
cps.to_csv(fp_out, index=False)
