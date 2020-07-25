'''
generate data dictionary for derived variables in post-simulation datasets
1. fmla_clean_2012
2. fmla_clean_2018
3. acs_sim
'''
import pandas as pd
pd.set_option('max_colwidth', 100)
pd.set_option('display.max_columns', 999)
pd.set_option('display.width', 200)
import numpy as np
from _5a_aux_functions import get_bool_num_cols

# a function to get codebook df template for manual fill in
def get_codebook_df(fp_d, fp_d0=None):
    d = pd.read_csv(fp_d)
    if fp_d0:
        d0 = pd.read_csv(fp_d0)
        # get derived vars
        derived_vars = set(d.columns) - set(d0.columns)
    else:
        derived_vars = set(d.columns)
    # get bool, num cols, set col data type for derive vars
    dct_drv = {}
    bool_cols, nonbool_cols = get_bool_num_cols(d[derived_vars])
    for c in bool_cols:
        dct_drv[c] = 'binary'
    for c in nonbool_cols:
        typ = d[c].dtype.__str__()
        if 'float' in typ:
            typ = 'float'
        dct_drv[c] = typ
    # generate codebook df
    df = pd.DataFrame.from_dict(dct_drv, orient='index')
    df = df.reset_index(drop=False)
    df.columns = ['Variable Name', 'Data Type']
    #df['Description'] = np.nan
    df = df.sort_values(by='Variable Name', ascending=True).reset_index(drop=True)
    return df
# --------------------------------------
# fmla 2012
# --------------------------------------
# get codebook template
fp_d = './data/fmla/fmla_2012/fmla_clean_2012.csv'
fp_d0 = './data/fmla/fmla_2012/fmla_2012_employee_revised_puf.csv'
df = get_codebook_df(fp_d, fp_d0)
# merge with current codebook to fill in some variable descriptions
cb = pd.read_excel('./docs/Python Microsim Model - Derived Data Dictionary.xlsx')
cb = cb[['Variable Name', 'Variable Label']].drop_duplicates(subset=['Variable Name'])
df = pd.merge(df, cb, how='left', on='Variable Name')
df = df.rename(columns={'Variable Label': 'Description'})
df.to_excel('./docs/data_dictionary_fmla_2012.xlsx', index=False)
# --------------------------------------
# fmla 2018
# --------------------------------------
# get codebook template
fp_d = './data/fmla/fmla_2018/fmla_clean_2018.csv'
fp_d0 = './data/fmla/fmla_2018/FMLA 2018 PUF/FMLA_2018_Employee_PUF.csv'
df = get_codebook_df(fp_d, fp_d0)
# merge with current codebook to fill in some variable descriptions
cb = pd.read_excel('./docs/Python Microsim Model - Derived Data Dictionary.xlsx')
cb = cb[['Variable Name', 'Variable Label']].drop_duplicates(subset=['Variable Name'])
df = pd.merge(df, cb, how='left', on='Variable Name')
df = df.rename(columns={'Variable Label': 'Description'})
df.to_excel('./docs/data_dictionary_fmla_2018.xlsx', index=False)
# --------------------------------------
# acs
# --------------------------------------
# get codebook template
fp_d = './output/output_20200526_230724_main simulation/acs_sim_ri_20200526_230724.csv'
df = get_codebook_df(fp_d, fp_d0)
# merge with current codebook to fill in some variable descriptions
cb = pd.read_excel('./docs/Python Microsim Model - Derived Data Dictionary.xlsx')
cb = cb[['Variable Name', 'Variable Label']].drop_duplicates(subset=['Variable Name'])
df = pd.merge(df, cb, how='left', on='Variable Name')
df = df.rename(columns={'Variable Label': 'Description'})
df.to_excel('./docs/data_dictionary_acs.xlsx', index=False)