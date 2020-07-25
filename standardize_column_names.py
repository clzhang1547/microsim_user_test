'''
code to standardize column names of R to be consistent with Python model
ACS - master, abf, summary files
FMLA

chris zhang 6/15/2020
'''
import pandas as pd
pd.set_option('max_colwidth', 100)
pd.set_option('display.max_columns', 999)
pd.set_option('display.width', 200)
import numpy as np

def get_col_overlaps(fps, data_name):
    '''

    :param fps: fp_p, fp_r, file paths to Py and R data files
    :param data_name: name of dataset, acs or fmla
        '''
    # Get cols of Python and R output
    dp = pd.read_csv(fp_p)
    dr = pd.read_csv(fp_r)
    cols_p, cols_r = dp.columns, dr.columns
    # Get cols in R only (to be removed)
    cols_r_only = set(cols_r) - set(cols_p)
    # Get cols in Py only (to be added)
    cols_p_only = set(cols_p) - set(cols_r)
    # Get cols in both Py and R
    cols_pr = set(cols_p).intersection(set(cols_r))
    # Export
    dc_r = pd.DataFrame(list(cols_r_only))
    dc_r.columns = ['col_r']
    dc_r = dc_r.sort_values(by='col_r')
    dc_r.to_excel('./meta_output/cols_%s_r_only.xlsx' % data_name, index=False)

    dc_p = pd.DataFrame(list(cols_p_only))
    dc_p.columns = ['col_p']
    dc_p = dc_p.sort_values(by='col_p')
    dc_p.to_excel('./meta_output/cols_%s_p_only.xlsx' % data_name, index=False)

    dc_pr = pd.DataFrame(list(cols_pr))
    dc_pr.columns = ['col_pr']
    dc_pr = dc_pr.sort_values(by='col_pr')
    dc_pr.to_excel('./meta_output/cols_%s_pr.xlsx' % data_name, index=False)
    return None

fp_p = './output/output_20200615_093547_main simulation/acs_sim_ri_20200615_093547.csv'
fp_r = 'C:/workfiles/Microsimulation/microsim_r_cz/output/test_execution_WY_post.csv'
fps = [fp_p, fp_r]
get_col_overlaps(fps, 'acs')
fp_p = './data/fmla/fmla_2018/fmla_clean_2018.csv'
fp_r = 'C:/workfiles/Microsimulation/microsim_r_cz/csv_inputs/fmla_clean_2018.csv'
fps = [fp_p, fp_r]
get_col_overlaps(fps, 'fmla')

