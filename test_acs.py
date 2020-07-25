'''
test consistency of ACS between Python and R

chris zhang 1/24/2020
'''
import pandas as pd
pd.set_option('max_colwidth', 100)
pd.set_option('display.max_columns', 999)
pd.set_option('display.width', 200)
import numpy as np
from _5a_aux_functions import get_bool_num_cols

# a function to read in data and reduce cols
def preprocess_data(fp_acs, cols):
    # Read in ACS
    d = pd.read_csv(fp_acs)
    # R - keep eligible workerstry:
    try:
        d = d[d['eligworker']==1]
    except KeyError:
        pass
    # R - rename len cols
    dct_cols = dict(zip(['squo_length_%s' % x for x in types], ['len_%s' % x for x in types]))
    dct_cols.update(dict(zip(['length_%s' % x for x in types], ['cfl_%s' % x for x in types])))
    dct_cols.update(dict(zip(['plen_%s' % x for x in types], ['cpl_%s' % x for x in types])))
    dct_cols.update(dict(zip(['ptake_%s' % x for x in types], ['takeup_%s' % x for x in types])))
    dct_cols.update({'weeks_worked': 'wkswork'})
    d = d.rename(columns=dct_cols)

    # TODO: remove below
    if 'takeup_illparent' not in d.columns:
        d.loc[d['take_illparent']==1, 'takeup_illparent']=1

    # reduce cols
    d = d[cols]

    # ## Standardize
    # # use N-ddof = N-1 as in R
    # # https://stackoverflow.com/questions/27296387/difference-between-r-scale-and-sklearn-preprocessing-scale/27297618
    #
    # # TODO: check how fillna works in R - need to be same as Python
    # # drop missing rows if any col is missing
    # #d = d.dropna(subset=Xs)
    # # standardize
    # for X in Xs:
    #     d['z_%s' % X] = (d[X] - d[X].mean()) / np.std(d[X], axis=0, ddof=1)
    # print(d[['SERIALNO', 'age','z_age', 'female', 'z_female']].head())
    # cols to return


    return d


## Read in data and preprocess
# id
id = 'SERIALNO'
# Xs, ys, w
Xs = ['widowed', 'divorced', 'separated', 'nevermarried',
      'female', 'age', 'agesq',
      'ltHS', 'someCol', 'BA', 'GradSch',
      'black', 'other', 'asian', 'native', 'hisp',
      'nochildren', 'faminc', 'coveligd']
types = ['own', 'matdis', 'bond', 'illchild', 'illspouse', 'illparent']
ys = ['take_%s' % x for x in types] + ['need_%s' % x for x in types] + ['resp_len']
w = 'PWGTP'
lens = ['len_%s' % x for x in types]
lens+= ['mnl_%s' % x for x in types]
lens+= ['cfl_%s' % x for x in types]
lens+= ['cpl_%s' % x for x in types]
cols_cost = ['takeup_%s' % x for x in types]
cols_cost += ['wage12', 'wkswork']
cols = [id] + Xs + ys + [w] + lens + cols_cost
# fp Python
fp_p = './output/output_20200303_170035_main simulation/acs_sim_ri_20200303_170035.csv'
# fp R
fp_r = './PR_comparison/check_acs/acs_R/RI_logitfull_test.csv'
# preprocess
dp = preprocess_data(fp_p, cols)
dr = preprocess_data(fp_r, cols)

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

# check lens
for c in lens:
    # fillna with 0 for dp[c] for Py
    dp[c] = dp[c].fillna(0)

    if c[:2] != 'z_':
        print('----------------- xvar = %s -----------------------' % c)
        print('--- %s - Py ---' % c)
        if len(dp[c].isna().value_counts()) == 2:
            print('nrow missing = %s' % dp[c].isna().value_counts()[True])
        print(dp[c].mean())
        print('--- %s - R ---' % c)
        if len(dr[c].isna().value_counts()) == 2:
            print('nrow missing = %s' % dr[c].isna().value_counts()[True])
        print(dr[c].mean())
# TODO: coveligd - both Py/R no missing. But #s mismatch

## Compute total outlay
# params
params = {}
params['wkbene_cap'] = 759
params['rrp'] = 0.6
pow_pop_multiplier = 1.02
def get_costs(df):
    # apply take up flag and weekly benefit cap, and compute total cost, 6 types
    costs = {}
    for t in types:
        # v = capped weekly benefit of leave type
        v = [min(x, params['wkbene_cap']) for x in
             ((df['wage12'] / df['wkswork'] * params['rrp']))]
        # inflate weight for missing POW
        w = df['PWGTP'] * pow_pop_multiplier
    
        # get program cost for leave type t - sumprod of capped benefit, weight, and takeup flag for each ACS row
        costs[t] = (v * df['cpl_%s' % t] / 5 * w * df['takeup_%s' % t]).sum()
    costs['total'] = sum(list(costs.values()))
    return costs

cp = get_costs(dp)
cr = get_costs(dr)


'''
  d <- d %>% mutate(base_benefits=wage12/(round(weeks_worked*5))*particip_length*benefit_prop)
  d <- d %>% mutate(actual_benefits=base_benefits)

  d <- d %>% mutate(actual_benefits= ifelse(actual_benefits>ceiling(week_bene_cap*particip_length)/5,
                                            ceiling(week_bene_cap*particip_length)/5, actual_benefits))
'''

acsr = pd.read_csv(fp_r)
acsr = acsr[acsr['eligworker']==1]
acsr['sum_plen'] = [sum(x) for x in acsr[['plen_%s' % x for x in types]].values]
(acsr['sum_plen'] - acsr['particip_length'])

# get total cost as in R code
def get_cost_R(df):
    # apply take up flag and weekly benefit cap, and compute total cost, 6 types
    uncapped_total_benefits = (df['wage12']/df['wkswork'])*params['rrp']*(df['particip_length']/5)
    total_caps = params['wkbene_cap']*df['particip_length']/5
    capped_total_benefits = [min(x[0], x[1]) for x in pd.concat([uncapped_total_benefits, total_caps], axis=1).values]
    total_cost = (capped_total_benefits*df['PWGTP']*pow_pop_multiplier).sum()
    return total_cost
acsr = pd.read_csv(fp_r)
acsr = acsr.rename(columns={'weeks_worked': 'wkswork'})
get_cost_R(acsr)

# get total cost as in Py code
def get_cost_Py(df):
    # apply take up flag and weekly benefit cap, and compute total cost, 6 types
    costs = {}
    for t in types:
        # v = capped weekly benefit of leave type
        v = [min(x, params['wkbene_cap']) for x in
             ((df['wage12'] / df['wkswork'] * params['rrp']))]
        # TODO: use this in simulation code - see underestimation! Consider up cpl to (close-to) cfl as in R
        # cpl_wk = cpl in weeks
        cpl_wk = (df['cpl_%s' % t] / 5)
        # inflate weight for missing POW
        w = df['PWGTP'] * pow_pop_multiplier

        # get program cost for leave type t - sumprod of capped benefit, weight, and takeup flag for each ACS row
        costs[t] = (v * cpl_wk * w * df['takeup_%s' % t]).sum()
    total_cost = sum(list(costs.values()))
    return total_cost
print('Total cost estimate from Py: ', get_cost_Py(dp))
print('Total cost estimate from R: ', get_cost_Py(dr))


# R has much larger CPL, check below
# TODO: check Py/R cfl>>cpl process differences
for x in types:
    for c in ['len_%s' % x, 'mnl_%s' % x, 'cfl_%s' % x, 'cpl_%s' % x]:
    #for c in ['cfl_%s' % x, 'cpl_%s' % x]:
        print('dp[dp[%s]>0][%s].mean()\n' % (c, c), dp[dp[c]>0][c].mean())
        print('dr[dr[%s]>0][%s].mean()\n' % (c, c), dr[dr[c]>0][c].mean())
    print('-----------------')

# check total candidate population of take_type
for t in types:
    print('dp[dp.take_%s==1].PWGTP.sum()\n' % t, dp[dp['take_%s' % t]==1].PWGTP.sum())
    print('dr[dr.take_%s==1].PWGTP.sum()\n' % t, dr[dr['take_%s' % t] == 1].PWGTP.sum())
    print('--------------------')

# check take up flags for types
for t in types:
    print('dp[dp[takeup_%s]==1][PWGTP].sum()\n' % t, dp[dp['takeup_%s' % t]==1]['PWGTP'].sum())
    print('dr[dr[takeup_%s]==1][PWGTP].sum()\n' % t, dr[dr['takeup_%s' % t]==1]['PWGTP'].sum())
    print('--------------------')
# check cond' CPF of draw program takers
for t in types:
    print('Py: conditional weighted mean of cpl_%s among takeup persons' % t,
          np.average(dp[(dp['cpl_%s' % t] > 0) & (dp['takeup_%s' % t] == 1)]['cpl_%s' % t],
                     weights=dp[(dp['cpl_%s' % t] > 0) & (dp['takeup_%s' % t] == 1)]['PWGTP']))
    print('R: conditional weighted mean of cpl_%s among takeup persons' % t,
          np.average(dr[(dr['cpl_%s' % t] > 0) & (dr['takeup_%s' % t] == 1)]['cpl_%s' % t],
                     weights=dr[(dr['cpl_%s' % t] > 0) & (dr['takeup_%s' % t] == 1)]['PWGTP']))
    print('------------------------------------------------')
# much bigger! seems like R has used wrong base for eligible worker pop
# should be ~350k but not 415k as in Excel state take up data
# TODO: LP to use ACS eligworker est pop as base to re-gen emp take up rates, apply in GUI and re-run model

# TODO: R's lens are much SHORTER than Py's for all types, check why


fp_p = './output/output_20200217_234056_main simulation/acs_sim_20200217_234056.csv'
acsp = pd.read_csv(fp_p)
for t in types:
    print('mean positive cpl for type = %s\n' % t, acsp[acsp['cpl_%s' % t]>0]['cpl_%s' % t].mean())

##
for t in types:
    for c in acsr.columns:
        if t in c:
            print(c)
    print('-------------')

##
# TODO: R post-sim ACS has no takes_up_illparent
# TODO: in roundstats file RI eligible pop =/= sum of PWGTP from R ACS (357531) which is the same as Py's
# TODO: R's plen still way bigger than Py's, causing over estimation of RI's outlay. line 712 in R4 code shows cpl/cfl link may be problematic
# TODO: cpl should <= cfl. cfl is close in Py/R but cpl diverges a lot. Also see cpl>cfl in R (own) not possible!
# TODO: What GUI takeup rate did R use? Rates in roundstats file look low - why need them?

##
# check: cfl>=cpl for everyone in R ACS?
for t in types:
    dr['c_%s' % t] = (dr['cfl_%s' % t] >= dr['cpl_%s' % t])
    print(dr['c_%s' % t].value_counts().sort_index())
for x in types:
    #for c in ['len_%s' % x, 'mnl_%s' % x, 'cfl_%s' % x, 'cpl_%s' % x]:
    for c in ['cfl_%s' % x, 'cpl_%s' % x]:
        #print('dp[dp[%s]>0][%s].mean()\n' % (c, c), dp[dp[c]>0][c].mean())
        print('dr[dr[%s]>0][%s].mean()\n' % (c, c), dr[dr[c]>0][c].mean())
    print('-----------------')

# use actual benefits col in R to get outlay
acsr = pd.read_csv(fp_r)
1.02*sum([x[0]*x[1] for x in acsr[acsr['eligworker']==1][['actual_benefits', 'PWGTP']].values])

# re-draw take up flags by updating min_takeup_cpl
from _5a_aux_functions import get_columns, get_sim_col, get_weighted_draws, weighted_shuffle
col_w='PWGTP'

def get_dp_with_updated_takeup_flag(acs, min_takeup_cpl, alpha=0):
    # alpha: exponent of CPL for mapping CPL to shuffle_weights = f(CPL)
    params = {}
    params['d_takeup'] = dict(zip(types, [0.0723, 0.0241, 0.0104, 0.0006, 0.0015, 0.0009]))
    for t in types:
        # cap user-specified take up for type t by max possible takeup = s_positive_cpl, in pop per sim results
        s_positive_cpl = acs[acs['cpl_%s' % t] >= min_takeup_cpl][col_w].sum() / acs[col_w].sum()
        # display warning for unable to reach target pop from simulated positive cpl_type pop
        if col_w == 'PWGTP':
            if params['d_takeup'][t] > s_positive_cpl:
                print('Warning: User-specified take up for type -%s- is capped '
                      'by maximum possible take up rate (share of positive covered-by-program length) '
                      'based on simulation results, at %s.' % (t, s_positive_cpl))
        takeup = min(s_positive_cpl, params['d_takeup'][t])
        p_draw = takeup / s_positive_cpl  # need to draw w/ prob=p_draw from cpl>min_takeup_cpl subpop, to get desired takeup
        # print('p_draw for type -%s- = %s' % (t, p_draw))
        # get take up indicator for type t - weighted random draw from cpl_type>min_takeup_cpl until target is reached
        acs['takeup_%s' % t] = 0
        # TODO: set alpha as parameter in GUI?
        draws = get_weighted_draws(acs[acs['cpl_%s' % t] >= min_takeup_cpl][col_w], p_draw,
                                   random_state=np.random.RandomState(12345),
                                   shuffle_weights=(acs[acs['cpl_%s' % t] >= min_takeup_cpl]['cpl_%s' % t])**alpha)
        # print('draws = %s' % draws)
        acs.loc[acs['cpl_%s' % t] >= min_takeup_cpl, 'takeup_%s' % t] \
            = draws

        # for main weight, check if target pop is achieved among eligible ACS persons
        if col_w == 'PWGTP':
            s_takeup = acs[acs['takeup_%s' % t] == 1][col_w].sum() / acs[col_w].sum()
            s_takeup = round(s_takeup, 4)
            print('Specified takeup for type %s = %s. '
                  'Effective takeup = %s. '
                  'Post-sim weighted share = %s' % (t, params['d_takeup'][t], takeup, s_takeup))
    return acs
acs = dr.copy()
dr = get_dp_with_updated_takeup_flag(acs, 5, alpha=1)
get_cost_Py(dr)

# update below for different output based on alpha values
fp_p = './output/output_20200224_202316_main simulation/acs_sim_20200224_202316.csv'
dp = preprocess_data(fp_p, cols)
# check take up flags for types
for t in types:
    print('dp[dp[takeup_%s]==1][PWGTP].sum()\n' % t, dp[dp['takeup_%s' % t]==1]['PWGTP'].sum())
    #print('dr[dr[takeup_%s]==1][PWGTP].sum()\n' % t, dr[dr['takeup_%s' % t]==1]['PWGTP'].sum())
    print('--------------------')
# check cond' CPF of draw program takers
for t in types:
    print('conditional weighted mean of cpl_%s among takeup persons' % t,
          np.average(dp[(dp['cpl_%s' % t] > 0) & (dp['takeup_%s' % t] == 1)]['cpl_%s' % t],
                     weights=dp[(dp['cpl_%s' % t] > 0) & (dp['takeup_%s' % t] == 1)]['PWGTP']))

## Plot
import matplotlib.pyplot as plt
import matplotlib
from Utils import format_chart
from collections import OrderedDict as od
# RI - Program outlay by leave type, Py results vs actual

# actual program data
actual = {}
actual['n_cases'] = od(zip(types, [26352, 8784, 3778, 205, 554, 332]))
for k in ['n_cases']:
    actual[k]['DI'] = actual[k]['own'] + actual[k]['matdis']
    actual[k]['PFL'] = actual[k]['bond'] + actual[k]['illchild'] \
                           + actual[k]['illspouse'] + actual[k]['illparent']
    actual[k]['TOTAL'] = actual[k]['DI'] + actual[k]['PFL']
actual['outlay'] = od(zip(['DI', 'PFL', 'TOTAL', ], [160.9, 8.6, 169.5, ]))
# simulation results
fp_p = './output/output_20200226_210803_main simulation/acs_sim_20200226_210803.csv'
fp_p_outlay = './output/output_20200226_210803_main simulation/program_cost_ri_20200226_210803.csv'
dp = pd.read_csv(fp_p)
dp_outlay = pd.read_csv(fp_p_outlay)
sim = {}
sim['n_cases'] = od({})
sim['outlay'] = od({})
for t in types:
    sim['n_cases'][t] = dp[dp['takeup_%s' % t]==1]['PWGTP'].sum()
    sim['outlay'][t] = round(dp_outlay.loc[dp_outlay['type']==t, 'cost'].values[0]/10**6, 1)
for k in ['n_cases', 'outlay']:
    sim[k]['DI'] = sim[k]['own'] + sim[k]['matdis']
    sim[k]['PFL'] = sim[k]['bond'] + sim[k]['illchild'] \
                           + sim[k]['illspouse'] + sim[k]['illparent']
    sim[k]['TOTAL'] = sim[k]['DI'] + sim[k]['PFL']

# plot for n_cases
fp_out = './output/'
title = '' # 'Comparison of Simulation Results vs Program Data, Number of Cases in RI'
fig, ax = plt.subplots(figsize=(8, 6), dpi=100)
ys = list(sim['n_cases'].values())[:-3]
zs = list(actual['n_cases'].values())[:-3]
ind = np.arange(len(ys))
width = 0.2
bar1 = ax.bar(ind-width/2, ys, width, align='center', capsize=5, color='indianred', ecolor='grey')
bar2 = ax.bar(ind+width/2, zs, width, align='center', capsize=5, color='tan', ecolor='grey')
ax.set_ylabel('Number of Cases Paid')
ax.set_xticks(ind)
ax.set_xticklabels(('Own Illness', 'Maternity', 'New Born Child',
                    'Ill Child', 'Ill Spouse', 'Ill Parent',))
ax.yaxis.grid(False)
ax.legend( (bar1, bar2,), ('Simulated', 'Actual',) )
ax.ticklabel_format(style='plain', axis='y')
#ax.get_yaxis().set_major_formatter(matplotlib.ticker.FuncFormatter(lambda x, p: format(int(x), ',')))
format_chart(fig, ax, title, bg_color='white', fg_color='k')
# save
plt.savefig(fp_out + 'RI_validation_n_cases.png', facecolor='white', edgecolor='grey') #

# plot for outlay
fp_out = './output/'
title = '' # 'Comparison of Simulation Results vs Program Data, Program Outlay in RI'
fig, ax = plt.subplots(figsize=(8, 6), dpi=100)
ys = list(sim['outlay'].values())[-3:]
zs = list(actual['outlay'].values())[-3:]
ind = np.arange(len(ys))
width = 0.2
bar1 = ax.bar(ind-width/2, ys, width, align='center', capsize=5, color='indianred', ecolor='grey')
bar2 = ax.bar(ind+width/2, zs, width, align='center', capsize=5, color='tan', ecolor='grey')
ax.set_ylabel('Program Outlay (million 2012 $)')
ax.set_xticks(ind)
ax.set_xticklabels(('Own Disability', 'Care for Family Member', 'Total',))
ax.yaxis.grid(False)
ax.legend( (bar1, bar2,), ('Simulated', 'Actual',) )
ax.ticklabel_format(style='plain', axis='y')
#ax.get_yaxis().set_major_formatter(matplotlib.ticker.FuncFormatter(lambda x, p: format(int(x), ',')))
format_chart(fig, ax, title, bg_color='white', fg_color='k')
# save
plt.savefig(fp_out + 'RI_validation_outlay.png', facecolor='white', edgecolor='grey') #



