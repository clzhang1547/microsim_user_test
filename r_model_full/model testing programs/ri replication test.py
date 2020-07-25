'''
partition ACS PUMS by state of work
then merge in person rows with same state of living (with different state of work)

for each state==st, output csv include persons who
(i) live in st and work in st
(ii) live in st and work in state!=st
(iii)  live in state!=st and work in st

for overseas place-of-work code , all person rows will go to a single file

for missing place-of-work code , all person rows will go to a single file
(for later re-weighting of workers if using state of work for eligiblity)

chris zhang 9/12/2019
'''

# TODO: make an 'inflated' PWGTP column to account for missing POWSP population, then compare pops in plot for states
# TODO: or do above when preparing data for plot - just compute a scaling factor and multiply POWSP pop by the factor

import pandas as pd
from time import time
import os.path

## Initialize
#  a DataFrame to store person rows with missing state of work
dm = pd.DataFrame([])
# a dict - mapping state code [st] to df of persons with POWSP==st
dct_pow = {}
# a df to store person rows with pow = overseas code
d_overseas = pd.DataFrame([])

## Read in data
# # read a few rows for testing
# reader = pd.read_csv("./data/csv_pus/ss16pusa.csv", iterator=True)
# d = reader.get_chunk(10**3)

# read US data by chunk
chunksize = 10 ** 6
for part in ['a', 'b', 'c', 'd']:
    dct_pow[part] = {}
    ichunk = 0
    for d in pd.read_csv('./data/csv_pus/ss16pus%s.csv' % part, chunksize=chunksize):
        ichunk += 1
        t0_chunk = time()
        n_dm_0 = len(dm) # n rows in dm before sending any rows in current chunk
        n_sent_0 = 0
        for st in dct_pow[part].keys():
            n_sent_0 += len(dct_pow[part][st]) # n rows in dct_pow[st] across st, before sending any rows in current chunk
        # reduce sample to civilian employed (ESR=1/2)
        # and have paid work (COW=1/2/3/4/5), including self-employed(COW=6/7)
        d = d[((d['ESR'] == 1) | (d['ESR'] == 2)) &
              ((d['COW'] == 1) | (d['COW'] == 2) | (d['COW'] == 3) | (d['COW'] == 4) | (d['COW'] == 5) |
               (d['COW'] == 6) | (d['COW'] == 7))]

        # send rows with missing pow to dm
        dm = dm.append(d[d['POWSP'].isna()])
        # send rows with pow=st OR  to dct_pow[st]
        t0 = time()
        for st in set(d[~d['POWSP'].isna()]['POWSP']):
            # below is done also for overseas pow code.
            # when saving output, they'll be lumped together as a single file
            if st in dct_pow[part].keys():
                dct_pow[part][st] = dct_pow[part][st].append(d[(d['POWSP']==st) | (d['ST']==st)])
            else:
                dct_pow[part][st] = d[(d['POWSP']==st) | (d['ST']==st)]
        t1 = time()
        print('All person rows in current chunk sent to dct_pow[st]. '
              'Time needed for this chunk = %s' % round((t1-t0), 0))

        # in current chunk , check if total number of rows sent is equal to that of original file
        n = 0
        for st in dct_pow[part].keys():
            n += len(dct_pow[part][st]) # total number of rows sent so far in current part
        print('Number of person rows with valid POWSP or ST that were sent in current chunk = %s' % (n - n_sent_0))
        print('Number of person rows with missing POWSP that were sent in current chunk = %s' % (len(dm) - n_dm_0))

        t1_chunk = time()
        print('--------------------------------------------------------')
        print('Chunk %s of US file part %s processed' % (ichunk, part),
              '\n Time needed = %s seconds' % round((t1_chunk - t0_chunk), 0))
        print('--------------------------------------------------------')

## Save files

# a dict from state fips to state abbrev
st_fips = pd.read_excel('./data/state_fips.xlsx')
dct_st = dict(zip(st_fips['st'], st_fips['state_abbrev']))

# ensure st fips is integer in dct_pow[part]
for part in dct_pow.keys():
    dct_pow[part] = {int(k): v for k, v in dct_pow[part].items()}
# save files for each st fips of each part
for part in dct_pow.keys():
    for st in dct_pow[part].keys():
        if st in dct_st.keys(): # st might be code for overseas pow
            dct_pow[part][st].to_csv('./output/pow_by_state_part/p%s_%s_pow_part_%s.csv' % ('0'*(2-len(str(st)))+str(st), dct_st[st].lower(), part), index=False)
            print('Output csv saved for st = %s(%s) in part = %s. Now saving the next file...' % (dct_st[st], st, part))
        else:
            d_overseas = d_overseas.append(dct_pow[part][st])
d_overseas.to_csv('./output/p_pow_overseas.csv', index=False)
dm.to_csv('./output/p_pow_missing.csv', index=False)
print('Done - all files saved.')

## For each st, combine parts (a~d) if file of that part is available

for st in dct_st.keys():
    t0 = time()
    df = pd.DataFrame([])
    for part in ['a', 'b', 'c', 'd']:
        fp = './output/pow_by_state_part/p%s_%s_pow_part_%s.csv' % ('0'*(2-len(str(st)))+str(st), dct_st[st].lower(), part)
        if os.path.isfile(fp):
            df = df.append(pd.read_csv(fp))
    if len(df)>0:
        df.to_csv('./output/pow_by_state/person_files/p%s_%s_pow.csv'
                  % ('0'*(2-len(str(st)))+str(st), dct_st[st].lower())
                  ,index=False)
    t1 = time()
    print('File combining (a~d) finished for state %s. Time needed = %s seconds.' % (dct_st[st], round((t1-t0),0)))

## Add in household files - for each p[state code]_[state name]_pow.csv, merge with US household file and get all cols
# print(set(d.columns).intersection(set(d_us.columns))
# {'ADJINC', 'PUMA', 'RT', 'SERIALNO', 'ST'} - remove all cols except SERIALNO from household file for merging

# read in 4 parts of US household files at once - don't read in for each state person file
t0 = time()
dct_h = {}
for part in ['a', 'b', 'c', 'd']:
    dct_h[part] = pd.read_csv('./data/csv_hus/ss16hus%s.csv' % part)
print('Read-in US household file completed. Time needed = %s.' % (time()-t0))

for st in dct_st.keys():
    t0 = time()
    fp_p = './output/pow_by_state/person_files/p%s_%s_pow.csv' % ('0' * (2 - len(str(st))) + str(st), dct_st[st].lower())
    if os.path.isfile(fp_p):
        print('Current producing H-POW files for state = (%s, %s)...' % (st, dct_st[st]))
        d_pow = pd.read_csv(fp_p, usecols=['SERIALNO']) # d_pow = pd.read_csv(fp_p) if need all person-file cols
        df = pd.DataFrame([]) # stores person-household data
        for part in ['a', 'b', 'c', 'd']:
            # if merging full P and H files, drop 4 duplicated cols when merging
            # otherwise, merge 1 SERIALNO col of P and full H file
            #d_pow_h = pd.merge(d_pow, dct_h[part].drop(columns=['ADJINC', 'PUMA', 'RT', 'ST']), on='SERIALNO', how='left')
            d_pow_h = pd.merge(d_pow, dct_h[part], on='SERIALNO', how='left')

            # keep d_pow_h at household level (unique hhid = SERIALNO)
            # missing this step will cause issue in ACS data cleaning, duplicating pop weights in pre/post-sim ACS etc
            d_pow_h = d_pow_h.drop_duplicates(subset=['SERIALNO'])

            if len(df)>0:
                d_pow_h = d_pow_h[df.columns]
            df = df.append(d_pow_h)
        # df.to_csv('./output/pow_by_state/person_household_files/ph%s_%s_pow.csv' % ('0' * (2 - len(str(st))) + str(st), dct_st[st].lower()))
        # # reduce df to household data only
        # cols_p_only = list(set(d_pow.columns) - set(['SERIALNO']))
        # df.drop(columns=cols_p_only).to_csv('./output/pow_by_state/household_files/h%s_%s_pow.csv' % ('0' * (2 - len(str(st))) + str(st), dct_st[st].lower()))

        # H-POW file may contain rows of persons without H-cols
        # remove if person row not in H data, i.e. WGTP missing
        df = df[df['WGTP'].notna()]

        # save csv
        df.to_csv('./output/pow_by_state/household_files/h%s_%s_pow.csv' % ('0' * (2 - len(str(st))) + str(st), dct_st[st].lower()), index=False)

        t1 = time()
        print('H-file cols added to person POW file for state %s. H-POW file saved. Time needed = %s seconds.' % (dct_st[st], round((t1 - t0), 0)))
    else:
        pass

## Compare total population in a state, state of living VS state of work
# a dict to store results
dct_pop = {}
# get state level numbers, send to dct_pop
for st in dct_st.keys():
    #st = 4
    fp = './output/pow_by_state/person_files/p%s_%s_pow.csv' \
         % ('0' * (2 - len(str(st))) + str(st), dct_st[st].lower())
    if os.path.isfile(fp):
        d = pd.read_csv(fp)
        dct_pop[st] = {}
        dct_pop[st]['worker_pop_residents'] = d[d['ST']==st]['PWGTP'].sum()
        dct_pop[st]['worker_pop_workers'] = d[d['POWSP']==st]['PWGTP'].sum()
    print('Population of residents/workers sent to dct_pop for state %s (%s)' % (dct_st[st], st))
# convert dct_pop to df
d_pop = pd.DataFrame.from_dict(dct_pop, orient='index')
d_pop = d_pop.reset_index().rename(columns={'index':'st'})
# drop the row for PR - ACS PUMS only has continent residents working in PR, but no PR residents
d_pop = d_pop[d_pop['st']!=72]
# save state pop data d_pop
d_pop.to_csv('./output/state_worker_pop_resident_pow.csv', index=False)
# scale worker_pop_workers to reflect correct population being represented
# PR: dct_pop[72] = {'worker_pop_residents': 0, 'worker_pop_workers': 1980}
# so in ACS PUMS US person files, state of living (ST) is within 50 states + DC.
# total population with pow in 50+1 is total ACS worker population minus worker population with pow = overseas
# because of missing POWSP, above total is larger than sum of population with pow in state across states
# need a scale-up multiplier to account for missing POWSP, so that above inequality becomes equality
# note: multiplier can only be at country level, since numerator = true total worker pop with POW = geo-area
# the geo-area cannot be state because we wouldn't know the true total worker pop with POW = st
# instead if geo-area=country then we can use total US pop - worker pop overseas

d_overseas = pd.read_csv('./output/p_pow_overseas.csv')
pop_pow_usa = d_pop['worker_pop_residents'].sum() - d_overseas['PWGTP'].sum()
multiplier = pop_pow_usa/d_pop['worker_pop_workers'].sum()
d_pop['worker_pop_workers_normalized'] = d_pop['worker_pop_workers'] * multiplier
# check
A, B, C = d_pop['worker_pop_residents'].sum(), d_overseas['PWGTP'].sum(), d_pop['worker_pop_workers_normalized'].sum()
print('A. Total worker population living in 50+1 = %s' % A)
print('B. Total worker population living in 50+1, pow is overseas = %s' % B)
print('C. Total worker population living in 50+1, pow is 50+1 = %s' % C)
print('A should be sum of B+C. Check: (A - B - C) = %s' % (A-B-C))

## Plot
import matplotlib
import matplotlib.pyplot as plt
import numpy as np

labels = [dct_st[x] for x in d_pop['st']]
pops_residents = d_pop['worker_pop_residents']
pops_workers = d_pop['worker_pop_workers_normalized']

x = np.arange(len(labels))  # the label locations
width = 0.35  # the width of the bars

fig, ax = plt.subplots()
rects1 = ax.bar(x - width/2, pops_residents, width, label='Residents in state', color='lightsteelblue')
rects2 = ax.bar(x + width/2, pops_workers, width, label='Workers in state', color='salmon')

# Add some text for labels, title and custom x-axis tick labels, etc.
ax.set_ylabel('Population')
ax.set_title('Population Comparison, Residents vs Workers in State')
ax.set_xticks(x)
ax.set_xticklabels(labels)
leg = ax.legend(prop=dict(size=18))


# Converting back to figure normalized coordinates to create new axis:
leg_pxls = leg.get_window_extent()
ax_pxls = ax.get_window_extent()
fig_pxls = fig.get_window_extent()

pad = 0.025
# ax2 = fig.add_axes([leg_pxls.x0/fig_pxls.width,
#                     ax_pxls.y0/fig_pxls.height,
#                     leg_pxls.width/fig_pxls.width,
#                     (ax_pxls.y0-leg_pxls.y0)/fig_pxls.height-pad])

ax2 = fig.add_axes([0.8,
                    0.75,
                    0.15,
                    0.07])

# eliminating all the tick marks:
ax2.tick_params(axis='both', left='off', top='off', right='off',
                bottom='off', labelleft='off', labeltop='off',
                labelright='off', labelbottom='off')

# adding some text:
note = "Note: %s workers living in US but \nworking overseas are not represented \nby Workers in state" % int(B)

ax2.text(0.025, 0.45, note, ha='left', wrap=True)
ax2.set_axis_off()

fig.tight_layout()
fig.set_size_inches(18.5, 10.5)
plt.show()
plt.savefig('./output/pop_comparison.png', bbox_inches='tight')

#####################################
