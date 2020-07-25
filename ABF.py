import pandas as pd
import numpy as np
import os


class ABF:
    def __init__(self, acs_file, benefits, eligible_size, max_taxable_earnings_per_person, benefits_tax,
                 average_state_tax, payroll_tax, output_dir=None):
        self.reps = ['PWGTP' + str(i) for i in range(1, 81)]
        self.keepcols = ['COW', 'POWSP', 'ST', 'PWGTP', 'age', 'female', 'wage12'] + self.reps
        self.acs_file = acs_file
        self.eligible_size = eligible_size
        self.max_taxable_earnings_per_person = max_taxable_earnings_per_person
        self.benefits_tax = benefits_tax
        self.average_state_tax = average_state_tax / 100
        self.payroll_tax = payroll_tax / 100
        self.benefits = benefits

        self.output_dir = output_dir
        self.abf_output = None
        self.pivot_tables = None

    def update_parameters(self, **kwargs):
        for key, value in kwargs.items():
            setattr(self, key, value)

        self.average_state_tax = self.average_state_tax / 100
        self.payroll_tax = self.payroll_tax / 100

    # FUNCTION #2: Conduct Final Calculations on the slimmer ABF Output dataset
    def abf_calcs(self, chunksize=9999999999):
        fp = self.get_abf_fp()
        self.reset_abf_output()
        self.pivot_tables = None

        append = False
        for df in pd.read_csv(fp, chunksize=chunksize):
            # Apply Taxable Wage Max
            if self.max_taxable_earnings_per_person is not None:  # TRUE (Constraint is applied)
                df['taxable_income_capped'] = np.where((df['wage12'] > self.max_taxable_earnings_per_person),
                                                       self.max_taxable_earnings_per_person, df['wage12'])
                index_names = df[df['wage12'] > self.max_taxable_earnings_per_person].index
                censor = len(index_names)
                message_censor = "We censored %s observations to the wage max" % censor
                print(message_censor)
            else:
                df['taxable_income_capped'] = df['wage12']

            # Step 1 - Calculate Point Estimates
            # Income
            # Intermediate output: unweighted income base (full geographic area)
            total_income = df['taxable_income_capped'].sum()

            # Total Weighted Income Base (full geographic area)
            df['income_w'] = df['taxable_income_capped'] * df['PWGTP']
            wage_bins = list(range(0, 210000, 25000))
            wage_ranges = ['[{}k - {}k)'.format(wage_bins[i] // 1000, wage_bins[i] // 1000 + 25)
                           for i in range(len(wage_bins) - 1)]
            df['wage_cat'] = pd.cut(x=df['wage12'], bins=wage_bins, labels=wage_ranges,
                                         right=False)
            total_income_w = df['income_w'].sum()
            print('Output: Weighted Income Base for Full Geographic Area:')
            print(total_income_w)

            # Tax Revenue
            # Unweighted tax revenue collected (full geographic area)
            df['ptax_rev_final'] = df['taxable_income_capped'] * self.payroll_tax

            # Total Weighted Tax Revenue (full geographic area)
            df['ptax_rev_w'] = df['income_w'] * self.payroll_tax
            total_ptax_rev_w = self.payroll_tax * total_income_w
            print('Output: Weighted Tax Revenue for Full Geographic Area:')
            print(total_ptax_rev_w)

            message = "The weighted estimated tax revenue is %s based on a payroll tax rate of %s and a income " \
                      "base of %s " % (total_ptax_rev_w, self.payroll_tax, total_income_w)
            print(message)

            # State Tax Revenue Recouped from Taxed Benefits
            if self.benefits_tax:
                recoup_tax_rev = self.average_state_tax * self.benefits
                print('Output: "State Tax Revenue Recouped from Taxed Benefits:')
                print(recoup_tax_rev)
                message = "With a state tax rate of %s and a benefit outlay of %s, the estimated state tax revenue is %s" \
                          % (self.average_state_tax, self.benefits, recoup_tax_rev)
                print(message)
            else:
                recoup_tax_rev = 0

            # Step 2 - Calculate Standard Errors with 80 Replicate Weights
            # replication weight cols
            # Income
            income_r = []
            for wt in self.reps:
                income_r.append(((df['taxable_income_capped'] * df[wt]).sum()))

            # print('80 Replicate Income:')
            # print(income_r)

            income_delta = []
            for i in income_r:
                income_delta.append((i - total_income_w) ** 2)

            income_se = ((sum(income_delta)) * (4 / 80)) ** .5

            # Tax Revenue
            tax_r = []
            for wt in self.reps:
                tax_r.append(((df['ptax_rev_final'] * df[wt]).sum()))

            # print('80 Replicate Tax Revenue:')
            # print(tax_r)

            tax_delta = []
            for i in tax_r:
                tax_delta.append((i - total_ptax_rev_w) ** 2)

            tax_se = ((sum(tax_delta)) * (4 / 80)) ** .5

            ###########Step 3: Calculate Confidence Intervals

            # Income
            total_income_w_uci = total_income_w + 1.96 * income_se
            print('Total Income Upper CI:')
            print(total_income_w_uci)
            total_income_w_lci = total_income_w - 1.96 * income_se
            print('Total Income Low CI:')
            print(total_income_w_lci)

            #Tax Revenue
            total_ptax_w_uci = total_ptax_rev_w + 1.96 * tax_se
            print('Total Income Upper CI:')
            print(total_ptax_w_uci)
            total_ptax_w_lci = total_ptax_rev_w - 1.96 * tax_se
            print('Total Income Low CI:')
            print(total_ptax_w_lci)

            # Return Dictionary with Final Output Values
            abf_output = {'Total Income (Weighted)': total_income_w, 'Total Income': total_income,
                          'Income Standard Error': income_se, 'Total Income Upper Confidence Interval': total_income_w_uci,
                          'Total Income Lower Confidence Interval': total_income_w_lci,
                          'Total Tax Revenue (Weighted)': total_ptax_rev_w,
                          'Tax Revenue Standard Error': tax_se,
                          'Total Tax Revenue Upper Confidence Interval': total_ptax_w_uci,
                          'Total Tax Revenue Lower Confidence Interval': total_ptax_w_lci,
                          'Tax Revenue Recouped from Benefits': recoup_tax_rev}

            pd.set_option('display.float_format', lambda x: '%.2f' % x)
            revenue_by_class = pd.pivot_table(df, index=["class_cat"], values=["income_w", "ptax_rev_w"], aggfunc=[np.sum])
            revenue_by_age = pd.pivot_table(df, index=["age_cat"], values=["income_w", "ptax_rev_w"], aggfunc=[np.sum])
            revenue_by_gender = pd.pivot_table(df, index=["gender_cat"], values=["income_w", "ptax_rev_w"],
                                               aggfunc=[np.sum])
            revenue_by_wage = pd.pivot_table(df, index=["wage_cat"], values=["income_w", "ptax_rev_w"],
                                             aggfunc=[np.sum])

            pivot_tables = {'Class of Worker': revenue_by_class, 'Age': revenue_by_age,
                            'Gender': revenue_by_gender, 'Wage': revenue_by_wage}

            if self.pivot_tables is None:
                self.pivot_tables = pivot_tables
            else:
                for category, pivot_table in self.pivot_tables.items():
                    self.pivot_tables[category] += pivot_table

            if not append:
                df.to_csv(os.path.join(self.output_dir, 'abf_temp.csv'), index=False)
            else:
                df.to_csv(os.path.join(self.output_dir, 'abf_temp.csv'), index=False, mode='a', header=False)

            self.abf_output += pd.DataFrame({'Value': list(abf_output.values())}, index=list(abf_output.keys()))

        os.remove(fp)
        os.rename(os.path.join(self.output_dir, 'abf_temp.csv'), fp)

    def clean_data(self, chunksize=9999999999):
        # TODO: fix chunkking code - current abf acs sim master is saved as last chunk, not full acs
        append = False
        for df in pd.read_csv(self.acs_file, usecols=self.keepcols, chunksize=chunksize):
            df['class_cat'] = ''
            cleanup = {1: "Private", 2: "Private", 3: "Local Govt.", 4: "State Govt.", 5: "Federal Govt.",
                       6: "Self-Employed", 7: "Self-Employed", 8: "Other", 9: "Other"}
            for i in df.index.values:
                if not pd.isnull(df.at[i, 'COW']):
                    df.at[i, 'class_cat'] = cleanup[int(float(df.at[i, 'COW']))]

            # Create Age Categories for display purposes (need to find out variable specification on the microsim side)
            age_ranges = ["[{0} - {1})".format(AGEP, AGEP + 10) for AGEP in range(0, 100, 10)]
            df['age_cat'] = pd.cut(x=df['age'], bins=list(range(0, 110, 10)), labels=age_ranges, right=False)

            # Create Gender Categories for display pruposes
            df['gender_cat'] = np.where(df['female'] == 1, 'female', 'male')

            # Code missing responses as missing
            df['gender_cat'] = np.where(np.isnan(df['female']), np.nan, df['gender_cat'])

            self.save_results(df, append=append)
            if not append:
                append = True

    def run(self, rerun=False, variables=None, chunksize=9999999999):
        if rerun and variables is not None:
            self.update_parameters(**variables)

        # Create Class variable to aggregate the COW variable for display purposes
        if not rerun:
            self.clean_data(chunksize=chunksize)
        self.abf_calcs(chunksize=chunksize)
        self.save_summary()
        out = {self.abf_output.index.values[i]: self.abf_output['Value'].values[i]
               for i in range(self.abf_output.shape[0])}
        print(out)
        return out, self.pivot_tables

    def reset_abf_output(self):
        output_categories = {'Total Income (Weighted)',
                             'Total Income',
                             'Income Standard Error',
                             'Total Income Upper Confidence Interval',
                             'Total Income Lower Confidence Interval',
                             'Total Tax Revenue (Weighted)',
                             'Tax Revenue Standard Error',
                             'Total Tax Revenue Upper Confidence Interval',
                             'Total Tax Revenue Lower Confidence Interval',
                             'Tax Revenue Recouped from Benefits'}
        self.abf_output = pd.DataFrame({'Value': 0}, index=output_categories)
        self.pivot_tables = None

    def save_results(self, df, output_dir=None, append=False):
        fp = self.get_abf_fp(output_dir=output_dir)

        if not append:
            df.to_csv(fp, index=False)
        else:
            df.to_csv(fp, index=False, mode='a', header=False)

    def save_summary(self, output_dir=None):
        if output_dir is None:
            output_dir = self.output_dir
        if self.output_dir is None:
            raise FileNotFoundError

        self.abf_output.to_csv(os.path.join(output_dir, 'abf_summary.csv'))

    def get_abf_fp(self, output_dir=None):
        if output_dir is None:
            output_dir = self.output_dir
        if self.output_dir is None:
            raise FileNotFoundError

        return os.path.join(output_dir, 'abf_' + os.path.split(self.acs_file)[-1])
