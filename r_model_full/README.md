# Source code for R version of Paid Leave Model 

This repository contains the code for the Microsim Paid Leave project

## Code
Purpose of each file of R code:

"0_NEW_master_execution_function.R" : Master policy simulation function calling all other functions based on user parameters

"1_NEW_cleaning_functions.R" : standardize/clean FMLA, ACS, and CPS survey variables

"2_NEW_impute_functions.R"  : Several imputation functions and subfunctions to clean and impute leave taking behavior and length within the FMLA survey itself, and apply a basic counterfactual leave taking effect in the presence of a leave program. FMLA leave taking behavior is then imputed into the ACS. To execute FMLA -> ACS imputation, the default method is a nearest neighbor function. TODO: build alternative imputation methods to swap nearest neighbor with.

"3_NEW_post_impute_functions.R" : define policy simulation functions

"4_output_analysis_functions.R": functions for post-simulation analysis and output

"TEST_execution.R" : sample execution of Master policy simulation function

"TEST_execution_states.R" : execution of Master policy simulation function on full state ACS data sets for NJ, CA, and RI. 
 Note that raw ACS files for this are not on github yet, still haven't figured out how to get Git Large File Service to work yet in order to get these up here.

## csv Inputs
ss16hri_short.csv: sample ACS household-level file

ss16pri_short.csv: sample ACS person-level file

fmla_2012_employee_restrict_puf.csv: FMLA 2012 raw file

CPS2014extract.csv: March CPS 2014 extract file
  TODO: Add cleaning code to allow this file to be replaced with other CPS files more easily

## Other files
KNN1_testing.R: code verifying KNN1_scratch function matches results of canned 'neighbr' package

## Documentation
Actual Leave Data 2012-2016.xlsx: excel spreadsheet of actual leave data
Building alt imputation methods.docx: doc describing how to code up alternative imputation methods into the model
Parameter dictionary.xlsx: Dictionary of paramters expected by policy_simulation() master function.
Parameters for states.xlsx: Parameters specifications for actual leave states (outdated currently)
Preliminary Simulation results clean.docx: Preliminary simulation results (outdated currently)

## R dataframes
d_acs.rds, d_cps.rds, d_fmla.rds: cleaned ACS, CPS, FMLA dataframes to quicken runtimes for programmer convenience while testing

d_fmla_impute_input.rds, d_acs_impute_input.rds, d_acs_impute_output.rds: Dataframes saved just before and after FMLA -> ACS imputation to use as expected input and output data sets for reference when building alternative imputation functions.

## Output
sample_output.csv: sample CSV of model output; an ACS data set with leave taking variables added
