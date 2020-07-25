## Convert RDS to CSV

setwd("C:\\workfiles\\Microsimulation\\microsim\\")
## Read in data - clean FMLA
d <- readRDS("PR_comparison/check_clean_fmla/d_fmla.rds", refhook = NULL)
## Save CSV
write.csv(d, "./PR_comparison/check_clean_fmla/fmla_clean_R.csv")

## Read in data - ACS
d <- readRDS("PR_comparison/check_acs/RI_work.rds", refhook = NULL)
## Save CSV
write.csv(d, "./PR_comparison/check_acs/RI_work.csv")