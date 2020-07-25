cat("\014")
options(error=recover)
#options(error=NULL)
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)

# import libraries
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
  return("OK")
}

global.libraries <- c('bnclassify', 'randomForest','magick','stats', 'rlist', 'MASS', 'plyr', 'dplyr', 
                      'survey', 'class', 'dummies', 'varhandle', 'oglmx', 
                      'foreign', 'ggplot2', 'reshape2','e1071','pander','ridge')
results <- sapply(as.list(global.libraries), pkgTest)

# Summarize basic statistics from FMLA after cleaning
source("1_cleaning_functions.R")
source("2_pre_impute_functions.R")
source("3_impute_functions.R")

# load FMLA data
d_fmla <- read.csv(paste0("./csv_inputs/", 'fmla_2012_employee_restrict_puf.csv'))
d_fmla <- clean_fmla(d_fmla, save_csv=FALSE)

# create binary_vars table
vars <- c('take_own',
          'take_matdis',
          'take_bond',
          'take_illchild',
          'take_illspouse',
          'take_illparent',
          'need_own',
          'need_matdis',
          'need_bond',
          'need_illchild',
          'need_illspouse',
          'need_illparent',
          'resp_len',
          'taker',
          'needer',
          'doctor',
          'hospital',
          'anypay',
          'prop_pay'
)
table_df <- data.frame(matrix(NA, length(vars), 3))
colnames(table_df) <- c('Variable', 'n_nonmiss', 'n_ones')
count=1
for (i in vars) {
  if (i != 'prop_pay') {
    var_table <- table(d_fmla[,i], useNA='always')
    table_df[count,'Variable'] <- i 
    table_df[count,'n_nonmiss'] <- sum(var_table) - var_table[[3]]
    table_df[count,'n_ones'] <- var_table[[2]]
    count = count + 1
  }
  else {
    var_table <- table(d_fmla[,i], useNA='always')
    table_df[count,'Variable'] <- i 
    table_df[count,'n_nonmiss'] <- sum(var_table) - var_table[[8]] 
    table_df[count,'n_ones'] <- var_table[[7]]
    count = count + 1
  }
}

write.csv(table_df, file = paste0('./output/binary_vars_R.csv'), row.names=FALSE)

# create leave_lengths table
leave_types <<- c("own","matdis","bond","illchild","illspouse","illparent")
table_df <- data.frame(matrix(NA, 6, 4))
colnames(table_df) <- c('Leave Type', 'n_nonmiss', 'mean','stdev')
count=1
for (i in leave_types) {
  len_var <- paste0('length_',i)
  var_table <- table(d_fmla[,len_var], useNA='always')
  nonmiss <- sum(var_table) - var_table[[3]]
  mean <- mean(d_fmla[,len_var], na.rm = TRUE)
  stdev <- sd(d_fmla[,len_var], na.rm = TRUE)
  table_df[count,'Leave Type'] <- i 
  table_df[count,'n_nonmiss'] <- sum(var_table) - var_table[[3]]
  table_df[count,'mean'] <- mean
  table_df[count,'stdev'] <- stdev
  count=count+1
}

write.csv(table_df, file = paste0('./output/leave_lengths_R.csv'), row.names=FALSE)

# measure leave taking/needing after intra imputation
d_fmla <- impute_intra_fmla(d_fmla, intra_impute=TRUE)
table_df <- data.frame(matrix(NA, 6, 3))
colnames(table_df) <- c('Leave Type', '# of Takers', '# of Needers')
count=1
for (i in leave_types) {
  take_var <- paste0('take_',i)
  need_var <- paste0('need_',i)
  take_table <- table(d_fmla[,take_var])
  need_table <- table(d_fmla[,need_var])
  table_df[count,'Leave Type'] <- i
  table_df[count,'# of Takers'] <- take_table[[2]]
  table_df[count,'# of Needers'] <- need_table[[2]]
  count= count + 1
}

write.csv(table_df, file = paste0('./output/leave_types_R.csv'), row.names=FALSE)