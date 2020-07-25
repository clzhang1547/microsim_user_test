# analysis for issue brief 

#rm(list=ls())
cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)
#options(error=NULL)
library('stringr')
library('ggplot2')
source("6_output_analysis_functions.R")

# load simulated csvs for each state
ri <- read.csv('output/RI_logitissue_brief_1.csv')
ca <- read.csv('output/CA_logitissue_brief_1.csv')
nj <- read.csv('output/NJ_logitissue_brief_1.csv')

# start a data frame to store all the summary statistics we need
states <- c('CA','NJ','RI')
leave_types <- c("own","illspouse","illchild","illparent","matdis","bond")
vars <- append(c(),values=c(paste0('ptake_', leave_types),c('ptake_DI','ptake_PFL'), paste0('plen_', leave_types), 
                            c('DI_plen','PFL_plen','bene_DI','bene_PFL','actual_benefits','eligworker')))
results <- data.frame(row.names = vars)
#for (i in vars)
results['CA'] <- NA
results['NJ'] <- NA
results['RI'] <- NA
results['CA_SE'] <- NA
results['NJ_SE'] <- NA
results['RI_SE'] <- NA

# calculate values for each var and state

# CA 
d <- ca
for (v in vars) {
  stats <- replicate_weights_SE(d, var=v)
  if (str_detect(v,'plen')==FALSE) {
    stats <- replicate_weights_SE(d, var=v, filt=d[v]>0)
    results[v,'CA'] <- stats['total']
    results[v,'CA_SE'] <- stats['total_SE']  
  } else {
    stats <- replicate_weights_SE(d, var=v, filt=d[v]>0)
    results[v,'CA'] <- stats[['estimate']]/5
    results[v,'CA_SE'] <- stats[['std_error']]/5
  }
}

# NJ
d <- nj
for (v in vars) {
  stats <- replicate_weights_SE(d, var=v)
  if (str_detect(v,'plen')==FALSE) {
    stats <- replicate_weights_SE(d, var=v, filt=d[v]>0)
    results[v,'NJ'] <- stats['total']
    results[v,'NJ_SE'] <- stats['total_SE']  
  } else {
    stats <- replicate_weights_SE(d, var=v, filt=d[v]>0)
    results[v,'NJ'] <- stats[['estimate']]/5
    results[v,'NJ_SE'] <- stats[['std_error']]/5
  }
}

# RI 
d <- ri
for (v in vars) {
  if (str_detect(v,'plen')==FALSE) {
    stats <- replicate_weights_SE(d, var=v, filt=d[v]>0)
    results[v,'RI'] <- stats['total']
    results[v,'RI_SE'] <- stats['total_SE']  
  } else {
    stats <- replicate_weights_SE(d, var=v, filt=d[v]>0)
    results[v,'RI'] <- stats[['estimate']]/5
    results[v,'RI_SE'] <- stats[['std_error']]/5
  }
}
results['source'] <- 'IMPAQ'
write.csv(results,file='output/issue_brief_1 nums 5_21.csv')


