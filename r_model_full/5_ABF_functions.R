
# """
# 5_ABF_functions.R
#
# These functions run ABF module calculations on the imputed ACS data set00.
#
# Jan 30, 2020
# Luke
# 
#
# """

# ============================ #
# 1.replicate_weights_SE
# ============================ #

# function to generate SE for an ACS variable using replicate weights
# following method specified by this document from Census:
# https://www2.census.gov/programs-surveys/acs/tech_docs/pums/accuracy/2012_2016AccuracyPUMS.pdf
replicate_weights_SE <- function(d, var, place_of_work, filt=TRUE) {
  # filter d by specified filter
  d <- d[filt,]
  # if place of work, multiply weight by 2% to adjust for missing values
  if (place_of_work){
    d$PWGTP <- d$PWGTP*1.02
    replicate_weights <- paste0('PWGTP',seq(1,80))
    for (i in replicate_weights) {
      d[,i] <- d[,i] * 1.02 
    }  
  }
  # base estimate of population mean, total
  x= weighted.mean(d[,var], d[,'PWGTP'], na.rm=TRUE)
  tot=sum(d[,var]* d[,'PWGTP'], na.rm=TRUE)
  
  # Estimates from replicate weights
  replicate_weights <- paste0('PWGTP',seq(1,80))
  count=0
  for (i in replicate_weights) {
    count=count+1
    assign(paste("x", count, sep = ""), weighted.mean(d[,var], d[,i], na.rm=TRUE))   
    assign(paste("tot", count, sep = ""), sum(d[,var]* d[,i], na.rm=TRUE))
  }
  replicate_means <- paste0('x',seq(1,80))
  
  # calculate standard error, confidence interval
  SE= sqrt(4/80*sum(sapply(mget(paste0('x',seq(1,80))), function(y) {(y-x)^2})))
  CI_low=x-1.96*SE
  CI_high=x+1.96*SE
  CI= paste("[",format(x-1.96*SE, digits=2, scientific=FALSE, big.mark=","),",", format(x+1.96*SE, digits=2, scientific=FALSE, big.mark=","),"]")
  total=sum(d[,var]*d[,'PWGTP'], na.rm=TRUE)
  total_SE= sqrt(4/80*sum(sapply(mget(paste0('tot',seq(1,80))), function(y) {(y-tot)^2})))
  total_CI_low= total-total_SE*1.96
  total_CI_high= total+total_SE*1.96
  total_CI=paste("[",format(total_CI_low, digits=2, scientific=FALSE, big.mark=","),",", format(total_CI_high, digits=2, scientific=FALSE, big.mark=","),"]")
  # return statistics
  stats= list(var, estimate=x, std_error=SE,confidence_int=CI,CI_low=CI_low,CI_high=CI_high, 
              total=total, total_SE=total_SE,total_CI_low=total_CI_low,total_CI_high=total_CI_high, total_CI=total_CI)
  for (i in stats[c(2:3,7:8,11)]) {
    i <- format(i, nsmall=3)
  }
  return(stats)
}


# ============================ #
# 2. run_ABF
# ============================ #
# master ABF execution function

run_ABF <- function(d, ABF_elig_size, ABF_max_tax_earn, ABF_bene_tax, ABF_avg_state_tax, 
                    ABF_payroll_tax, ABF_bene,output,place_of_work,ABF_detail_out) {

  if (ABF_max_tax_earn>0) {
    d <- d %>% mutate(taxable_income_capped=ifelse(wage12>ABF_max_tax_earn,
                                                                   ABF_max_tax_earn,wage12))   
  } else {
    d <- d %>% mutate(taxable_income_capped=wage12)
  }
  

  # Step 1 - Calculate Point Estimates
  # Income
  # Intermediate output: unweighted income base (full geographic area)
  total_income = sum(d$taxable_income_capped, na.rm=TRUE)
  
  # Total Weighted Income Base (full geographic area)
  d['income_wgted'] <- d$taxable_income_capped * d$PWGTP
  total_wgted_income = sum(d$income_wgted, na.rm=TRUE)
  
  # Tax revenue
  # Unweighted tax revenue collected (full geographic area)
  d$ptax_rev_final = d$taxable_income_capped * ABF_payroll_tax
  
  # Total Weighted Tax Revenue (full geographic area)
  d$ptax_rev_wgted = d$ptax_rev_final * d$PWGTP
  total_ptax_rev_w= total_wgted_income * ABF_payroll_tax

  # if ABF_bene not suppplied, then calculate from the simulated benefits  
  if (is.null(ABF_bene)) {
    ABF_bene=sum(d$actual_benefits*d$PWGTP,na.rm=TRUE)
  }
  
  # State Tax Revenue Recouped from Taxed Benefits
  if (ABF_bene_tax==TRUE) {
    recoup_tax_rev = ABF_avg_state_tax * ABF_bene
  } else {
    recoup_tax_rev = 0
  }
  
  # Get standard errors from replicate weights and output results CSV
  vars=c('ptax_rev_final','taxable_income_capped')
  mean=c()
  SE=c()
  CI=c()
  total=c()
  total_SE=c()
  total_CI=c()
  for (i in vars) {
    temp=replicate_weights_SE(d, i, place_of_work)
    mean=c(mean, temp[2])
    SE=c(SE, temp[3])
    CI=c(CI, temp[4])
    total=c(total, temp[7])
    total_SE=c(total_SE, temp[8])
    total_CI=c(total_CI, temp[11])
  }
  
  mean=unname(unlist(mean))
  SE=unname(unlist(SE))
  CI=unname(unlist(CI))
  total=unname(unlist(total))
  total_SE=unname(unlist(total_SE))
  total_CI=unname(unlist(total_CI))
  
  var_names=c('Total Tax Revenue', 'Total Taxable Income')
  d_out=data.frame(var_names,mean,SE,CI,total, total_SE, total_CI)
  # add recouped state benefits to df
  recoup_row=data.frame(var_names="State Tax Revenue Recouped from Taxed Benefits", mean=NA, SE=NA, CI=NA, total=recoup_tax_rev, total_SE=NA, total_CI=NA)
  d_out <- rbind(d_out, recoup_row)
  colnames(d_out) <- c("Variable","Mean", "Standard Error of Mean", "Confidence Interval","Population Total", "Pop Total Standard Error", "Pop Total CI")
  
  if (ABF_detail_out){
    write.csv(d_out,file=paste0('./output/',output,"_ABF_stats.csv"), row.names= FALSE)
  }
  
  # output meta summary file
  # TODO: remove placeholders
  meta_summ <- c(
    'Tax Revenue Recouped from Benefits'= recoup_tax_rev,
    'Income Standard Error' = 1,
    'Total Income Upper Confidence Interval' = 1,
    'Total Tax Revenue (Weighted)' = 1,
    'Tax Revenue Standard Error' = 1,
    'Total Income Lower Confidence Interval' = 1,
    'Total Tax Revenue Upper Confidence Interval' = 1,
    'Total Income (Weighted)' = 1,
    'Total Tax Revenue Lower Confidence Interval' = 1,
    'Total Income' = 1
  )
  write.csv(data.frame(meta_summ), file='output/abf_summary.csv')
  
  # placeholder vars for meta file  
  # TODO: implement proper coding of these 
   d_copy <- d
   for (i in c('class', 'age_cat', 'GENDER_CAT', 'taxable_income_capped', 'income_w', 'wage_cat', 'ptax_rev_final', 'ptax_rev_w')) {
     if (i %in% names(d_copy) == FALSE) {
       d_copy[i] <- 0 
     }
   }
   # output meta file 
   write.csv(d_copy, file=paste0('./output/abf_acs_sim_r_model.csv'))
   
   
  
  return(d)
}