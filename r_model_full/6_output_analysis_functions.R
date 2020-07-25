
# """
# File: output_analysis_functions
#
# Functions to analyze simulation output 
# 
# Luke
# 
# """
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table of Contents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1. replicate_weights_SE
# see 5_ABF_functions.R





# ============================ #
# 2. standard_summary_stats
# ============================ #
# function to produce some standard summary statistics of relevant leave taking and other vars in csv format
standard_summary_stats <-function(d, output, out_dir,place_of_work) {
  # if place of work, multiply weight by 2% to adjust for missing values
  if (place_of_work){
    d$PWGTP <- d$PWGTP*1.02
    replicate_weights <- paste0('PWGTP',seq(1,80))
    for (i in replicate_weights) {
      d[,i] <- d[,i] * 1.02 
    }  
  }
  ptake_vars=c()
  ptake_names=c()
  for (i in leave_types) {
    ptake_vars=c(ptake_vars,paste("ptake_",i,sep=""))
    ptake_names=c(ptake_names, paste("Took & got benefits for",i,'leave'))
  }
  
  
  # define columns of csv
  vars=c('eligworker', 'particip', 'particip_length',ptake_vars, 'actual_benefits')
  mean=c()
  SE=c()
  CI=c()
  total=c()
  total_SE=c()
  total_CI=c()
  for (i in vars) {
    temp=replicate_weights_SE(d, i,place_of_work)
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
  
  var_names=c('Eligible for leave program', 'Participated in leave program', 'Length of Participation in Days', ptake_names,'Amount of Benefits Received ($)')
  d_out=data.frame(var_names,mean,SE,CI,total, total_SE, total_CI)
  colnames(d_out) <- c("Variable","Mean", "Standard Error of Mean", "Confidence Interval","Population Total", "Pop Total Standard Error", "Pop Total CI")
  write.csv(d_out,file=file.path(out_dir, paste0(output,'_stats.csv'), fsep = .Platform$file.sep), row.names= FALSE)
}

# ============================ #
# 2. state_compar_stats
# ============================ #

# function to produce summary statistics to compare with actual state of relevant leave taking and other vars in csv format
state_compar_stats <-function(d, output, out_dir,place_of_work) {
  # if place of work, multiply weight by 2% to adjust for missing values
  if (place_of_work){
    d$PWGTP <- d$PWGTP*1.02
    replicate_weights <- paste0('PWGTP',seq(1,80))
    for (i in replicate_weights) {
      d[,i] <- d[,i] * 1.02 
    }  
  }
  ptake_vars=c()
  ptake_names=c()
  plen_vars=c()
  plen_names=c()
  for (i in leave_types) {
    ptake_vars=c(ptake_vars,paste("ptake_",i,sep=""))
    ptake_names=c(ptake_names, paste("Participated for",i,'leave'))
    plen_vars=c(plen_vars,paste("plen_",i,sep=""))
    plen_names=c(plen_names, paste("Num of Days Participated for",i,'leave'))
  }
  
  # define columns of csv
  vars=c('eligworker',ptake_vars, 'particip',plen_vars, 'actual_benefits')
  mean=c()
  SE=c()
  CI=c()
  total=c()
  total_SE=c()
  total_CI=c()
  for (i in vars) {
    # for benefit and len vars, filter to just non zero vals 
    if (grepl('plen', i) | grepl('bene', i)){
      temp= replicate_weights_SE(d, i,place_of_work,d[i]>0)  
    } else {
      temp=replicate_weights_SE(d, i,place_of_work)  
    }
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
  
  var_names=c('Workers eligible for leave program', ptake_names, 'Participated for any reason', plen_names, 'Benefits Received ($), total')
  d_out=data.frame(var_names,mean,SE,CI,total, total_SE, total_CI)
  # manipulate the length ofparticipation vars
  # divide by 5 to match format of state actual data output
  for (j in c('mean','SE')) {
    for (i in leave_types ) {
      d_out[d_out$var_names==paste('Num of Days Participated for', i, 'leave'),j] <- d_out[d_out$var_names==paste('Num of Days Participated for', i, 'leave'),j]/5
    }
  }
  # transform pop nums to weeks as well
  for (j in c('total','total_SE')) {
    for (i in leave_types ) {
      d_out[d_out$var_names==paste('Num of Days Participated for', i, 'leave'),j] <- d_out[d_out$var_names==paste('Num of Days Participated for', i, 'leave'),j]/5
    }
  }
  
  # regenerate CI's with new SE's, means
  d_out$CI= paste("[",format(d_out$mean-1.96*d_out$SE, digits=2, scientific=FALSE, big.mark=","),",", 
                  format(d_out$mean+1.96*d_out$SE, digits=2, scientific=FALSE, big.mark=","),"]")
  d_out$total_CI= paste("[",format(d_out$total-1.96*d_out$total_SE, digits=2, scientific=FALSE, big.mark=","),",", 
                  format(d_out$total+1.96*d_out$total_SE, digits=2, scientific=FALSE, big.mark=","),"]")
  colnames(d_out) <- c("Variable","Mean", "Standard Error of Mean", "Confidence Interval","Population Total", "Pop Total Standard Error", "Pop Total CI")
  write.csv(d_out,file=file.path(out_dir, paste0(output,'_rawstats.csv'), fsep = .Platform$file.sep), row.names= FALSE)
  
  
  # create rounded results with cleaned up names
  round_mean=format(mean, digits=2, scientific=FALSE, big.mark=",")
  round_SE=format(SE,  digits=2, scientific=FALSE, big.mark=",")
  round_CI=CI
  round_total=format(total, digits=2, scientific=FALSE, big.mark=",")
  round_total_SE=format(total_SE,  digits=2, scientific=FALSE, big.mark=",")
  round_total_CI=total_CI
  
  d_out=data.frame(var_names,round_mean,round_SE,round_CI,round_total, round_total_SE, round_total_CI)
  colnames(d_out) <- c("Variable","Mean", "Standard Error of Mean", "Confidence Interval","Population Total", "Pop Total Standard Error", "Pop Total CI")
  write.csv(d_out,file=file.path(out_dir, paste0(output,'_roundstats.csv'), fsep = .Platform$file.sep), row.names= FALSE)
  
  if (makelog==TRUE) {
    
    
    # add results to log file
    cat("------------------------------", file = log_name, sep="\n", append = TRUE)
    cat("Results", file = log_name, sep="\n", append = TRUE)
    cat("------------------------------", file = log_name, sep="\n", append = TRUE)
    cat(pandoc.table.return(d_out[,c(1,5:7)], split.tables = Inf, justify = 'left'), 
        file = log_name, sep="\n", append = TRUE)
    
    cat("------------------------------", file = log_name, sep="\n", append = TRUE)
    cat("Runtime", file = log_name, sep="\n", append = TRUE)
    cat("------------------------------", file = log_name, sep="\n", append = TRUE)
    cat(Sys.time() - timestart, file = log_name, sep="\n", append = TRUE)
    print(Sys.time() - timestart)
  }
}

# ============================ #
# 3. length_compar
# ============================ #
# function to compare status quo and leave taking variables 
take_compar <- function(d, output, out_dir,place_of_work) {
  # if place of work, multiply weight by 2% to adjust for missing values
  if (place_of_work){
    d$PWGTP <- d$PWGTP*1.02
    replicate_weights <- paste0('PWGTP',seq(1,80))
    for (i in replicate_weights) {
      d[,i] <- d[,i] * 1.02 
    }  
  }
  length_vars=paste0("length_",leave_types)
  length_names=paste("Counterfactual",leave_types,'leave')
  squo_vars = paste0("squo_", length_vars)
  squo_names=paste("Status Quo",leave_types,'leave')
  vars=c(length_vars, squo_vars)
  mean=c()
  SE=c()
  CI=c()
  total=c()
  total_SE=c()
  total_CI=c()
  for (i in vars) {
    temp=replicate_weights_SE(d %>% filter(get(i)>0), i,place_of_work)
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
  
  var_names=c(length_names, squo_names)
  d_out=data.frame(var_names,mean,SE,CI,total, total_SE, total_CI)
  colnames(d_out) <- c("Variable","Mean", "Standard Error of Mean", "Confidence Interval","Population Total", "Pop Total Standard Error", "Pop Total CI")
  write.csv(d_out,file=file.path(out_dir, paste0(output,'_takestats.csv'), fsep = .Platform$file.sep), row.names= FALSE)
  
  # graph leave length distributions 
  for (i in length_vars) {
    png(paste0(out_dir,i , '_cfact.png'))
    if (nrow(d %>% filter(get(i)>0))> 0 ){
      hist(d %>% filter(get(i)>0) %>% pull(get(i)), main = paste('Counterfactual leave distribution', i), xlab = "Length in Days", ylab = "Frequency", breaks=20)  
    }
    dev.off()
  }
  for (i in squo_vars) {
    png(paste0(out_dir,i , '_squo.png'))
    if (nrow(d %>% filter(get(i)>0))> 0 ){
      hist(d %>% filter(get(i)>0) %>% pull(get(i)), main = paste('Status quo leave distribution', i), xlab = "Length in Days", ylab = "Frequency", breaks=20)  
    }
    dev.off()
  }
  
  # combine graphs into a single pic file 
  cfact_img <- image_read(paste0(out_dir,length_vars[1] , '_cfact.png'))
  for (i in length_vars) {
    img <- image_read(paste0(out_dir,i , '_cfact.png'))
    cfact_img <- image_append(c(cfact_img, img), stack = TRUE)
    file.remove(paste0(out_dir,i , '_cfact.png'))
  }
  image_write(cfact_img, path = paste0(out_dir,output,"_cfact_lengths.png"), format = "png")

  squo_img <- image_read(paste0(out_dir,squo_vars[1] , '_squo.png'))
  for (i in squo_vars) {
    img <- image_read(paste0(out_dir,i , '_squo.png'))
    squo_img <- image_append(c(squo_img, img), stack = TRUE)
    file.remove(paste0(out_dir,i , '_squo.png'))
  }
  image_write(squo_img, path = paste0(out_dir,output,"_squo_lengths.png"), format = "png")
}

#=====================================================
# 4. create_meta_file
#=====================================================
create_meta_file <-function(d, out_dir,place_of_work) {
  # if place of work, multiply weight by 2% to adjust for missing values
  if (place_of_work){
    d$PWGTP <- d$PWGTP*1.02
    replicate_weights <- paste0('PWGTP',seq(1,80))
    for (i in replicate_weights) {
      d[,i] <- d[,i] * 1.02 
    }  
  }
  # meta output file for leave costs 
  meta_cost <- data.frame(row.names = leave_types)
  for (i in leave_types)  {
    var <- paste0('bene_',i)
    temp <- replicate_weights_SE(d, var,place_of_work)
    meta_cost[i, 'cost'] <- temp[7]
    meta_cost[i, 'ci_lower'] <- temp[[7]] + temp[[8]] *1.96 
    meta_cost[i, 'ci_upper'] <- temp[[7]] - temp[[8]] *1.96
  }
  temp <- replicate_weights_SE(d, 'actual_benefits',place_of_work)
  meta_cost['total', 'cost'] <- temp[7]
  meta_cost['total', 'ci_lower'] <-  temp[[7]] + temp[[8]] *1.96
  meta_cost['total', 'ci_upper'] <-  temp[[7]] - temp[[8]] *1.96
  write.csv(meta_cost,file=paste0(out_dir, 'program_cost_',format(Sys.time(), "%Y%m%d_%H%M%S"),'.csv'))
}