# Program to nationally estimate leave lengths from FMLA data 

cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)

d_fmla <- readRDS(paste0("./R_dataframes/","d_fmla.rds"))
len_Vars <- paste0('length_', c("own","illspouse","illchild","illparent","matdis","bond"))

for (i in len_Vars) {
  print(weighted.mean(d_fmla[d_fmla[i]!=0, i], w= d_fmla[d_fmla[i]!=0,'weight'], na.rm=TRUE))
}

for (i in len_Vars) {
  print(c(i, weighted.mean(d_fmla[d_fmla[i]!=0 & d_fmla['resp_len']==0,i], w= d_fmla[d_fmla[i]!=0 & d_fmla['resp_len']==0,'weight'], na.rm=TRUE)))
  print(c(i, weighted.mean(d_fmla[d_fmla[i]!=0 & d_fmla['resp_len']==1,i], w= d_fmla[d_fmla[i]!=0 & d_fmla['resp_len']==1,'weight'], na.rm=TRUE)))
}