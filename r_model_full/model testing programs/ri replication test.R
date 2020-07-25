library(plyr)
library(dplyr)
d_py <- read.csv('csv_inputs/p44_ri_pow.csv')
d_r <- readRDS('R_dataframes/work_states/RI_work.rds')

print(paste('d_py has',nrow(d_py),'rows,','d_r has',nrow(d_r),'rows'))
diff<-nrow(d_py)-nrow(d_r)
print(paste('difference of',diff,'rows'))

kid <- nrow(d_py %>% filter(AGEP<18))
print('py includes 16/17 yr olds, R does not.')
print(paste('accounts for',kid,' rows'))
print(paste(diff-kid,'rows remain'))

d_filt <- d_py %>% filter(AGEP>=18)

table(d_filt$POWSP)
table(d_r$POWSP)