cat("\014")  
options(error=recover)
library('ggplot2')
library('extrafont')
library('plyr')
library('dplyr')
library('varhandle')
source("5_output_analysis_functions.R")

state <- 'MD'
# load simulation data set
d <- read.csv(paste0('output/',state,'_simulation.csv'))
stats <- read.csv(paste0('output/',state,'_simulation_rawstats.csv'))

# note leave types and their clean label
leave_types <- c("own","illspouse","illchild","illparent","matdis","bond")
clean_types<- c(
  'own'='Own Sickness',
  'illspouse'='Ill Spouse',
  'illchild'='Ill Child',
  'illparent'='Ill Parent',
  'matdis'='Maternal Disability',
  'bond'='Child Bonding'
)

# Scenario 1 - Cost estimate
# create data frame of costs by leave type 
d_cost <- data.frame(row.names = paste(clean_types, 'Benefits'))
for (i in leave_types){
  label <- paste(clean_types[i],'Benefits')
  result <- replicate_weights_SE(d, paste0('bene_',i))
  d_cost[label,'Benefits_Paid_Out'] <- result['total']
  d_cost[label,'SE'] <- result['total_SE']
}
d_cost['All Types',] <- colSums(d_cost)
for (i in leave_types){
  label <- paste(clean_types[i],'Benefits')
  d_cost[label,'Type'] <- label
}
d_cost['All Types','Type'] <- 'All Types'

# generate graphs
ggplot(data=d_cost, aes(x=Type, y=Benefits_Paid_Out)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Benefits Outlayed ($ Millions)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0('$',format(x/1000000, big.mark=",", scientific=FALSE), "M")) +
  # graph title
  ggtitle('$ Benefits Paid Out by Leave Type') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,face='bold'),
        text=element_text(size = 10)) +
  
  # Data labels
  geom_text(aes(x = Type, y = Benefits_Paid_Out * 1.1+ 1000000, 
                label =paste0("$",format(Benefits_Paid_Out/1000000, big.mark=",", nsmall=1, digits=0, scientific=FALSE)
                              , "M"))) +
  geom_errorbar(aes(ymin=Benefits_Paid_Out-SE*1.96, ymax=Benefits_Paid_Out+SE*1.96), width=.2)  +

ggsave(file="./exhibits/Scenario 1 - Estimated Costs.png", width=6.5, dpi=300)

# Scenario 2 - Impact on Low Wage Workers
# Measuring the how low wage workers benefit from the program
# first identify those who are low wage as those under 200% of the poverty line. 
# adding in random determinate of household size until cleaning is rerun
if ('NPF' %in% colnames(d)==FALSE) {
  np_wgt <- c(17.2,25.3,17.7,19.4,11.2,5.2,2.2,1.7)
  d['NPF'] <- sample(1:8, nrow(d), replace=T,prob=np_wgt)
}
# add poverty thresholds based on number of ppl in HH 
d <- d %>% mutate(pov_thresh=ifelse(NPF==1, 24280,NA))
d <- d %>% mutate(pov_thresh=ifelse(NPF==2, 32920,pov_thresh))
d <- d %>% mutate(pov_thresh=ifelse(NPF==3, 41560,pov_thresh))
d <- d %>% mutate(pov_thresh=ifelse(NPF==4, 50200,pov_thresh))
d <- d %>% mutate(pov_thresh=ifelse(NPF==5, 58840,pov_thresh))
d <- d %>% mutate(pov_thresh=ifelse(NPF==6, 67480,pov_thresh))
d <- d %>% mutate(pov_thresh=ifelse(NPF==7, 76120,pov_thresh))
d <- d %>% mutate(pov_thresh=ifelse(NPF==8, 84760,pov_thresh))

# subset original data to low income only
d_low <- d%>% filter(faminc<pov_thresh)

# measure A - how much more income did this program provide this population?
# make historgram of benefits received
act_ben_vals <- d_low %>% filter(d_low$actual_benefits>0) 
x_break <- seq(0, max(act_ben_vals$actual_benefits)+1000, by=1000)

ggplot(act_ben_vals, aes(x = actual_benefits, freq=TRUE, weight = PWGTP)) + geom_histogram(breaks=x_break) +
  ylab('# of Individuals')+
  xlab('$ Benefits Received')+
  # graph title
  ggtitle('Histogram of Program Benefits Received') +
  # add total benefits received
  geom_text(aes(x = quantile(act_ben_vals$actual_benefits, c(.98)), y = 0, 
                label =paste0("Total Benefits to Low Income: $",format(sum(d_low$actual_benefits*d_low$PWGTP)/1000000, big.mark=",", 
                    nsmall=1, digits=0, scientific=FALSE), "M")),vjust=-35)

ggsave(file="./exhibits/Scenario 2 - Low Income Impact - Benefits Received.png", width=6.5, dpi=300)

# measure B - increase in leave taken
# measure length of leave taken by type, and how much it increased in the face of the program
d_chg <- data.frame(row.names = clean_types)
d_chg['Type'] <- clean_types
for (type in leave_types) {
  i <- clean_types[type]
  ptake_var <- paste0('ptake_',type)
  len_var <- paste0('length_',type)
  squo_var <- paste0('squo_length_',type)
  d_chg[i, 'takers'] <- sum(d_low[,ptake_var]*d_low$PWGTP)
  d_chg[i, 'tot_length'] <- sum(d_low[,len_var]*d_low$PWGTP)
  d_chg[i, 'squo_length'] <- sum(d_low[,squo_var]*d_low$PWGTP)
}
d_chg['pct_inc'] <- d_chg$tot_length / d_chg$squo_length - 1

ggplot(data=d_chg, aes(x=Type, y=pct_inc)) +  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Percent Change in Leave Taken') +
  # graph title
  ggtitle('Percent Change in Leave Taken by Leave Type') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,face='bold'),
        text=element_text(size = 10)) +
  
  # Data labels
  geom_text(aes(x = Type, y = pct_inc + .01,  label =paste0('+',sprintf("%.1f%%", 100 * pct_inc))))

ggsave(file="./exhibits/Scenario 2 - Low Income Impact - Additional Leave Taken.png", width=6.5, dpi=300)

# measure C - how many low income individuals took advantage of benefits 
ggplot(data=d_chg, aes(x=Type, y=takers)) +  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Number of Low-Income Beneficiaries') +
  # graph title
  ggtitle('Number of Low-Income Beneficiaries by Leave Type') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,face='bold'),
        text=element_text(size = 10)) +
  
  # Data labels
  geom_text(aes(x = Type, y = takers + 200, label=takers))

ggsave(file="./exhibits/Scenario 2 - Low Income Impact - Number of Beneficiaries.png", width=6.5, dpi=300)

