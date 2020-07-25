cat("\014")  
basepath <- rprojroot::find_rstudio_root_file()
setwd(basepath)
options(error=recover)
library(plyr)
library(ggplot2)
library(reshape2)
library(varhandle)

# read in IMPAQ results
impaq <- read.csv('output/issue_brief_1 nums 5_21.csv')
names(impaq)[names(impaq) == "X"] <- "var"
names(impaq)[names(impaq) == "source"] <- "model"

# input ACM results
ACM <- read.csv('issue brief 1 - ACM comparison/ACM Benchmark 1.csv')
temp <- unfactor(ACM)
temp['var'] <- ACM['var']
ACM <- temp
ACM[ACM['var']=='eligworker',] <-impaq[impaq['var']=='eligworker',]
ACM[ACM['var']=='5','var'] <- 'eligworker'
ACM['model'] <- 'ACM'
ACM[ACM=='']<- 0
ACM[is.na(ACM)]<- 0
drop_vars <- c('ptake_DI','ptake_PFL','plen_own','plen_matdis','PFL_plen','bene_DI','bene_PFL')
ACM <- ACM[!ACM$var %in% drop_vars,]
ACM[ACM['var']=='actual_benefits',c('CA','NJ','RI','CA_SE','NJ_SE','RI_SE')]<-
    ACM[ACM['var']=='actual_benefits',c('CA','NJ','RI','CA_SE','NJ_SE','RI_SE')]*1000000

# read in actual results 
actual <- read.csv('issue brief 1 - ACM comparison/actual leave data.csv')
names(actual)[names(actual) == "source"] <- "model"


# melt data together
d <- melt(impaq[c('var','CA','NJ','RI','model')])
d <- rbind(d,melt(ACM[c('var','CA','NJ','RI','model')]))
d <- rbind(d,melt(actual[c('var','CA','NJ','RI','model')]))
names(d)[names(d) == "variable"] <- "state"

# melt SE separately (all 0's for actual data)
d_se <- melt(impaq[c('var','CA_SE','NJ_SE','RI_SE','model')])
d_se <- rbind(d_se,melt(ACM[c('var','CA_SE','NJ_SE','RI_SE','model')]))
temp_se <- melt(actual[c('var','CA','NJ','RI','model')])
temp_se$variable <- paste0(temp_se$variable,'_SE')
temp_se$value <- 0
d_se <- rbind(d_se,temp_se)
names(d_se)[names(d_se) == "variable"] <- "state"

# add labels to both 
d <- merge(d, actual[c('var','label','Leave_Type')],all.x=TRUE)
d_se <- merge(d_se, actual[c('var','label','Leave_Type')],all.x=TRUE)

# don't want to include those without labels, which have no actual data
d <- d[(complete.cases(d)),]
d <- d[order(d$var,d$state,d$model),]

# merge in SE values to d 
d_se <- d_se[complete.cases(d_se),]
d_se['se_value']<- d_se['value']
d_se['state_merge'] <- sapply(d_se['state'], substring, 1, 2)
d <- merge(d, d_se[c('var','model','state_merge','se_value')],
           by.x=c('var','model','state'),by.y=c('var','model','state_merge'),all.x=TRUE)  

d <- d[order(d$var,d$state,d$model),]

# sort leave types by following order
# Own/Matdis/Bond/Ill Child/Ill Spouse/Ill Parent

Leave_Type<- c('Own Illness','Maternal Disability','Own/Maternal ','Child Bonding','Ill Child',
                'Ill Parent','Ill Spouse ','Eligible Workers','Benefits')

d$Leave_Type <- factor(d$Leave_Type, levels = Leave_Type)
d$model[d$model=='IMPAQ'] <- 'IMPAQ-DOL'
d$model <- factor(d$model, levels = c('IMPAQ-DOL','ACM','Actual'))

# make graphs 
# Exhibit 1 ---- Comparing Total Benefits 
ggplot(data=d[d['var']=='actual_benefits',], aes(x=state, y=value,fill=model)) +
  # bar chart
  geom_bar(stat="identity", position='dodge') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Benefits Outlayed (Millions)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(format(x/1000000, big.mark=",", scientific=FALSE))) +

  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,face='bold'), 
        text=element_text(size = 11)) +
  # Data labels
  geom_text(size=4,position = position_dodge(width= 1),aes(y=value+250000000,
                label=paste0(format(value/1000000, big.mark=",", nsmall=0, digits=2, scientific=FALSE)
                              ))) +
  geom_errorbar(position = position_dodge(width= 1),aes(ymin=value-se_value*1.96, ymax=value+se_value*1.96), width=.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  scale_fill_manual(values=c("#820023","#B2A97E",'grey')) + 
  xlab("State")+ theme(legend.title = element_blank())

ggsave(file="./exhibits/IB1_benefit_outlay.png", width=8, dpi=300)

# Exhibit 2 ---- Comparing Total Eligible Workers
ggplot(data=d[d['var']=='eligworker',], aes(x=state, y=value,fill=model)) +
  # bar chart
  geom_bar(stat="identity", position='dodge') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Eligible Workers (Millions)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(format(x/1000000, big.mark=",", scientific=FALSE))) +

  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,face='bold'), 
        text=element_text(size = 11)) +
  # Data labels
  geom_text(size=4,position = position_dodge(width= 1),aes(y=value+1000000,
                                                       label=paste0(format(value/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                                                                    ))) +
  geom_errorbar(position = position_dodge(width= 1),aes(ymin=value-se_value*1.96, ymax=value+se_value*1.96), width=.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  scale_fill_manual(values=c("#820023","#B2A97E",'grey')) + 
  xlab("State")+ theme(legend.title = element_blank())
        
ggsave(file="./exhibits/IB2_eligible_workers.png", width=8, dpi=300)

# Exhibit 3 ---- California, Number of leave takers
ggplot(data=d[d['state']=='CA' & grepl('ptake',d$var),], aes(x=Leave_Type, y=value,fill=model)) +
  # bar chart
  geom_bar(stat="identity", position='dodge') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Participants (Thousands)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(format(x/1000, big.mark=",", scientific=FALSE))) +

  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,face='bold'),
        text=element_text(size = 11)) +
  # Data labels
  geom_text(size=4,position = position_dodge(width= 1),aes(y=value+25000,
                                                       label=paste0(format(value/1000, big.mark=",", nsmall=0, digits=2, scientific=FALSE)
                                                                    ))) +
  geom_errorbar(position = position_dodge(width= 1),aes(ymin=value-se_value*1.96, ymax=value+se_value*1.96), width=.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  scale_fill_manual(values=c("#820023","#B2A97E",'grey')) + 
  xlab("State")+ theme(legend.title = element_blank())
        

ggsave(file="./exhibits/IB3_CA_Leave_Takers.png", width=8, dpi=300)

# Exhibit 5 ---- New Jersey, Number of leave takers
ggplot(data=d[d['state']=='NJ' & grepl('ptake',d$var),], aes(x=Leave_Type, y=value,fill=model)) +
  # bar chart
  geom_bar(stat="identity", position='dodge') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Participants (Thousands)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(format(x/1000, big.mark=",", scientific=FALSE))) +
  
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,face='bold'),
        text=element_text(size = 11)) +
  # Data labels
  geom_text(size=4,position = position_dodge(width= 1),aes(y=value+4000,
                                                    label=paste0(format(value/1000, big.mark=",", nsmall=0, digits=2, scientific=FALSE)
                                                                 ))) +
  geom_errorbar(position = position_dodge(width= 1),aes(ymin=value-se_value*1.96, ymax=value+se_value*1.96), width=.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  scale_fill_manual(values=c("#820023","#B2A97E",'grey')) + 
  xlab("State")+ theme(legend.title = element_blank())
        

ggsave(file="./exhibits/IB5_NJ_Leave_Takers.png", width=8, dpi=300)

# Exhibit 6 ---- New Jersey, leave length
ggplot(data=d[d['state']=='NJ' & grepl('plen',d$var),], aes(x=Leave_Type, y=value,fill=model)) +
  # bar chart
  geom_bar(stat="identity", position='dodge') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Length of Participation (Weeks)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(format(x, big.mark=",", scientific=FALSE))) +
  
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,face='bold'),
        text=element_text(size = 11)) +
  # Data labels
  geom_text(size=4,position = position_dodge(width= 1),aes(y=value+1,
                                                    label=paste0(format(value, big.mark=",", nsmall=1, digits=2, scientific=FALSE)))) +
  geom_errorbar(position = position_dodge(width= 1),aes(ymin=value-se_value*1.96, ymax=value+se_value*1.96), width=.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  scale_fill_manual(values=c("#820023","#B2A97E",'grey')) + 
  xlab("State") + theme(legend.title = element_blank())
        

ggsave(file="./exhibits/IB6_NJ_Leave_Length.png", width=8, dpi=300)

# Exhibit 7 ---- Rhode Island, Number of leave takers
ggplot(data=d[d['state']=='RI' & grepl('ptake',d$var),], aes(x=Leave_Type, y=value,fill=model)) +
  # bar chart
  geom_bar(stat="identity", position='dodge') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Participants (Thousands)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(format(x/1000, big.mark=",", scientific=FALSE))) +
  
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,face='bold'),
        text=element_text(size = 11)) +
  # Data labels
  geom_text(size=4,position = position_dodge(width= 1),aes(y=value*1.05+1000,
                                                    label=paste0(format(value/1000, big.mark=",", nsmall=1, digits=1, scientific=FALSE)
                                                                 ))) +
  geom_errorbar(position = position_dodge(width= 1),aes(ymin=value-se_value*1.96, ymax=value+se_value*1.96), width=.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_fill_manual(values=c("#820023","#B2A97E",'grey')) + 
  xlab("State") + theme(legend.title = element_blank())
        

ggsave(file="./exhibits/IB7_RI_Leave_Takers.png", width=8, dpi=300)

