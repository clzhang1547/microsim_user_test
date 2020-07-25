# creating some graphs from method testing analysis

# import data
graphics.off()
bi_df <- read.csv('./output/detail_meth_compar_wgt.csv')
num_df <- read.csv('./output/leave_num_stats.csv')
pred_df <- read.csv('./output/pred_type_stats.csv')
prop_df <- read.csv('./output/prop_pay_stats.csv')
library('ggplot2')
library('extrafont')
library('plyr')
library('dplyr')
library('varhandle')
options(error=NULL)
#font_import()
#loadfonts(device = "win")

# Exhibit 1 ---- Comparing predicted vs actual mean Prop_pay values in the test population
ggplot(data=prop_df %>% filter(Method != 'random'), aes(x=Method, y=Predicted_Prop_Pay_Avg)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Predicted Mean Proportion of Pay Received') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  # graph title
  ggtitle('Predicted vs. Actual Mean Proportion of Leave Pay Received from Employer') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = prop_df$Actual_Prop_Pay_Avg[1], size=1,color='red2', linetype='dashed') +
  # label for reference line
  geom_text(aes(.6,prop_df$Actual_Prop_Pay_Avg[1],
                label = paste0('Actual Prop\nPay Mean:\n',sprintf("%.1f%%", 100 * prop_df$Actual_Prop_Pay_Avg[1])),
                vjust = -.25,  hjust = 0),
            family='Arial',
            size=4,
            color='red2') +
  # Data labels
  geom_text(aes(x = Method, y = Predicted_Prop_Pay_Avg + 0.02, 
                label = sprintf("%.1f%%", 100 * Predicted_Prop_Pay_Avg))) +
  geom_errorbar(aes(ymin=Predicted_Prop_Pay_Avg-Predicted_SE*1.96, ymax=Predicted_Prop_Pay_Avg+Predicted_SE*1.96), width=.2) 

ggsave(file="./exhibits/1_prop_pay_avg.png", width=6.5, dpi=300)

# Exhibit 2 ---- Accuracy of predicted prop_pay values ------

ggplot(data=prop_df %>% filter(Method != 'random'), aes(x=Method, y=Accuracy)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Prediction Accuracy') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  # graph title
  ggtitle('Prediction Accuracy of Proportion of Leave Pay Received from Employer') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Accuracy + 0.05, 
                label = sprintf("%.1f%%", 100 * Accuracy))) +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = (prop_df%>%filter(Method=='random')%>%select(Accuracy))[[1]], size=1,color='red2', linetype='dashed') +
  # label for reference line
  geom_text(aes(.6,(prop_df%>%filter(Method=='random')%>%select(Accuracy))[[1]],
                label = paste0('Random\nAccuracy:\n',sprintf("%.1f%%", 100 * prop_df%>%filter(Method=='random')%>%select(Accuracy))),
                vjust = 1,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')
ggsave(file="./exhibits/2_prop_pay_acc.png", width=6.5, dpi=300)


# Exhibit 3 ---- Accuracy of predicted type of leave values ------

ggplot(data=pred_df %>% filter(Method != 'random'), aes(x=Method, y=Accuracy)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Prediction Accuracy') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  # graph title
  ggtitle('Prediction Accuracy of Type of Leave Taken') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Accuracy + 0.05, 
                label = sprintf("%.1f%%", 100 * Accuracy))) +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = (pred_df%>%filter(Method=='random')%>%select(Accuracy))[[1]], size=1,color='red2', linetype='dashed') +
  # label for reference line
  geom_text(aes(.6,(pred_df%>%filter(Method=='random')%>%select(Accuracy))[[1]],
                label = paste0('Random\nAccuracy:\n',sprintf("%.1f%%", 100 * pred_df%>%filter(Method=='random')%>%select(Accuracy))),
                vjust = 1,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')

ggsave(file="./exhibits/3_pred_type_acc.png", width=6.5, dpi=300)

# Exhibit 4 ---- Comparing predicted vs. actual leaves taken Nationally
ggplot(data=num_df, aes(x=Method, y=Predicted_Leave_Num)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Leaves Taken (Millions)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(format(x/1000000, big.mark=",", scientific=FALSE), "M")) +
  # graph title
  ggtitle('Predicted vs. Actual Leaves Taken Nationally') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = num_df$Actual_Leave_Num[1], size=1,color='red2', linetype='dashed') +
  # label for reference line
  geom_text(aes(.6,num_df$Actual_Leave_Num[1],
                label = paste0('Actual Leaves Taken:\n',
                  format(num_df$Actual_Leave_Num[1]/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                               , "M"),
                vjust = -.25,  hjust = 0),
            family='Arial',
            size=4,
            color='red2') +
  # Data labels
  geom_text(aes(x = Method, y = Predicted_Leave_Num + 3000000, 
                label =paste0(format(Predicted_Leave_Num/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                              , "M"))) +
  geom_errorbar(aes(ymin=Predicted_Leave_Num-Predicted_SE*1.96, ymax=Predicted_Leave_Num+Predicted_SE*1.96), width=.2) 

ggsave(file="./exhibits/4_num_leaves_tot.png", width=6.5, dpi=300)

# Exhibit 5 ---- Accuracy of predicted num_leaves values ------

ggplot(data=num_df  %>% filter(Method != 'random'), aes(x=Method, y=Accuracy)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Prediction Accuracy') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  # graph title
  ggtitle('Prediction Accuracy of Number Of Leaves Taken') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Accuracy + 0.02, 
                label = sprintf("%.1f%%", 100 * Accuracy))) +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = (num_df%>%filter(Method=='random')%>%select(Accuracy))[[1]], size=1,color='red2', linetype='dashed') +
  # label for reference line
  geom_text(aes(.6,(num_df%>%filter(Method=='random')%>%select(Accuracy))[[1]],
                label = paste0('Random\nAccuracy:\n',sprintf("%.1f%%", 100 * num_df%>%filter(Method=='random')%>%select(Accuracy))),
                vjust = 1,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')

ggsave(file="./exhibits/5_num_leaves_acc.png", width=6.5, dpi=300)

# Exhibit 6 ---- Comparing predicted vs. actual leave takers nationally
bi_df_filt <- bi_df %>% filter(Variable == 'pred_leave')
ggplot(data=bi_df_filt  %>% filter(Method != 'random'), aes(x=Method, y=Predicted_Positives)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Leave Takers (Millions)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(format(x/1000000, big.mark=",", scientific=FALSE), "M")) +
  # graph title
  ggtitle('Predicted vs. Actual Leave Takers Nationally') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Predicted_Positives + 1800000, 
                label =paste0(format(Predicted_Positives/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                              , "M"))) +
  geom_errorbar(aes(ymin=Predicted_Positives-Predicted_SE*1.96*Total_Predictions, ymax=Predicted_Positives+Predicted_SE*1.96*Total_Predictions), width=.2)  +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = bi_df_filt$Actual_Positives[1], size=1,color='red2', linetype='dashed') +
  # label for reference line
  geom_text(aes(.6,bi_df_filt$Actual_Positives[1],
                label = paste0('Actual Leave Takers:\n',
                               format(bi_df_filt$Actual_Positives[1]/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                               , "M"),
                vjust = -.25,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')

ggsave(file="./exhibits/6_leave_takers_tot.png", width=6.5, dpi=300)


# Exhibit 6a ---- Comparing predicted vs. actual leave takers nationally
bi_df_filt <- bi_df %>% filter(Variable == 'pred_need')
ggplot(data=bi_df_filt  %>% filter(Method != 'random'), aes(x=Method, y=Predicted_Positives)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Leave Takers (Millions)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(format(x/1000000, big.mark=",", scientific=FALSE), "M")) +
  # graph title
  ggtitle('Predicted vs. Actual Leave Needers Nationally') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Predicted_Positives + 1100000, 
                label =paste0(format(Predicted_Positives/1000000, big.mark=",", nsmall=1, digits=1, scientific=FALSE)
                              , "M"))) +
  geom_errorbar(aes(ymin=Predicted_Positives-Predicted_SE*1.96*Total_Predictions, ymax=Predicted_Positives+Predicted_SE*1.96*Total_Predictions), width=.2)  +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = bi_df_filt$Actual_Positives[1], size=1,color='red2', linetype='dashed') +
  # label for reference line
  geom_text(aes(.6,bi_df_filt$Actual_Positives[1],
                label = paste0('Actual Leave Needers:\n',
                               format(bi_df_filt$Actual_Positives[1]/1000000, big.mark=",", nsmall=1, digits=1, scientific=FALSE)
                               , "M"),
                vjust = -.75,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')

ggsave(file="./exhibits/6a_leave_needers_tot.png", width=6.5, dpi=300)

# Exhibit 7 ---- Accuracy of predicted leave takers ------
bi_df_filt <- bi_df %>% filter(Variable == 'pred_leave')
ggplot(data=bi_df_filt %>% filter(Method != 'random'), aes(x=Method, y=Accuracy)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Prediction Accuracy') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  # graph title
  ggtitle('Prediction Accuracy of who is a Leave Taker') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Accuracy + 0.05, 
                label = sprintf("%.1f%%", 100 * Accuracy))) +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = (bi_df_filt%>%filter(Method=='random')%>%select(Accuracy))[[1]], size=1,color='red2', linetype='dashed') +
  # label for reference line
  geom_text(aes(.6,(bi_df_filt%>%filter(Method=='random')%>%select(Accuracy))[[1]],
                label = paste0('Random\nAccuracy:\n',sprintf("%.1f%%", 100 * bi_df_filt%>%filter(Method=='random')%>%select(Accuracy))),
                vjust = 1,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')

ggsave(file="./exhibits/7_leave_takers_acc.png", width=6.5, dpi=300)

# Exhibit 7a ---- Accuracy of predicted leave needers------
bi_df_filt <- bi_df %>% filter(Variable == 'pred_need')
ggplot(data=bi_df_filt %>% filter(Method != 'random'), aes(x=Method, y=Accuracy)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Prediction Accuracy') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  # graph title
  ggtitle('Prediction Accuracy of who is a Leave Needer') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Accuracy + 0.05, 
                label = sprintf("%.1f%%", 100 * Accuracy))) +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = (bi_df_filt%>%filter(Method=='random')%>%select(Accuracy))[[1]], size=1,color='red2', linetype='dashed') +
  # label for reference line
  geom_text(aes(.6,(bi_df_filt%>%filter(Method=='random')%>%select(Accuracy))[[1]],
                label = paste0('Random\nAccuracy:\n',sprintf("%.1f%%", 100 * bi_df_filt%>%filter(Method=='random')%>%select(Accuracy))),
                vjust = 1,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')

ggsave(file="./exhibits/7a_leave_needers_acc.png", width=6.5, dpi=300)

# Exhibit 8 ---- Precision of predicted leave takers ------
bi_df_filt <- bi_df %>% filter(Variable == 'pred_leave')
ggplot(data=bi_df_filt %>% filter(Method != 'random'), aes(x=Method, y=Precision)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Prediction Precision') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  # graph title
  ggtitle('Prediction Precision of who is a Leave Taker') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Precision + 0.05, 
                label = sprintf("%.1f%%", 100 * Precision)))+
  # reference line for actual prop_pay_average
  geom_hline(yintercept = (bi_df_filt%>%filter(Method=='random')%>%select(Precision))[[1]], size=1,color='red2', linetype='dashed') +
  # label for reference line
  geom_text(aes(.6,(bi_df_filt%>%filter(Method=='random')%>%select(Precision))[[1]],
                label = paste0('Random\nPrecision:\n',sprintf("%.1f%%", 100 * bi_df_filt%>%filter(Method=='random')%>%select(Precision))),
                vjust = 1,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')
ggsave(file="./exhibits/8_leave_takers_precision.png", width=6.5, dpi=300)

# Exhibit 9 ---- Recall of predicted leave takers ------
bi_df_filt <- bi_df %>% filter(Variable == 'pred_leave')
ggplot(data=bi_df_filt %>% filter(Method != 'random'), aes(x=Method, y=Recall)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Prediction Recall') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  # graph title
  ggtitle('Prediction Recall of who is a Leave Taker') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Recall + 0.05, 
                label = sprintf("%.1f%%", 100 * Recall)))+
  # reference line for actual prop_pay_average
  geom_hline(yintercept = (bi_df_filt%>%filter(Method=='random')%>%select(Recall))[[1]], size=1,color='red2', linetype='dashed') +
  # label for reference line
  geom_text(aes(.6,(bi_df_filt%>%filter(Method=='random')%>%select(Recall))[[1]],
                label = paste0('Random\nRecall:\n',sprintf("%.1f%%", 100 * bi_df_filt%>%filter(Method=='random')%>%select(Recall))),
                vjust = -.25,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')

ggsave(file="./exhibits/9_leave_takers_recall.png", width=6.5, dpi=300)

# Exhibit 10 ----- Estimated Benefit Outlays for RI for each method
# create data frame of benefit outlay estimates
ncols <- 5
ri_df <-  matrix(0, ncol = ncols, nrow = 0)
# populate data frame with results from each method
methods <- c('logit','random', 'KNN1', 'KNN_multi','random_forest','Naive_Bayes', 'ridge_class')
for (meth in methods) {
  df <- read.csv(paste0('./output/RI_',meth,"_method_rawstats.csv"))
  result <- df %>% filter(Variable == 'Benefits Received ($), total')
  row <- matrix(c(meth, result[[5]],result[[6]],result[[5]]- 1.96*result[[6]],result[[5]] + 1.96*result[[6]]), ncol = ncols, nrow = 1)
  ri_df <- rbind(ri_df, row)
}
ri_df <- as.data.frame(ri_df)
ri_df <- unfactor(ri_df)

colnames(ri_df) <- c('Method','Estimate','SE','CI_low','CI_high')

ggplot(data=ri_df, aes(x=Method, y=Estimate)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Benefits Outlayed (Millions)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0('$',format(x/1000000, big.mark=",", scientific=FALSE), "M")) +
  # graph title
  ggtitle('Predicted vs. Actual Benefits Outlayed - Rhode Island') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Estimate + 100000000, 
                label =paste0("$",format(Estimate/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                              , "M"))) +
  geom_errorbar(aes(ymin=Estimate-SE*1.96, ymax=Estimate+SE*1.96), width=.2)  +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = 175659993, size=1,color='red2', linetype='dashed') +
  # label for reference line
  # hardcoded from a
  geom_text(aes(2.75,175659993,
                label = paste0('Actual Benefits Outlayed\n (2012-2016 Annual Mean):\n$',
                               format(175659993/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                               , "M"),
                vjust = -.3,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')

ggsave(file="./exhibits/10_RI_bene_outlay_tot.png", width=6.5, dpi=300)

# Exhibit 11 ----- Estimated Benefit Outlays for NJ for each method
# create data frame of benefit outlay estimates
ncols <- 5
nj_df <-  matrix(0, ncol = ncols, nrow = 0)
# populate data frame with results from each method
methods <- c('logit','random', 'KNN1', 'KNN_multi','random_forest','Naive_Bayes', 'ridge_class')
for (meth in methods) {
  df <- read.csv(paste0('./output/NJ_',meth,"_method_rawstats.csv"))
  result <- df %>% filter(Variable == 'Benefits Received ($), total')
  row <- matrix(c(meth, result[[5]],result[[6]],result[[5]]- 1.96*result[[6]],result[[5]] + 1.96*result[[6]]), ncol = ncols, nrow = 1)
  nj_df <- rbind(nj_df, row)
}
nj_df <- as.data.frame(nj_df)
nj_df <- unfactor(nj_df)

colnames(nj_df) <- c('Method','Estimate','SE','CI_low','CI_high')

ggplot(data=nj_df, aes(x=Method, y=Estimate)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Benefits Outlayed (Millions)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0('$',format(x/1000000, big.mark=",", scientific=FALSE), "M")) +
  # graph title
  ggtitle('Predicted vs. Actual Benefits Outlayed - New Jersey') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Estimate + 100000000, 
                label =paste0("$",format(Estimate/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                              , "M"))) +
  geom_errorbar(aes(ymin=Estimate-SE*1.96, ymax=Estimate+SE*1.96), width=.2)  +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = 506940000, size=1,color='red2', linetype='dashed') +
  # label for reference line
  # hardcoded from a
  geom_text(aes(2.75,506940000,
                label = paste0('Actual Benefits Outlayed\n (2012-2016 Annual Mean):\n$',
                               format(506940000/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                               , "M"),
                vjust = -.3,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')

ggsave(file="./exhibits/11_NJ_bene_outlay_tot.png", width=6.5, dpi=300)

# Exhibit 12 ----- Estimated Benefit Outlays for CA for each method
# create data frame of benefit outlay estimates
ncols <- 5
ca_df <-  matrix(0, ncol = ncols, nrow = 0)
# populate data frame with results from each method
methods <- c('logit','random', 'KNN1', 'KNN_multi','random_forest','Naive_Bayes', 'ridge_class')
for (meth in methods) {
  df <- read.csv(paste0('./output/CA_',meth,"_method_rawstats.csv"))
  result <- df %>% filter(Variable == 'Benefits Received ($), total')
  row <- matrix(c(meth, result[[5]],result[[6]],result[[5]]- 1.96*result[[6]],result[[5]] + 1.96*result[[6]]), ncol = ncols, nrow = 1)
  ca_df <- rbind(ca_df, row)
}
ca_df <- as.data.frame(ca_df)
ca_df <- unfactor(ca_df)

colnames(ca_df) <- c('Method','Estimate','SE','CI_low','CI_high')

ggplot(data=ca_df, aes(x=Method, y=Estimate)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Benefits Outlayed (Millions)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0('$',format(x/1000000, big.mark=",", scientific=FALSE), "M")) +
  # graph title
  ggtitle('Predicted vs. Actual Benefits Outlayed - New Jersey') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Estimate + 100000000, 
                label =paste0("$",format(Estimate/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                              , "M"))) +
  geom_errorbar(aes(ymin=Estimate-SE*1.96, ymax=Estimate+SE*1.96), width=.2)  +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = 506940000, size=1,color='red2', linetype='dashed') +
  # label for reference line
  # hardcoded from a
  geom_text(aes(2.75,506940000,
                label = paste0('Actual Benefits Outlayed\n (2012-2016 Annual Mean):\n$',
                               format(506940000/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                               , "M"),
                vjust = -.3,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')

ggsave(file="./exhibits/12_CA_bene_outlay_tot.png", width=6.5, dpi=300)




# Exhibit 10 ALT ----- Estimated Benefit Outlays for RI for each method,POW
# create data frame of benefit outlay estimates
ncols <- 5
ri_df <-  matrix(0, ncol = ncols, nrow = 0)
# populate data frame with results from each method
methods <- c('logit','random', 'KNN1', 'KNN_multi','random_forest','Naive_Bayes', 'ridge_class')
for (meth in methods) {
  df <- read.csv(paste0('./output/RI_',meth,"_method_POW_rawstats.csv"))
  result <- df %>% filter(Variable == 'Benefits Received ($), total')
  row <- matrix(c(meth, result[[5]],result[[6]],result[[5]]- 1.96*result[[6]],result[[5]] + 1.96*result[[6]]), ncol = ncols, nrow = 1)
  ri_df <- rbind(ri_df, row)
}
ri_df <- as.data.frame(ri_df)
ri_df <- unfactor(ri_df)

colnames(ri_df) <- c('Method','Estimate','SE','CI_low','CI_high')

ggplot(data=ri_df, aes(x=Method, y=Estimate)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Benefits Outlayed (Millions)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0('$',format(x/1000000, big.mark=",", scientific=FALSE), "M")) +
  # graph title
  ggtitle('Predicted vs. Actual Benefits Outlayed - Rhode Island, POW') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Estimate + 100000000, 
                label =paste0("$",format(Estimate/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                              , "M"))) +
  geom_errorbar(aes(ymin=Estimate-SE*1.96, ymax=Estimate+SE*1.96), width=.2)  +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = 175659993, size=1,color='red2', linetype='dashed') +
  # label for reference line
  # hardcoded from a
  geom_text(aes(2.75,175659993,
                label = paste0('Actual Benefits Outlayed\n (2012-2016 Annual Mean):\n$',
                               format(175659993/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                               , "M"),
                vjust = -.3,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')

ggsave(file="./exhibits/POW_10_RI_bene_outlay_tot.png", width=6.5, dpi=300)

# Exhibit 11 ----- Estimated Benefit Outlays for NJ for each method,POW
# create data frame of benefit outlay estimates
ncols <- 5
nj_df <-  matrix(0, ncol = ncols, nrow = 0)
# populate data frame with results from each method
methods <- c('logit','random', 'KNN1', 'KNN_multi','random_forest','Naive_Bayes', 'ridge_class')
for (meth in methods) {
  df <- read.csv(paste0('./output/NJ_',meth,"_method_POW_rawstats.csv"))
  result <- df %>% filter(Variable == 'Benefits Received ($), total')
  row <- matrix(c(meth, result[[5]],result[[6]],result[[5]]- 1.96*result[[6]],result[[5]] + 1.96*result[[6]]), ncol = ncols, nrow = 1)
  nj_df <- rbind(nj_df, row)
}
nj_df <- as.data.frame(nj_df)
nj_df <- unfactor(nj_df)

colnames(nj_df) <- c('Method','Estimate','SE','CI_low','CI_high')

ggplot(data=nj_df, aes(x=Method, y=Estimate)) +
  # bar chart
  geom_bar(stat="identity", fill = 'burlywood2') +
  # theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # y axis label
  ylab('Benefits Outlayed (Millions)') +
  # y axis tick labels
  scale_y_continuous(labels = function(x) paste0('$',format(x/1000000, big.mark=",", scientific=FALSE), "M")) +
  # graph title
  ggtitle('Predicted vs. Actual Benefits Outlayed - New Jersey, POW') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Estimate + 100000000, 
                label =paste0("$",format(Estimate/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                              , "M"))) +
  geom_errorbar(aes(ymin=Estimate-SE*1.96, ymax=Estimate+SE*1.96), width=.2)  +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = 506940000, size=1,color='red2', linetype='dashed') +
  # label for reference line
  # hardcoded from a
  geom_text(aes(2.75,506940000,
                label = paste0('Actual Benefits Outlayed\n (2012-2016 Annual Mean):\n$',
                               format(506940000/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                               , "M"),
                vjust = -.3,  hjust = 0),
            family='Arial',
            size=4,
            color='red2')

ggsave(file="./exhibits/POW_11_NJ_bene_outlay_tot.png", width=6.5, dpi=300)
