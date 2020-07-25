
####################################################
# creating some graphs from method testing analysis#
####################################################

# ----------------------------------
# Author: P.M.
# Date: 02/20/2019
# ----------------------------------

library(ggplot2); library(ggrepel); library(extrafont); library(plyr); library(dplyr); library(varhandle); 
library(RColorBrewer)

# --------------------------------------------------------------------------------------------------------------------
# DATASETS
# --------------------------------------------------------------------------------------------------------------------

graphics.off()
bi_df <- read.csv('output/detail_meth_compar_wgt.csv')
num_df <- read.csv('output/leave_num_stats.csv')
pred_df <- read.csv('output/pred_type_stats.csv')
prop_df <- read.csv('output/prop_pay_stats.csv')

options(error=NULL)
# font_import()
# loadfonts(device = "win")

# start df for heat map of all results 
methods <- c('Exhibit','Measure','Type','Actual','logit','random', 'KNN1', 'KNN_multi','random_forest','Naive_Bayes', 'ridge_class')
heat_df <- data.frame(matrix(ncol = length(methods), nrow = 0))
colnames(heat_df) <- methods

# --------------------------------------------------------------------------------------------------------------------
# Exhibit 1 ---- Comparing predicted vs actual mean Prop_pay values in the test population
# --------------------------------------------------------------------------------------------------------------------

ggplot(data=prop_df %>%  mutate(Method = factor(Method, levels=c("random", "KNN_multi", "KNN1", "logit", "Naive_Bayes", "random_forest", "ridge_class")))
       , aes(x=Method, y=Predicted_Prop_Pay_Avg)) +
      
      geom_bar(stat="identity", fill = '#D6CFAC') +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  
      ggtitle('Predicted vs. Actual Mean Proportion of Leave Pay\n Received from Employer') +
      theme(plot.title = element_text(size=12, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  
      ylab(NULL) +
      theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
      # ylab('Predicted Mean Proportion of Pay Received') +
      # scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  
      xlab(NULL) + 
      scale_x_discrete(labels = c("Random", "KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
      theme(axis.text.x = element_text(size = 10)) +
  
      geom_hline(yintercept = prop_df$Actual_Prop_Pay_Avg[1], size=1,color='#820023', linetype='longdash') +
      geom_text(aes(4.6,prop_df$Actual_Prop_Pay_Avg[1],
                    label = paste0('Actual Proportion\nPay Mean:\n',sprintf("%.1f%%", 100 * prop_df$Actual_Prop_Pay_Avg[1])),
                    vjust = -.25,  hjust = 0),
                family='Arial',
                size=4,
                color='#820023') +
  

      geom_text(aes(x = Method, y = Predicted_Prop_Pay_Avg + 0.05, 
                    label = sprintf("%.1f%%", 100 * Predicted_Prop_Pay_Avg)), size=4, family="Arial", fontface="bold") +
      geom_errorbar(aes(ymin=Predicted_Prop_Pay_Avg-Predicted_SE*1.96, ymax=Predicted_Prop_Pay_Avg+Predicted_SE*1.96), width=.2) 

#ggsave(file="./exhibits/1_prop_pay_avg.png", width=6.5, dpi=300)
ggsave(file="Exhibits/1_prop_pay_avg.png", width=6.5, dpi=300)

# add results to heat map df
append_row <- prop_df['Predicted_Prop_Pay_Avg']
rownames(append_row) <- prop_df$Method
append_row <- data.frame(t(append_row))
append_row['Exhibit'] <- '8'
append_row['Method'] <- 'Predicted/Actual Prop_Pay'
append_row['Type'] <- 'FMLA-to-FMLA Aggregate'
append_row['Actual'] <- prop_df$Predicted_Prop_Pay_Avg[1]
heat_df <- rbind(heat_df, append_row)


# --------------------------------------------------------------------------------------------------------------------
# Exhibit 2 ---- Accuracy of predicted prop_pay values ------
# --------------------------------------------------------------------------------------------------------------------

ggplot(data=prop_df %>% filter(Method != 'random'), aes(x=Method, y=Accuracy)) +
  
  geom_bar(stat="identity", fill = '#D6CFAC') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  
  ggtitle('Prediction Accuracy of Proportion of Leave Pay\n Received from Employer') +
  theme(plot.title = element_text(size=12, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  # ylab('Prediction Accuracy') +
  # scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  
  xlab(NULL) + 
  scale_x_discrete(labels = c("KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
  theme(axis.text.x = element_text(size = 10)) +
  
  geom_hline(yintercept = (prop_df%>%filter(Method=='random')%>%select(Accuracy))[[1]], size=1,color='#820023', linetype='longdash') +
  geom_text(aes(.6,(prop_df%>%filter(Method=='random')%>%select(Accuracy))[[1]],
                label = paste0('Random\nAccuracy:\n',sprintf("%.1f%%", 100 * prop_df%>%filter(Method=='random')%>%select(Accuracy))),
                vjust = 1,  hjust = 0),
            family='Arial',
            size=4,
            color='#820023') +
  
  geom_text(aes(x = Method, y = Accuracy + .05, label = sprintf("%.1f%%", 100 * Accuracy)), size=4, family="Arial", fontface="bold") +
  
# ggsave(file="./exhibits/2_prop_pay_acc.png", width=6.5, dpi=300)
ggsave(file="Exhibits/2_prop_pay_acc.png", width=6.5, dpi=300)

# add results to heat map df
append_row <- prop_df['Accuracy']
rownames(append_row) <- prop_df$Method
append_row <- data.frame(t(append_row))
append_row['Exhibit'] <- '11'
append_row['Method'] <- 'Prop_Pay Accuracy'
append_row['Type'] <- 'FMLA-to-FMLA Individual'
append_row['Actual'] <- 'N/A'
heat_df <- rbind(heat_df, append_row)

# exhibit removed from report
# # --------------------------------------------------------------------------------------------------------------------
# # Exhibit 3 ---- Accuracy of predicted type of leave values ------
# # --------------------------------------------------------------------------------------------------------------------
# 
# ggplot(data=pred_df %>% filter(Method != 'random'), aes(x=Method, y=Accuracy)) +
#   
#   geom_bar(stat="identity", fill = '#D6CFAC') +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
#   
#   ggtitle('Prediction Accuracy of Type of Leave Taken') +
#   theme(plot.title = element_text(size=12, hjust = 0.5,family='Arial',face='bold'), 
#         text=element_text(size = 10, family='Arial')) +
#   
#   ylab(NULL) +
#   theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
#   # ylab('Prediction Accuracy') +
#   # scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
#   
#   xlab(NULL) + 
#   scale_x_discrete(labels = c("KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
#   theme(axis.text.x = element_text(size = 10)) +
#   
#   geom_hline(yintercept = (pred_df%>%filter(Method=='random')%>%select(Accuracy))[[1]], size=1,color='#820023', linetype='longdash') +
#   geom_text(aes(.6,(pred_df%>%filter(Method=='random')%>%select(Accuracy))[[1]],
#                 label = paste0('Random\nAccuracy:\n',sprintf("%.1f%%", 100 * pred_df%>%filter(Method=='random')%>%select(Accuracy))),
#                 vjust = 1,  hjust = 0),
#             family='Arial',
#             size=4,
#             color='#820023') +
#   
#   geom_text(aes(x = Method, y = Accuracy + .03, label = sprintf("%.1f%%", 100 * Accuracy)), size=4, family="Arial", fontface="bold") +
# 
# #ggsave(file="./exhibits/3_pred_type_acc.png", width=6.5, dpi=300)
# ggsave(file="Exhibits/3_pred_type_acc.png", width=6.5, dpi=300)
# 
# # add results to heat map df
# append_row <- pred_df['Accuracy']
# rownames(append_row) <- pred_df$Method
# append_row <- data.frame(t(append_row))
# append_row['Exhibit'] <- ''
# append_row['Method'] <- 'Type of Leave Accuracy'
# append_row['Type'] <- 'FMLA-to-FMLA Individual'
# heat_df <- rbind(heat_df, append_row)

# --------------------------------------------------------------------------------------------------------------------
# Exhibit 4 ---- Comparing predicted vs. actual leaves taken Nationally
# --------------------------------------------------------------------------------------------------------------------

ggplot(data=num_df %>%  mutate(Method = factor(Method, levels=c("random", "KNN_multi", "KNN1", "logit", "Naive_Bayes", "random_forest", "ridge_class"))), aes(x=Method, y=Predicted_Leave_Num)) +
  
  geom_bar(stat="identity", fill = '#D6CFAC') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  
  ggtitle('Predicted vs. Actual Leaves Taken Nationally') +
  theme(plot.title = element_text(size=12, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  #   ylab('Leaves Taken (Millions)') +
  #   scale_y_continuous(labels = function(x) paste0(format(x/1000000, big.mark=",", scientific=FALSE), "M")) +
  
  xlab(NULL) + 
  scale_x_discrete(labels = c("Random","KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
  theme(axis.text.x = element_text(size = 10)) +
  
  geom_hline(yintercept = num_df$Actual_Leave_Num[1], size=1,color='#820023', linetype='longdash') +
  
  geom_text(aes(1.6,num_df$Actual_Leave_Num[1],
                label = paste0('Actual Leaves Taken:\n',
                               format(num_df$Actual_Leave_Num[1]/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                               , " M"),
                vjust = -.25,  hjust = 0),
            family='Arial',
            size=4,
            color='#820023') +
  
  geom_text(aes(x = Method, y = Predicted_Leave_Num + 3000000, 
                label =paste0(format(Predicted_Leave_Num/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                              , " M")), size=4, family="Arial", fontface="bold", vjust = 0) +
  geom_errorbar(aes(ymin=Predicted_Leave_Num-Predicted_SE*1.96, ymax=Predicted_Leave_Num+Predicted_SE*1.96), width=.2) 

#ggsave(file="./exhibits/4_num_leaves_tot.png", width=6.5, dpi=300)
ggsave(file="Exhibits/4_num_leaves_tot.png", width=6.5, dpi=300)

# add results to heat map df
append_row <- num_df['Predicted_Leave_Num']
rownames(append_row) <- num_df$Method
append_row <- data.frame(t(append_row))
append_row['Exhibit'] <- '7'
append_row['Method'] <- 'Predicted/Actual Number of Leaves Taken'
append_row['Type'] <- 'FMLA-to-FMLA Aggregate'
append_row['Actual'] <- num_df$Actual_Leave_Num[1]
heat_df <- rbind(heat_df, append_row)

#exhibt not used
# # --------------------------------------------------------------------------------------------------------------------
# # Exhibit 5 ---- Accuracy of predicted num_leaves values ------
# # --------------------------------------------------------------------------------------------------------------------
# 
# ggplot(data=num_df  %>% filter(Method != 'random'), aes(x=Method, y=Accuracy)) +
#   
#   geom_bar(stat="identity", fill = '#D6CFAC') +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
#   
#   ggtitle('Prediction Accuracy of Number Of Leaves Taken') +
#   theme(plot.title = element_text(size=12, hjust = 0.5,family='Arial',face='bold'), 
#         text=element_text(size = 10, family='Arial')) +
#   
#   ylab(NULL) +
#   theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
#   #   ylab('Prediction Accuracy') +
#   #   scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
#   
#   xlab(NULL) + 
#   scale_x_discrete(labels = c("KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
#   theme(axis.text.x = element_text(size = 10)) +
#   
#   geom_text(aes(x = Method, y = Accuracy + 0.03, label = sprintf("%.1f%%", 100 * Accuracy)), size=4, family="Arial", fontface="bold") +
#   geom_hline(yintercept = (num_df%>%filter(Method=='random')%>%select(Accuracy))[[1]], size=1,color='#820023', linetype='dashed') +
#   geom_text(aes(.6,(num_df%>%filter(Method=='random')%>%select(Accuracy))[[1]],
#                 label = paste0('Random\nAccuracy:\n',sprintf("%.1f%%", 100 * num_df%>%filter(Method=='random')%>%select(Accuracy))),
#                 vjust = 1,  hjust = 0),
#             family='Arial',
#             size=4,
#             color='#820023')
#   
# #ggsave(file="./exhibits/5_num_leaves_acc.png", width=6.5, dpi=300)
# ggsave(file="Exhibits/5_num_leaves_acc.png", width=6.5, dpi=300)
# 
# # add results to heat map df
# append_row <- num_df['Accuracy']
# rownames(append_row) <- num_df$Method
# append_row <- data.frame(t(append_row))
# append_row['Exhibit'] <- ''
# append_row['Method'] <- 'Leaves Taken Accuracy'
# append_row['Type'] <- 'FMLA-to-FMLA Individual'
# heat_df <- rbind(heat_df, append_row)

# --------------------------------------------------------------------------------------------------------------------
# Exhibit 6 ---- Comparing predicted vs. actual leave takers nationally
# --------------------------------------------------------------------------------------------------------------------

bi_df_filt <- bi_df %>% filter(Variable == 'pred_leave') %>%  mutate(Method = factor(Method, levels=c("random", "KNN_multi", "KNN1", "logit", "Naive_Bayes", "random_forest", "ridge_class")))
ggplot(data=bi_df_filt  %>% filter(Method != 'random'), aes(x=Method, y=Predicted_Positives)) +
  
  geom_bar(stat="identity", fill = '#D6CFAC') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  
  ggtitle('Predicted vs. Actual Leave Takers Nationally') +
  theme(plot.title = element_text(size=12, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  #    ylab('Leave Takers (Millions)') +
  #    scale_y_continuous(labels = function(x) paste0(format(x/1000000, big.mark=",", scientific=FALSE), "M")) +
  
  xlab(NULL) + 
  scale_x_discrete(labels = c("KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
  theme(axis.text.x = element_text(size = 10)) +
  
  geom_text(aes(x = Method, y = Predicted_Positives + 1800000, 
                label =paste0(format(Predicted_Positives/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                              , " M")), size=4, family="Arial", fontface="bold") +
  geom_errorbar(aes(ymin=Predicted_Positives-Predicted_SE*1.96*Total_Predictions, ymax=Predicted_Positives+Predicted_SE*1.96*Total_Predictions), width=.2)  +
  geom_hline(yintercept = bi_df_filt$Actual_Positives[1], size=1,color='#820023', linetype='longdash') +
  geom_text(aes(.6,bi_df_filt$Actual_Positives[1],
                label = paste0('Actual Leave Takers:\n',
                               format(bi_df_filt$Actual_Positives[1]/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                               , " M"),
                vjust = -.25,  hjust = 0),
            family='Arial',
            size=4,
            color='#820023')

#ggsave(file="./exhibits/6_leave_takers_tot.png", width=6.5, dpi=300)
ggsave(file="Exhibits/6_leave_takers_tot.png", width=6.5, dpi=300)

# add results to heat map df
append_row <- bi_df_filt['Predicted_Positives']
rownames(append_row) <- bi_df_filt$Method
append_row <- data.frame(t(append_row))
append_row['Exhibit'] <- '6'
append_row['Method'] <- 'Predicted/Actual Leaves Takers'
append_row['Type'] <- 'FMLA-to-FMLA Aggregate'
append_row['Actual'] <- bi_df_filt$Actual_Positives[1]
heat_df <- rbind(heat_df, append_row)

# --------------------------------------------------------------------------------------------------------------------
# Exhibit 6a ---- Comparing predicted vs. actual leave needers nationally
# --------------------------------------------------------------------------------------------------------------------

bi_df_filt <- bi_df %>% filter(Variable == 'pred_need')
ggplot(data=bi_df_filt %>%  mutate(Method = factor(Method, levels=c("random", "KNN_multi", "KNN1", "logit", "Naive_Bayes", "random_forest", "ridge_class"))), aes(x=Method, y=Predicted_Positives)) +
  
  geom_bar(stat="identity", fill = '#D6CFAC') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  
  # axis formatting
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  
  xlab(NULL) + 
  scale_x_discrete(labels = c("Random","KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
  theme(axis.text.x = element_text(size = 10)) +
  
  ggtitle('Predicted vs. Actual Leave Needers Nationally') +
  theme(plot.title = element_text(size=12, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  
  geom_text(aes(x = Method, y = Predicted_Positives + 1100000, 
                label =paste0(format(Predicted_Positives/1000000, big.mark=",", nsmall=1, digits=1, scientific=FALSE)
                              , " M")), size=4, family="Arial", fontface="bold") +
  geom_errorbar(aes(ymin=Predicted_Positives-Predicted_SE*1.96*Total_Predictions, ymax=Predicted_Positives+Predicted_SE*1.96*Total_Predictions), width=.2)  +
  geom_hline(yintercept = bi_df_filt$Actual_Positives[1], size=1,color='#820023', linetype='longdash') +
  geom_text(aes(1.6,bi_df_filt$Actual_Positives[1],
                label = paste0('Actual Leave Needers:\n',
                               format(bi_df_filt$Actual_Positives[1]/1000000, big.mark=",", nsmall=1, digits=1, scientific=FALSE)
                               , " M"),
                vjust = -.75,  hjust = 0),
            family='Arial',
            size=4,
            color='#820023')
ggsave(file="./exhibits/6a_leave_needers_tot.png", width=6.5, dpi=300)

# add results to heat map df
append_row <- bi_df_filt['Predicted_Positives']
rownames(append_row) <- bi_df_filt$Method
append_row <- data.frame(t(append_row))
append_row['Exhibit'] <- '9'
append_row['Method'] <- 'Predicted/Actual Leaves Needers'
append_row['Type'] <- 'FMLA-to-FMLA Aggregate'
append_row['Actual'] <- bi_df_filt$Actual_Positives[1]
heat_df <- rbind(heat_df, append_row)

# --------------------------------------------------------------------------------------------------------------------
# Exhibit 7a ---- Accuracy of predicted leave needers------
# --------------------------------------------------------------------------------------------------------------------

bi_df_filt <- bi_df %>% filter(Variable == 'pred_need')
ggplot(data=bi_df_filt %>% filter(Method != 'random'), aes(x=Method, y=Accuracy)) +
  geom_bar(stat="identity", fill = '#D6CFAC') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  
  # axis formatting
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  
  xlab(NULL) + 
  scale_x_discrete(labels = c("KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
  theme(axis.text.x = element_text(size = 10)) +
  
  # graph title
  ggtitle('Prediction Accuracy of who is a Leave Needer') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Accuracy + 0.06, 
                label = sprintf("%.1f%%", 100 * Accuracy)), size=4, family="Arial", fontface="bold") +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = (bi_df_filt%>%filter(Method=='random')%>%select(Accuracy))[[1]], size=1,color='#820023', linetype='longdash') +
  # label for reference line
  geom_text(aes(.6,(bi_df_filt%>%filter(Method=='random')%>%select(Accuracy))[[1]],
                label = paste0('Random\nAccuracy:\n',sprintf("%.1f%%", 100 * bi_df_filt%>%filter(Method=='random')%>%select(Accuracy))),
                vjust = 1,  hjust = 0),
            family='Arial',
            size=4,
            color='#820023',
            linetype='longdash')

ggsave(file="./exhibits/7a_leave_needers_acc.png", width=6.5, dpi=300)

# add results to heat map df
append_row <- bi_df_filt['Accuracy']
rownames(append_row) <- bi_df_filt$Method
append_row <- data.frame(t(append_row))
append_row['Exhibit'] <- '12'
append_row['Method'] <- 'Leaves Needers Accuracy'
append_row['Type'] <- 'FMLA-to-FMLA Individual'
append_row['Actual'] <- "N/A"
heat_df <- rbind(heat_df, append_row)

# --------------------------------------------------------------------------------------------------------------------
# Exhibit 7 ---- Accuracy of predicted leave takers ------
# --------------------------------------------------------------------------------------------------------------------

bi_df_filt <- bi_df %>% filter(Variable == 'pred_leave')
ggplot(data=bi_df_filt %>% filter(Method != 'random'), aes(x=Method, y=Accuracy)) +
  
  geom_bar(stat="identity", fill = '#D6CFAC') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  
  ggtitle('Prediction Accuracy of who is a Leave Taker') +
  theme(plot.title = element_text(size=12, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  #       ylab('Prediction Accuracy') +
  #       scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  
  xlab(NULL) + 
  scale_x_discrete(labels = c("KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
  theme(axis.text.x = element_text(size = 10)) +
  
  geom_text(aes(x = Method, y = Accuracy + 0.05, 
                label = sprintf("%.1f%%", 100 * Accuracy)), size=4, family="Arial", fontface="bold") +
  geom_hline(yintercept = (bi_df_filt%>%filter(Method=='random')%>%select(Accuracy))[[1]], size=1,color='#820023', linetype='dashed') +
  geom_text(aes(.6,(bi_df_filt%>%filter(Method=='random')%>%select(Accuracy))[[1]],
                label = paste0('Random\nAccuracy:\n',sprintf("%.1f%%", 100 * bi_df_filt%>%filter(Method=='random')%>%select(Accuracy))),
                vjust = 1,  hjust = 0),
            family='Arial',
            size=4,
            color='#820023')

#ggsave(file="./exhibits/7_leave_takers_acc.png", width=6.5, dpi=300)
ggsave(file="Exhibits/7_leave_takers_acc.png", width=6.5, dpi=300)

# add results to heat map df
append_row <- bi_df_filt['Accuracy']
rownames(append_row) <- bi_df_filt$Method
append_row <- data.frame(t(append_row))
append_row['Exhibit'] <- '10'
append_row['Method'] <- 'Leaves Takers Accuracy'
append_row['Type'] <- 'FMLA-to-FMLA Individual'
append_row['Actual'] <- "N/A"
heat_df <- rbind(heat_df, append_row)

# Precision and Recall exhibits pulled from report
# # --------------------------------------------------------------------------------------------------------------------
# # Exhibit 8 ---- Precision of predicted leave takers ------
# # --------------------------------------------------------------------------------------------------------------------
# 
# bi_df_filt <- bi_df %>% filter(Variable == 'pred_leave')
# ggplot(data=bi_df_filt %>% filter(Method != 'random'), aes(x=Method, y=Precision)) +
#   
#   geom_bar(stat="identity", fill = '#D6CFAC') +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
#   
#   ggtitle('Prediction Precision of who is a Leave Taker') +
#   theme(plot.title = element_text(size=12, hjust = 0.5,family='Arial',face='bold'), 
#         text=element_text(size = 10, family='Arial')) +
#   
#   ylab(NULL) +
#   theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
#   # ylab('Prediction Precision') +
#   # scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
#   
#   xlab(NULL) + 
#   scale_x_discrete(labels = c("KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
#   theme(axis.text.x = element_text(size = 10)) +
#   
#   geom_text(aes(x = Method, y = Precision + 0.05, 
#                 label = sprintf("%.1f%%", 100 * Precision)), size=4, family="Arial", fontface="bold")+
#   # reference line for actual prop_pay_average
#   geom_hline(yintercept = (bi_df_filt%>%filter(Method=='random')%>%select(Precision))[[1]], size=1,color='#820023', linetype='dashed') +
#   # label for reference line
#   geom_text(aes(.6,(bi_df_filt%>%filter(Method=='random')%>%select(Precision))[[1]],
#                 label = paste0('Random\nPrecision:\n',sprintf("%.1f%%", 100 * bi_df_filt%>%filter(Method=='random')%>%select(Precision))),
#                 vjust = 1,  hjust = 0),
#             family='Arial',
#             size=4,
#             color='#820023')
# 
# #ggsave(file="./exhibits/8_leave_takers_precision.png", width=6.5, dpi=300)
# ggsave(file="Exhibits/8_leave_takers_precision.png", width=6.5, dpi=300)
# 
# # --------------------------------------------------------------------------------------------------------------------
# # Exhibit 9 ---- Recall of predicted leave takers ------
# # --------------------------------------------------------------------------------------------------------------------
# 
# bi_df_filt <- bi_df %>% filter(Variable == 'pred_leave')
# ggplot(data=bi_df_filt %>% filter(Method != 'random'), aes(x=Method, y=Recall)) +
#   
#   geom_bar(stat="identity", fill = '#D6CFAC') +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
#   
#   ggtitle('Prediction Recall of who is a Leave Taker') +
#   theme(plot.title = element_text(size=12, hjust = 0.5,family='Arial',face='bold'), 
#         text=element_text(size = 10, family='Arial')) +
#   
#   ylab(NULL) +
#   theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
#   # ylab('Prediction Recall') +
#   # scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
#   
#   xlab(NULL) + 
#   scale_x_discrete(labels = c("KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
#   theme(axis.text.x = element_text(size = 10)) +
#   
#   geom_text(aes(x = Method, y = Recall + 0.05, 
#                 label = sprintf("%.1f%%", 100 * Recall)), size=4, family="Arial", fontface="bold")+
#   geom_hline(yintercept = (bi_df_filt%>%filter(Method=='random')%>%select(Recall))[[1]], size=1,color='#820023', linetype='dashed') +
#   geom_text(aes(.6,(bi_df_filt%>%filter(Method=='random')%>%select(Recall))[[1]],
#                 label = paste0('Random\nRecall:\n',sprintf("%.1f%%", 100 * bi_df_filt%>%filter(Method=='random')%>%select(Recall))),
#                 vjust = -.25,  hjust = 0),
#             family='Arial',
#             size=4,
#             color='#820023')
# 
# #ggsave(file="./exhibits/9_leave_takers_recall.png", width=6.5, dpi=300)
# ggsave(file="Exhibits/9_leave_takers_recall.png", width=6.5, dpi=300)

# --------------------------------------------------------------------------------------------------------------------
# Exhibit 10 ----- Estimated Benefit Outlays for RI for each method
# --------------------------------------------------------------------------------------------------------------------

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

ggplot(data=ri_df  %>%  mutate(Method = factor(Method, levels=c("random", "KNN_multi", "KNN1", "logit", "Naive_Bayes", "random_forest", "ridge_class")))
       , aes(x=Method, y=Estimate)) +
  # bar chart
  geom_bar(stat="identity", fill = '#D6CFAC') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  
  # axis formatting
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  
  xlab(NULL) + 
  scale_x_discrete(labels = c("Random","KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
  theme(axis.text.x = element_text(size = 10)) +
  
  # graph title
  ggtitle('Predicted vs. Actual Benefits Outlayed - Rhode Island') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Estimate + 100000000, 
                label =paste0("$",format(Estimate/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                              , "M")), size=4, family="Arial", fontface="bold") +
  geom_errorbar(aes(ymin=Estimate-SE*1.96, ymax=Estimate+SE*1.96), width=.2)  +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = 175659993, size=1,color='#820023', linetype='longdash') +
  # label for reference line
  # hardcoded from actual data
  geom_text(aes(4.65,175659993,
                label = paste0('Actual Benefits Outlayed\n (2012-2016 Annual Mean):\n$',
                               format(175659993/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                               , "M"),
                vjust = -.3,  hjust = 0),
            family='Arial',
            size=4,
            color='#820023')

ggsave(file="./exhibits/10_RI_bene_outlay_tot.png", width=6.5, dpi=300)

# add results to heat map df
append_row <- ri_df['Estimate']
rownames(append_row) <- ri_df$Method
append_row <- data.frame(t(append_row))
append_row['Exhibit'] <- '3'
append_row['Method'] <- 'RI Benefits Outlayed'
append_row['Type'] <- 'FMLA-to-ACS Aggregate'
append_row['Actual'] <- 175659993
heat_df <- rbind(heat_df, append_row)

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

ggplot(data=nj_df  %>%  mutate(Method = factor(Method, levels=c("random", "KNN_multi", "KNN1", "logit", "Naive_Bayes", "random_forest", "ridge_class")))
       , aes(x=Method, y=Estimate)) +
  # bar chart
  geom_bar(stat="identity", fill = '#D6CFAC') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  
  # axis formatting
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  
  xlab(NULL) + 
  scale_x_discrete(labels = c("Random","KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
  theme(axis.text.x = element_text(size = 10)) +
  
  # graph title
  ggtitle('Predicted vs. Actual Benefits Outlayed - New Jersey') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Estimate + 100000000, 
                label =paste0("$",format(Estimate/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                              , "M")),size=4, family="Arial", fontface="bold") +
  geom_errorbar(aes(ymin=Estimate-SE*1.96, ymax=Estimate+SE*1.96), width=.2)  +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = 506940000, size=1,color='#820023', linetype='dashed') +
  # label for reference line
  # hardcoded from a
  geom_text(aes(3.75,506940000,
                label = paste0('Actual Benefits Outlayed\n (2012-2016 Annual Mean):\n$',
                               format(506940000/1000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                               , "M"),
                vjust = -.3,  hjust = 0),
            family='Arial',
            size=4,
            color='#820023')

ggsave(file="./exhibits/11_NJ_bene_outlay_tot.png", width=6.5, dpi=300)


# add results to heat map df
append_row <- nj_df['Estimate']
rownames(append_row) <- nj_df$Method
append_row <- data.frame(t(append_row))
append_row['Exhibit'] <- '4'
append_row['Method'] <- 'NJ Benefits Outlayed'
append_row['Type'] <- 'FMLA-to-ACS Aggregate'
append_row['Actual'] <- 506940000
heat_df <- rbind(heat_df, append_row)

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

ggplot(data=ca_df  %>%  mutate(Method = factor(Method, levels=c("random", "KNN_multi", "KNN1", "logit", "Naive_Bayes", "random_forest", "ridge_class")))
       , aes(x=Method, y=Estimate)) +
  # bar chart
  geom_bar(stat="identity", fill = '#D6CFAC') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  
  # axis formatting
  ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  
  xlab(NULL) + 
  scale_x_discrete(labels = c('Random',"KNN Multi", "KNN1", "Logit", "Naive Bayes", "Random\nForest", "Ridge Class")) +
  theme(axis.text.x = element_text(size = 10)) +
  
  # graph title
  ggtitle('Predicted vs. Actual Benefits Outlayed - California') +
  # theme of plot text
  theme(plot.title = element_text(size=11, hjust = 0.5,family='Arial',face='bold'), 
        text=element_text(size = 10, family='Arial')) +
  # Data labels
  geom_text(aes(x = Method, y = Estimate + 2500000000, 
                label =paste0("$",format(Estimate/1000000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                              , "B")),size=4, family="Arial", fontface="bold") +
  geom_errorbar(aes(ymin=Estimate-SE*1.96, ymax=Estimate+SE*1.96), width=.2)  +
  # reference line for actual prop_pay_average
  geom_hline(yintercept = 5169800000, size=1,color='#820023', linetype='dashed') +
  # label for reference line
  # hardcoded from a
  geom_text(aes(3.75,5169800000,
                label = paste0('Actual Benefits Outlayed\n (2012-2016 Annual Mean):\n$',
                               format(5169800000/1000000000, big.mark=",", nsmall=1, digits=2, scientific=FALSE)
                               , "B"),
                vjust = -.3,  hjust = 0),
            family='Arial',
            size=4,
            color='#820023')

ggsave(file="./exhibits/12_CA_bene_outlay_tot.png", width=6.5, dpi=300)


# add results to heat map df
append_row <- ca_df['Estimate']
rownames(append_row) <- ca_df$Method
append_row <- data.frame(t(append_row))
append_row['Exhibit'] <- '5'
append_row['Method'] <- 'CA Benefits Outlayed'
append_row['Type'] <- 'FMLA-to-ACS Aggregate'
append_row['Actual'] <- 5169800000
heat_df <- rbind(heat_df, append_row)


# --------------------------------------------------------------------------------------------------------------------
# Heatmap
# --------------------------------------------------------------------------------------------------------------------
# reorder columns
heat_df <- heat_df %>% select(Exhibit, Method, Type, Actual, random, logit, KNN1, KNN_multi,random_forest, Naive_Bayes, ridge_class)
heat_df <- heat_df[order(heat_df$Exhibit),] 
  
# export to xlsx for touching up in excel
write.csv(heat_df, file="./exhibits/heat_map_raw.csv", row.names=FALSE)