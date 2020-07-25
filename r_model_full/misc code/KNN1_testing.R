# ============
# KNN1_test.R 
# 8 Oct 2018
# Luke
# testing to see if KNN1 scratch performs same as canned package.
# ============

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
library(neighbr)
library(plyr)
library(dplyr)
# create sample data set
set.seed(124)

d=data.frame(row.names = seq(1,100))


xvars <- c("widowed", "divorced", "separated", "nevermarried", "female", 
           "agesq", "ltHS", "someCol", "BA", "GradSch", "black", 
           "white", "asian", "hisp","nochildren")
for (i in xvars) {
  d[i] <- runif(100)
  
  #because I scale training data and neighbr can't, need to have maximum value of 1
  # in each column in both training and test data to get same results
  d[sample(1:50,1),i]=1
  d[sample(51:100,1),i]=1
}
d$empid=as.numeric(rownames(d))
d$clabel=c(round(runif(100)))

d_train <- d[1:50,]

# only take xvars in d_test for canned
d_test <- d[51:100,c(1:(length(xvars)))]

# small change to test set to make compatable for scratch, adding empid column
d_test2 <- d[51:100,c(1:(length(xvars)+1))]

# canned neighbr package
d_can <- knn(train_set=d_train,test_set=d_test,k=1,categorical_target = "clabel", id="empid", comparison_measure = 'euclidean')

# my scratch algorithm
source("2a.KNN1_match.R")

d_scr <- KNN1_scratch(d_train, d_test2, "empid", "clabel", train_cond="TRUE", test_cond="TRUE")

#should be the same
print(table(d_can$test_set_scores==d_scr$clabel))