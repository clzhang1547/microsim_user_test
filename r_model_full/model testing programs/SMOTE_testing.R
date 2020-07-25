library(MASS)
library(plyr)
library(dplyr)
library(DMwR)
options(error=recover)

df <- read.csv('csv_inputs/dummy_data.csv')

df <- df %>% mutate(y_mod= ifelse(y>.95,1,0))
df$y_mod <- as.factor(df$y_mod) 
smote_df <- SMOTE(y_mod ~ x1 + x2 + x3,df)