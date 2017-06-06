setwd("/home/xueting.shao/lendingClub")

#######################################################
##This is used to sample data targeting unbalance problem
#######################################################

##before sampling shuffle data order in train
train = train[sample(nrow(train)),]
table(train$y)
#0      1 
#11459 220280 
#X2(0)     X1 (1)
#11459 220280 

#======down sampling
library(caret)
set.seed(9560)
down_train <- downSample(x = train[, !colnames(train) %in% "y"],
                         y = train$y)
table(down_train$Class)
#X2    X1 
#11459 11459

##up and smote is not used yet
#======up sampling
set.seed(9560)
up_train <- upSample(x = train[, !colnames(train) %in% "y"],
                     y = train$y)
table(up_train$Class)
#0      1 
#220280 220280

#======SMOTE
library(DMwR)
set.seed(9560)
smote_train <- SMOTE(y ~ ., data  = train, perc.over = 100, perc.under = 100)  
table(smote_train$label) 
save.image(file = "../data/copy.RData")

