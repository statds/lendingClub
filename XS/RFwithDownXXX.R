#######################################################
##This is used to model a random forest with down-sampling data
#######################################################
library(caret)

##=======================================
##1.Rough model
##=======================================
##features will be considered in modeling
f_down = colnames(down_train)[!colnames(down_train) %in% c(fs_no_use, "quarter")]
#some NA's 
summary(down_train[, f_down])
##find out colnums with NA's
f_down_na = rep(FALSE, length(f_down))
i = 1
for( f in f_down){
  f_down_na[i] = any(is.na(down_train[, f]))
  i = i + 1
}
f_down_nas = f_down[f_down_na]

##get the nlevel of features with NA's
f_down_nas_levels = sapply(f_down_nas, 
                           function(x) nlevels(factor(down_train[, x])))
###par(mfrow = c( 2, 2))
###sapply(f_down_nas, function(x) plot( down_train[, x] ~ down_train$Class, main = x))
###par(mfrow = c( 1, 1))

###library(woe)
###sapply(f_down_nas, 
###       function(x) woe(Data = down_train, x, TRUE, "Class", 5, Bad = 0, Good = 1))

#sd of each numeric features
f_down_sd = apply(down_train[, f_down], 2, FUN = stats::sd, na.rm = TRUE)
plot(f_down_sd)
f_down_sd[f_down_sd == 0]
##large variance in sd scales -> consider standardization?

##just delete those features with NA's and get a initial model
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

set.seed(5627)
fit1 <- train(Class ~ ., data = down_train[, f_down[!f_down_na]], 
              method = "rf",
              ntree = 50,
              metric = "ROC",
              trControl = ctrl)
fit1$results

library(randomForest)
fit1.0 <- randomForest(Class ~., data = down_train[, f_down[!f_down_na]], 
                       importance = TRUE,
                       ntree = 100)
fit1$results
fit1$bestTune
fit1$finalModel
importance(fit1$finalModel)
varImpPlot(fit1$finalModel, n.var = 15)
pred1_train = predict(fit1, down_train, type = "raw")
confusionMatrix(data = pred1_train, reference = down_train$Class)
#almost perfect
pred1_validation = predict(fit1, validate, type = "raw")
confusionMatrix(data = pred1_validation, reference = validate$y)
#Confusion Matrix and Statistics
#            Reference
#Prediction    X2    X1
#X2  2805 11936
#X1    76 84302
#Accuracy : 0.8788          
#95% CI : (0.8768, 0.8808)
#No Information Rate : 0.9709          
#P-Value [Acc > NIR] : 1               
#Kappa : 0.2835          
#Mcnemar's Test P-Value : <2e-16          
                                          
#            Sensitivity : 0.97362         
#            Specificity : 0.87597         
#         Pos Pred Value : 0.19029         
#         Neg Pred Value : 0.99910         
#             Prevalence : 0.02907         
#         Detection Rate : 0.02830         
#   Detection Prevalence : 0.14872         
#      Balanced Accuracy : 0.92480         
                                          
#       'Positive' Class : X2  
pred1_validate_prob = predict(fit1, validate, type = "prob")
probTwoClassPlot(pred1_validate_prob$X1, validate$y,
                 titleT = "Fit1: Random Forest with 50 trees")

#roc curve
roc1 <- roc(validate$y, pred1_validate_prob$X1,
            levels = c("X1", "X2"))
plot(roc1)
#Area under the curve: 0.9794

#it's not that good, dive to see any highly related features

##=======================================
##2.Rough model with feature selection by pearson-correlation
##=======================================
#correlation of each numeric feature pairs
class_down_train = sapply(f_down[!f_down_na], function(f) class(down_train[,f]))
table(class_down_train)
#factor integer numeric 
#16      39      17
for (f in names(class_down_train)[class_down_train == "integer"]){
  down_train[, f] = as.numeric(down_train[, f])
}
class_down_train = sapply(f_down[!f_down_na], function(f) class(down_train[,f]))
table(class_down_train)

corr_down = cor(down_train[, names(class_down_train)[class_down_train == "numeric"]])

big_cor_down = highCor(corr_down, 0.8)
summary(big_cor_down)
fs_big_count = count(c(big_cor_down[, 3], big_cor_down[,4]))
fs_big_count[ order(fs_big_count$freq, decreasing = TRUE), ]

fs_delete = fs_big_count$x[!(fs_big_count$x %in%
c("recoveries", "total_pymnt", "out_prncp", "funded_amnt", "open_acc", "num_actv_rev_tl",
"tot_cur_bal","total_bal_ex_mort","revol_bal","num_bc_tl","num_op_rev_tl")), drop = TRUE]

f_down_nna_small0.8 = f_down[!f_down_na]
f_down_nna_small0.8 = f_down_nna_small0.8[ !f_down_nna_small0.8 %in% fs_delete]

##fit another one
set.seed(5627)
fit2 <- train(Class ~ ., data = down_train[, f_down_nna_small0.8], 
              method = "rf",
              ntree = 50,
              metric = "ROC",
              trControl = ctrl)
fit2$results
fit2$bestTune
fit2$finalModel
importance(fit2$finalModel)
varImpPlot(fit2$finalModel, n.var = 15)
pred2_train = predict(fit2, down_train, type = "raw")
confusionMatrix(data = pred2_train, reference = down_train$Class)

pred2_validation = predict(fit2, validate, type = "raw")
confusionMatrix(data = pred2_validation, reference = validate$y)
#Confusion Matrix and Statistics
#           Reference
#Prediction    X2    X1
#X2  2660 21517
#X1   221 74721
#Accuracy : 0.7807          
#95% CI : (0.7781, 0.7833)
#No Information Rate : 0.9709          
#P-Value [Acc > NIR] : 1               
#Kappa : 0.1526          
#Mcnemar's Test P-Value : <2e-16          
#            Sensitivity : 0.92329         
#            Specificity : 0.77642         
#         Pos Pred Value : 0.11002         
#         Neg Pred Value : 0.99705         
#             Prevalence : 0.02907         
#         Detection Rate : 0.02684         
#   Detection Prevalence : 0.24392         
#      Balanced Accuracy : 0.84985         
#       'Positive' Class : X2 
##comments: it's even worse...
pred2_validate_prob = predict(fit2, validate, type = "prob")
probTwoClassPlot(pred2_validate_prob$X1, validate$y,
                 titleT = "Fit2: Random Forest with 50 trees")
##the plot is not good given the low percentages of extreme probs(0.0 & 1.0)

#roc curve
roc2 <- roc(validate$y, pred2_validate_prob$X1,
            levels = c("X1", "X2"))
plot(roc2)
#Area under the curve: 0.9028
