################################################################################
### regularized logistics regression model
### with lasso type penalty
################################################################################


## source functions
source("functions.R")

## attach packages needed (will install any missing package automatically)
needpack <- c("glmnet", "pROC", "ggplot2", "grDevices")
ipak(needpack)

## load processed data
load("../cleanData/loanQ3.RData")


## call the simple wrapper function for glmnet
## where the tuning parameter lambda is determined by 10-folds cross-validation
f1 <- y ~ loan_amnt + term + installment + emp_length + home_ownership +
    annual_inc + verification_status + purpose + delinq_2yrs + open_acc +
    pub_rec + revol_bal + total_acc + initial_list_status + inq_last_12m +
    int_rate0 + grade + mog
logis <- glmnet2(f1, data = loanQ3, family = "binomial")


## save the results
if (! dir.exists("../results/")) dir.create("../results/")
save(logis, file = "../results/logis.RData")


## output png
if (! dir.exists("figs")) dir.create("figs")
png(file = "figs/lambda.png", width = 700, height = 500)
par(mar = c(2.5, 2.5, 0, 0), mgp = c(1.5, 0.5, 0)) # remove margin
plot(logis[[2L]])
dev.off()


## prediction on training set
predQ3 <- predict2(logis, s = "lambda.1se", type = "response")

## ROC curve and auc value
(rocQ3 <- roc(loanQ3$y, predQ3, percent = TRUE, ci = TRUE))
save(rocQ3, file = "../results/rocQ3.RData")


## set up some nice-looking colors
colVec <- gg_color_hue(3)

## output ROC plot
rocDat3 <- as.data.frame(rocQ3[1:4])
p3 <- ggplot(rocDat3, aes(x = (100 - specificities) / 100,
                          y = sensitivities / 100)) +
    geom_line(color = colVec[1]) +
    geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "3313") +
    geom_hline(yintercept = 0.9, color = colVec[2], linetype = 2) +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    ggtitle("2016 Quarter 3") +
    theme_bw()


### prediction for quarter 1
load("../cleanData/loanQ1.RData")
matListQ1 <- glmnet2(f1, loanQ1, predOnly = TRUE)
newx1 <- matListQ1$xMat
predQ1 <- predict2(glmnetObj = logis[[2L]], newx = newx1,
                   s = "lambda.1se", type = "response")
(rocQ1 <- roc(matListQ1$y, predQ1, percent = TRUE, ci = TRUE))
save(rocQ1, file = "../results/rocQ1.RData")

## output ROC plot
rocDat1 <- as.data.frame(rocQ1[1:4])
p1 <- ggplot(rocDat1, aes(x = (100 - specificities) / 100,
                          y = sensitivities / 100)) +
    geom_line(color = colVec[1]) +
    geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "3313") +
    geom_hline(yintercept = 0.9, color = colVec[2], linetype = 2) +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    ggtitle("2016 Quarter 1") +
    theme_bw()


### prediction for quarter 2
load("../cleanData/loanQ2.RData")
matListQ2 <- glmnet2(f1, loanQ2, predOnly = TRUE)
newx2 <- matListQ2$xMat
predQ2 <- predict2(glmnetObj = logis[[2L]], newx = newx2,
                   s = "lambda.1se", type = "response")
(rocQ2 <- roc(matListQ2$y, predQ2, percent = TRUE, ci = TRUE))
save(rocQ2, file = "../results/rocQ2.RData")

## output ROC plot
rocDat2 <- as.data.frame(rocQ2[1:4])
p2 <- ggplot(rocDat2, aes(x = (100 - specificities) / 100,
                          y = sensitivities / 100)) +
    geom_line(color = colVec[1]) +
    geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "3313") +
    geom_hline(yintercept = 0.9, color = colVec[2], linetype = 2) +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    ggtitle("2016 Quarter 2") +
    theme_bw()



## combine plots
png("figs/roc.png", width = 800, height = 500)
multiplot(p1, p2, p3, cols = 3)
dev.off()
