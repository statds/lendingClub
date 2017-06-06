setwd("/Users/xueting.shao/Documents/Xueting/DS/lendingClub/R")
#######################################################
##This is a rough version of data clean from Xueting
#######################################################

##=======================================
##1.Load Data & simple analysis
##=======================================
rawDatFile3 <- "../data/LoanStats_2016Q3.csv" # path to raw csv file
nRow3 <- system(paste("wc -l", rawDatFile3), intern = TRUE) #99125
## exclude the frist row, header, and last four rows (totally 6 rows)
nRow3 <- as.numeric(strsplit(nRow3, split = " ")[[1L]][4L]) - 6L
##the first url line was deleted already
loanQ3 <- read.table(rawDatFile3, header = TRUE, sep = ",", nrows = nRow3)

rawDatFile1 <- "../data/LoanStats_2016Q1.csv"
rawDatFile2 <- "../data/LoanStats_2016Q2.csv"
rawDatFile4 <- "../data/LoanStats_2016Q4.csv"

nRow1 <- system(paste("wc -l", rawDatFile1), intern = TRUE) #133892
nRow2 <- system(paste("wc -l", rawDatFile2), intern = TRUE) #
nRow4 <- system(paste("wc -l", rawDatFile4), intern = TRUE) #

nRow1 <- as.numeric(strsplit(nRow1, split = " ")[[1L]][3L]) - 6L
nRow2 <- as.numeric(strsplit(nRow2, split = " ")[[1L]][4L]) - 6L
nRow4 <- as.numeric(strsplit(nRow4, split = " ")[[1L]][3L]) - 6L

loanQ1 <- read.table(rawDatFile1, header = TRUE, sep = ",", nrows = nRow1)
loanQ2 <- read.table(rawDatFile2, header = TRUE, sep = ",", nrows = nRow2)
loanQ4 <- read.table(rawDatFile4, header = TRUE, sep = ",", nrows = nRow4)

loans = rbind(loanQ1, loanQ2, loanQ3, loanQ4)
loans = transform(loans, quarter = c( rep(1, nRow1), rep(2, nRow2), rep(3, nRow3), rep(4, nRow4)))
rm(loanQ1, loanQ2, loanQ3, loanQ4)

### create binary response indicating whether the loan was fully funded
## a loan was fully funded if and only if loan_amnt == funded_amnt_inv?
with(loans, summary(loan_amnt - funded_amnt_inv))

loans$y <- as.integer(with(loans, loan_amnt == funded_amnt_inv))
table(loans$y)
#0      1 
#20889 413514 
 
mean(loans$y)
#0.9519133

##For this project, we are trying to predict whether the loan will be fully funded,
##according to the fully funded rate, this is an balance problem : 
##  obs no. of classA is much bigger than that of classB

##for this kind of problem:
##(1)data level(sampling)
##  down-sampling, upp-samppling, smote
##(2)model level
## cost-sensitive learning i.e. assign different cost for mislabeling the minor class and majority class

##given a tree model, it doesn't matter if some features (like int_rate) are treated as numeric/factor
##so, i will not transform some factor features into numeric here

##=======================================
##2.Feature-wise Cleaning
##=======================================
table(loans$emp_title) #Nlp
table(loans$loan_status) #is it the result of loan get fully funded?
table(loans$desc)

table(loans$pub_rec)
table(loans$collections_12_mths_ex_med)
table(loans$mths_since_last_major_derog)
table(loans$policy_code) #only one level
table(loans$dti_joint)
table(loans$verification_status, loans$verification_status_joint) #this two are different...
table(loans$acc_now_delinq)

table(loans$last_credit_pull_d, loans$y)
#useless feature
fs_no_use = c("id", "member_id", "issue_d", "pymnt_plan","url", "zip_code", "addr_state",
               "collections_12_mths_ex_med", "policy_code",
              "emp_title", "loan_status", "desc",
              "revol_bal_joint", "sec_app_earliest_cr_line", "sec_app_inq_last_6mths", 
              "sec_app_mort_acc", "sec_app_open_acc", "sec_app_revol_util", "sec_app_open_il_6m", "sec_app_num_rev_accts",
              "sec_app_chargeoff_within_12_mths", "sec_app_collections_12_mths_ex_med", "sec_app_mths_since_last_major_derog")

##2.1NLP process
##Note: codes below are copied from wenjie's dataClean.R
### emp_title: main text part needs processing
## trim the leading and trailing white-space, lower case
loans$emp_title <- trimws(tolower(as.character(loans$emp_title)))
## extract all non-letter characters
letterVec <- do.call(c, strsplit(loans$emp_title, ""))
ind <- ! letterVec %in% c(letters, LETTERS)
nonLetters <- unique(letterVec[ind])
## escape $ * + . ? [ ] ^ { } | ( ) , by add leading \\
escapeSet <- unlist(strsplit("$ * + . ? [ ] ^ { } | ( ) \\",
                             split = " ", fixed = TRUE))
nonLetters <- sapply(nonLetters, function(a) {
  if (! a %in% escapeSet)
    return(a)
  out <- paste0("\\", a, collapse = "")
  if (a == "\\\\")
    paste0("\\\\", out, collapse = "")
  out
})
nonLetters <- paste0(nonLetters, collapse = "|")
titleList <- gsub(nonLetters, " ", loans$emp_title)
## job titles in words for each loan
titleList <- stri_split_regex(titleList, " ", omit_empty = TRUE)

## divide jobs into 10 different groups based on mog
mogList <- grepMog() #from wenjie's function
mog <- mogScore(titleList, mogList) #from wenjie's function
loans$mog <- factor(mog, levels = seq_len(10),
                   labels = c(letters[c(seq_len(8L), 11L)], "others"))
(tmpTab <- table(loans$mog))
prop.table(tmpTab) * 100

##2.2 decide factor/numeric features
colnames(loans)[sapply(colnames(loans), function(x) class(loans[, x]) == "factor")]
loans$y = factor(loans$y)
summary(loans[,colnames(loans)[sapply(colnames(loans), function(x) class(loans[, x]) == "factor")]])

loans$int_rate = sapply(loans$int_rate, function(x) 
  as.numeric(gsub("[^0-9.]",replacement = "", as.character(x))))
#int_rate should be xx%

loans$revol_util = sapply(loans$revol_util, function(x) 
  as.numeric(gsub("[^0-9.]",replacement = "", as.character(x))))
#revol_util should be xx%

#date type:
#(1)
loans$earliest_cr_line = as.Date(paste0("01-",loans$earliest_cr_line), format = "%d-%b-%Y")
loans$earliest_cr_line[1:100]
min(loans$earliest_cr_line)
#[1] "1933-03-01"
loans$earliest_cr_line = difftime(loans$earliest_cr_line, min(loans$earliest_cr_line), units = "days")
loans$earliest_cr_line = as.numeric(loans$earliest_cr_line)

#(2)
levels(loans$issue_d)
#(3)
levels(loans$last_pymnt_d)
#(4)
levels(loans$next_pymnt_d)
#(5)
levels(loans$last_credit_pull_d)

#convert factor levels into valid R variable name for later model use
for (f in colnames(loans)) {
  if (class(loans[[f]])=="factor") {
    levels <- unique(c(loans[[f]]))
    loans[[f]] <- factor(loans[[f]],
                         labels = make.names(levels))
  }
}

##=======================================
##3.Divide into Train, Validate, Test
##=======================================
table(loans$quarter)
train = loans[loans$quarter == 1 | loans$quarter == 2,]
table(train$quarter)

validate = loans[loans$quarter == 3,]
test = loans[loans$quarter == 4,]

train$y = factor(train$y)
validate$y = factor(validate$y)
test$y = factor(test$y)

#convert factor levels into valid R variable name for later model use
for (f in colnames(train)) {
  if (class(train[[f]])=="factor") {
    levels <- unique(c(train[[f]]))
    train[[f]] <- factor(train[[f]],
                               labels = make.names(levels))
  }
}

for (f in colnames(validate)) {
  if (class(validate[[f]])=="factor") {
    levels <- unique(c(validate[[f]]))
    validate[[f]] <- factor(validate[[f]],
                         labels = make.names(levels))
  }
}

for (f in colnames(test)) {
  if (class(test[[f]])=="factor") {
    levels <- unique(c(test[[f]]))
    test[[f]] <- factor(test[[f]],
                            labels = make.names(levels))
  }
}



save(list = c("loans", "ind", "letterVec", "mog", "mogList", "titleList"), file = "../data/LoansDF.RData")
rm(loans, ind, letterVec, mog, mogList, titleList)



