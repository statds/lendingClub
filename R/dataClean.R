################################################################################
### import and process raw data
################################################################################


## read arguments from terminal
args <- commandArgs(trailingOnly = TRUE)
qID <- eval(parse(text = args[1L]))


## attach functions
source("../utils_template.R")
source("functions.R")


## attach package needed
need.packages("stringi")


### import csv file
## need skip the first line url and and last four useless rows
rawDatFile <- paste0("../data/LoanStats_2016Q", qID, ".csv")
nRow <- system(paste("wc -l", rawDatFile), intern = TRUE)
## exclude the frist row, header, and last four rows (totally 6 rows)
nRow <- as.numeric(strsplit(nRow, split = " ")[[1]][1]) - 6L
loan <- rawDat <- read.table(rawDatFile, header = TRUE, sep = ",",
                             skip = 1, nrows = nRow)
rawName <- paste0("rawDat", qID)
assign(rawName, rawDat)
save(list = rawName, file = paste0("../cleanData/rawDat", qID, ".RData"))
rm(rawDat, list = rawName); gc()


### main text information needs processing?
## desc: Loan description provided by the borrower
loan$desc <- trimws(as.character(loan$desc))
## as.numeric(table(loan$desc))
## most of levels are missing, useless
loan$desc <- NULL

## urls cannot be accessed without internal account.
## pymnt_plan is "n" always
loan$url <- loan$pymnt_plan <- NULL


### id: A unique LC assigned ID for the loan listing
## may be kept but should not be considered in the model
## similar for member_id: A unique LC assigned Id for the borrower member
stopifnot(nrow(loan) == nRow)
stopifnot(nRow == length(unique(loan$id)))
stopifnot(nRow == length(unique(loan$member_id)))
loan$member_id <- NULL
loan$id <- seq_len(nRow)


### create binary response indicating whether the loan was fully funded
## a loan was fully funded if and only if loan_amnt == funded_amnt_inv?

## https://help.lendingclub.com/hc/en-us/articles/213757368-Partially-funded-loans
## Nearly all loans receive full investor backing—and many are fully backed in a
## few days. But since we’re a credit marketplace, there are times when loans
## aren’t backed by investors for the full amount of your loan offer.

## not TRUE for quarter 2?
## stopifnot(with(loan, all.equal(loan_amnt, funded_amnt)))

with(loan, summary(loan_amnt - funded_amnt_inv))
loan$funded_amnt <- NULL                 # remove funded_amnt
loan$y <- as.integer(with(loan, loan_amnt == funded_amnt_inv))

## rate of loan getting fully funded
mean(loan$y)
## 0.9709342, pretty high
## the correct prediction rate is supposed to be higher than this number?

## term
with(loan, table(term))

## normalize loan_amnt, installment, revol_bal, last_pymnt_amnt
loan$loan_amnt <- scale01(loan$loan_amnt)
loan$installment <- scale01(loan$installment)
loan$revol_bal <- scale01(loan$revol_bal)
loan$last_pymnt_amnt <- scale01(loan$last_pymnt_amnt)

## re-scale int_rate: Interest Rate on the loan
## by maximum-minimum standardization
loan$int_rate0 <- as.numeric(gsub("%", "", loan$int_rate))
summary(loan$int_rate0)
loan$int_rate0 <- scale01(loan$int_rate0)
## remove useless variables
loan$int_rate <- NULL

## either grade or sub_grade should be used, not of both of them
## or convert them into numeric scale since the relationship
## should be monotonic, which means subjects with better grade
## should be more likely to get fully funded
xtabs(~ sub_grade + grade, loan)
## tmp <- sort(levels(loan$sub_grade))
## loan$numGrade <- as.numeric(factor(loan$sub_grade, levels = tmp))
## ## standardize
## loan$numGrade <- scale01(loan$numGrade)


### emp_title: main text part needs processing
## trim the leading and trailing white-space, lower case
loan$emp_title <- trimws(tolower(as.character(loan$emp_title)))
## extract all non-letter characters
letterVec <- do.call(c, strsplit(loan$emp_title, ""))
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
titleList <- gsub(nonLetters, " ", loan$emp_title)
## job titles in words for each loan
titleList <- stri_split_regex(titleList, " ", omit_empty = TRUE)

## divide jobs into 10 different groups based on mog
mogList <- grepMog()
mog <- mogScore(titleList, mogList)
loan$mog <- factor(mog, levels = seq_len(10),
                     labels = c(letters[c(seq_len(8L), 11L)], "others"))
(tmpTab <- table(loan$mog))
prop.table(tmpTab) * 100


### purpose: not too many levels, no need to further process
(tmpTab <- table(loan$purpose))
prop.table(tmpTab) * 100

## only 1 wedding, and 70 renewable_energy, so merge them to group other
loan$purpose <- ifelse(loan$purpose %in% c("wedding", "renewable_energy"),
                         "other", as.character(loan$purpose))
loan$purpose <- factor(loan$purpose)
## relevel purpose by "debt_consolidation"
loan$purpose <- relevel(loan$purpose, ref = "debt_consolidation")


### emp_length
table(loan$emp_length)


### home_ownership
loan$home_ownership <- tolower(loan$home_ownership)

xtabs(~ y + home_ownership, data = loan)
##    home_ownership
## y     any mortgage   own  rent
##   0     0     1365   343  1173
##   1     6    45396 11982 38855

## remove 6 any cases or combine them with mortgage?
## loan <- subset(loan, ! home_ownership %in% "any")
## let's combine them with mortgage
loan$home_ownership <- ifelse(loan$home_ownership == "any", "mortgage",
                                as.character(loan$home_ownership))
loan$home_ownership <- factor(loan$home_ownership)

## annual_inc, take log(x + 1) and standardize it
loan$annual_inc <- as.numeric(scale01(log1p(loan$annual_inc)))


### save the processed loana
if (! dir.exists("../cleanData")) dir.create("../cleanData")
outName <- paste0("loanQ", qID)
assign(outName, loan)
save(list = outName, file = paste0("../cleanData/", outName, ".RData"))
