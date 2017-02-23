R := R CMD BATCH --vanilla

rawData := $(wildcard ../data/LoanStats_2016Q*.csv)
cleanData := $(patsubst ../data/LoanStats_2016Q%.csv,\
	../cleanData/loanQ%.RData, $(rawData))

.PHONY: all
all: $(cleanData)

$(cleanData): dataClean.R
	bash import.sh

.PHONY: clean
clean:
	rm -rf *.Rout *.Rhistory
