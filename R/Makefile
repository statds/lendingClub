R := R CMD BATCH --vanilla

qID := 1 2 3 4
rawDataDir := ../rawData
rawData := $(patsubst %,../rawData/LoanStats_2016Q%.csv,$(qID))
rawData := $(patsubst %,../rawData/LoanStats_2016Q%.csv,$(qID))
cleanData := $(patsubst ../rawData/LoanStats_2016Q%.csv,\
	../cleanData/loanQ%.RData, $(rawData))


# .PHONY: all
# all:


.PHONY: dataCleaning
dataCleaning: $(cleanData)
$(cleanData): $(rawData) dataClean.R import.sh
	@bash import.sh


.PHONY: getRawData
getRawData: $(rawData)
$(rawData): | wgetData.sh $(rawDataDir)
	@bash wgetData.sh

$(rawDataDir):
	@mkdir -p $(rawDataDir)

.PHONY: clean
clean:
	rm -rf *.Rout *.Rhistory
