#!/bin/bash
# echo "Input: ../data/*.csv"
# echo "Output: ../cleanData/*.RData"
for qID in {1..3}
do
    date
    echo "Data cleaning for quarter $qID"
    R CMD BATCH --vanilla '--args '$qID'' dataClean.R
    echo "Raw data for quarter $qID were imported and cleaned."
    mv dataClean.Rout dataClean_$qID.Rout
done
date
echo "All done!"
