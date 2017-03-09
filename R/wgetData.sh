#!/bin/bash

# set directory for raw data
dir=../rawData

# the url of lending club
home_url=https://resources.lendingclub.com

printf "Downloading raw data from lending club for year 2016...\n\n"
for qID in {1..4}
do
    wget -N -P $dir $home_url/LoanStats_2016Q$qID.csv.zip
    unzip -o -qq $dir/LoanStats_2016Q$qID.csv.zip -d $dir
    printf "Downloaded and uncompressed quarter $qID data\n\n"
done
printf "Raw data were all downloaded and uncompressed to $dir\n\n"
