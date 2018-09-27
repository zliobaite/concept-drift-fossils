Readme
=======

This repository is to accompany a research article:
I. Zliobaite (2018). Concept drift over geological times: predictive modeling baselines for analyzing the mammalian fossil record. Data mining and knowledge discovery. In press.

##Datasets##

###Present day###

*Occurence data which species occur in which sites
    data_present_occurence.csv
*Climate data - what's the productivity at each site
    data_present_climate.csv
*Dental traits of species
    data_present_teeth.csv
*Nearest relative species for each species
    data_nearest_relatives.csv

###Fossils###

*Occurence data which fossil species occur at which sites
    data_fossil_occurence.csv
*Dental traits of fossil species
    data_fossil_teeth.csv

Fossil data is derived from the Turkana dataset (Fortelius et al 2016), version 47 (which is the published version).

Fossil dental trait dataset is based on version 6 (this reference is for internal data tracking purposes).

Present day dental trait dataset is based on version 10 (this reference is for internal data tracking purposes).

###Code###

The code is given as is and will reproduce the plots reported in the paper. There are extra experiments in the code that are not reported in the paper. Running modeling variants may require changing the parameters within the script.
run1, run2, run3 indicates the running sequence.

##References##

Fortelius, M., Žliobaitė, I., Kaya, F., Bibi, F., Bobe, R., Leakey, L., Leakey, M., Patterson, D., Rannikko, J., Werdelin, L. (2016). An ecometric analysis of the fossil mammal record of the Turkana Basin. Philosophical Transactions B 371(1698), p. 1-13.
