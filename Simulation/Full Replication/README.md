# full simulation code

To successfully run the codes in this part to generate Table 1 and Table 2, please make sure you have the following packages installed in R.

- dplyr
- netmeta
- parallel
- foreach
- doParallel
- doRNG
- DEoptimR
- OssaNMA

## Folder Simulation for Table 1

File `run.sh` runs 20 analysis (R files) in parallel. It outputs 20 csv files and saves them in `Simulation for Table1/table/`. 

Table 1 in the manuscript is built up by the 20 csv files in `Simulation for Table1/table/`. Each csv file represents one row in Table 1. 

Running time: I run `run.sh` on Pronto Cluster and it takes 1.5 ~ 2 hours to generate each csv file. Since I run each analysis runs in parallel so each csv file is generated around the same time. Please feel free to check `Termial Saved Output` for how I connect to the Pronto and run `run.sh` in the terminal. 

## Folder Simulation for Table 2

File `run.sh` runs the even and uneven allocation for 4 different log odds ratios (lor) in parallel. It outputs 8 csv files and saves them in `Simulation for Table2/table/`. 

Table 2 in the manuscript is built up by the 8 csv files in `Simulation for Table2/table/`. Each csv file represents one row in Table 2. 

Running time: I run `run.sh` on Pronto Cluster and it takes 1.5 ~ 2 hours to generate each csv file. Since I run each analysis runs in parallel so each csv file is generated around the same time. 



