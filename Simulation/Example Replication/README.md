# Example simulation code (less simulation rounds compared with the full simulation code, and it is able to run in
# your local laptop within reasonable time)

To successfully run the codes in this part to generate Table 1 and Table 2 in a similar version, please make sure you have the following packages installed in R.

- dplyr
- netmeta
- parallel
- foreach
- doParallel
- doRNG
- DEoptimR
- OssaNMA

## Folder 'Simulation for Table 1'

File `runall.R` outputs 20 rows in Table 1 one by one. It outputs 20 csv files and saves them in `Example Replication/Simulation for Table1/table/`. 

A similar/example version of Table 1 in the manuscript is built up by these 20 csv files. Each csv file represents one row in Table 1. 

## Folder 'Simulation for Table 2'

File `runall.R` outputs 8 rows in Table 2 one by one. It outputs 8 csv files and saves them in `Example Replication/Simulation for Table2/table/`. 

A similar/example version of Table 2 in the manuscript is built up by these 8 csv files. Each csv file represents one row in Table 2. 




