
####### 20 csv files are generate from the following codes ######################################################
####### each of them is one row of Table 1 #####################################################################
####### Note: this is EXAMPLE code, so the simulation round is set to be less than 10,000 ######################
####### The result is SIMILAR to Table 1 but WON'T be the SAME #################################################
####### If it still runs slowly on your laptop, try to reduce the parameter 'nrep' to reduce the running time,##
####### If so, the results will be less similar to Table 2 as 'nrep' is reduced. ##############################
####### If you don't mind running for a while longer, feel free to increase the parameter 'nrep'###############
####### If so, the results will be more similar to Table 2 as 'nrep' is increased. ############################
####### Before running the script, make sure you have the packages listed in README.md installed ###############

## load functions 
source(file = "./functions/sim_each_row_function.R")

## simulation rounds = 100, output the 1st row in Table 1
## it takes 3 mins to run it 
sim_res_each_row(r = 1, nrep = 100) # output table is saved in ./table/table1_row_1.csv

## simulation rounds = 100, output the 2nd row in Table 1
## it takes 3 mins to run it 
sim_res_each_row(r = 2, nrep = 100) # output table is saved in ./table/table1_row_2.csv

## simulation rounds = 100, output the 3rd row in Table 1
## it takes 3 mins to run it 
sim_res_each_row(r = 3, nrep = 100) # output table is saved in ./table/table1_row_3.csv

## simulation rounds = 100, output the 4th row in Table 1
## it takes 3 mins to run it 
sim_res_each_row(r = 4, nrep = 100) # output table is saved in ./table/table1_row_4.csv

## simulation rounds = 100, output the 5th row in Table 1
## it takes 3 mins to run it 
sim_res_each_row(r = 5, nrep = 100) # output table is saved in ./table/table1_row_5.csv

## simulation rounds = 100, output the 6th row in Table 1
## it takes 3 mins to run it 
sim_res_each_row(r = 6, nrep = 100) # output table is saved in ./table/table1_row_6.csv

## simulation rounds = 100, output the 7th row in Table 1
## it takes 3 mins to run it 
sim_res_each_row(r = 7, nrep = 100) # output table is saved in ./table/table1_row_7.csv

## simulation rounds = 100, output the 8th row in Table 1
## it takes 3 mins to run it 
sim_res_each_row(r = 8, nrep = 100) # output table is saved in ./table/table1_row_8.csv

## simulation rounds = 200, output the 9th row in Table 1
## it takes 6 mins to run it 
sim_res_each_row(r = 9, nrep = 200) # output table is saved in ./table/table1_row_9.csv

## simulation rounds = 100, output the 10th row in Table 1
## it takes 3 mins to run it 
sim_res_each_row(r = 10, nrep = 100) # output table is saved in ./table/table1_row_10.csv

## simulation rounds = 200, output the 11th row in Table 1
## it takes 6 mins to run it 
sim_res_each_row(r = 11, nrep = 200) # output table is saved in ./table/table1_row_11.csv

## simulation rounds = 200, output the 12th row in Table 1
## it takes 6 mins to run it 
sim_res_each_row(r = 12, nrep = 200) # output table is saved in ./table/table1_row_12.csv

## simulation rounds = 200, output the 13th row in Table 1
## it takes 6 mins to run it 
sim_res_each_row(r = 13, nrep = 200) # output table is saved in ./table/table1_row_13.csv

## simulation rounds = 200, output the 14th row in Table 1
## it takes 6 mins to run it 
sim_res_each_row(r = 14, nrep = 200) # output table is saved in ./table/table1_row_14.csv

## simulation rounds = 100, output the 15th row in Table 1
## it takes 3 mins to run it 
sim_res_each_row(r = 15, nrep = 100) # output table is saved in ./table/table1_row_15.csv

## simulation rounds = 100, output the 16th row in Table 1
## it takes 3 mins to run it 
sim_res_each_row(r = 16, nrep = 100) # output table is saved in ./table/table1_row_16.csv

## simulation rounds = 50, output the 17th row in Table 1
## it takes 1.5 mins to run it 
sim_res_each_row(r = 17, nrep = 50) # output table is saved in ./table/table1_row_17.csv

## simulation rounds = 50, output the 18th row in Table 1
## it takes 1.5 mins to run it 
sim_res_each_row(r = 18, nrep = 50) # output table is saved in ./table/table1_row_18.csv

## simulation rounds = 50, output the 19th row in Table 1
## it takes 1.5 mins to run it 
sim_res_each_row(r = 19, nrep = 50) # output table is saved in ./table/table1_row_19.csv

## simulation rounds = 50, output the 20th row in Table 1
## it takes 1.5 mins to run it 
sim_res_each_row(r = 20, nrep = 50) # output table is saved in ./table/table1_row_20.csv
