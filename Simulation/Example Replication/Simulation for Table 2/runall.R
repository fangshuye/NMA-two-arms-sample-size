
####### 8 csv files are generate from the following codes ######################################################
####### each of them is one row of Table 2 #####################################################################
####### Note: this is EXAMPLE code, so the simulation round is set to be less than 10,000 ######################
####### The result is SIMILAR to Table 2 but WON'T be the SAME #################################################
####### If it still runs slowly on your laptop, try to reduce the parameter 'nrep' to reduce the running time,##
####### If so, the results will be less similar to Table 2 as 'nrep' is reduced. ##############################
####### If you don't mind running for a while longer, feel free to increase the parameter 'nrep'###############
####### If so, the results will be more similar to Table 2 as 'nrep' is increased. ############################
####### Before running the script, make sure you have the packages listed in README.md installed ###############

## 1. simulation result for uneven rows in Table 2
source(file = "./functions/uneven_allocation_function.R")

## simulation rounds = 100, log odds ratio = 0.299
## it takes 3 mins to run it 
uneven_sim_res(r = 1, nrep = 100) # output table is saved in ./table/table2_uneven_lor_1.csv

## simulation rounds = 100, log odds ratio = 0.4
## it takes 3 mins to run it 
uneven_sim_res(r = 2, nrep = 100) # output table is saved in ./table/table2_uneven_lor_2.csv

## simulation rounds = 100, log odds ratio = 0.5
## it takes 3 mins to run it 
uneven_sim_res(r = 3, nrep = 100) # output table is saved in ./table/table2_uneven_lor_3.csv

## simulation rounds = 100, log odds ratio = 0.6
## it takes 3 mins to run it 
uneven_sim_res(r = 4, nrep = 100) # output table is saved in ./table/table2_uneven_lor_4.csv

## 2. simulation result for even rows in Table 2

source(file = "./functions/even_allocation_function.R")

## simulation rounds = 100, log odds ratio = 0.299
## it takes 3 mins to run it 
even_sim_res(r = 1, nrep = 100) # output table is saved in ./table/table2_even_lor_1.csv

## simulation rounds = 200, log odds ratio = 0.4
## it takes 6 mins to run it 
even_sim_res(r = 2, nrep = 200) # output table is saved in ./table/table2_even_lor_2.csv

## simulation rounds = 100, log odds ratio = 0.5
## it takes 3 mins to run it 
even_sim_res(r = 3, nrep = 100) # output table is saved in ./table/table2_even_lor_3.csv

## simulation rounds = 100, log odds ratio = 0.6
## it takes 3 mins to run it 
even_sim_res(r = 4, nrep = 100) # output table is saved in ./table/table2_even_lor_4.csv
