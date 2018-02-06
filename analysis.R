# this the analytical code for a comparative Q-Method study on CEGEBE
# script-authors: klara.johanna.winkler@uni-oldenburg.de, nils.droste@ufz.de

# v 0.1 (2018-02-06)

# 0 preps ----

# loading packages
library(qmethod)

# reading data
qsorts <- read.csv(paste(getwd(),"/data/TESTING_A_QMethod_comparative_study_on_Circular_Green_and_BioEconomy_approaches_1_0_TESTING.csv",sep = ""), header = T, sep = ';')