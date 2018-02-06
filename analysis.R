# this the analytical code for a comparative Q-Method study on CEGEBE
# script-authors: klara.johanna.winkler@uni-oldenburg.de, nils.droste@ufz.de

# v 0.1 (2018-02-06)

# 0 preps ----

# loading packages
library(qmethod)

# reading data #might need some specifications because we have had changes in the questionnaire while saving all to the same file
raw.data <- read.csv(paste(getwd(),"/data/TESTING_A_QMethod_comparative_study_on_Circular_Green_and_BioEconomy_approaches_1_0_TESTING.csv",sep = ""), header = T, sep = ';', nrows=13)

# check for duplicates
duplicated(raw.data[,3:38])

# transposing the dataframe to required format for qmethod #note that there still is some interesting data stored in raw.data
qsorts <- as.data.frame(t(raw.data[!duplicated(raw.data[,3:38]),3:38]))
colnames(qsorts) <- raw.data[!duplicated(raw.data[,3:38]),]$sid


# 1 analysis ----

# setting the variables
factors <- 3 # The number of factors to extract and rotate # TODO: understand what this is

# run the analysis
results <-  qmethod(qsorts, 
                    nfactors = factors,
                    rotation = "varimax", # Can be replaced by "none"
                    forced = TRUE)
