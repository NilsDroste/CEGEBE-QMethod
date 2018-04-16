# this the analytical code for a comparative Q-Method study on CEGEBE
# script-authors: klara.johanna.winkler@uni-oldenburg.de, nils.droste@ufz.de

# v 0.1 (2018-02-06)

# 0 preps ----

# loading packages
library(qmethod)

# reading data #might need some specifications because we have had changes in the questionnaire while saving all to the same file
raw.data <- read.csv(paste(getwd(),"/data/A_QMethod_comparative_study_on_Circular_Green_and_BioEconomy_approaches_1_0.csv",sep = ""), header = T, sep = ';', nrows=13)

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



# 2 check factor loadings ----

# See the factor loadings
round(results$loa, digits = 2)

# See the flagged Q-sorts: those indicating 'TRUE'
results$flag

# Eigenvalues, total explained variability, and number of Q-sorts significantly loading
results$f_char$characteristics # apperently eigenvalues > 1 are the Kaiser (1960) rule for maintaining factors ...?



# 3 See the results ----
summary(results)

# Plot the z-scores for statements
# Statements are sorted from highest consensus (bottom) to highest disagreement (top).
# Filled symbols indicate distinguishing statements

plot(results)

# Reorder the statements from highest to lowest scores for each factor
# Put z-scores and factor scores together
scores <- cbind(round(results$zsc, digits=2), results$zsc_n)
nfactors <- ncol(results$zsc)
col.order <- as.vector(rbind(1:nfactors, (1:nfactors)+nfactors))
scores <- scores[col.order]
scores

# Order the table from highest to lowest z-scores for factor 1
scores[order(scores$zsc_f1, decreasing = T), ]
# (to order according to other factors, replace 'f1' for 'f2' etc.)

# Explore the table of distinguishing and consensus statements
# See a detailed explanation of this table in Zabala (2014, pp. 167-8).

# Full table
results$qdc



# 4 obtain consensus and distinguishing statements ----

# Consensus statements
results$qdc[which(results$qdc$dist.and.cons == "Consensus"), ]

# Statements distinguishing all factors
results$qdc[which(results$qdc$dist.and.cons == "Distinguishes all"), ]

# Statements distinguishing factor 1 (for results of > 2 factors)
results$qdc[which(results$qdc$dist.and.cons == "Distinguishes f1 only"), ]
# Statements distinguishing factor 2 (for results of > 2 factors)
results$qdc[which(results$qdc$dist.and.cons == "Distinguishes f2 only"), ]
# Statements distinguishing factor 3 (for results of > 2 factors)
results$qdc[which(results$qdc$dist.and.cons == "Distinguishes f3 only"), ]



# 5 TODOs ----

# check the bootstrapping algorithm fom Zabala, Unai (2016) @ http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0148087
