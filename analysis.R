# this the analytical code for a comparative Q-Method study on CEGEBE
# script-authors: klara.johanna.winkler@uni-oldenburg.de, nils.droste@ufz.de

# v 0.1 (2018-02-06)

# 0 preps ----

# loading packages
library(qmethod)

# reading data #might need some specifications because we have had changes in the questionnaire while saving all to the same file
raw.data <- read.csv(paste(getwd(),"/data/A_QMethod_comparative_study_on_Circular_Green_and_BioEconomy_approaches_1_0.csv",sep = ""), header = T, sep = ';')
statements <- read.csv(paste(getwd(),"/data/statements.csv",sep = ""),header=T, stringsAsFactors = F,sep=",")

# check for duplicates
duplicated(raw.data[,3:38])

# transposing the dataframe to required format for qmethod #note that there still is some interesting data stored in raw.data
qsorts <- as.data.frame(t(raw.data[!duplicated(raw.data[,3:38]),3:38]))
colnames(qsorts) <- raw.data[!duplicated(raw.data[,3:38]),]$sid



# 1 standard analysis ----


# one method to define number of factors: scree from https://www.statmethods.net/advstats/factor.html 
library(nFactors)
# TODO: check because eigenvalues computed by this method are NOT equal to the one computed by qmethod
# Reason: eigenvals in qmethod()/qfacharact() are computed as colSums(as.data.frame(unclass(principal(cor(qsorts, method = "pearson"), nfactors = 2, rotate = "varimax")$loadings))^2) which is the same as what is reported as "SS loadings" in principal() and is basically sum of squared loading 
ap <- parallel(subject=nrow(qsorts),var=ncol(qsorts), rep=100,cent=.05)
ev <- eigen(cor(qsorts, method = "pearson"))
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea, model="components")
plotnScree(nS) # says 2 factors only

#compute eigenvalues and explained variance for a variety of factors
l <- list()
for (i in 2:9){
  tmp <-  qmethod(qsorts, 
                      nfactors = i,
                      rotation = "varimax", # Can be replaced by "none"
                      forced = TRUE)
  s <- summary(tmp)
  cat("Q resulst for ", i, " factors: \n")
  explVar <- sum(s[4,])
  eigVal <- s[3,]
  l[[i-1]] <- list()
  l[[i-1]]$factors <- i
  l[[i-1]]$explVar <-  explVar
  l[[i-1]]$eigVal <-  eigVal
  l[[i-1]]$flags <- tmp$flagged
}
# l shows that 8 factors have all factor eigenvalues > 1 and expl. variance increases. At the same time, only up to 3 factors at least 2 respondends are flagged for each factor. We thus choose 3 factors. 

# setting the variables
factors <- 3 # The number of factors to extract and rotate
  
# run the chosen analysis
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

#png("factor_plot.png", width=1500, height=1000, units="px",res=150) #used for printing out results to png
plot(results)
#dev.off() #needed for printing plot

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
#View(results$qdc)

# 4 obtain consensus and distinguishing statements ----

# Consensus statements
results$qdc[which(results$qdc$dist.and.cons == "Consensus"), ]

# Statements distinguishing all factors
results$qdc[which(results$qdc$dist.and.cons == "Distinguishes all"), ]

#(for results of > 2 factors)
# Statements distinguishing factor 1 
results$qdc[which(results$qdc$dist.and.cons == "Distinguishes f1 only"), ]
# Statements distinguishing factor 2 
results$qdc[which(results$qdc$dist.and.cons == "Distinguishes f2 only"), ]
# Statements distinguishing factor 3 
results$qdc[which(results$qdc$dist.and.cons == "Distinguishes f3 only"), ]


# 5 write out data to xls ----

library(xlsx)
res <- results$qdc
rownames(res) <- statements[,2]
write.xlsx(res,"QMethodResults.xls", sheetName="Results")

rownames(scores) <- statements[,2]
write.xlsx(scores,"QMethod_z-scores.xls", sheetName="Z-Scores")


write.xlsx(as.data.frame(results$flag),"QMethods_Flags.xls", sheetName="Flags")


# 6 bootstrap ----

# TODO: check the bootstrapping algorithm fom Zabala, Unai (2016) @ http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0148087

set.seed(456) #setting seed / random number for reproducibility reasons
bs_results <- qmboots(qsorts,nfactors = factors,nsteps=1000) #at least sample size times 40 steps, takes time. This is the bootstrapping
qmb.summary(bs_results) # gives the results
# cbind(qmb_sum$qsorts[,10:12],as.data.frame(results$flag)[order(rownames(as.data.frame(results$flag))),]) # to compare automatic flagging by standard qmethod and bootstrapping. Only flag for statement 2071129 should apparently be set for factor 2 or 3, but none has a high flagging freq

# plotting of results by either factors or z-scors, and sorted by difference or standard dev.
qmb.plot(qmb.summary(bs_results),type="loa",nfactors = 3,sort="difference")
qmb.plot(qmb.summary(bs_results),type="loa",nfactors = 3,sort="sd")
qmb.plot(qmb.summary(bs_results),type="zsc",nfactors = 3,sort="difference")
qmb.plot(qmb.summary(bs_results),type="zsc",nfactors = 3,sort="sd")


qmb.summary(bs_results)$statements#[,13:21]
qmb.summary(bs_results)$qsorts

#results based on bootstrapping

# consensus and distinguishing factors
res.bts <- qdc(qsorts,nfactors=factors,qmb.summary(bs_results)$statements[,c(4,6,8)], qfcharact(as.data.frame(unclass(principal(cor(qsorts, method = "pearson"),nfactors = factors, rotate="varimax")$loadings)),qflag(qmb.summary(bs_results)$qsorts[,c(4,6,8)],36),qmb.summary(bs_results)$statements[,c(4,6,8)],3,qmb.summary(bs_results)$qsorts[,c(4,6,8)])$sd_dif) # complicated form of accessing comparable results to standard qmethod algorithm...

#scores
scores.bts <- as.data.frame(cbind(round(qmb.summary(bs_results)$statements[,4],2),qmb.summary(bs_results)$statements[,16],round(qmb.summary(bs_results)$statements[,6],2),qmb.summary(bs_results)$statements[,17],round(qmb.summary(bs_results)$statements[,8],2),qmb.summary(bs_results)$statements[,18]))

#flags
flags_bts <- qflag(qmb.summary(bs_results)$qsorts[,c(4,6,8)],36)

#write out data
rownames(res.bts) <- statements[,2]
write.xlsx(res.bts,"QMethodResults_bootstrap.xls", sheetName="Bootstrap results")

names(scores.bts) <- c("zsc_f1_bts", "fsc_f1_bts", "zsc_f2_bts", "fsc_f2_bts", "zsc_f3_bts", "fsc_f3_bts")
rownames(scores.bts) <- statements[,2]
write.xlsx(scores.bts,"QMethod_z-scores_bootstrap.xls", sheetName="Bootstrap Scores")

write.xlsx(as.data.frame(flags_bts),"QMethods_Flags_bootstrap.xls", sheetName="Bootstrap Flags")

png("Results_Zscores_Bootstrap.png", width = 600, height = 600, units = "px")
qmb.plot(qmb.summary(bs_results),type="zsc",nfactors = 3,sort="difference")
dev.off()

png("Results_Loadings_Bootstrap.png", width = 600, height = 600, units = "px")
qmb.plot(qmb.summary(bs_results),type="zsc",nfactors = 3,sort="difference")
dev.off()
