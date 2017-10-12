# # Subset abundance data by technical replicates and transform 
# SRM.Reps.A <- t(SRM.data.screened.noPRTC[,c(grepl("-A$", colnames(SRM.data.screened.noPRTC)))]) #A reps
# SRM.Reps.B <- t(SRM.data.screened.noPRTC[,c(grepl("-B$", colnames(SRM.data.screened.noPRTC)))]) #B reps
# SRM.Reps.C <- t(SRM.data.screened.noPRTC[,c(grepl("-C$", colnames(SRM.data.screened.noPRTC)))]) #C reps
# SRM.Reps.D <- t(SRM.data.screened.noPRTC[,c(grepl("-D$", colnames(SRM.data.screened.noPRTC)))]) #D reps
# 
# #Replace NAs with 0s
# SRM.Reps.A[is.na(SRM.Reps.A)] <- 0 
# SRM.Reps.B[is.na(SRM.Reps.B)] <- 0 
# SRM.Reps.C[is.na(SRM.Reps.C)] <- 0 
# SRM.Reps.D[is.na(SRM.Reps.D)] <- 0 
# 
# nSamples <- length(SRMsamples)
# nTransitions <- nrow(SRM.data.screened.noPRTC) #Number of transitions used
# A <- SRM.Reps.A[grepl(SRMsamples[i], rownames(SRM.Reps.A)),i]
# B <- SRM.Reps.B[grepl(SRMsamples[i], rownames(SRM.Reps.B)),i]
# C <- SRM.Reps.C[grepl(SRMsamples[i], rownames(SRM.Reps.C)),i]
# D <- SRM.Reps.D[grepl(SRMsamples[i], rownames(SRM.Reps.D)),i]

# Make a dataframe of filenames
# correlationFilenames <- data.frame(filenames = colnames(SRM.Reps.A),modifier = rep(".jpeg", ncol(SRM.Reps.A))) 
# correlationFilenames$full <- paste(correlationFilenames$filenames, correlationFilenames$modifier) #Merge the two columns together in a third column. This column has the full filename that will be used

# SRMsamples[1]
# SRM.Reps.A[grepl(SRMsamples[1], rownames(SRM.Reps.A)),]
# SRM.Reps.A[grep("G013", rownames(SRM.Reps.A)),]

########## My attempt above failed. So, I downloaded the abundance data, replaced sample names with simple G###-1, G###-2, G###-3, G###-4 names, not skipping any numbers. Then I reimported and proceeded. 

# Need to figure out how to generate a database with R values for each transition
# This data does not include all reps, since I had 3 & 4 reps for several samples, some of which are much better aligned than those used in this quick analysis.

SRM.data.screened.noPRTC.t <- t(SRM.data.screened.noPRTC[,c(!grepl("Protein Name|Transition|Peptide Sequence|Fragment Ion", colnames(SRM.data.screened.noPRTC)))])
write.csv(SRM.data.screened.noPRTC.t, "~/Downloads/SRM.data.screened.noPRTC.t.csv")
# EDITED IN EXCEL QUICKLY! This needs to be updated
SRM.data.screened.noPRTC.t.edited <- read.csv(url("https://github.com/RobertsLab/Paper-DNR-Geoduck-Proteomics/raw/master/data/SRM/SRM.data.screened.noPRTC.t.csv"), na.strings = "NA", header=TRUE, row.names = 1)

# Subset abundance data by technical replicates and transform
SRM.Reps.1 <- SRM.data.screened.noPRTC.t.edited[c(grepl("-1$", rownames(SRM.data.screened.noPRTC.t.edited))),] #1 reps
SRM.Reps.2 <- SRM.data.screened.noPRTC.t.edited[c(grepl("-2$", rownames(SRM.data.screened.noPRTC.t.edited))),] #2 reps
SRM.Reps.3 <- SRM.data.screened.noPRTC.t.edited[c(grepl("-3$", rownames(SRM.data.screened.noPRTC.t.edited))),] #3 reps
SRM.Reps.4 <- SRM.data.screened.noPRTC.t.edited[c(grepl("-4$", rownames(SRM.data.screened.noPRTC.t.edited))),] #4 reps

#Replace NAs with 0s
SRM.Reps.1[is.na(SRM.Reps.1)] <- 0
SRM.Reps.2[is.na(SRM.Reps.2)] <- 0
SRM.Reps.3[is.na(SRM.Reps.3)] <- 0
SRM.Reps.4[is.na(SRM.Reps.4)] <- 0

nrow(SRM.Reps.1)
nrow(SRM.Reps.2)
nrow(SRM.Reps.3)
nrow(SRM.Reps.4) #dataframes must have same dimensions when run through linear model in R

# Make a dataframe of filenames
dir.create("~/Downloads/Tech-rep-regressions/")
setwd(dir = "~/Downloads/Tech-rep-regressions/") #Change working directory so files are saved in the same directory as the R script
getwd() #Confirm changes
correlationFilenames <- data.frame(filenames = colnames(SRM.Reps.1),modifier = rep(".jpeg", ncol(SRM.Reps.1))) 
correlationFilenames$full <- paste(correlationFilenames$filenames, correlationFilenames$modifier) #Merge the two columns together in a third column. This column has the full filename that will be used
nSamples <- length(SRMsamples)
nTransitions <- nrow(SRM.data.screened.noPRTC) #Number of transitions used

# transitionModel.test <- lm(SRM.Reps.2[,i] ~ SRM.Reps.1[,i]) #Predict Replicate 2 from Replicate 1
# plot(x= SRM.Reps.1[,i], 
#      y = SRM.Reps.2[,i], 
#      xlab = "Replicate 1 Area", ylab = "Replicate 2 Area", 
#      main = correlationFilenames$filename[i], type = "n") #Create plot, but do not plot points
# text(x = SRM.Reps.1[,i], y = SRM.Reps.2[,i], labels = rownames(SRM.Reps.1), cex = 0.7) #Plot sample ID instead of points
# abline(transitionModel.test, col = "red") #Plot regression
# legend("topleft", bty = "n", legend = paste("R2 =", format(summary(transitionModel)$adj.r.squared, digits=4))) #Plot R-squared value

for(i in 1:nTransitions) { #For all transitions
  transitionModel <- lm(SRM.Reps.2[,i] ~ SRM.Reps.1[,i]) #Predict Replicate 2 from Replicate 1
  fileName <- correlationFilenames$full[i] #Set filename choice as the ith entry
  jpeg(filename = fileName, width = 1000, height = 1000) #Save .jpeg using set filename
  plot(x= SRM.Reps.1[,i], 
       y = SRM.Reps.2[,i], 
       xlab = "Replicate 1 Area", ylab = "Replicate 2 Area", 
       main = correlationFilenames$filename[i], type = "n") #Create plot, but do not plot points  
  text(x = SRM.Reps.1[,i], y = SRM.Reps.2[,i], labels = rownames(SRM.Reps.1), cex = 0.7) #Plot sample ID instead of points
  abline(transitionModel, col = "red") #Plot regression
  legend("topleft", bty = "n", legend = paste("R2 =", format(summary(transitionModel)$adj.r.squared, digits=4))) #Plot R-squared value
  dev.off() #Turn off plotting mechanism
}

